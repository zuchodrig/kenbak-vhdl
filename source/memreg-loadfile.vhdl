library IEEE;
use IEEE.std_logic_1164.all,  std.textio.all, IEEE.numeric_std.all;
use work.myfuncs.all;

architecture memreg_loadfile of memreg is
  
  signal data : std_logic_vector(depth*2-1 downto 0 );
  signal data_2 : std_logic_vector(depth*2-1 downto 0 );
  signal counter : natural range 0 to depth-1;

  procedure memset(signal data_sgn : inout std_logic_vector(depth*2-1 downto 0 ); constant addr: natural; constant val: natural range 0 to 255) is
  begin
      data_sgn(addr*8+7 downto addr*8) <= std_logic_vector (to_unsigned(val, 8));
  end procedure;

    subtype type_byte is bit_vector(0 to 7);               -- bit 0 is msb
    type memory_array is 
      array (natural range 0 to depth*2-1) of natural;

    procedure load(mem : out memory_array) is
      
      file binary_file : text is in "kenbak.out";
      variable L : line;
      variable addr : natural;
      variable onebyte : natural range 0 to 255;
      
      procedure read_hex_natural(Ll : inout line; addrl : out natural) is
        variable result : natural := 0;
        variable ch : character;
      begin
        for i in 1 to 2 loop
          read(Ll, ch);
          if ('0' <= ch and ch <= '9') then
            result := result*16 + character'pos(ch) - character'pos('0');
          else
            result := result*16 + character'pos(ch) - character'pos('a') + 10;
          end if;
        end loop;
        addrl := result;
      end read_hex_natural;
      
      procedure read_hex_byte(Ll : inout line; word : out natural range 0 to 255) is
        --variable result : byte;
        variable result : natural range 0 to 255;
        variable digit, r : natural := 0;
        variable ch : character;
      begin
        read(Ll, ch);  -- the space between addr and data
        result := 0;
        for i in 4 to 5 loop
          read(Ll, ch);
          if ('0' <= ch and ch <= '9') then
            digit := character'pos(ch) - character'pos('0');
          else
            digit := character'pos(ch) - character'pos('a') + 10;
          end if;
          result := result * 16;
          result := result + digit;
          --result(r to r+3) := natural_to_bv(digit, 4);
          r := r + 4;
        end loop;
        word := result;
      end read_hex_byte;
      
    begin
      while not endfile(binary_file) loop
        readline(binary_file, L);
        read_hex_natural(L, addr);
        read_hex_byte(L, onebyte);
        --
        write(L, addr);
        write(L, ' ');
        write(L, onebyte);
        writeline(output, L);
        --
        mem(addr) := onebyte;
      end loop;
    end load;


  -- signal mem_array: memory_array;

begin
  process(clk, rst)
  variable L : line;
  variable cnt : natural;
  variable adr: natural;
  variable out_digit, temp_cnt : natural;
  variable new_data, old_data :std_logic_vector(7 downto 0);
  variable offset : natural;
  variable mem_array: memory_array;

  begin
    if rst = '1'
    then
      load(mem_array);
      -- L register holds address of next byte, so counting starts from -1
      counter <= depth-8;
      for k in 0 to depth*2-1 loop
        data(k) <= '0';
        data_2(k) <= '0';
      end loop;
      write(L, string'("reg rst:"));
      writeline(output, L);

      memset(data, 0, 16#f3#);
      memset(data, 1, 40);
      memset(data, 2, 16#0#);
      for i in 0 to 255 loop
        memset(data, i, mem_array(i));
      end loop;

    elsif rising_edge(clk)
    then
       --d_out <= data(0);
       --for k in 1 to depth-1 loop
       --  data(k-1) <= data(k);
       --end loop;
       --data(depth-1) <= d_in;

       cnt := counter + 1;
       if cnt > depth - 1 then
         cnt := 0;
       end if;

       out0 <= data(cnt);
       out1 <= data(cnt + depth);

       --if data(counter) /= d_in then
         write(L, string'("reg ID="));
         write(L, string'(" d_out: "));
         write(L, to_char(data(counter)));
         
         write(L, string'(" in0: "));
         write(L, to_char(in0));
         write(L, string'(" in1: "));
         write(L, to_char(in1));
       --end if;

       data(counter) <= ite(in0 = '1','1','0');
       data(counter+depth) <= ite(in1 = '1','1','0');

       write(L, string'(" reg_counter="));
       write(L, counter);

       if (0>1) --if (counter/8 *8=counter)
	     then
         temp_cnt := counter;
         temp_cnt := temp_cnt + 8;
         if temp_cnt >= depth then
           temp_cnt := temp_cnt - depth;
         end if;
           write(L, string'(" data["));
           write(L,  counter/8 );
           write(L, string'("]="));
           --write(L, data[counter]
           write(L, raw_format_slv_hex(data(counter+7 downto counter)));

          writeline(output, L);
       end if; -- 0>1
       writeline(output, L);
	   
       --write(L, string'("mem[0]="));
       --write(L, raw_format_slv_hex(data(7 downto 0)));

        for slice in 0 to 1 loop
          offset := slice*depth;
          if counter/8*8 = counter and counter > 7 then
            old_data := data_2(offset+counter-1 downto offset+counter-8);
            new_data := data(offset+counter-1 downto offset+counter-8);
            if new_data /= old_data then
              write(L, string'("wrote mem["));
              write(L, counter/8 - 1 + offset/8);
              write(L, string'("] "));

              write(L, string'("was hex "));
              write(L, raw_format_slv_hex(old_data));
              write(L, string'(" := hex "));
              write(L, raw_format_slv_hex(new_data));
              writeline(output, L);
            end if;
          end if;
        end loop;

       if counter = depth-1 then
         write(L, string'("memdump of reg"));
         writeline(output, L);

         for p in 0 to depth*2/8 - 1 loop
          if data(p*8+7 downto p*8) /= data_2(p*8+7 downto p*8) then
            write(L, string'("mem["));
            write(L, p);
            write(L, string'("] "));

            write(L, string'("was hex "));
            write(L, raw_format_slv_hex(data_2(p*8+7 downto p*8)));
            write(L, string'(" := hex "));
            write(L, raw_format_slv_hex(data(p*8+7 downto p*8)));
            write(L, string'(" bitstring: "));
            write(L, to_bstring( data(p*8+7 downto p*8) ));

            writeline(output, L);
          end if;
        end loop;
       writeline(output, L);
       end if;

       if cnt = 0 then
         data_2 <= data;
       end if;

       counter <= cnt;
    end if;
  end process;
end architecture memreg_loadfile;
