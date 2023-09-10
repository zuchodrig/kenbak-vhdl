library IEEE;
use IEEE.std_logic_1164.all,  std.textio.all, IEEE.numeric_std.all;
use work.myfuncs.all;

architecture memory_loadfile of memory is

    constant low_address : natural := 0;
    constant high_address : natural := mem_size - 1;
    
    subtype byte is std_logic_vector(7 downto 0);
    
    type memory_array is
      array (natural range low_address to high_address) of byte;

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
        --write(L, addr);
        --write(L, ' ');
        --write(L, onebyte);
        --writeline(output, L);
        --
        
        mem(addr) := std_logic_vector (to_unsigned(onebyte, 8));
      end loop;
    end load;


begin
  process(rst, address, neg_OE, neg_WE)
  variable L : line;
  variable cnt : natural;
  variable adr: natural;
  variable out_digit, temp_cnt : natural;
  variable new_data, old_data :std_logic_vector(7 downto 0);
  variable offset : natural;
  variable mem_array: memory_array;
  variable addr : natural;
  variable is_undefined : std_logic;
  begin
    if rst = '1'
    then
      load(mem_array);
    else
      is_undefined := '0';
      for i in 0 to 7 loop
        is_undefined := is_undefined xor address(i);
      end loop;

      --write(L, string'(" is_undefined:"));
      --write(L,  to_char(is_undefined) );
      --writeline(output, L);

      if is_undefined = 'U' then
        addr := 0;
      else
        addr := to_integer(unsigned(address));
      end if;

      --write(L, string'(" mem(161)= "));
      --write(L,  to_bstring(mem_array(161)) );
      --writeline(output, L);


      if neg_we = '0'
      then
        write(L, string'(" tristating:"));
        write(L,  to_bstring(data) );
        writeline(output, L);

        data <= "ZZZZZZZZ";
      end if;

      if neg_oe = '0'
      then
        if neg_we = '1' 
        then
          write(L, string'(" reading addr= "));
          write(L,  (addr) );
          write(L, string'(" result= "));
          write(L,  to_bstring(mem_array(addr)) );
          writeline(output, L);
          data <= mem_array(addr);
        end if;
      end if;

      if rising_edge(neg_we)
      then
        writeline(output, L);
        if is_undefined = 'U' then
          assert is_undefined /= 'U' report "Writing to undefined address" severity failure;
        end if;
        mem_array(addr) := data;
        write(L, string'(" rising edge WE , writing by (addr, data): ("));
        write(L,  addr );
        write(L, string'(", "));
        write(L,  to_integer(unsigned(data)) );
        write(L, string'(") "));
        writeline(output, L);
        if neg_oe = '0'
        then
          data <= mem_array(addr);
        else
          data <= "ZZZZZZZZ";
        end if;

      end if;

    end if;
  end process;
end architecture memory_loadfile;
