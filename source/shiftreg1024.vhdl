
library IEEE;
use IEEE.std_logic_1164.all,  std.textio.all, IEEE.numeric_std.all;


-- this is the entity
entity lolANDGATE is
  port ( 
    I1 : in std_logic;
    I2 : in std_logic;
    O  : out std_logic);
end entity lolANDGATE;

-- this is the architecture
architecture lolRTL of lolANDGATE is
begin
  O <= I1 and I2;
end architecture lolRTL;

library IEEE;
use IEEE.std_logic_1164.all,  std.textio.all, IEEE.numeric_std.all;
use work.myfuncs.all;

entity shiftreg1024 is
  generic (depth : integer ; internal_id : integer);
  port ( 
    d_in : in std_logic;
    d_out : out std_logic;
    rst : in std_logic;
    clk  : in std_logic);
end entity shiftreg1024;

architecture shiftreg1024_impl of shiftreg1024 is
  signal data : std_logic_vector(depth-1 downto 0 );
  signal data_2 : std_logic_vector(depth-1 downto 0 );
  signal counter : natural range 0 to depth-1;

  procedure memset(signal data_sgn : inout std_logic_vector(depth-1 downto 0 ); constant addr: natural; constant val: natural range 0 to 255) is
  --procedure memset (addr: natural; vdal: natural) is
  begin
      data_sgn(addr*8+7 downto addr*8) <= std_logic_vector (to_unsigned(val, 8));
  end procedure;

  procedure prog_16bitadd_testdata(signal data_sgn : inout std_logic_vector(depth-1 downto 0 )) is
  begin
      memset(data_sgn, 250-128, 230);
      memset(data_sgn, 251-128, 240);
      memset(data_sgn, 230-128, 16#CD#);
      memset(data_sgn, 231-128, 16#34#);
      memset(data_sgn, 240-128, 16#EF#);
      memset(data_sgn, 241-128, 16#12#);
  end procedure;

  
  procedure prog_16bitadd(signal data_sgn : inout std_logic_vector(depth-1 downto 0 ); constant startpos : natural range 0 to 255 ) is
  variable addr : natural range 0 to 255;
  variable addr_A, addr_B :  natural range 0 to 255;
  begin
      addr_A := 250;
      addr_B := 251;
      --(250) address of A, (251) address of B, little-endian
      --result placed in A
      
      addr := startpos;

      memset(data_sgn, addr, 16#93#);  -- ld X, 1
      memset(data_sgn, addr+1, 1);
      addr := addr + 2;
      
      --memset(data_sgn, addr, 16#1F#);  -- st  A, ((250)+X) indirect-indx
      --memset(data_sgn, addr+1, addr_A);  
      --addr := addr + 2;
      
      
      memset(data_sgn, addr, 16#15#);  -- ld A, (250) indirect
      memset(data_sgn, addr+1, addr_A);  
      addr := addr + 2;

      memset(data_sgn, addr, 16#05#);  -- add A, (251) indirect
      memset(data_sgn, addr+1, addr_B);  
      addr := addr + 2;

      --memset(data_sgn, addr, 16#54#);  -- LD B, (129) -- get carry bit
      --memset(data_sgn, addr+1, 8#201#);  
      --addr := addr + 2;


      memset(data_sgn, addr, 16#1D#);  -- st (250) indirect A, 
      memset(data_sgn, addr+1, addr_A);  
      addr := addr + 2;

      memset(data_sgn, addr, 16#17#);  -- ld A, ((250)+X) indirect
      memset(data_sgn, addr+1, addr_A);  
      addr := addr + 2;

      memset(data_sgn, addr, 16#07#);  -- add A, ((251)+X) indirect-indx
      memset(data_sgn, addr+1, addr_B);  
      addr := addr + 2;
      
      memset(data_sgn, addr, 16#C2#);  -- skip if no carry (129:1)
      memset(data_sgn, addr+1, 8#201#);  
      addr := addr + 2;

      memset(data_sgn, addr, 16#03#);  -- add A, 1  (imm) -- add carry
      memset(data_sgn, addr+1, 1);  
      addr := addr + 2;
      
      memset(data_sgn, addr, 16#1F#);  -- st  A, ((250)+X) indirect-indx
      memset(data_sgn, addr+1, addr_A);  
      addr := addr + 2;
      
      
      
  end procedure;
  
 -- imm : C
 -- mem : mem[C]
 -- ind: mem[mem[C] ];
 -- idx: mem[X+C]
 -- ind+idx: mem[mem[C]+X]


begin
  process(clk, rst)
  variable L : line;
  variable cnt : natural;
  variable adr: natural;
  variable out_digit, temp_cnt : natural;
  variable new_data, old_data :std_logic_vector(7 downto 0);
  begin
    if rst = '1'
    then
      -- L register holds address of next byte, so counting starts from -1
      counter <= depth-8;
      for k in 0 to depth-1 loop
        data(k) <= '0';
        data_2(k) <= '0';
      end loop;
      for k in 0 to depth/8 - 1 loop
        --data(k*8+7 downto k*8) <= x"B8";
        --data_2(k*8+7 downto k*8) <= x"B8";
      end loop;
      write(L, string'("reg rst,id="));
      write(L, internal_id);
      writeline(output, L);

      --if internal_id = 1 then
	  --memset(data, 126 , 16#33# );
	  --memset(data, 127 , 16#CC# );
	  --memset(data, 0 , 16#AA# );
      --end if;

      if internal_id = 0 then
        --prog_16bitadd(data, 4);
      else
        --prog_16bitadd_testdata(data);
      end if;


      if internal_id = 0 then
	  memset(data, 0, 16#ee#);
	  memset(data, 1, 16#ee#);
	  memset(data, 2, 51 ); 
	  memset(data, 3, 16#04#);


      if (2>1) then	  
      memset(data,  4 ,  8#223# );  --ld x, 15
      memset(data,  5 ,  8#17# );
      memset(data,  6 ,  8#236# );  -- mov[x+3],x
      memset(data,  7 ,  8#3# );
      memset(data,  8 ,  8#236# );  -- mov[x+7],x
      memset(data,  9 ,  8#7# );
      --memset(data,  10 ,  8#236# );  -- mov[x+15],x
      --memset(data,  11,  8#17# );

      memset(data,  10, 16#1F#);  -- mov  mem[mem[50]+x], A  ind-dex EA=72+51 = 123 ok
      memset(data,  11, 50);


      memset(data,  12,  8#203#);  -- add x,1
      memset(data,  13,  8#1# );
      memset(data,  14 ,  8#344# );  -- jmp 4
      memset(data,  15 ,  8#6# );
      end if;    


if (1>2) then
      memset(data,  4 , 16#1c# );-- store A, 200
      memset(data, 5, 8#200#);  --  200
      memset(data,  6 , 16#14# );-- ld A, 377
      memset(data, 7, 8#377#);  --  200

       memset(data, 8, 16#03#);  -- add A, 1
      memset(data, 9, 8#1#);  --  1

      memset(data, 10, 8#344#);--   JUMP back to Start:
      memset(data, 11, 8#004#);--
end if;    
      -- 

      --ror 4 executes as ror 1
	  -- same shr same rol
      --memset(data, 4, 16#99#); -- 99 -- shift left A, 3
      --memset(data, 5, 16#81#); -- 81 -- shift left A, 4
      --memset(data, 6, 16#91#); -- 91 -- shift left A, 2
      --memset(data, 7, 16#89#); -- 89 -- shift left A, 1


       if (2>3) then
       memset(data,  4 , 8#034# );-- store A, 200
       memset(data, 5, 8#200#);  --  200
       memset(data, 6, 8#003#); --   ADD A 1  (add one to the A register)
       memset(data, 7, 8#001#);-- (the constant 1, to be added)
       memset(data, 8, 8#344#);--   JUMP back to Start:
       memset(data, 9, 8#004#);--
       end if;



	   --memset(data,  4 , 16#74# ); -- J and mark if Z, B
	   --memset(data,  4 , 16#F4# ); -- J mark
	   --memset(data,  5 , 50 );     -- 50
       if (1>2) then
       memset(data,  4 , 16#FA# );-- SKP 1 A,7
       memset(data, 5, 0);  --  A
       
       memset(data, 6, 16#43#);  -- add B, 3
       memset(data, 7, 3);  --  3
       
       memset(data, 8, 16#1C#);  -- st A, (0200)
       memset(data, 9, 8#200#);  --  3

       --memset(data, 6, 16#0#); -- halt
       memset(data, 10 , 16#E4#  ); -- J ind 50
	   memset(data, 11 , 4 );

       memset(data, 12, 16#0#); -- halt

       memset(data, 51, 16#03#);  -- add A, 3
       memset(data, 52, 3);  -- add A, 3
	   memset(data, 53 , 16#EC#  ); -- J ind 50
	   memset(data, 54 , 50 );
       end if;
       


        --memset(data, 20, 16#0#); -- halt


        if(0>1) then
        memset(data, 4, 8#102#); -- bitset, 0
        memset(data, 5, 2);  -- X
        memset(data, 6, 8#112#); -- bitset, 1
        memset(data, 7, 2);  -- X
        memset(data, 8, 8#132#); -- bitset, 3
        memset(data, 9, 2);  -- X
        memset(data, 10, 8#172#); -- bitset, 7
        memset(data, 11, 2);  -- X

        memset(data, 12, 8#002#); -- bitclear 0
        memset(data, 13, 2);  -- X
        memset(data, 14, 8#012#); -- bitclear, 1
        memset(data, 15, 2);  -- X
        memset(data, 16, 8#032#); -- bitclear, 3
        memset(data, 17, 2);  -- X
        memset(data, 18, 8#072#); -- bitclear, 7
        memset(data, 19, 2);  -- X
        end if;

      --memset(data, 5, 16#19#); -- 19 -- shift right A, 3
      --memset(data, 4, 16#01#); -- 01 -- shift right A, 4
      --memset(data, 6, 16#11#); -- 11 -- shift right A, 2
      --memset(data, 7, 16#09#); -- 09 -- shift right A, 1

      --memset(data, 4, 16#59#); -- 59 -- rotate right A, 3
      --memset(data, 5, 16#41#); -- 41 -- rotate right A, 4
      --memset(data, 6, 16#51#); -- 51 -- rotate right A, 2
      --memset(data, 7, 16#49#); -- 49 -- rotate right A, 1
      
      
      	  -- 1101 0011
	  -- 0011 1100 
	  --memset(data, 4, 16#C9#); -- rotate left  A 1, 
	  --memset(data, 5, 16#D1#); -- rotate left  A 2, 
	  --memset(data, 6, 16#C1#); -- rotate left A 4, 
	  --memset(data, 6, 16#0#); -- halt
       -- 1101 0011  
       -- 1010 0111
       -- 1001 1110
       -- 0110 0010

      
	  --memset(data, 4, 16#41#); -- rotate right A 4, 
	  --memset(data, 5, 16#49#); -- rotate right A 1, 
	  --memset(data, 5, 16#51#); -- rotate right A 2, 
	  --memset(data, 6, 16#59#); -- rotate right A 3, 
	    --memset(data, 7, 16#41#); -- rotate right A 4, 
	  --memset(data, 7, 16#49#); -- rotate right A 1, 
       -- 0001 0011  
       -- 1000 1001
       -- 0110 0010
       -- 0100 1100
       -- 0010 0110 // rotate right 1 instead of 4 




	  --memset(data, 6, 16#C3#); -- and A, 0xCC
	  --memset(data, 7, 16#cc#);

      

	  --memset(data, 4, 16#1c#); --034 , store mem direct ; x"1B"; 033 would be store imm
	  --memset(data, 5, 16#80#);
	  --memset(data, 6, 16#03#);
	  --memset(data, 7, 16#01#);
	  --memset(data, 8, 16#1d#);   -- mov ind(A), A -addr 8
	  --memset(data, 9, 16#00#);


      if (1>3) then	  

      --memset(data, 16#4#, 16#5e#);  -- mov (X+0x11), B  -- addr C
      --memset(data, 16#5#, 16#11#);  -- mem[0x66] <= 7A

      -- ind= i1 , dex=i0
      memset(data, 16#4#, 16#1D#);  -- mov mem[mem[2]], A IND -- works  EA=51
      memset(data, 16#5#, 2);

      memset(data, 16#6#, 16#1E#);  -- mov mem[X+3],  A  DEX  - works (EA=54
      memset(data, 16#7#, 3);

      memset(data, 16#8#, 16#1F#);  -- mov  mem[mem[50]+x], A  ind-dex EA=72+51 = 123 ok
      memset(data, 16#9#, 50);

      memset(data, 16#a#, 16#1C#);  -- mov mem[50], A   MEM
      memset(data, 16#b#, 50);

      memset(data, 16#c#, 16#1B#);  -- mov [P+1], A   IMM (EA = 0xD)
      memset(data, 16#d#, 11);       
	  

      memset(data, 16#e#, 16#e4#);  -- jmp 04
      memset(data, 16#f#, 16#04#);  

      end if;

      memset(data, 50, 72);  
      memset(data, 51, 74);  
      memset(data, 52, 76);  
      memset(data, 53, 78);  
      memset(data, 54, 70);  

	  
	  
      end if; -- internal id 0

      --if internal_id = 0 then
	  --memset(data, 126 , 16#ee# );
	  --memset(data, 127 , 16#ee# );
	  --memset(data, 0 , 16#ee# );
      --end if;


      d_out <= data(0);



-- 000       000   A register is zero
--       001       000   B register is zero
--       002       000   X register is zero
 --      003       004   Program Counter is 004 (start at 004)
--  Start: 004       033   Store A 200
 --      005       200      (the address to store to)
 --      006       003   ADD A 1  (add one to the A register)
 --      007       001      (the constant 1, to be added)
 --      010       344   JUMP back to Start:
 --      011       004 

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

       d_out <= data(cnt);

       --if data(counter) /= d_in then
         write(L, string'("reg ID="));
         write(L, internal_id);
         write(L, string'(" d_out: "));
         write(L, to_char(data(counter)));
         
         write(L, string'(" d_in: "));
         write(L, to_char(d_in));
       --end if;

       data(counter) <= ite(d_in = '1','1','0');

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
       end if;
       writeline(output, L);
	   
       --write(L, string'("mem[0]="));
       --write(L, raw_format_slv_hex(data(7 downto 0)));

       if counter/8*8 = counter and counter > 8 then
          old_data := data_2(counter-1 downto counter-8);
		  new_data := data(counter-1 downto counter-8);
          if new_data /= old_data then
            write(L, string'("wrote mem["));
            write(L, counter/8 - 1 + internal_id*128);
            write(L, string'("] "));

            write(L, string'("was hex "));
            write(L, raw_format_slv_hex(old_data));
            write(L, string'(" := hex "));
            write(L, raw_format_slv_hex(new_data));
            writeline(output, L);
          end if;
       end if;


       if counter = depth-1 then
         write(L, string'("memdump of reg ID="));
         write(L, internal_id);
         writeline(output, L);

         for p in 0 to depth/8 - 1 loop
          if data(p*8+7 downto p*8) /= data_2(p*8+7 downto p*8) then
            write(L, string'("mem["));
            write(L, p + internal_id*128);
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
end architecture shiftreg1024_impl;
