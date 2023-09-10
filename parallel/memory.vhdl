library IEEE;
use IEEE.std_logic_1164.all,  std.textio.all, IEEE.numeric_std.all;

entity memory is

  generic (mem_size : positive; 
           Thold : Time                 --  address hold time
          );

  port (
        rst: in std_logic;
        neg_OE : in std_logic;
        neg_WE : in std_logic;
      	address : in std_logic_vector(7 downto 0);
	data : inout std_logic_vector(7 downto 0)
      );

end memory;
