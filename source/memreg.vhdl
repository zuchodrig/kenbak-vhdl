library IEEE;
use IEEE.std_logic_1164.all,  std.textio.all, IEEE.numeric_std.all;
use work.myfuncs.all;

entity memreg is
  generic (depth : integer);
  port ( 
    in0, in1 : in std_logic;
    out0, out1 : out std_logic;
    rst : in std_logic;
    clk  : in std_logic);
end entity memreg;
