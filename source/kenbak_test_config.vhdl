library IEEE;
use IEEE.std_logic_1164.all;

configuration kenbak_test_behavior of kenbak_test is

  for kenbak_bench

   --for amemreg1:comp_shiftreg1024
   -- use entity work.shiftreg1024(shiftreg1024_impl)
   -- generic map (depth => 1024, internal_id => 0)
   -- port map (d_in=>comp_in, d_out=>comp_out, clk=>comp_clk, rst=>comp_rst);
   --end for;

   --for amemreg2:comp_shiftreg1024
   -- use entity work.shiftreg1024(shiftreg1024_impl)
   -- generic map (depth => 1024, internal_id => 1)
   -- port map (d_in=>comp_in, d_out=>comp_out, clk=>comp_clk, rst=>comp_rst);
   --end for;

   for memreg : comp_memreg_loadfile
    use entity work.memreg(memreg_loadfile)
    generic map (depth => 1024)
    port map (
     in0=>comp_in0,
     in1=>comp_in1,
     out0=>comp_out0,
     out1=>comp_out1,
     clk=>comp_clk, rst=>comp_rst);
   end for;


  end for;

end kenbak_test_behavior;
