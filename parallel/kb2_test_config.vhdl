library IEEE;
use IEEE.std_logic_1164.all;

configuration kb2_test_behavior of kb2_test is

  for kb2_bench

   for memory : comp_memory_loadfile
    use entity work.memory(memory_loadfile)
    generic map (mem_size => 256, Thold => 10 ns)
    port map (
     rst=>comp_rst,
     data=>comp_data, address=>comp_address,
     neg_oe => comp_neg_oe,
     neg_we => comp_neg_we);
   end for;

  end for;

end kb2_test_behavior;
