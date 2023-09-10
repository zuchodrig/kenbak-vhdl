library IEEE;
use IEEE.std_logic_1164.all, std.textio.all;
--, std_logic_textio.all;
use IEEE.numeric_std.all;
-- ieee.std_logic_arith.all;
-- use work.strings.all;
use work.myfuncs.all;

entity kb2_test is
end kb2_test;


architecture kb2_bench of kb2_test is

  component comp_memory_loadfile
  port(
        comp_rst: in std_logic;
        comp_neg_OE : in std_logic;
        comp_neg_WE : in std_logic;
      	comp_address : in std_logic_vector(7 downto 0);
	comp_data : inout std_logic_vector(7 downto 0));
  end component;

  signal data : std_logic_vector(7 downto 0);
  signal address : std_logic_vector(7 downto 0);
  signal clock, rst, OE, WE : std_logic;
  signal stop_sim: std_logic;
  signal displ_run_signal : std_logic;
  signal output_register : std_logic_vector(7 downto 0);

begin

  
  memory : comp_memory_loadfile
    port map (rst, OE, WE, address, data);

    
  cpu: entity work.kb2_cpu(kb2_cpu_impl)
    port map (
    address => address,
    data_bus => data ,
    mem_we => WE,
    mem_oe => OE,
    displ_run => displ_run_signal,
    clk => clock,
    rst => rst);
	    

  clock_driver : process
  variable L,L2 : line;
  variable out_digit : natural;
  begin
    clock <= '1','0' after 1 us;
    wait for 2 us;
    if stop_sim = '1' then
      wait;
    end if;



    write(L, string'(" stop_sim:"));
    write(L, to_char( stop_sim ));
    writeline(output, L);



  end process clock_driver;

  rst_driver : process
  variable L,L2 : line;
  variable out_digit : natural;
  begin
    OE <= '1';
    WE <= '1';
    OE <= '0';
    rst <= '1', '0' after 5 us;
    WE <= 'Z';
    OE <= 'Z';

    wait for 6000 ms;

  end process rst_driver;

  process_output: process(address, data, WE)
  begin
    if rising_edge(WE) then
      if address = "10000000" then
        output_register <= data;
      end if;
    end if;
  end process;


  sim_driver : process
  variable L,L2 : line;
  variable out_digit : natural;
  variable halted : boolean;
  begin
    stop_sim <= '0';

    halted := false;
    --wait for 2 ms;
    --write(L, string'(" B components : displ_run_signal , output:"));
    --write(L, to_bstring( displ_run_signal & output_register));
    --writeline(output, L);

    wait until displ_run_signal = '0' for 7000 ms;
    if displ_run_signal = '0' then
      write(L, string'(" program halted "));
      halted := true;
    else
      write(L, string'(" program timed out"));
    end if;
    --write(L, (halted));
    writeline(output, L);
    stop_sim <= '1';
    if halted then
      assert output_register = X"EE" report "output register wrong" severity failure;
    end if;
    wait;

  end process sim_driver;



end kb2_bench;
