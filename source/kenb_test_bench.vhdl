library IEEE;
use IEEE.std_logic_1164.all, std.textio.all;
--, std_logic_textio.all;
use IEEE.numeric_std.all;
-- ieee.std_logic_arith.all;
-- use work.strings.all;
use work.myfuncs.all;

entity kenbak_test is
end kenbak_test;


architecture kenbak_bench of kenbak_test is

 component comp_shiftreg1024
  port ( 
    comp_in : in std_logic;
    comp_out : out std_logic;
    comp_clk  : in std_logic;
    comp_rst  : in std_logic);
  end component;
  
  component comp_memreg_loadfile
  port ( 
    comp_in0, comp_in1 : in std_logic;
    comp_out0, comp_out1 : out std_logic;
    comp_clk  : in std_logic;
    comp_rst  : in std_logic);
  end component;
  
  signal dl0,dl1, jdl0, jdl1 : std_logic;
  signal clock, rst : std_logic;

   -- instan-ed
  signal input_byte_buttons: std_logic_vector(7 downto 0);
  signal output_register : std_logic_vector(7 downto 0);
  signal btn_start, btn_stmem, btn_rdmem, btn_setaddr, btn_stop, btn_displ_addr, btn_clear : std_logic;
  signal cdispl_addr, cdispl_mem, displ_run, cdispl_input : std_logic;
  signal stop_sim: std_logic;
  
begin

  --memreg1 : comp_shiftreg1024
  --  port map (JDL0, DL0, clock, rst);
  --memreg2 : comp_shiftreg1024
  --  port map (JDL1, DL1, clock, rst);
  
  memreg : comp_memreg_loadfile
    port map (JDL0, JDL1, DL0, DL1, clock, rst);

    
  cpu: entity work.kenb_cpu(kenb_cpu_impl)
    port map (
    mem0_out => DL0, mem1_out => DL1, mem0_in => JDL0, mem1_in => JDL1,
    
    rst => rst,
    clk => clock,
    input_data_btn => input_byte_buttons,
    data_output_register => output_register,
    
    start => btn_start,
    stmem => btn_stmem, 
    rdmem => btn_rdmem, 
    setaddr =>  btn_setaddr, 
    stop =>  btn_stop, 
    displ_addr =>  btn_displ_addr, 
    clear =>  btn_clear,
    
    cdispl_addr => cdispl_addr,
    cdispl_mem => cdispl_mem,
    displ_run => displ_run,
    cdispl_input => cdispl_input);
   

  clock_driver : process
  variable L,L2 : line;
  variable out_digit : natural;
  begin
    clock <= '1','0' after 1 us;
    wait for 2 us;
    if stop_sim = '1' then
      wait;
    end if;

    if ( 2>3) then 
      out_digit := 0;
      if clock = '1' then
        out_digit := 1;
      end if;
      write(L, string'(" clock: "));
      write(L, out_digit);
      writeline(output, L);
    end if;


    --clock <= '0';
    --wait for 10 ns;

    out_digit := 0;
    if clock = '1' then
      out_digit := 1;
    end if;
    write(L2, string'(" clock: "));
    write(L2, out_digit);
    --writeline(output, L2);
  end process clock_driver;

  rst_driver : process
  variable L,L2 : line;
  variable out_digit : natural;
  begin
    rst <= '1', '0' after 5 us;
   
    wait for 6000 ms;
  end process rst_driver;


  sim_driver : process
  variable L,L2 : line;
  variable out_digit : natural;
  begin
    stop_sim <= '0';
    input_byte_buttons <= "00000000";

    btn_start <= '0';
    btn_stmem <= '0';
    btn_rdmem <= '0';
    btn_setaddr <= '0';
 
 
    btn_displ_addr <= '0'; --displ addr
    btn_stop <= '0'; -- halt
    btn_clear <= '0'; --clear
    wait for 40 us;
    --cl <= '1'; --clear
    --wait for 4 ms;
    --cl <= '0'; --clear
    --wait for 4 ms;
   
    

    if (2>2) then    
      input_byte_buttons <= "00100000";
      wait for 8 ms;
      input_byte_buttons <= "00000000";
      wait for 4 ms;
      btn_setaddr <= '1';
      wait for 4 ms;
      btn_setaddr <= '0';
      wait for 4 ms;
      input_byte_buttons <= "00010000";
      wait for 4 ms;
      input_byte_buttons <= "00000000";
      wait for 4 ms;
      btn_stmem <= '1';
      wait for 4 ms;
      btn_stmem <= '0';
      wait for 4 ms;
      btn_stmem <= '1';
      wait for 4 ms;
      btn_stmem <= '0';
      wait for 4 ms;
    end if;

    write(L, string'(" wait start "));
    write(L, string'(" ks="));
    write(L, to_integer(unsigned(output_register)));

    writeline(output, L);
    

    wait for 1 ms;
    btn_start <= '1';
    wait for 4 ms;
    btn_start <= '0';
    
    --wait for 35 ms;
    --ht <= '1'; -- halt
    wait for 5 ms;
    write(L, string'(" wait expired; "));
    write(L, string'(" ks="));
    write(L, to_integer(unsigned(output_register)));

    writeline(output, L);
      
    wait until displ_run = '0' for 7000 ms;
    if displ_run = '0' then
    write(L, string'(" program halted"));
    else
    write(L, string'(" wait expired"));
    end if;
    writeline(output, L);
    
    -- wait for 5000 ms;
    --assert output_register /= X"EE" report "output register wrong" severity failure;
    assert output_register = X"EE" report "output register wrong" severity failure;
    stop_sim <= '1';
    wait;
    -- severity error;


  end process sim_driver;



end kenbak_bench;
