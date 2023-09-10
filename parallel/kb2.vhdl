library IEEE;
use IEEE.std_logic_1164.all,  std.textio.all, IEEE.numeric_std.all;
use work.myfuncs.all;

entity kb2_cpu is
  port ( 
    address : out std_logic_vector(7 downto 0);
    data_bus : inout std_logic_vector(7 downto 0);
    mem_we : out std_logic;
    mem_oe : out std_logic;
    displ_run: out std_logic;
    rst : in std_logic;
    clk  : in std_logic);
end entity kb2_cpu;

architecture kb2_cpu_impl of kb2_cpu is

function condition_match(i_reg: std_logic_vector(7 downto 0); busvalue: std_logic_vector(7 downto 0)) return boolean is
variable is_zero, is_positive, jump_condition_met : std_logic;
begin
   is_zero := '0';
   if busvalue = "00000000"
   then 
     is_zero := '1';
   end if;
   is_positive := not busvalue(7);

   jump_condition_met :=  ite(
   (i_reg(7) and i_reg(6)) = '1' or
   ((i_reg(2 downto 0 ) & is_zero ) = "0110") or 
   ((i_reg(2 downto 0) & is_positive) = "1101") or
   ((i_reg(2 downto 0) & is_zero) = "1001") or
   ((i_reg(2 downto 0) & is_positive) = "1010") or
   ((i_reg(2 downto 0) & is_positive & is_zero) = "11110"), '1','0');
   return jump_condition_met = '1';
end function;


function statename(state : std_logic_vector(4 downto 0))  return string is
 constant states_str : string := "00SASBSCSDSESFSGSHSJSKSLSMSNSOSPSQSRSSSTSUSVSWSXSYSZQAQBQCQDQEQF.."; 
 variable pos: natural;
 begin
  if (state(0) = 'U') then return "UU"; end if;
  if (state(0) xor state(1) xor state(2) xor state(3) xor state(4)) = 'X' then
    return "XX";
  end if;
  pos := to_integer(unsigned(state));
  return states_str(pos*2+1 to pos*2+2);
 end function;


function bit_manip_func(i_reg: std_logic_vector(7 downto 0); current_bit: natural range 0 to 7; inp_bit: std_logic) return std_logic is
  variable L: line;
begin
  if (i_reg and "10000111") /= "00000010" then
    write(L, string'(" not bit_manip opcode:"));
    write(L, to_bstring(i_reg and "10000111"));
    writeline(output, L);
    return inp_bit;
  end if;
  if current_bit /= to_integer(unsigned(i_reg(5 downto 3))) then
    write(L, string'("   bit_manip wrong phase"));
    writeline(output, L);
    return inp_bit;
  end if;

  write(L, string'(" bit_manip old/new value:"));
  write(L, to_bstring(inp_bit & i_reg(6) ));
  writeline(output, L);


  return i_reg(6);
end function;


-- i_reg: parallel load from memory data bus, parallel out
-- w_reg: parallel from bus, shift out to alu. Input from w (keep) or alu.
-- b_reg: parallel from bus, shift out to alu
-- a_reg - serial load from alu or w_reg or keep, parallel output to address reg
-- c_reg - serial load from alu or bit_manip(b_reg) or keep, parallel output to data bus.

  signal address_dbg : std_logic_vector(7 downto 0);
  
  signal mem_oe_dbg, mem_we_dbg :  std_logic;
    
    
  signal s_reg: std_logic_vector(4 downto 0);
  signal addr_reg : std_logic_vector(7 downto 0); -- serial in register
  signal addr_reg_out : std_logic_vector(7 downto 0); -- parallel load from addr_reg
  signal read_process_phase : natural range 0 to 14;
  signal read_process_start, read_process_end : std_logic;
  signal read_process_start_latch : std_logic; -- set in clocked process

  signal i_reg : std_logic_vector(7 downto 0);
  signal read_i_reg, read_w_reg_after : std_logic;

  signal b_reg : std_logic_vector(7 downto 0);
  signal read_b_reg_before, read_b_reg_phase_1, read_b_reg_after : std_logic;
  
  signal wreg_from_a_p1, areg_from_alu : std_logic;
  signal creg_from_b_reg : std_logic; -- c_reg input, a_reg if '0' else  b_reg
  signal creg_from_alu : std_logic; -- c_reg input, a_reg if '0' else  b_reg
  --signal areg_from_w_reg : std_logic; -- a_reg input, alu output is '0' else  w_reg
  signal keep_w_reg, alu_destination_a, alu_destination_b, keep_c_reg : std_logic;

  -- address selection
  signal addr_from_w, addr_reg_p, addr_reg_x, addr_abx_instr, addr_ab_shift_instr : std_logic;
 
  signal read_w_reg_before, read_w_reg_phase_1 : std_logic;
  signal w_reg : std_logic_vector(7 downto 0);
  
  signal c_reg : std_logic_vector(7 downto 0);
  signal store_c_reg : std_logic; -- edge to store shift stage to output stage
  signal store_flags_reg : std_logic; -- edge to store flags after add/sub
  signal store_to_mem : std_logic; -- any of two above is true
  signal c_reg_oe : std_logic; -- enable of output stage (negative)

  signal P_incr_1, P_incr_2, P_incr_4 : std_logic;
  signal condition_match_register : std_logic;

  signal cycle_count : natural range 0 to 1024*1024;
  signal ALU_SUM, ALU_A,ALU_B, ALU_A_INTL, ALU_B_INTL, ALU_C, ALU_C_reg, alu_carry_reset : std_logic;
  signal alu_opcode_section : std_logic_vector(2 downto 0);
  signal ALU_OP_a, ALU_OP_b, ALU_OP_add, ALU_OP_sub, ALU_OP_lneg, ALU_OP_and, ALU_OP_or : std_logic;
  --ALU_OP_a means result=a, unchanged; ALU_OP_b - LD command
  signal carry_flag, ovf_flag, alu_ovf_flag_generate, alu_carry_flag_generate,
    alu_prev_sign_value, should_set_flags_in_sw  : std_logic;
    
  
  signal B_shift_input, C_shift_input : std_logic;
  
  signal SA,SB,SC,SD : std_logic;
  
  constant S_null_v : std_logic_vector (4 downto 0) := "00000";
  constant SA_v : std_logic_vector (4 downto 0) := "00001"; -- A=3
  constant SB_v : std_logic_vector (4 downto 0) := "00010"; -- W: M[3], I:=M[w]
  constant SC_v : std_logic_vector (4 downto 0) := "00011"; -- ? 2-byte opcode =>SD 1byte->ST
  constant SD_v : std_logic_vector (4 downto 0) := "00100"; -- A:=A+1 ; w := M[A]
  constant SE_v : std_logic_vector (4 downto 0) := "00101"; -- when done: JI+IND=> SF; ~ind*dex => SH; BM+~TM*MEM*~J => SK else SM
  constant SF_v : std_logic_vector (4 downto 0) := "00110"; -- start
  constant SG_v : std_logic_vector (4 downto 0) := "00111"; -- w := m[w]; when done: expects in w; dex => SH; ~dex*~j*~tm  => SK else  SM
  constant SH_v : std_logic_vector (4 downto 0) := "01000"; -- start
  constant SJ_v : std_logic_vector (4 downto 0) := "01001"; -- B=x; w:=w+B; done; TM => SM else => SK
  constant SK_v : std_logic_vector (4 downto 0) := "01010"; -- start
  constant SL_v : std_logic_vector (4 downto 0) := "01011"; -- w:=m[w]; when done: goto SM

  constant SM_v : std_logic_vector (4 downto 0) := "01100"; -- switch: ~TM*~j*~BM->SN ; j->SP
  constant SN_v : std_logic_vector (4 downto 0) := "01101"; -- ALU/LD op: a/b/x <= alu(a/b/x, w) start. w contains operand value
  constant SO_v : std_logic_vector (4 downto 0) := "01110"; -- b<=mem, c<=alu(w, b);  mem<=c done ? 
  constant SP_v : std_logic_vector (4 downto 0) := "01111"; -- jump start
  constant SQ_v : std_logic_vector (4 downto 0) := "10000"; -- jump loading mem ? check condition ? yes=>SR else SW; in case of SW stop cycle count
  constant SR_v : std_logic_vector (4 downto 0) := "10001"; -- perform actual jump (not jm): c:=w; addr=3; write mem[3]=c;  
  constant SS_v : std_logic_vector (4 downto 0) := "10010"; --- j&mark: b:=mem[3]; a:=w; c:=b; mem[a]:=c; ;; c:=b+1; mem[3]:=c
  constant ST_v : std_logic_vector (4 downto 0) := "10011"; -- 1 byte opcode
  constant SU_v : std_logic_vector (4 downto 0) := "10100";  -- 1 byte contd
  constant SV_v : std_logic_vector (4 downto 0) := "10101";
  constant SW_v : std_logic_vector (4 downto 0) := "10110"; -- store CY+OV flags if needed
  constant SX_v : std_logic_vector (4 downto 0) := "10111"; -- load P,   increment P done; -> SA
  constant SY_v : std_logic_vector (4 downto 0) := "11000"; -- STore operation; w contains address; done
  constant SZ_v : std_logic_vector (4 downto 0) := "11001"; -- bit test ; => SX
  constant QA_v : std_logic_vector (4 downto 0) := "11010"; -- bit set?
  constant QB_v : std_logic_vector (4 downto 0) := "11011"; -- bit set end;
  constant QC_v : std_logic_vector (4 downto 0) := "11100";
  constant QD_v : std_logic_vector (4 downto 0) := "11101";
  constant QE_v : std_logic_vector (4 downto 0) := "11110";
  constant QF_v : std_logic_vector (4 downto 0) := "11111"; -- halt

  constant adding_phase_start : natural := 2;
 
  signal is_2byte_opcode : std_logic; 
  signal opcode_alu, opcode_alu_not_tm, op_immed, op_mem, op_ind, op_dex, op_jump_direct, op_jump_indirect, op_jump,
   opcode_store, opcode_store_imm, opcode_bit_manip, opcode_bit_test,
   opcode_shift_rotate, opcode_shift, opcode_rotate, opcode_direction_left,  opcode_direction_right : std_logic;
  signal shift_count_raw, shift_count_part_1 : unsigned(2 downto 0);
  signal shift_phase_1, shift_phase_2 : std_logic;
  signal opcode_nop, opcode_halt : std_logic;
 
  
begin

  opcode_store_imm <= '1' when (i_reg(7 downto 6) /= "11" and i_reg(5 downto 0) = "011011") else '0';
  opcode_store <= '1' when (i_reg(7 downto 6) /= "11" and i_reg(5 downto 3) = "011" and i_reg(2 downto 0) /= "010" ) else '0';

  -- 'longlogic' signal in original was store (a/b/x), IMM
  
                       
  -- (not i_reg(3) or not i_reg(4) or (i_reg(7) AND i_reg(6)));
   -- load/logic/arith except 'store' operation [xx]x[00/01/10]xxx  or [11]x[xx]xxx
  opcode_alu <= not i_reg(5) and not (i_reg(7) AND i_reg(6)) and 
                       (i_reg(2) or (i_reg(0) and i_reg(1)));

  opcode_alu_not_tm <= '0' when i_reg(7 downto 6) /= "11" and i_reg(5 downto 3) = "011" else opcode_alu;
  
  op_immed <= opcode_alu when (i_reg(2 downto 0) = "011")  else '0';
  op_mem <= opcode_alu when (i_reg(2 downto 0) = "100")  else '0'; --mem
  op_ind <= opcode_alu when (i_reg(2 downto 0)  = "101" or i_reg(2 downto 0)  = "111")  else '0'; --ind or ind/dex
  op_dex <= opcode_alu when ((i_reg and "00000110") = "00000110")  else '0'; -- ind/idx or idx, requires fetching X
  
  opcode_bit_manip <= '1' when i_reg(7) = '0' and (i_reg(2 downto 0) = "010") else '0';
  opcode_bit_test <= '1' when i_reg(7) = '1' and (i_reg(2 downto 0) = "010") else '0';
  opcode_shift_rotate <= '1' when  i_reg(2 downto 0) = "001"  else '0';
  opcode_shift <= opcode_shift_rotate and not i_reg(6);
  opcode_rotate <= opcode_shift_rotate and i_reg(6); 
  opcode_direction_left <= opcode_shift_rotate and i_reg(7);
  opcode_direction_right <= opcode_shift_rotate and not i_reg(7);
  opcode_nop <= '1' when  i_reg(2 downto 0) = "000" and i_reg(7)='1'  else '0';
  opcode_halt <= '1' when  i_reg(2 downto 0) = "000" and i_reg(7)='0'  else '0';
 
  op_jump_indirect <=  '1' when (((i_reg and "00101000") = "00101000") and opcode_bit_manip = '0' and opcode_bit_test ='0') else '0';
  op_jump_direct <=    '1' when (((i_reg and "00101000") = "00100000") and opcode_bit_manip = '0' and opcode_bit_test ='0') else '0';
  op_jump <= op_jump_direct or op_jump_indirect;
  

  should_set_flags_in_sw <= '1' when (opcode_alu = '1') and ((alu_opcode_section = "000") or (alu_opcode_section = "001")) else '0';

 states : process(clk, rst)
 variable L, L2, L3: line;
 variable is_zero, is_positive, jump_condition_met : std_logic;
 variable bit_position : natural range 0 to 7;
 begin
   if rst = '1' then
     s_reg <= "00000";
   elsif rising_edge(clk) then
     if s_reg = "00000" then
       s_reg <= SA_v;
     elsif s_reg = SA_v then
       s_reg <= SB_v;
     elsif s_reg = SB_v then
       if read_process_end = '1' then
         s_reg <= SC_v;
       end if;

     elsif s_reg = SC_v then
       P_incr_1 <= '0';
       P_incr_2 <= '0';
       P_incr_4 <= '0';
       if is_2byte_opcode = '1' then
         s_reg <= SD_v;
       else
         s_reg <= ST_v;
       end if;
     elsif s_reg = SD_v then
       s_reg <= SE_v;
     elsif s_reg = SE_v then
       if read_process_end = '1' then
         if (op_jump_indirect or op_ind) = '1'  then
           s_reg <= SF_v;
         elsif (not op_ind and op_dex) = '1' then
           s_reg <= SH_v;
         elsif (opcode_bit_test or (not opcode_store and op_mem and not op_jump)) = '1' then
           s_reg <= SK_v;
         else
           s_reg <= SM_v;
         end if;
       end if;
     elsif s_reg = SF_v then
         s_reg <= SG_v;
     elsif s_reg = SG_v then
       if read_process_end = '1' then
         if op_dex = '1' then s_reg <= SH_v;
         elsif (not op_dex and not op_jump and not opcode_store) = '1' then s_reg <= SK_v;
         else s_reg <= SM_v;
         end if;
       end if;
     elsif s_reg = SH_v then
       s_reg <= SJ_v;
     elsif s_reg = SJ_v then
       if read_process_end = '1' then
         if opcode_store = '1' then
           s_reg <= SM_v;
         else
           s_reg <= SK_v;
         end if;
       end if;
     elsif s_reg = SK_v then
       s_reg <= SL_v;
     elsif s_reg = SL_v then
       if read_process_end = '1' then
      
         write(L, string'(" op_jump & not opcode_store & not opcode_bit_manip:"));
         write(L, to_bstring(op_jump & not opcode_store & not opcode_bit_manip));
         writeline(output, L);
         s_reg <= SM_v;
       
       end if;
     elsif s_reg = SM_v then
         if (not op_jump and not opcode_store and not opcode_bit_manip and not opcode_bit_test) = '1' then
           s_reg <= SN_v;
         elsif op_jump = '1' then 
           s_reg <= SP_v;
         elsif opcode_store = '1' then
           s_reg <= SY_v;
         elsif opcode_bit_test = '1' then
           s_reg <= SZ_v;
         elsif opcode_bit_manip = '1' then
           s_reg <= QA_v;
         end if;
     elsif s_reg = SN_v then
       s_reg <= SO_v;
     elsif s_reg = SO_v then
       -- mem in w; load A/B/X into B, perform alu op, result to C, store back to A/B/X
       P_incr_2 <= '1';
       if read_process_end = '1' then
         s_reg <= SW_v;
       end if;
       
       
     elsif s_reg = SY_v then
       P_incr_2 <= '1';
       -- address in w. Set bus address from cmd; read b=mem[A/B/X]; (b->c; w->a; ) ; write c to mem[a]
       if read_process_end = '1' then
         s_reg <= SW_v;
       end if;
     elsif s_reg = SP_v then
       s_reg <= SQ_v;
       
     -- jump and jump-mark Address in w.
     -- jump: A=3; w->c; mem[3]=c;
     -- jump-mark : (w->a; A=3; b=mem[3]; b+2 -> c; mem[a]=c; )  then: a+1 -> c; A=3; mem[3]=c;

     -- combine it: SQ:  A=3; b=mem[3]; b+2->c; w->a; keep w; A=0/1/2; condition_match_register = (check condition);
     -- now c=P; a=jump addr
     --
     -- SR: if not cond => SW; else: keep c; keep a (a:= a+0)  if jm: mem[w]=c; => SS
     -- SS:  c = w+1 if jm else w;  mem[3]=c; =>SA (here condition is satisfied, skip 'increase P' step
     
     elsif s_reg = SQ_v then
       if read_process_end = '1' then
         is_zero := not (data_bus(0) or data_bus(1) or data_bus(2) or data_bus(3) or data_bus(4) or data_bus(5) or data_bus(6) or data_bus(7));
         is_positive := not data_bus(7);

         jump_condition_met :=  ite(
         (i_reg(7) and i_reg(6)) = '1' or
         ((i_reg(2 downto 0 ) & is_zero ) = "0110") or 
         ((i_reg(2 downto 0) & is_positive) = "1101") or
         ((i_reg(2 downto 0) & is_zero) = "1001") or
         ((i_reg(2 downto 0) & is_positive) = "1010") or
         ((i_reg(2 downto 0) & is_positive & is_zero) = "11110"), '1','0');
         
         condition_match_register  <= jump_condition_met;

         write(L, string'("jump_condition_met & is_zero & is_positive: "));
         write(L, to_bstring( jump_condition_met & is_zero & is_positive ));
         write(L, string'(" data_bus checking: "));
         write(L, to_bstring( data_bus ));
         writeline(output, L);


         --if jump_condition_met = '1' then 
           s_reg <= SR_v;
         --else
         --  s_reg <= SW_v;
         --end if;
       end if;
     elsif s_reg = SR_v then
       if condition_match_register = '0' then
         P_incr_2 <= '1';
         s_reg <= SW_v;
       end if;
       if read_process_end = '1' then
         s_reg <= SS_v;
       end if;

     elsif s_reg = SS_v then
       if read_process_end = '1' then
         s_reg <= SA_v;
       end if;

     elsif s_reg = ST_v then
       -- one-byte operations:
       P_incr_1 <= '1';
       if opcode_shift_rotate = '1' then
       end if;
       if opcode_halt = '1' then
         s_reg <= QF_v;
       end if;
       if opcode_nop = '1' then
         s_reg <= SW_v;
       end if;
       
       if read_process_end = '1' then
         s_reg <= SU_v;
       end if;
     elsif s_reg = SU_v then
       if read_process_end = '1' then
         s_reg <= SW_v;
       end if;

     elsif s_reg = SW_v then
       if (read_process_end = '1') or (should_set_flags_in_sw = '0') then
         s_reg <= SX_v;
       end if;
     elsif s_reg = SX_v then
       if read_process_end = '1' then
         s_reg <= SA_v;
       end if;
     elsif s_reg = SZ_v then
       if read_process_phase = 1 then
         bit_position := to_integer(unsigned(i_reg(5 downto 3)));
         if w_reg(bit_position) = i_reg(6)  then -- then skip
           P_incr_4 <= '1';
         else
           P_incr_2 <= '1';
         end if;
       end if;
       if read_process_end = '1' then
         s_reg <= SW_v;
       end if;
     elsif s_reg = QA_v then
       -- w->a ; read m[a]=>w;
       if read_process_end = '1' then
         s_reg <= QB_v;
       end if;
     elsif s_reg = QB_v then
       if read_process_end = '1' then
         P_incr_2 <= '1';
         s_reg <= SW_v;
       end if;
     end if;
   end if;
 end process;


  SA <= ite(S_reg = "00000", '1','0');
  SB <= ite(S_reg = "00001", '1','0');
  SC <= ite(S_reg = "00010", '1','0');
  SD <= ite(S_reg = "00011", '1','0');

  alu_carry_reset <= '1' when read_process_phase = adding_phase_start-1 else '0';
  alu_carry_flag_generate <= '1' when (s_reg = SO_v) and (read_process_phase = adding_phase_start+7) else '0';
  alu_ovf_flag_generate <= '1' when (s_reg = SO_v) and (read_process_phase = adding_phase_start+7) else '0';
  
   

read_i_reg_process : process(clk, rst)
begin
  if rising_edge(clk) then
    if read_i_reg = '1' then
      i_reg <= data_bus;
    end if;
  end if;
end process;

is_2byte_opcode <= i_reg(1) or i_reg(2);

process_alu_op : process(clk, rst)
variable L: line;
variable bitt: std_logic;
begin
  if rst = '1' then
    alu_c_reg <= '0';
  elsif rising_edge(clk) then
    if alu_carry_reset = '1' then
      if (ALU_OP_sub ='1' or ALU_OP_lneg='1') then
        alu_c_reg <= '1';
      else
        alu_c_reg <= '0';
      end if;
    else
      alu_c_reg <= ALU_C;
    end if;
    
    -- phases to generate carry and overflow flags
    if alu_ovf_flag_generate = '1' then
      bitt := (ALU_SUM xor ALU_B_INTL ) and (ALU_B_INTL xor ALU_A_INTL xor '1');
      ovf_flag <= bitt;
      write(L, string'(" setting ovf_flag:"));
      write(L, to_char(bitt));
      writeline(output, L);

    end if;
    
    if alu_carry_flag_generate = '1' then
      carry_flag <= ALU_C xor ALU_OP_sub;
    end if;


  end if;
end process;


ALU_SUM <= 
ALU_A_INTL and ALU_B_INTL when ALU_OP_and = '1' else
ALU_A_INTL or ALU_B_INTL when ALU_OP_or = '1' else
(ALU_A_INTL xor  ALU_B_INTL xor ALU_C_reg);

ALU_C <= (ALU_A_INTL and ALU_B_INTL) or (ALU_A_INTL and ALU_C_reg) or (ALU_B_INTL and ALU_C_reg);

-- carry (borrow) for A-B : (B and not  C) or (B and not A)

ALU_A_INTL <= '0' when ALU_OP_b = '1' else ALU_A;
ALU_B_INTL <= 
  not ALU_B when (ALU_OP_sub='1' or ALU_OP_lneg='1') else
  '0' when ALU_OP_a = '1' else
  ALU_B;

ALU_B <= w_reg(0) when (s_reg = SJ_v or s_reg=SO_v or s_reg=SX_v or s_reg = SS_v) -- adding B to W
     else b_reg(0) when (s_reg = SQ_v) -- adding const to B
     else addr_reg(0);
ALU_A <= b_reg(0) when (s_reg = SJ_v or s_reg=SO_v) -- adding B to W
     else '1' when (s_reg = SE_v) and read_process_phase = 2  -- incrementing address P when reading 2nd instruction byte
     else '1' when (s_reg = SS_v) and read_process_phase = 2 and i_reg(4) = '1' -- incrementing jump            address when jump-mark command
     else '1' when (s_reg = SQ_v) and read_process_phase = 3 -- return address for jump-mark command
     else '1' when (s_reg = SX_v) and read_process_phase = 2 and P_incr_1 = '1'
     else '1' when (s_reg = SX_v) and read_process_phase = 3 and P_incr_2 = '1'
     else '1' when (s_reg = SX_v) and read_process_phase = 4 and P_incr_4 = '1'
     else '0';


 
 alu_opcode_section <= (i_reg(7) and i_reg(6)) & i_reg(4) & i_reg(3);
  
 -- default operator for ALU is add, otherwise seleted for 'SO' phase by  opcode:
 ALU_OP_add <= '1' when (s_reg /= SO_v) or alu_opcode_section = "000" else '0';
 ALU_OP_a <=  '1' when (s_reg = SO_v) and alu_opcode_section = "101" else '0';
 ALU_OP_b <=  '1' when (s_reg = SO_v) and alu_opcode_section = "010" else '0';
 ALU_OP_sub <=  '1' when (s_reg = SO_v) and alu_opcode_section = "001" else '0';
 ALU_OP_lneg <=  '1' when (s_reg = SO_v) and alu_opcode_section = "111" else '0';
 ALU_OP_and <=  '1' when (s_reg = SO_v) and alu_opcode_section = "110" else '0';
 ALU_OP_or <=  '1' when (s_reg = SO_v) and alu_opcode_section = "100" else '0';

set_read_process_start_latch : process(clk, rst)
begin
  if rising_edge(clk) then
    read_process_start_latch <= ite( ((s_reg = SQ_v or s_reg=SR_v or s_reg=QA_v or s_reg=ST_v or (s_reg=SC_v and is_2byte_opcode='0') or s_reg=SO_v or s_reg=SW_v ) and read_process_phase = 12) or
    (( s_reg=SM_v or (s_reg=SC_v and is_2byte_opcode='0') )  and read_process_phase = 0 ), '1', '0');
  end if;
end process;
-- shifts w_reg to address reg then loads w_reg from memory
-- also shitfts a+1 => a and load w from result
read_w_reg : process(clk, rst)
variable L, L2, L3: line;
variable a_input, w_input, b_input, c_input: std_logic;
begin
  if rst = '1' then
    read_process_phase <= 0;
  elsif rising_edge(clk) then
    if read_process_start = '1' then
      if read_w_reg_before = '1' then
        w_reg <= data_bus;
      end if;
      if read_b_reg_before = '1' then
         b_reg <= data_bus;
      end if;
      read_process_phase <= 1;
      read_process_end <= '0';
    elsif read_process_phase > 0 and (read_process_phase < 12 or  (read_process_phase < 13 and store_to_mem = '1' )) then
      read_process_phase <= read_process_phase + 1;
    end if;
    if read_process_phase < adding_phase_start + 8 then

      if read_process_phase = 1 then
        if read_b_reg_phase_1 = '1' then
           b_reg <= data_bus;
        end if;
        if read_w_reg_phase_1 = '1' then
           w_reg <= data_bus;
        end if;
      end if;
      if read_process_phase >= adding_phase_start then

        if areg_from_alu = '1' then
          a_input := ALU_SUM;
        else
          a_input := w_reg(0);
        end if;
        c_input := a_input;
        
        if creg_from_b_reg = '1' or creg_from_alu = '1' or keep_c_reg = '1' or (opcode_shift_rotate = '1' and (shift_phase_1 or shift_phase_2)='1') then 
          if keep_c_reg = '1' then
            c_input := c_reg(0);
            write(L, string'(" keep "));
          elsif opcode_shift_rotate = '1' then
            c_input := C_shift_input;
            write(L, string'(" c_shift "));
          elsif creg_from_alu = '1' then
            c_input := ALU_SUM;
            write(L, string'(" alu "));
          else
            write(L, string'(" bit_manip "));
            c_input := bit_manip_func(i_reg, read_process_phase - adding_phase_start, b_reg(0));
          end if;

          write(L, string'(" c_input:"));
          write(L, to_bstring( c_input & c_input));
          writeline(output, L);
        end if;
        
        --if areg_from_w_reg = '1' then
        --end if;
        addr_reg(7 downto 0) <= a_input & addr_reg(7 downto 1);
        c_reg(7 downto 0) <= c_input & c_reg(7 downto 1);
        if keep_w_reg ='1' then
          w_input := w_reg(0);
        else
          w_input := ALU_SUM;
        end if;
        b_input := b_reg(0);
        --b_reg(7 downto 0) <= b_input & b_reg(7 downto 1);
        w_reg(7 downto 0) <= w_input & w_reg(7 downto 1);

        if opcode_shift_rotate = '1' then
          if (shift_count_part_1 > read_process_phase - adding_phase_start) or shift_phase_2='1' then
            b_reg(7 downto 0) <= B_shift_input & b_reg(7 downto 1);
          else
            -- keep b unshifted
          end if;
        else
          b_reg(7 downto 0) <= b_input & b_reg(7 downto 1);
        end if;
        -- B->C (suring shift_phase_2) is shifted always

        -- B_shift_input, C_shift_input

       -- shift ?  B  ->  C
       -- right shift : shift right by N, Binput = B(7) then B->C (all 8 bits)
       -- right rotate : shift right by N, Binput = B(0); then B->C (all 8 bits)
       -- left rotate : rotate right by 8-N then (all 8 bits) B->C
       -- left shift : rotate right by 8-N
       -- e.g. 2 bit left shift:
       -- First rotate B=>B right by 6  (= 8-2):
       -- 76543210 => 54321076
       -- then shift B->C all 8 bits with first N bits = 0
       -- shift_count_part_1, shift_count_part_2 (always 8)




        
      end if;
    --- shift done, 
    elsif read_process_phase = 10 then
      addr_reg_out <= addr_reg;
      if store_to_mem = '1' then
      end if;
      -- if ( ALU_OP_add or ALU_OP_sub) = '1' and s_reg = SW_v then
      -- end if;
    elsif read_process_phase = 11 then
      if read_w_reg_after = '1' then
        w_reg <= data_bus;
      end if;
      read_process_end <= '1';
    elsif read_process_phase = 12 then
      read_process_end <= '0';
      read_process_phase <= 0;
    else
    end if;
  end if;
 end process;

mem_we_dbg <= '0' when (read_process_phase = 11 and store_to_mem = '1') else '1';
mem_we <= mem_we_dbg;
 
c_reg_oe <= '0' when read_process_phase >= 10 and read_process_phase <= 12 and store_to_mem = '1' else '1';
mem_oe_dbg <= '1' when read_process_phase >= 10 and read_process_phase <= 12 and store_to_mem = '1' else '0';
mem_oe <= mem_oe_dbg;
data_bus <= "ZZZZZZZZ" when c_reg_oe='1' else
 c_reg when store_c_reg = '1' else
 "000000" & carry_flag & ovf_flag when store_flags_reg = '1' else "UUUUUUUU";

address_dbg <=  addr_reg when (addr_reg_p or addr_reg_x or addr_abx_instr or addr_ab_shift_instr or store_flags_reg) = '0' 
   else "00000011" when addr_reg_p = '1'
   else "00000010" when addr_reg_x = '1'
   else "0000000" & i_reg(5) when addr_ab_shift_instr = '1'
   else "000000" & i_reg(7) & i_reg(6) when (addr_abx_instr = '1' and ((i_reg(7) and i_reg(6)) = '0') ) --a/b/x for add/sub/cond jumps
   else "00000000"  when addr_abx_instr = '1' and ((i_reg(7) and i_reg(6)) = '1')
   else "100000" & std_logic_vector(to_unsigned(( to_integer(unsigned'(i_reg(7) & i_reg(6)))  + 1 ), 2)) when store_flags_reg = '1'  ; -- logic/lneg/etc operate with A only
   
   
--address_dbg  <= std_logic_vector(to_unsigned(25, 8));

--address_dbg <= "000000" & std_logic_vector(to_unsigned(( to_integer(unsigned'(i_reg(7) & i_reg(6)))  + 1 ), 2));

-- unsigned'(i_reg(7) & i_reg(6))


address <= address_dbg;


 --if adr_from_w = '1' then
 --  address <= addr_reg;
 --else if addr_reg_p = '1' then
 --  address <= "00000011";
 --else if addr_abx_instr = '1' then
 --  address <= "000000" & i_reg(7) & i_reg(6);
 --else if addr_ab_shift_instr = '1' then
 --  address <= "0000000" & i_reg(5);
 --end if;


addr_from_w <= '1' when s_reg = SD_v else '0';
addr_reg_p <= '1' when (s_reg = SA_v or s_reg = SA_v or s_reg = SX_v or 
(s_reg = SQ_v and (read_process_phase < 9)) or s_reg = SS_v) else '0';
addr_reg_x <= '1' when (s_reg = SH_v) else '0';
read_w_reg_before <= '1' when (s_reg = SA_v or s_reg = SB_v ) else '0';  -- or s_reg = SW_v or s_reg = SX_v
read_w_reg_phase_1 <= '1' when (s_reg = SX_v) else '0';
read_b_reg_before <= '1' when (s_reg = SH_v or s_reg = SN_v or s_reg = SY_v or s_reg = QB_v) else '0';
read_b_reg_phase_1 <= '1' when (s_reg = SQ_v or s_reg = ST_v) else '0';

store_c_reg <= '1' when (s_reg = SO_v or s_reg = SX_v or (s_reg = SR_v and i_reg(4) = '1') or
   (s_reg = SS_v) or (s_reg = SY_v) or (s_reg = QB_v) or (s_reg = SU_v)
 -- (s_reg = SR_v and condition_match(i_reg, data_bus)) 
 ) else '0';
 
store_flags_reg <= '1' when s_reg = SW_v and ( ALU_OP_add or ALU_OP_sub) = '1' else '0';
store_to_mem <= store_flags_reg or store_c_reg;

read_i_reg <= '1' when (s_reg = SB_v and read_process_end='1') else '0';
read_w_reg_after <= '1' when (s_reg = SE_v or s_reg = SG_v or s_reg = SL_v ) else '0'; --   or s_reg = QA_v

wreg_from_a_p1 <= '1' when (s_reg = SC_v or s_reg=SD_v) else '0';
areg_from_alu <= '1' when (s_reg = SE_v or s_reg = SJ_v or s_reg = SO_v or s_reg = SX_v or s_reg = SR_v or s_reg = SS_v) else '0';

creg_from_b_reg <= '1' when (s_reg = SY_v  or s_reg = QB_v) else '0';
--areg_from_w_reg <= '1' when (s_reg = SQ_v) else '0';
creg_from_alu <= '1' when (s_reg = SQ_v) else '0';


keep_w_reg <= '1' when ( s_reg = SQ_v or s_reg = SR_v or s_reg = SS_v or s_reg = QA_v) else '0' ;
keep_c_reg <= '1' when ( s_reg = SR_v  ) else '0' ;
alu_destination_a <= '1' ;
alu_destination_b <= '0' ; 

addr_abx_instr <= '1' when (s_reg = SO_v or s_reg = SN_v or s_reg=SP_v or s_reg=SQ_v or (s_reg=SY_v and read_process_phase < 6) ) else '0';
addr_ab_shift_instr <= '1' when (s_reg = ST_v or s_reg = SU_v) else '0';


read_process_start <= '1' when (s_reg = SA_v or s_reg = SD_v or s_reg = SF_v or s_reg=SH_v or s_reg=SK_v or s_reg=SN_v or s_reg=SP_v
 --or (s_reg=SQ_v and read_process_phase = 12)
 or read_process_start_latch = '1'
 or (s_reg=SW_v and should_set_flags_in_sw = '0')
 ) else '0';

shift_phase_1 <= '1' when s_reg=ST_v else '0';
shift_phase_2 <= '1' when s_reg=SU_v else '0';

shift_count_raw <= ite(i_reg(4 downto 3) = "00", unsigned'("100"), unsigned('0' & i_reg(4 downto 3))); -- unsigned('0' & i_reg(4 downto 3)) + 1;
   --(std_logic_vector(unsigned('0' & i_reg(4 downto 3)) + 1))(1 downto 0);
shift_count_part_1 <= shift_count_raw when  opcode_direction_right = '1' else 
  8 - shift_count_raw;
--opcode_shift_rotate, opcode_shift, opcode_rotate, opcode_direction_left,  opcode_direction_right : std_logic;

B_shift_input <= b_reg(7) when (opcode_shift = '1' and opcode_direction_right = '1') else b_reg(0);

C_shift_input <= '0' when ( shift_count_raw + adding_phase_start > read_process_phase ) and (shift_phase_2 and opcode_direction_left and opcode_shift)='1' else b_reg(0);

displ_run <= ite(s_reg /= QF_v, '1', '0');

process_clk_debug : process(clk)
  variable L, L2, L3: line;
  variable out_digit : natural range 0 to 255;
begin
  if rising_edge(clk) then
    cycle_count <= cycle_count + 1;
    write(L, string'(" cycle:"));
    write(L, cycle_count);

    write(L, string'(" state:"));
    write(L, statename(s_reg));
    write(L, string'(" "));
    write(L, to_bstring(s_reg));
    
    write(L, string'(" addr_reg:"));
    write(L, to_bstring(addr_reg));

    write(L, string'(" i_reg:"));
    write(L, to_bstring(i_reg));

    write(L, string'(" w_reg:"));
    write(L, to_bstring(w_reg));
    
    write(L, string'(" data_bus:"));
    write(L, to_bstring(data_bus));
    
    write(L, string'(" address:"));
    write(L, to_bstring(address_dbg));

    
    write(L, string'(" read_process_phase:"));
    write(L, read_process_phase);
    
    write(L, string'(" read_process_start, read_process_start_latch, end, addr_from_w, areg_from_alu, addr_reg_p:"));
    write(L, to_bstring(read_process_start & read_process_start_latch & read_process_end & addr_from_w & areg_from_alu & addr_reg_p));
    
    writeline(output, L);

   
    write(L, string'(" alu, alu_not_tm, imm/mem/ind/dex/jd/ji/j" &
   "-X- tm/tm_imm/BM: "));
    write(L, to_bstring(    
opcode_alu & opcode_alu_not_tm & op_immed &  op_mem &  op_ind &  op_dex &  op_jump_direct &  op_jump_indirect &  op_jump & 
   'X' &
   opcode_store &  opcode_store_imm & opcode_bit_manip    
    ));

    write(L, string'(" alu_opcode_section"));
    write(L, to_bstring(  alu_opcode_section));

    write(L, string'(" b_reg:"));
    write(L, to_bstring(b_reg));

    write(L, string'(" c_reg:"));
    write(L, to_bstring(c_reg));

    write(L, string'(" mem_we & c_reg_oe & mem_oe & store_c_reg & condition_match_register:"));
    write(L, to_bstring(
    mem_we_dbg & c_reg_oe & mem_oe_dbg & store_c_reg & condition_match_register  ));
    
    
    write(L, string'(" alu_sum & ALU_C_reg & read_w_reg_before"));
    write(L, to_bstring(alu_sum & ALU_C_reg & read_w_reg_before ));


    write(L, string'(" ALU_SUM & ALU_A_INTL & ALU_B_INTL: "));
    write(L, to_bstring(ALU_SUM & ALU_A_INTL & ALU_B_INTL));


    --write(L, string'(" ALU_OP_a & ALU_OP_b & ALU_OP_add & ALU_OP_sub & ALU_OP_lneg & ALU_OP_and & ALU_OP_or "));
    --write(L, to_bstring( ALU_OP_a & ALU_OP_b & ALU_OP_add & ALU_OP_sub & ALU_OP_lneg & ALU_OP_and & ALU_OP_or ));


    write(L, string'(" ALU_A & ALU_B & ALU_C_REG:"));
    write(L, to_bstring( ALU_A & ALU_B & ALU_C_REG ));

    --write(L, string'(" P_incr_1, P_incr_2, P_incr_4 & B_shift_input & C_shift_input:"));
    --write(L, to_bstring( P_incr_1 & P_incr_2 & P_incr_4  & B_shift_input & C_shift_input));
    
    --write(L, string'(" shift_count_part_1, shift_count_raw:"));
    --write(L, to_integer(shift_count_part_1));
    --write(L, string'(" , "));
    --write(L, to_integer(shift_count_raw));
    
    write(L, string'(" addr_reg_p & addr_reg_x  & addr_abx_instr  & addr_ab_shift_instr  & store_flags_reg & carry_flag & ovf_flag:"));
    write(L, to_bstring( addr_reg_p  & addr_reg_x  & addr_abx_instr  & addr_ab_shift_instr  & store_flags_reg & carry_flag & ovf_flag));
    



    
    writeline(output, L);
    writeline(output, L);
    

  end if;
  
end process;



end architecture;


--
--
--
