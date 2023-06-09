library IEEE;
use IEEE.std_logic_1164.all,  std.textio.all, IEEE.numeric_std.all;
use work.myfuncs.all;


entity kenb_cpu is
  port ( 
    mem0_out, mem1_out : in std_logic;
    mem0_in, mem1_in : out std_logic;
    rst : in std_logic;
    clk  : in std_logic;
    input_data_btn: in std_logic_vector(7 downto 0);
    data_output_register: out std_logic_vector(7 downto 0);
    start, stmem, rdmem, setaddr, stop, displ_addr, clear : in std_logic;
    cdispl_addr, cdispl_mem, displ_run, cdispl_input : out std_logic
  );
end entity kenb_cpu;

--      go <= start;
--      en <= stmem;
--      dd <= rdmem;
--      ea <= setaddr;
--    da <= displ_addr; --displ addr
--    ht <= stop; -- halt
--    cl <= clear; --clear


architecture kenb_cpu_impl of kenb_cpu is

function statename(state : std_logic_vector(4 downto 0))  return string is
 constant states : string := "QCQDQFQESRSSSUSESAQBSXSYSBSTSV..SMSHSKSFSNSJSLSGSPSWSCSDSZ..SQ...."; 
 variable pos: natural;
 begin
  if (state(0) = 'U') then return "UU"; end if;
  if (state(0) xor state(1) xor state(2) xor state(3) xor state(4)) = 'X' then
    return "XX";
  end if;
  pos := to_integer(unsigned(state));
  return states(pos*2+1 to pos*2+2);
 end function;




  -- signal data : std_logic_vector(depth-1 downto 0 );
  -- signal counter : natural range 0 to depth-1;

  signal cycle_count : natural range 0 to 1204*1024;

  --signal data, data_out, clock, rst : std_logic;
  --signal data2, data2_out : std_logic;

  signal CP : std_logic;
  signal phases_T : std_logic_vector(7 downto 0);
  signal t7, t7_neg : std_logic;
  signal c7_neg, c7 : std_logic;
  signal mem_count : natural range 0 to 1023;
  signal L_reg : std_logic_vector(7 downto 0);
  --signal L_reg_carry : std_logic;
  signal A, A_tmp1, ADD, B,C,CR, SUM : std_logic;
  signal BM, BU,BT_latch, BT, BD : std_logic;
  signal CL, CM, CY : std_logic;
  signal DA, DD : std_logic;
  signal dl0,dl1, jdl0, jdl1 : std_logic;
  signal ED, EN_OR_DD : std_logic;
  signal F1F2_ff , F1, F2 : std_logic;
  signal G1,G2,G3,G4,G5, JS4A,JS4B,JS3A,JS3B,JS2A, JS1A,JS1B, JS0A, JS0B : std_logic;
  signal HF, HT : std_logic;
  signal i_reg : std_logic_vector(7 downto 0);
  signal I7_AND_I6 ,  NEG_I7_OR_NEG_I6, i_10, i_11, NEG_I_12 ,  I_12 ,  NEG_I7_or_NEG_I6_or_I4_I3, longlogic : std_logic;
  signal i2_or_not_i1_or_i0, i2_ornoti1_ori0_andi5_andi3 : std_logic;
  signal i7_or_not_i6, not_i7_or_i6, i7_and_not_i6, not_i7_and_i6 : std_logic;
  signal not_QC_or_not_IN_or_not_EA : std_logic;

  signal JS0,JS1,JS2,JS3,JS4 : std_logic;
  signal J, JC, JD, JI, JM, JP : std_logic;
  signal KP : std_logic;
  signal LC, LS, LS_old : std_logic;
  signal OF_sgn, OV : std_logic;
  signal po, pt, pf : std_logic;
  signal qb, qc, qd, qe, qf : std_logic;
  signal neg_QC_or_neg_IN_or_NEG_EA : std_logic;
  signal R, RR, RR_old : std_logic;
  signal SU_OR_SX, SA, SB, SC, SD, SE, SF, SG, SH, SJ, SK, SL, SM, SN,  SP, SQ, SR, SS, ST, SU, SV, SW, SX, SY, SZ : std_logic;
  signal S_reg : std_logic_vector(4 downto 0);
  signal TM, TS, TX : std_logic;
  signal TX_reg : std_logic_vector(3 downto 0);
  signal UT : std_logic;
  signal W_reg : std_logic_vector(7 downto 0);
  signal WD, WJ, WK, WL, WT : std_logic;
  signal JW7_temp1, JW7_temp2, JW7, JW3 : std_logic;
  signal ft : std_logic;
  signal p3_tmp_1,p3_tmp_2,p3_tmp_3,p3_tmp_4 : std_logic;
  signal a7, mr : std_logic;
  signal go, en, ea : std_logic;
  -- go_in, en_in, dd_in, ea_in,
  --signal input_byte_buttons: std_logic_vector(7 downto 0);
  signal process_outputs_RS1, process_outputs_RS1_not, process_outputs_RS2, process_outputs_RS2_not : std_logic;
  signal in_sgn, ks,x1,x2,x3,x4 : std_logic;
  signal k_reg : std_logic_vector(7 downto 0);
 
  
  signal neg_i7_or_neg_i6_or_i4_and_i3 : std_logic;
  signal temp1, temp2 : std_logic;
  
   -- debugs
   signal state_prev :  std_logic_vector(4 downto 0);

   signal RR_count, LS_count: natural range 0 to 7;



begin
  process(clk, rst)
    variable L : line;
    variable cnt : natural;
    variable adr: natural;
  begin
  end process;

  CP <= clk;

  t7 <= phases_T(7);
  t7_neg <= not t7;

  phase_maker : process(cp, rst)
  begin
    if rst = '1'
    then
      phases_T <= X"01";
      mem_count <= 1016;
    elsif rising_edge(CP) then
      phases_T <= phases_T(6 downto 0)  & phases_T(7);
      mem_count <= (mem_count + 1) mod 1024;
    end if;
  end process phase_maker;

  c7_neg <= not CP and phases_T(7);
  c7 <= not c7_neg;

  process_L_reg : process(cp)
  begin
    if rst = '1'
    then
      L_reg <= X"01";
      LC  <= '0';
    elsif rising_edge(CP) then
      L_reg <= (LC xor L_reg(0) ) & L_reg(7 downto 1);
      LC <= (LC and L_reg(0)) or t7;
    end if;
  end process;

  process_CM : process(cp, rst)
  begin
    if rst = '1' then
      CM <= '0';
    elsif rising_edge(CP) then
      CM <= ((not R xor L_reg(0)) and CM) or T7;
    end if;
  end process;

  --page 3
  i7_or_not_i6 <= i_reg(7) or not i_reg(6);
  not_i7_or_i6 <= not i_reg(7) or i_reg(6);
  i7_and_not_i6 <= i_reg(7) and not i_reg(6);
  not_i7_and_i6 <= not i_reg(7) and i_reg(6);

  p3_tmp_1 <= (((SU or SX) and i_reg(5))
     or  (not_i7_and_i6 and sm) or sa or st)
      and phases_T(0);
  p3_tmp_2 <= ((sm and i7_and_not_i6) or sa or sh or st) and phases_T(1);
  p3_tmp_3 <= (sd or sc or sf or qd or sr or sk ) and W_reg(0);
  R <= p3_tmp_1 or p3_tmp_2 or p3_tmp_3; -- or qc; -- error: in schematics qc is inverted, '...or NOT QC' or no QC at all as second NAND input is 0.
  mr <= (A7 and DL1) or (DL0 and not A7);

  process_A7 : process(cp, rst)
  begin
    if rst = '1' then
      A7 <= '0';
    elsif rising_edge(cp) and T7 = '1' then
      A7 <= p3_tmp_3 or qc;
    end if;
  end process;

  -- page4
  process_inputs_ctl : process(cp, rst)
  begin
    if rst = '1' then
      in_sgn <= '0';
      ut <= '0';
    elsif rising_edge(cp) and T7 = '1'
    then
      in_sgn <= LC;
      ut <= in_sgn;
    end if;
  end process;

  process_inputs_ctl2 : process(in_sgn, rst)
  begin
    if (rst = '1') then
      go <= '0';
      en <= '0'; --enter data
      dd <= '0'; --displ data
      ea <= '0'; --set addr
    elsif falling_edge(in_sgn)
    then
      go <= start;
      en <= stmem;
      dd <= rdmem;
      ea <= setaddr;
    end if;
  end process;





  -- page 5,6
  
  
  proc_inpbuttons_rst :  process(rst)
  variable L : line;
  begin
    if rst = '1'
    then
       write(L, string'(" proc_inpbuttons_rst "));
       writeline(output, L);

      --process_outputs_RS1 <= '0';
      --process_outputs_RS1_not <= '1';
      --process_outputs_RS2 <= '0';
      --process_outputs_RS2_not <= '1';
    end if;
  end process;
  
  proc_inpbuttons :  process(input_data_btn, cp)
  variable acc : std_logic;
  begin
    acc := '0';
    for inp_btn in 0 to 7 loop
      acc := acc or (phases_t(inp_btn) and input_data_btn(inp_btn));
    end loop;
   BU <= acc;
  end process;

  process_outputs_RS1 <= DA or DD or not process_outputs_RS1_not;
  process_outputs_RS1_not <= SA or CL or BU or not process_outputs_RS1;

  process_outputs_RS2 <= DA or SA or not process_outputs_RS2_not;
  process_outputs_RS2_not <= DD or CL or BU or not process_outputs_RS2;

  X1 <= process_outputs_RS1 and process_outputs_RS2;
  X2 <= process_outputs_RS1 and process_outputs_RS2_not;
  X3 <= process_outputs_RS1_not and process_outputs_RS2;
  X4 <= process_outputs_RS1_not and process_outputs_RS2_not;
  --output 3 is not qc;
  
  -- X4 : CL or BU (when cleared or button input pressed)  KS=active when L=127/255, KS data from DL1
  -- X3 :  SA  -- when executing KS active when L=0/128 , KS data from DL1
  -- X2 : DD (display data )?   KS is active during QE state, reads from memory
  -- X1 : DA (display address)? 

  -- in_sgn 1 if L=0 or 128 ut=1 if L=1 or 129
  -- x4- k_reg = mem[377]
  -- x3- k_reg = mem[200]
  KS <= (qe and (x2 or x1)) or  (in_sgn and x4) or (ut and x3);
                 

  --page 7,8
  process_k_reg : process(cp)
  variable k_reg_bit : std_logic;
  begin
    if rising_edge(CP) and ks = '1' then
      k_reg_bit := (x1 and W_reg(0)) or (DL1 and (x3 or x4)) or (mr and x2);
      k_reg <= k_reg_bit & k_reg(7 downto 1);
    end if;
  end process;

  EN_OR_DD <= en or dd;
  not_QC_or_not_IN_or_not_EA <= not (EA and QC and IN_sgn);

  --page 9,10

  process (cp, rst)
  begin
    if rst = '1'
    then
      S_reg <= "00000";
    elsif rising_edge(CP) and T7 = '1' then
      S_reg(0) <= JS0;
      S_reg(1) <= JS1;
      S_reg(2) <= JS2;
      S_reg(3) <= JS3;
      S_reg(4) <= JS4;
    end if;
  end process;
 
  QC <= ite(S_reg = "00000", '1','0');
  QD <= ite(S_reg = "00001", '1','0');
  QF <= ite(S_reg = "00010", '1','0');
  QE <= ite(S_reg = "00011", '1','0');
  SR <= ite(S_reg = "00100", '1','0');
  SS <= ite(S_reg = "00101", '1','0');
  SU <= ite(S_reg = "00110", '1','0');
  SE <= ite(S_reg = "00111", '1','0');

  SA <= ite(S_reg = "01000", '1','0');
  QB <= ite(S_reg = "01001", '1','0');
  SX <= ite(S_reg = "01010", '1','0');
  SY <= ite(S_reg = "01011", '1','0');
  SB <= ite(S_reg = "01100", '1','0');
  ST <= ite(S_reg = "01101", '1','0');
  SV <= ite(S_reg = "01110", '1','0');

  SM <= ite(S_reg = "10000", '1','0');
  SH <= ite(S_reg = "10001", '1','0');
  SK <= ite(S_reg = "10010", '1','0');
  SF <= ite(S_reg = "10011", '1','0');
  SN <= ite(S_reg = "10100", '1','0');
  SJ <= ite(S_reg = "10101", '1','0');
  SL <= ite(S_reg = "10110", '1','0');
  SG <= ite(S_reg = "10111", '1','0');

  SP <= ite(S_reg = "11000", '1','0');
  SW <= ite(S_reg = "11001", '1','0');
  SC <= ite(S_reg = "11010", '1','0');
  SD <= ite(S_reg = "11011", '1','0');
  SZ <= ite(S_reg = "11100", '1','0');

  SQ <= ite(S_reg = "11110", '1','0');

  JS4 <=  (SJ or SM or SE or SG) or (G1 or JS4A or JS4b or G4 or SH);
  JS3 <=  (SS or SY or SV or SN or SZ) or G3 or JS3A or JS3B  or QB or SA; --error:  missed SY
  JS2 <=  ((SH or SK or SF or SA) and CM) or G2 or G5 or JS2A or SR or ST or SP or SQ;
  JS1 <=  G1 or G2 or JS1A or JS1B or G3 or QE;
  JS0 <=  ((SR or SX or SC) and CM) or JS0A or JS0B or G4 or SF or G5 or SH or QD;
 
  --page 11,12
  G1 <= (SB and not ED) or SK or SF or SC;
  G2 <= SD or SU;
  G3 <= (SB and not ED) or SC or SW or SX;
  G4 <= SV and  i_reg(0);
  G5 <= SZ and JC;
  JS4A <= ST and CM;
  JS4B <= SL and (i_reg(2) or not i_reg(1) or i_reg(0));
  JS3B <=  not JS4B and SL;
  SU_OR_SX <= SU or SX;
  JS3A <=  (SU_OR_SX and CM) or
         ((i_reg(5) or not i_12) and CM and SM) or
         ((i_reg(4) or not CM) and ST) or
         (go and QC);
  JS2A <= (i_reg(5) or i_12) and CM and SM;
  JS1A <= (SE and I_10) or
          (QD and CM) or
          (QF and (EN or DA or DD)) or
          (JS4A and i_reg(4));
  JS1B <= not ((not SG or i_reg(5) or i_reg(1)) and not SJ) and i_12;
  JS0A <= (QB and go) or
          (i_11 and SE) or  -- ~ind*dex=> SH ; JI+ind=>SF
          (SD and (i_reg(3) or i_reg(2))) or
          (QC and (EN or DA or DD));
  JS0B <= (QC and go) or (ST and not CM) or (SG and i_reg(1) and not i_reg(5));

  -- page 13
  FT <=  (qc and in_sgn) or sd or sq or ss or qe or sp;



  -- page 14,15

  I7_AND_I6 <= I_REG(6) and I_REG(7);
  NEG_I7_OR_NEG_I6 <= not I7_AND_I6;
  I7_AND_NOT_I6 <= I_REG(7) and (not I_REG(6));
  NOT_I7_AND_I6 <= I_REG(6) and (not I_REG(7));
  NEG_I_12 <= i_reg(3) and i_reg(4) and NEG_I7_OR_NEG_I6;
  I_12 <= not NEG_I_12;
  NEG_I7_or_NEG_I6_or_I4_I3 <=  not (not (i_reg(3) and i_reg(4)) and i_reg(7) and i_reg(6));
  longlogic <= i_reg(3) and i_reg(4) and i_reg(0) and i_reg(1) and not i_reg(2) and not i_reg(5) and neg_i7_or_neg_i6;
  neg_i7_or_neg_i6_or_i4_and_i3 <=  not i_reg(7) or not i_reg(6) or (i_reg(4) and i_reg(3));

  -- longlogic : xx011011  (xx = 01, 10, 11) (store (A,B,X) IMM)

  process_ireg : process(cp, rst)
  begin
   if rst = '1'
   then
      i_reg <= X"00";
   elsif rising_edge(cp) and FT='1'
   then
      I_REG <= sum & I_REG(7 downto 1);
   end if;
  end process;

  -- page 16,17
  process_wreg : process(cp, rst)
  begin
   if rst = '1'
   then
      w_reg <= X"00";
   elsif rising_edge(cp) and HF='1'
   then
      W_REG(7 downto 4) <= JW7 & W_REG(7 downto 5);
      W_REG(3 downto 0) <= JW3 & W_REG(3 downto 1);
   end if;
  end process;

  i2_ornoti1_ori0_andi5_andi3 <=  i2_or_not_i1_or_i0 and i_reg(5) and i_reg(3);

  i_10 <= (i_12 and not i_reg(1) and not i_reg(5) and i_reg(2)) or
          (not i_reg(5) and i_reg(2) and i_reg(0)) or
          (not (i_reg(2) or not i_reg(1) or i_reg(0))) or
          i2_ornoti1_ori0_andi5_andi3;

  i_11 <= ((i_reg(0) or i_reg(1)) and not i_reg(5) and i_reg(2))  --fixed was not i_reg(2)
          or i2_ornoti1_ori0_andi5_andi3;
  -- i_11 : ='xx0xx1(1|1)' load/store/alu with NOT IMM  or 'xx1x1~(010)' (JPI, JMI)

  HF <=  not (RR or i_reg(7)) or
         (RR and i_reg(7)) or not SW;
  -- for left shift/rotate , hf is 0 for starting N cycles, then 1
  -- for right shift/rotate, hs is 1 for starting N cycles, then 0

  JW7_temp1 <= (QE or SE or SB or SG or SL or SV or (QC and in_sgn and EA) or SJ);
  JW7_temp2 <= not ( sw and not i_reg(6) and not i_reg(7) ); -- 0 when shift right 

  JW7 <= (JW7_temp1 and sum) or
         (not JW7_temp1 and JW7_temp2 and W_reg(0)) or  -- fixed not JW7_temp1
         (not JW7_temp2 and w_reg(7));
  JW3 <= w_reg(4) and not (i_reg(7) and not i_reg(6) and not LS and SW);
         -- i_reg: shift left, bit entering w(3) nulled starting from T4

  -- page 18,19

  -- implementation with capacitor on output does not work
  -- tx_n controls numbers of bits to shift.
  -- tx(0): 1 bit shift; tx(3)= 4 bit
  process_tx : process(CP)
  variable shift_instr : natural range 0 to 7;
  begin
    if rst = '1'
    then
      tx_reg <= "0000";
      rr_count <= 0;
      LS_count <= 0;
    elsif rising_edge(CP)
    then
      if rr_count /= 0 then
        rr_count <= rr_count - 1;
      end if;
      if LS_count /= 0 then
	      LS_count <= LS_count - 1;
      end if;

      if phases_t(3) = '1' then
        shift_instr := to_integer(unsigned'(i_reg(4) & i_reg(3)));
        if shift_instr = 0 then
            shift_instr := 4;
        end if;
        LS_count <= shift_instr;
      end if;

      if T7 = '1' then
	    --rr_count  <= to_integer(unsigned'(i_reg(4) & i_reg(3)));
        --case std_logic_vector(1 downto 0)'(TX & TX) is
        shift_instr := to_integer(unsigned'(i_reg(4) & i_reg(3)));
        if shift_instr = 0 then
          shift_instr := 4;
        end if;
        RR_count <= shift_instr;
		
        TX_reg(0) <= not (i_reg(3) and i_reg(4));
        TX_reg(1) <= not (not i_reg(3) and i_reg(4));
        TX_reg(2) <= not (i_reg(3) and not i_reg(4));
        TX_reg(3) <= not (not i_reg(3) and not i_reg(4));
      else 
        tx_reg <= tx_reg(2 downto 0) & tx_reg(3);
      end if;
    end if;
	
  end process;
  tx <= tx_reg(3);


  RR <= '0' when RR_count > 0 else '1';
  LS <= '0' when LS_count > 0 else '1';

  -- RR should be held 0 starting from T0 for N cycles as amount of shift
  -- LS goes to 0 starting from T4 for N cycles, otherwise it's 1

  process_RR_latch : process(phases_T(0), phases_T(4), TX)
  begin
    if phases_t(0) = '1' then
      RR_old <= '0';
    end if;
    if TX = '0' and phases_t(0) = '0' then
      RR_old <= '1';
    end if;
    if phases_t(4) = '1' then
      LS_old <= '0';
    end if;
    if TX = '0' and phases_t(4) = '0' then
      LS_old <= '1';
    end if;
    if phases_t(0) = '1' then
      BT_latch <= '1';
    end if;
    if phases_t(4) = '1' then
      BT_latch <= '0';
    end if;
  end process;
  BT <= (BT_latch and not TX and not i_reg(5)) or 
        (not BT_latch and not TX and i_reg(5));

  --page 19
  SUM <= A xor not B xor not C;
  ADD <= not (SN and i_reg(3)); -- selects ADD or SUB operation
  CR <=  not ( (not C and ( ADD xor A)) or
         (not B and ( ADD xor A)) or
         (not B and not C)
         );
  OF_sgn <= ((ADD xor A) and not B and C) or
        (not (ADD xor A) and B and not C);

  process_carry : process(cp)
  begin
    if rst = '1'
    then
      C <= '0';
    elsif rising_edge(CP) then
    -- stored carry is reset before T0
      C <= not T7 AND CR;
    end if;
  end process;

  -- page 20
  A_TMP1 <= QE or (longlogic and SE);

  A <= not (
     ((SN and i_reg(4)) or not MR or A_TMP1) and 
      not (A_TMP1 and w_reg(0))
     );

  B <=  (sb and PO) or
     (phases_t(0) and  ((longlogic and SE) or (QE and EN_OR_DD)) ) or
     (w_reg(0) and ( (SN and neg_i7_or_neg_i6_or_i4_and_i3) or SJ  )) or -- fix : SN and...
     (phases_t(1) and SQ);

  --page 21
  proc_tscovb : process(CP)
  begin
    if rising_edge(CP) then
      if T7 = '1' then 
        -- (not (i6 & i7)) & ~i5 & ~i4 => add/sub instructions
        -- flags are written in SA phase, after result was writted during SN phase, but on DL1 memory
        TS <=  not ( (i_reg(6) and i_reg(7)) or i_reg(5) or i_reg(4) or not SN);
        CY <= CR;
        OV <= OF_sgn;
      end if;
      BD <= (BD or MR) AND not T7;
    end if;
  end process;

  JC <=  (MR and not ( i_reg(2) and (not i_reg(0) or i_reg(1) ) ) ) or
         (not i_reg(0) and not MR and not BD) or
         (i_reg(1) and not MR and BD) or
         I7_AND_I6; -- error: was NEG_I7_OR_NEG_I6

  --page 22
  KP <= (MR xor not i_reg(6)) and BM and i_reg(7);
  process_ef_pfto : process(cp, rst)
  variable sv_7_1 : std_logic;
  begin
    if rst = '1' then
     PO <= '0';
     PT <= '0';
     PF <= '0';
    elsif rising_edge(CP) then
     if (not SB xor ST) = '1' then
       PF <= PF or KP;
       PT <= (PT and not KP) or SE;
       PO <= PO or SU or SQ;
       sv_7_1 := not SV or i_reg(7) or i_reg(0);
       ED <= not ( ( (not (sv_7_1 and SD and HT))  and sv_7_1 and not ED )
          or QC);
     else
       PO <= PT;
       PT <= PF;
       PF <= '0';
     end if;
    end if;
  end process;

  -- page 23
 
  WJ <= not ((not SN or i_reg(5) ) and not SB);
  WK <= ( ( ( i_reg(7) and i_reg(6) and not i_reg(3) and (MR or not i_reg(4)) ) or i_reg(5) )
          and SN ) or
         (SQ or SY);
  WL <= SS or (QE and EN);
  WD <=  ( WK and w_reg(0)) or (WL and i_reg(0)) or (WJ and SUM) or (BM and i_reg(6));
  WT <=  WK or (BM and not i_reg(7)) or WL or WJ; -- error, was phases_t(7)

  i2_or_not_i1_or_i0 <= i_reg(2) or not i_reg(1) or i_reg(0);
  BM <= not i2_or_not_i1_or_i0 and BT and SL; -- bit manipulation instruction result

  -- page 24
  JDL0 <= ite((not A7 and WT) = '1', WD, DL0);
  JDL1 <= ((WT and A7) and WD) or
           (not (WT and A7) and not TS and DL1 and (not CL or not IN_sgn)) or --error:  was not CL and not IN_sgn
           (((phases_t(0) and OV) or (phases_t(1) and CY)) and TS) or
           (in_sgn and BU); --error: term was missing

  process_F1F2 : process(cp, T7)
  begin
    if falling_edge(CP) then
      F1F2_ff <= T7 or not F1F2_ff;
    end if;
    F1 <= CP and F1F2_ff;
    F2 <= CP and not F1F2_ff;
  end process;

  -- directly connected input buttons
  da <= displ_addr; --displ addr
  ht <= stop; -- halt
  cl <= clear; --clear

  -- shift registers
  dl0 <= mem0_out;
  dl1 <= mem1_out;
  mem0_in <= jdl0;
  mem1_in <= jdl1;


  --ouput LEDs
  cdispl_addr <= X1;
  cdispl_mem <= X2;
  displ_run <= not QC;
  cdispl_input <= X4;
  
  process_reg_debug: process(cp)
  variable counter : natural;
  variable L: line;
  begin
    --FT='1' loading seq i_reg else keeping i_reg
    --HF='1' loading w_reg else keeping
    -- ks = '1' - loading KS reg
    if rising_edge(cp)
    then
      if FT='1'
      then
        counter := counter +1;
      end if;
      if KS='1'
      then
        counter := counter +1;
      end if;
      if HF='0'
      then
        counter := counter +1;
      end if;
      write(L, string'(" loads count:"));
      write(L, counter);
  	  writeline(output, L);
    end if;
  end process;
  


  process_ks_debug : process(ks)
  variable L: line;
  begin
  if falling_edge(ks)
  then
    write(L, string'(" ks_write:"));
    write(L, to_integer(unsigned(k_reg)));
	  writeline(output, L);
  end if;
  if rising_edge(ks)
  then
    write(L, string'(" ks_begin"));
	  writeline(output, L);
  end if;
  end process;
    
  process_clk_debug : process(cp)
  variable L, L2, L3: line;
  variable out_digit : natural range 0 to 255;
  
    procedure print_debug is
    begin
      write(L, string'(" cycle:"));
      write(L, cycle_count);
      writeline(output, L);
     
      
      if FT = '1' then
        write(L3, string'("FT=1 "));
      end if;
      if A7 = '1' then
        write(L3, string'("A7=1 "));
      else
        write(L3, string'("A7=0 "));
      end if;

      --if (jdl0 /= dl0)  then
      write(L, string'("  JDL0, DL0 "));
      write(L, to_bstring(JDL0 & DL0 ));
      --end if;
      write(L, string'(" JDL1, DL1: "));
      --if (jdl1 /= dl1) then
      write(L, to_bstring( JDL1 & DL1));
      --end if;
      
      
      write(L3, string'(" JDL1 components: "));
      write(L3, to_bstring(
         ((WT and A7) and WD)  &
           (not (WT and A7) and not TS and DL1 and (not CL or not IN_sgn))  &
           (((phases_t(0) and OV) or (phases_t(1) and CY)) and TS) &
           (in_sgn and BU)
           ));
      write(L3, string'(" JDL1 sub components2: "));
      write(L3, to_bstring(
       not (WT and A7) & not TS & DL1 & (not CL or not IN_sgn)) );
           
      writeline(output, L3);      

      --JDL1
      --write(L, string'("(((phases_t(0) and OV) or (phases_t(1) and CY)) and TS);"));
      --write(L, to_bstring(  phases_t(0) & OV & phases_t(1) & CY &  TS ));


      write(L3, string'(" phT:"));
      out_digit := to_integer(unsigned(phases_T));
      write(L3, out_digit);
      write(L3, string'(" "));

      if ed = '1' then
        write(L, string'(" ED:1"));
      end if;
      write(L, string'(" state:"));
      write(L, statename(s_reg));

      if phases_T(0)='1' then
        write(L, string'(" L:"));
        out_digit := to_integer(unsigned(L_reg));
        write(L, out_digit);
      end if;

      write(L, string'(" i_reg:"));
      write(L, to_bstring(i_reg));
      
      write(L, string'(" W_reg:"));
      write(L, to_bstring(W_reg));
      

      write(L, string'(" SUM, A xor not B xor not C; CR, A_TMP1 "));
      write(L, to_bstring(  SUM & A &  B &  C & CR & A_TMP1));
      
      if phases_t(7) = '1' then
        if CM = '1' then
          write(L3, string'("CM=1"));
        else
          write(L3, string'("CM=0"));
        end if;
      end if;
      writeline(output, L3);


      --write(L, string'(" jdl "));
      --write(L, to_bstring( JDL1 & JDL0 ));
      --write(L, string'(" jdl "));
      --write(L, to_bstring( A7 & WT & WD & DL0 & TS & DL1 & CL & IN_sgn &  phases_t(0) & OV & phases_t(1) & CY & TS ));


      --write(L, string'(" C "));
      --write(L, to_bstring( QE & longlogic & SE ));
        


      write(L, string'(" MR, LC, T7,  IN: "));
      write(L, to_bstring(MR & LC & T7 & in_sgn));

      write(L, string'(" mem:"));
      write(L, mem_count);

      --write(L, string'(" s_reg: "));
      --write(L, to_bstring(s_reg) );


      write(L, string'(" in_sgn qc R: "));
      write(L, to_bstring(in_sgn & qc & R) );
      
      write(L, string'(" k_reg: "));
      write(L, to_bstring(k_reg) );

      if(2>1) then      
      write(L, string'(" tx_reg:"));
      write(L, to_bstring( tx_reg  ) );
      write(L, string'(" rr_count:"));
      write(L, rr_count );
      write(L, string'(" tx & phases_t(0) & rr & ls & hf & bt & bm: "));
      write(L, to_bstring( tx & phases_t(0) & rr & ls & hf & bt & bm ) );
      end if;



      write(L3, string'(" KS data mask: x1 x2 x3 x4 & ut & in_sgn & KS:"));
      write(L3, to_bstring(x1 & x2 & x3 & x4 & ut & in_sgn & KS));
      write(L3, string'(" rs1, not_rs1, rs2, not_rs2:"));

      write(L3, to_bstring(process_outputs_RS1 & process_outputs_RS1_not &
        process_outputs_RS2 & process_outputs_RS2_not));
	  writeline(output, L3);
      
      write(L3, string'("UT:"));
      write(L3, to_char( UT));
      writeline(output, L3);
      write(L3, string'("in_sgn:"));
      write(L3, to_char( in_sgn));
      writeline(output, L3);
      write(L3, string'("BU:"));
      write(L3, to_char( BU));
      writeline(output, L3);
      
      
	  if KS='1' then
        write(L3, string'(" KS data:"));
        write(L3, to_char( 
         (x1 and W_reg(0)) or (DL1 and (x3 or x4)) or (mr and x2)));
        writeline(output, L3);
	  end if;
	  
      --write(L3, string'("ut & x3  &  RS1 & RS1_not &  RS2 & RS2_not:"));
      --write(L3, to_bstring(ut &  x3 &   process_outputs_RS1 & process_outputs_RS1_not & process_outputs_RS2 &  process_outputs_RS2_not));
      --write(L3, string'(" k_reg frags: "));
      --write(L3, to_bstring((x1 and W_reg(0)) & (DL1 and (x3 or x4)) & (mr and x2)));
      --writeline(output, L3);
	  
	  --  process_outputs_RS1 & process_outputs_RS2 : 0,0
	  
	  
            

      --write(L, string'(" js3 "));
      --write(L, to_bstring( SS & SV & SN & SZ & G3 & JS3A & JS3B  & QB & SA ));
      --write(L, string'(" js0 "));
      --write(L, to_bstring( SR & SX & SC & CM & JS0A & JS0B & G4 & SF & G5 & SH & QD ));


      --write(L, string'(" go; qc "));
      --write(L, to_bstring(   go & QC ));
      write(L, string'(" go & en & dd & ea & DA & CL:"));
      write(L, to_bstring(go & en & dd & ea & DA & CL ));
      
      write(L, string'(" WD, WT, JS3A,  JS3B "));
      write(L, to_bstring( WD & WT & JS3A & JS3B));


      --write(L, string'(" js3a "));
      --write(L, to_bstring(  (SU_OR_SX and CM) &
      --   ((i_reg(5) or not i_12) and CM and SM) &
      --   ((i_reg(4) or not CM) and ST) &
      --   (go and QC) ));


      --write(L, string'(" js0 all:"));
      --write(L, to_bstring(  SR & SX & SC & (not CM) & JS0A & JS0B & G4 & SF & G5 & SH & QD ));

      --write(L, string'(" cm calc all:"));
      --write(L, to_bstring( R & L_reg(0) & CM & T7 ));




      writeline(output, L);

      --not JW7_temp1 and JW7_temp2       01  keeping  11 loading

      --write(L, string'("JW7_temp1, JW7_temp2:"));
      --write(L, to_bstring(JW7_temp1 & JW7_temp2 ));
      --write(L, string'(" JW7:"));
      --write(L, to_char( JW7 ));
      --write(L, string'("  W reg load , loop (JW7_temp1 & (not JW7_temp2)):"));
      --write(L, to_bstring(JW7_temp1 & (not JW7_temp2) ));
      --writeline(output, L);

      if SE='1' then
      write(L, string'(" B components :"));
      write(L, to_bstring(
       (sb and PO) &
       (phases_t(0) and  ((longlogic and SE) or (QE and EN_OR_DD)) ) &
       (w_reg(0) and ( neg_i7_or_neg_i6_or_i4_and_i3 or SJ  )) &
       (phases_t(1) and SQ)
       ));
      writeline(output, L);
      end if;      
      
      if state_prev /= s_reg then
        write(L, string'(" s_reg changed:"));
        writeline(output, L);
	  end if;

      state_prev <= s_reg;
    end procedure;

  begin
    if rising_edge(cp) then
      cycle_count <= cycle_count + 1;
      print_debug;
    end if;
  end process;
  
  data_output_register <= k_reg;
  
end architecture kenb_cpu_impl;


