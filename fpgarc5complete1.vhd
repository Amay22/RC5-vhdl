
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
USE WORK.rc5_pkg.ALL;

entity kt is
port (clock,  outsel : in std_logic;
	--halfSel : in std_logic_vector(1 downto 0);
	clr: IN STD_LOGIC;
	tempVal : in std_logic_vector (63 downto 0);
	key_rdy: out STD_LOGIC;
	key_vld: IN STD_LOGIC;
--	skey :inout ROM1;
	di_vld: IN STD_LOGIC;
	rc5Val :out std_logic_vector (63 downto 0);
	do_rdy: OUT STD_LOGIC );

end kt;

architecture kt of kt is

   component rc5_key IS							--DEFINED keyFFERENT PORTS IN ENTITY
	port(
	skey: OUT ROM1;
	clock: IN STD_LOGIC;
	clr: IN STD_LOGIC;
	key_vld: IN STD_LOGIC;
	key_rdy: OUT STD_LOGIC );
	end component rc5_key;
	

component rc5enc IS
    PORT
    (
	clr	: IN	STD_LOGIC;
	clk	: IN	STD_LOGIC;
	skey: IN ROM1;
	din	: IN	STD_LOGIC_VECTOR(63 DOWNTO 0);
	di_vld	: IN	STD_LOGIC;  -- input is valid
	dout	: OUT	STD_LOGIC_VECTOR(63 DOWNTO 0);
	do_rdy	: OUT	STD_LOGIC   -- output is ready
     );
END component;

component decryption IS
    PORT
    (
	clr	: IN	STD_LOGIC;
	clock	: IN	STD_LOGIC;
	din	: IN	STD_LOGIC_VECTOR(63 DOWNTO 0);
	di_vld	: IN	STD_LOGIC;  -- input is valid
	dout	: OUT	STD_LOGIC_VECTOR(63 DOWNTO 0);
	skey: in ROM1;
	dec_rdy	: OUT	STD_LOGIC   -- output is ready
     );
END component;
	

		
	
	
	-- internal signals
   signal keyVal: std_logic_vector (31 downto 0);
		signal skey: work.rc5_pkg.ROM1;
	signal enVal, decVal : std_logic_vector(63 downto 0);
	signal en_rdy : std_logic;
	signal dec_rdy : std_logic;

	
	
begin
	

		encryptor: 	rc5enc port map (din => tempVal,
												dout => enVal,
												skey=>skey,
												clk => clock,
												clr => clr,
												di_vld => di_vld,
												do_rdy => en_rdy);
	key_generate: 	rc5_key port map (skey => skey, clock => clock, clr => clr, key_vld => key_vld, key_rdy => key_rdy);                            
	

	decryptor: 	decryption port map (din => tempVal, 
												dout => decVal,
												clock => clock,
												skey=>skey,
												clr => clr,
												di_vld => di_vld,
												dec_rdy => dec_rdy);                          
                              
	 with outsel select
      do_rdy <= en_rdy when '0',
                    dec_rdy when others;
   with outsel select
      rc5Val <= enVal when '0',
                    decVal when others;

	

end kt;




----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    13:49:33 10/30/2014 
-- Design Name: 
-- Module Name:    rc5enc - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
USE WORK.rc5_pkg.ALL;
ENTITY rc5enc IS
    PORT
    (
	clr	: IN	STD_LOGIC;
	clk	: IN	STD_LOGIC;
	skey: IN ROM1;
	din	: IN	STD_LOGIC_VECTOR(63 DOWNTO 0);
	di_vld	: IN	STD_LOGIC;  -- input is valid
	dout	: OUT	STD_LOGIC_VECTOR(63 DOWNTO 0);
	do_rdy	: OUT	STD_LOGIC   -- output is ready
     );
END rc5enc;
ARCHITECTURE behavioral OF rc5enc IS
    SIGNAL i_cnt	: STD_LOGIC_VECTOR(3 DOWNTO 0); -- round counter

    SIGNAL ab_xor	: STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL a_rot	: STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL a	: STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL a_pre	: STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL a_reg	: STD_LOGIC_VECTOR(31 DOWNTO 0); -- register A

    SIGNAL ba_xor: STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL b_rot	: STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL b	: STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL b_pre	: STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL b_reg	: STD_LOGIC_VECTOR(31 DOWNTO 0);
-- define a type for round keys
--instantiate round key rom with 26 round keys
  -- RC5 state machine has five states 
   
	
	TYPE  StateType IS (ST_IDLE, --
                                 ST_PRE_ROUND, -- in this state RC5 pre-round op is performed 
                                 ST_ROUND_OP, -- in this state RC5 round op is performed. The state machine remains in this state for twelve clock cycles.
                                 ST_READY -- 
                                  );
   -- RC5 state machine has five states: idle, pre_round, round and ready
   SIGNAL  state   :   StateType;
BEGIN
 -- A=((A XOR B)<<<B) + S[2*i];
 ab_xor <= a_reg XOR b_reg;
 WITH b_reg(4 DOWNTO 0) SELECT
  a_rot<=ab_xor(30 DOWNTO 0) & ab_xor(31) WHEN "00001",
            ab_xor(29 DOWNTO 0) & ab_xor(31 DOWNTO 30) WHEN "00010",
				ab_xor(28 DOWNTO 0) & ab_xor(31 DOWNTO 29) WHEN "00011",
				ab_xor(27 DOWNTO 0) & ab_xor(31 DOWNTO 28) WHEN "00100",
				ab_xor(26 DOWNTO 0) & ab_xor(31 DOWNTO 27) WHEN "00101",
				ab_xor(25 DOWNTO 0) & ab_xor(31 DOWNTO 26) WHEN "00110",
				ab_xor(24 DOWNTO 0) & ab_xor(31 DOWNTO 25) WHEN "00111",
				ab_xor(23 DOWNTO 0) & ab_xor(31 DOWNTO 24) WHEN "01000",
				ab_xor(22 DOWNTO 0) & ab_xor(31 DOWNTO 23) WHEN "01001",
				ab_xor(21 DOWNTO 0) & ab_xor(31 DOWNTO 22) WHEN "01010",
				ab_xor(20 DOWNTO 0) & ab_xor(31 DOWNTO 21) WHEN "01011",
				ab_xor(19 DOWNTO 0) & ab_xor(31 DOWNTO 20) WHEN "01100",
				ab_xor(18 DOWNTO 0) & ab_xor(31 DOWNTO 19) WHEN "01101",
				ab_xor(17 DOWNTO 0) & ab_xor(31 DOWNTO 18) WHEN "01110",
				ab_xor(16 DOWNTO 0) & ab_xor(31 DOWNTO 17) WHEN "01111",
				ab_xor(15 DOWNTO 0) & ab_xor(31 DOWNTO 16) WHEN "10000",
				ab_xor(14 DOWNTO 0) & ab_xor(31 DOWNTO 15) WHEN "10001",
				ab_xor(13 DOWNTO 0) & ab_xor(31 DOWNTO 14) WHEN "10010",
				ab_xor(12 DOWNTO 0) & ab_xor(31 DOWNTO 13) WHEN "10011",
				ab_xor(11 DOWNTO 0) & ab_xor(31 DOWNTO 12) WHEN "10100",
				ab_xor(10 DOWNTO 0) & ab_xor(31 DOWNTO 11) WHEN "10101",
				ab_xor(9 DOWNTO 0) & ab_xor(31 DOWNTO 10) WHEN "10110", 
				ab_xor(8 DOWNTO 0) & ab_xor(31 DOWNTO 9) WHEN "10111",
				ab_xor(7 DOWNTO 0) & ab_xor(31 DOWNTO 8) WHEN "11000",
				ab_xor(6 DOWNTO 0) & ab_xor(31 DOWNTO 7) WHEN "11001",
				ab_xor(5 DOWNTO 0) & ab_xor(31 DOWNTO 6) WHEN "11010",
				ab_xor(4 DOWNTO 0) & ab_xor(31 DOWNTO 5) WHEN "11011",
				ab_xor(3 DOWNTO 0) & ab_xor(31 DOWNTO 4) WHEN "11100",
				ab_xor(2 DOWNTO 0) & ab_xor(31 DOWNTO 3) WHEN "11101",
				ab_xor(1 DOWNTO 0) & ab_xor(31 DOWNTO 2) WHEN "11110",
				ab_xor(0) & ab_xor(31 DOWNTO 1) WHEN "11111",
				ab_xor WHEN OTHERS;
  
		a_pre<=din(63 DOWNTO 32) + skey(0); -- A = A + S[0]
		a<=a_rot + skey(CONV_INTEGER(i_cnt & '0'));  -- S[2*i]
-- B=((B XOR A) <<<A)	+ S[2*i+1]
    
	 ba_xor <= b_reg XOR a;
    
	 WITH a(4 DOWNTO 0) SELECT
        b_rot<=ba_xor(30 DOWNTO 0) & ba_xor(31) WHEN "00001",
					ba_xor(29 DOWNTO 0) & ba_xor(31 DOWNTO 30) WHEN "00010",
					ba_xor(28 DOWNTO 0) & ba_xor(31 DOWNTO 29) WHEN "00011",
					ba_xor(27 DOWNTO 0) & ba_xor(31 DOWNTO 28) WHEN "00100",
					ba_xor(26 DOWNTO 0) & ba_xor(31 DOWNTO 27) WHEN "00101",
					ba_xor(25 DOWNTO 0) & ba_xor(31 DOWNTO 26) WHEN "00110",
					ba_xor(24 DOWNTO 0) & ba_xor(31 DOWNTO 25) WHEN "00111",
					ba_xor(23 DOWNTO 0) & ba_xor(31 DOWNTO 24) WHEN "01000",
					ba_xor(22 DOWNTO 0) & ba_xor(31 DOWNTO 23) WHEN "01001",
					ba_xor(21 DOWNTO 0) & ba_xor(31 DOWNTO 22) WHEN "01010",
					ba_xor(20 DOWNTO 0) & ba_xor(31 DOWNTO 21) WHEN "01011",
					ba_xor(19 DOWNTO 0) & ba_xor(31 DOWNTO 20) WHEN "01100",
					ba_xor(18 DOWNTO 0) & ba_xor(31 DOWNTO 19) WHEN "01101",
					ba_xor(17 DOWNTO 0) & ba_xor(31 DOWNTO 18) WHEN "01110",
					ba_xor(16 DOWNTO 0) & ba_xor(31 DOWNTO 17) WHEN "01111",
					ba_xor(15 DOWNTO 0) & ba_xor(31 DOWNTO 16) WHEN "10000",
					ba_xor(14 DOWNTO 0) & ba_xor(31 DOWNTO 15) WHEN "10001",
					ba_xor(13 DOWNTO 0) & ba_xor(31 DOWNTO 14) WHEN "10010",
					ba_xor(12 DOWNTO 0) & ba_xor(31 DOWNTO 13) WHEN "10011",
					ba_xor(11 DOWNTO 0) & ba_xor(31 DOWNTO 12) WHEN "10100",
					ba_xor(10 DOWNTO 0) & ba_xor(31 DOWNTO 11) WHEN "10101",
					ba_xor(9 DOWNTO 0) & ba_xor(31 DOWNTO 10) WHEN "10110",
					ba_xor(8 DOWNTO 0) & ba_xor(31 DOWNTO 9) WHEN "10111",
					ba_xor(7 DOWNTO 0) & ba_xor(31 DOWNTO 8) WHEN "11000",
					ba_xor(6 DOWNTO 0) & ba_xor(31 DOWNTO 7) WHEN "11001",
					ba_xor(5 DOWNTO 0) & ba_xor(31 DOWNTO 6) WHEN "11010",
					ba_xor(4 DOWNTO 0) & ba_xor(31 DOWNTO 5) WHEN "11011",
					ba_xor(3 DOWNTO 0) & ba_xor(31 DOWNTO 4) WHEN "11100",
					ba_xor(2 DOWNTO 0) & ba_xor(31 DOWNTO 3) WHEN "11101",
					ba_xor(1 DOWNTO 0) & ba_xor(31 DOWNTO 2) WHEN "11110",
					ba_xor(0) & ba_xor(31 DOWNTO 1) WHEN "11111",
					ba_xor WHEN OTHERS;

         
    b_pre <= din(31 DOWNTO 0) + skey(1);  -- B = B + S[1]
    b<=b_rot + skey(CONV_INTEGER(i_cnt & '1'));  -- S[2*i+1]
    -- A register
    
	 PROCESS(clr, clk)  BEGIN
        IF(clr='0') THEN
           a_reg<=(OTHERS=>'0');
        ELSIF(clk'EVENT AND clk='1') THEN
            IF(state=ST_PRE_ROUND) THEN   a_reg<=a_pre;
           ELSIF(state=ST_ROUND_OP) THEN   a_reg<=a;   
			  END IF;
        END IF;
    END PROCESS;
    
-- B register
    PROCESS(clr, clk) 
	 BEGIN

        IF(clr='0') THEN
           b_reg<=(OTHERS=>'0');
        ELSIF(clk'EVENT AND clk='1') THEN
           IF(state=ST_PRE_ROUND) THEN   b_reg<=b_pre;
          ELSIF(state=ST_ROUND_OP) THEN   b_reg<=b;   
			 END IF;
        END IF;
    END PROCESS;   
   
	PROCESS(clr, clk)
   
	BEGIN
      IF(clr='0') THEN
         state<=ST_IDLE;
      ELSIF(clk'EVENT AND clk='1') THEN
         CASE state IS
            WHEN ST_IDLE=>  IF(di_vld='1') THEN state<=ST_PRE_ROUND;  END IF;
            WHEN ST_PRE_ROUND=>    state<=ST_ROUND_OP;
            WHEN ST_ROUND_OP=>  IF(i_cnt="1100") THEN state<=ST_READY;  END IF;
            WHEN ST_READY=>   state<=ST_IDLE;
         END CASE;
      END IF;
   END PROCESS;
-- round counter
    PROCESS(clr, clk)  BEGIN
        IF(clr='0') THEN
           i_cnt<="0001";
        ELSIF(clk'EVENT AND clk='1') THEN
           IF(state=ST_ROUND_OP) THEN
              IF(i_cnt="1100") THEN   i_cnt<="0001";
              ELSE    i_cnt<=i_cnt+'1';    END IF;
           END IF;
        END IF;
    END PROCESS;   
dout<=a_reg & b_reg;

    WITH state SELECT
        do_rdy<=	'1' WHEN ST_READY,
		'0' WHEN OTHERS;
END behavioral;

----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    13:49:33 10/30/2014 
-- Design Name: 
-- Module Name:    rc5enc - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------

LIBRARY IEEE;								--DEFINED ALL LIBRARIES TO USE
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
USE WORK.rc5_pkg.ALL;
ENTITY decryption IS						--DEFINED DIFFERENT PORTS IN ENTITY
PORT(
din: IN STD_LOGIC_VECTOR(63 DOWNTO 0);
dout: OUT STD_LOGIC_VECTOR(63 DOWNTO 0);
skey:in ROM1;

clock: IN STD_LOGIC;
clr: IN STD_LOGIC;
di_vld: IN STD_LOGIC;
dec_rdy: OUT STD_LOGIC );
END decryption ;

ARCHITECTURE df of decryption is					--DEFINED DIFFERENT STATES 
TYPE STATETYP IS 
       (ST_IDLE, 							-- THIS STATE REMAIN ACTIVE UNTILL clock ='0' AND DI_VLD ='0'
	ST_POST_RND, 							-- THIS STATE BECOME ACTIVE WHEN ALL THE ITERATIONS HAVE BEEN PERFOMED 
        ST_RND_OP,							-- THIS STATE COMPLETE ITERATIONS
	ST_RDY);							-- UNDER THIS STATE YOU WILL GET YOUR OUTPUT


SIGNAL  state:  STATETYP;
SIGNAL a: STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL b: STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL a_post: STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL b_post: STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL a_reg: STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL b_reg: STD_LOGIC_VECTOR(31 DOWNTO 0); 
SIGNAL asub: STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL bsub: STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL arot: STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL brot: STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL count: STD_LOGIC_VECTOR(3 DOWNTO 0);

BEGIN
-- BELOW I HAVE PERFORMED ALL NECCESSARY OPERATIONS LIKE XOR ,SHIFT AND OR.							
asub <= a_reg - skey(CONV_INTEGER(count & '0'));
WITH b(4 DOWNTO 0) SELECT						-- SHIFT OPERATION
arot<=  asub(0) & asub(31 DOWNTO 1) WHEN "00001",
	asub(1 DOWNTO 0) & asub(31 DOWNTO 2) WHEN"00010",
	asub(2 DOWNTO 0) & asub(31 DOWNTO 3) WHEN"00011",
	asub(3 DOWNTO 0) & asub(31 DOWNTO 4) WHEN"00100",
	asub(4 DOWNTO 0) & asub(31 DOWNTO 5) WHEN"00101",
	asub(5 DOWNTO 0) & asub(31 DOWNTO 6) WHEN"00110",
	asub(6 DOWNTO 0) & asub(31 DOWNTO 7) WHEN"00111",
	asub(7 DOWNTO 0) & asub(31 DOWNTO 8) WHEN"01000",
	asub(8 DOWNTO 0) & asub(31 DOWNTO 9) WHEN"01001",
	asub(9 DOWNTO 0) & asub(31 DOWNTO 10) WHEN"01010",
	asub(10 DOWNTO 0) & asub(31 DOWNTO 11) WHEN"01011", 
	asub(11 DOWNTO 0) & asub(31 DOWNTO 12) WHEN"01100",
	asub(12 DOWNTO 0) & asub(31 DOWNTO 13) WHEN"01101",
	asub(13 DOWNTO 0) & asub(31 DOWNTO 14) WHEN"01110",
	asub(14 DOWNTO 0) & asub(31 DOWNTO 15) WHEN"01111",
	asub(15 DOWNTO 0) & asub(31 DOWNTO 16) WHEN"10000",
	asub(16 DOWNTO 0) & asub(31 DOWNTO 17) WHEN"10001",
	asub(17 DOWNTO 0) & asub(31 DOWNTO 18) WHEN"10010",
	asub(18 DOWNTO 0) & asub(31 DOWNTO 19) WHEN"10011",
	asub(19 DOWNTO 0) & asub(31 DOWNTO 20) WHEN"10100",
	asub(20 DOWNTO 0) & asub(31 DOWNTO 21) WHEN"10101",
	asub(21 DOWNTO 0) & asub(31 DOWNTO 22) WHEN"10110",
	asub(22 DOWNTO 0) & asub(31 DOWNTO 23) WHEN"10111",
	asub(23 DOWNTO 0) & asub(31 DOWNTO 24) WHEN"11000",
	asub(24 DOWNTO 0) & asub(31 DOWNTO 25) WHEN"11001",
	asub(25 DOWNTO 0) & asub(31 DOWNTO 26) WHEN"11010",
	asub(26 DOWNTO 0) & asub(31 DOWNTO 27) WHEN"11011",
	asub(27 DOWNTO 0) & asub(31 DOWNTO 28) WHEN"11100",
	asub(28 DOWNTO 0) & asub(31 DOWNTO 29) WHEN"11101",
	asub(29 DOWNTO 0) & asub(31 DOWNTO 30) WHEN"11110",
	asub(30 DOWNTO 0) & asub(31) WHEN"11111",
	asub WHEN OTHERS;
	a <= arot XOR b;
bsub <= b_reg - skey(CONV_INTEGER(count & '1'));
WITH a_reg(4 DOWNTO 0) SELECT
brot <= bsub(0) & bsub(31 DOWNTO 1)    WHEN "00001",
	bsub(1 DOWNTO 0) & bsub(31 DOWNTO 2) WHEN"00010",
	bsub(2 DOWNTO 0) & bsub(31 DOWNTO 3) WHEN"00011",
	bsub(3 DOWNTO 0) & bsub(31 DOWNTO 4) WHEN"00100",
	bsub(4 DOWNTO 0) & bsub(31 DOWNTO 5) WHEN"00101",
	bsub(5 DOWNTO 0) & bsub(31 DOWNTO 6) WHEN"00110",
	bsub(6 DOWNTO 0) & bsub(31 DOWNTO 7) WHEN"00111",
	bsub(7 DOWNTO 0) & bsub(31 DOWNTO 8) WHEN"01000",
	bsub(8 DOWNTO 0) & bsub(31 DOWNTO 9) WHEN"01001",
	bsub(9 DOWNTO 0) & bsub(31 DOWNTO 10) WHEN"01010",
	bsub(10 DOWNTO 0) & bsub(31 DOWNTO 11) WHEN"01011", 
	bsub(11 DOWNTO 0) & bsub(31 DOWNTO 12) WHEN"01100",
	bsub(12 DOWNTO 0) & bsub(31 DOWNTO 13) WHEN"01101",
	bsub(13 DOWNTO 0) & bsub(31 DOWNTO 14) WHEN"01110",
	bsub(14 DOWNTO 0) & bsub(31 DOWNTO 15) WHEN"01111",
	bsub(15 DOWNTO 0) & bsub(31 DOWNTO 16) WHEN"10000",
	bsub(16 DOWNTO 0) & bsub(31 DOWNTO 17) WHEN"10001",
	bsub(17 DOWNTO 0) & bsub(31 DOWNTO 18) WHEN"10010",
	bsub(18 DOWNTO 0) & bsub(31 DOWNTO 19) WHEN"10011",
	bsub(19 DOWNTO 0) & bsub(31 DOWNTO 20) WHEN"10100",
	bsub(20 DOWNTO 0) & bsub(31 DOWNTO 21) WHEN"10101",
	bsub(21 DOWNTO 0) & bsub(31 DOWNTO 22) WHEN"10110",
	bsub(22 DOWNTO 0) & bsub(31 DOWNTO 23) WHEN"10111",
	bsub(23 DOWNTO 0) & bsub(31 DOWNTO 24) WHEN"11000",
	bsub(24 DOWNTO 0) & bsub(31 DOWNTO 25) WHEN"11001",
	bsub(25 DOWNTO 0) & bsub(31 DOWNTO 26) WHEN"11010",
	bsub(26 DOWNTO 0) & bsub(31 DOWNTO 27) WHEN"11011",
	bsub(27 DOWNTO 0) & bsub(31 DOWNTO 28) WHEN"11100",
	bsub(28 DOWNTO 0) & bsub(31 DOWNTO 29) WHEN"11101",
	bsub(29 DOWNTO 0) & bsub(31 DOWNTO 30) WHEN"11110",
	bsub(30 DOWNTO 0) & bsub(31) WHEN"11111",
	bsub WHEN OTHERS;
	b<= brot XOR a_reg;

PROCESS(clock,clr)							-- SELECTING DIFFERENT STATES ACCORDING TO INPUTS
BEGIN
IF(clr = '0') THEN
state <= ST_IDLE;
ELSIF(clock'EVENT AND clock ='1') THEN
CASE state is
WHEN ST_IDLE => IF di_vld = '1' THEN state <= ST_RND_OP; END IF;
WHEN ST_RND_OP => IF(COUNT = "0001") THEN state <= ST_POST_RND; END IF;
WHEN ST_POST_RND => state <=  ST_RDY;
WHEN ST_RDY => STATE<= ST_IDLE;
END CASE;
END IF;
END PROCESS;

PROCESS(clock,clr)							-- PERFORMING LOOP INCREMENT FUNCTION DEPENDINH UPON CLOCK AND STATES
BEGIN
IF(clr='0') THEN 
count<="1100"; 
ELSIF(clock'EVENT AND clock ='1')THEN
IF(state = ST_RND_OP) THEN
IF(count = "0001") THEN
count <= "1100";
ELSE count <= count - '1';
END IF;
END IF;
END IF;
END PROCESS;

PROCESS(clock,clr)							--DEFINTION FOR VALUE OF A ACCORDING TO DIFFERENT STATES
BEGIN
IF(clr = '0') THEN
a_post <=(OTHERS=>'0');
ELSIF(clock'EVENT AND clock ='1') THEN
IF(state = ST_IDLE) THEN
a_reg <= din(63 DOWNTO 32); END IF;
IF(state = ST_RND_OP) THEN
a_reg <= a;END IF;
IF(state = ST_POST_RND) THEN
a_reg <= a_reg - skey(0);
END IF;
END IF;

END PROCESS;

PROCESS(clock,clr)							--DEFINTION FOR VALUE OF B ACCORDING TO DIFFERENT STATES
BEGIN
IF(clr = '0') THEN
b_post <=(OTHERS=>'0');
ELSIF(clock'EVENT AND clock ='1') THEN
IF(state = ST_IDLE) THEN
b_reg <= din(31 DOWNTO 0);END IF;
IF(state = ST_RND_OP) THEN
b_reg <= b; END IF;
IF(state = ST_POST_RND) THEN
b_reg <= b_reg - skey(1);
END IF;

END IF;
END PROCESS;


WITH state SELECT							--WHEN ALL THE STATES ARE COMPLETED THEN OUTPUT SIGNAL dec_rdy BECOMES ACTIVE
dec_rdy <= '1' WHEN ST_RDY,
            '0' WHEN OTHERS;

dout <= a_reg & b_reg;	

END df;
LIBRARY IEEE; 									--DEFINED ALL LIBRARIES TO USE	
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
USE WORK.rc5_pkg.ALL;

ENTITY rc5_key IS							--DEFINED keyFFERENT PORTS IN ENTITY
PORT(
 skey: OUT ROM1;
clock: IN STD_LOGIC;
clr: IN STD_LOGIC;
key_vld: IN STD_LOGIC;
key_rdy: OUT STD_LOGIC );
END rc5_key;

ARCHITECTURE df of rc5_key is						--DEFINED keyFFERENT STATES 
TYPE STATETYP IS 
        (ST_IDLE, 								-- THIS STATE REMAIN ACTIVE UNTILL clock ='0' AND key_VLD ='0'
       	 ST_KEY_IN, 	 							-- THIS STATE ADD INTIAL TWO VALUES OF ROM
         ST_KEY_EXP,								-- THIS STATE COMPLETE ITERATIONS
	 ST_RDY);								-- UNDER THIS STATE YOU WILL GET YOUR OUTPUT
SIGNAL  STATE:  STATETYP;
SIGNAL s : ROM1;
SIGNAL l : ROM1;

SIGNAL a_circ: STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL b_circ: STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL i_cnt: STD_LOGIC_VECTOR(4 DOWNTO 0);
SIGNAL j_cnt: STD_LOGIC_VECTOR(1 DOWNTO 0);
SIGNAL r_cnt: STD_LOGIC_VECTOR(6 DOWNTO 0);
SIGNAL temp: STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL a_reg: STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL b_reg: STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL a: STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL b: STD_LOGIC_VECTOR(31 DOWNTO 0);
signal key: STD_LOGIC_VECTOR(127 DOWNTO 0);
BEGIN
key<=X"12082249120822491208224912082249";

PROCESS(clock,clr)							-- SELECTING keyFFERENT STATES ACCORkeyNG TO INPUTS
BEGIN
IF(clr = '0') THEN
STATE <= ST_IDLE;
ELSIF(clock'EVENT AND clock ='1') THEN
CASE STATE is
WHEN ST_IDLE => IF key_vld = '1' THEN STATE <= ST_KEY_IN; END IF;
WHEN ST_KEY_IN => STATE <=  ST_KEY_EXP;
WHEN ST_KEY_EXP => IF(r_cnt = "1001101") THEN STATE <= ST_RDY; END IF;
WHEN ST_RDY =>  IF key_vld = '0' THEN STATE<= ST_IDLE; END IF;
END CASE;
END IF;
END PROCESS;

PROCESS(clock,clr)							-- PERFORMING LOOP INCREMENT FUNCTION DEPENkeyNH UPON CLOCK AND STATES
BEGIN
IF(clr = '0')
THEN i_cnt <= "00000";
ELSIF(clock'EVENT AND clock ='1')THEN
IF(STATE = ST_IDLE)
THEN i_cnt <= "00000";END IF;
IF(STATE = ST_KEY_EXP) THEN
IF(i_cnt = "11001") THEN
i_cnt <= "00000";
ELSE i_cnt <= i_cnt + '1';
END IF;
END IF;
END IF;
END PROCESS;

PROCESS(clock,clr)							-- PERFORMING LOOP INCREMENT FUNCTION DEPENkeyNH UPON CLOCK AND STATES
BEGIN
IF(clr = '0')
THEN r_cnt <= "0000000";
ELSIF(clock'EVENT AND clock ='1')THEN
IF(STATE = ST_KEY_EXP) THEN
IF(r_cnt = "1001101") THEN
r_cnt <= "0000000";
ELSE r_cnt <= r_cnt + '1';
END IF;
END IF;
END IF;
END PROCESS;

PROCESS(clock,clr)							-- PERFORMING LOOP INCREMENT FUNCTION DEPENkeyNH UPON CLOCK AND STATES
BEGIN
IF(clr = '0')
THEN j_cnt <= "00";
ELSIF(clock'EVENT AND clock ='1')THEN
IF(STATE = ST_KEY_EXP) THEN
IF(j_cnt = "11") THEN
j_cnt <= "00";
ELSE j_cnt <= j_cnt + '1';
END IF;
END IF;
END IF;
END PROCESS;


PROCESS(clock,clr)							--DEFINTION FOR VALUE OF A ACCORDINNG TO keyFFERENT STATES
BEGIN
IF(clr = '0') THEN
a_reg <=(OTHERS=>'0');
ELSIF(clock'EVENT AND clock ='1') THEN
IF(STATE = ST_KEY_EXP) THEN
a_reg <= a_circ;
END IF;
END IF;
END PROCESS;


PROCESS(clock,clr)							--DEFINTION FOR VALUE OF B ACCORDING TO keyFFERENT STATES
BEGIN
IF(clr = '0') THEN
b_reg <= (OTHERS=>'0');
ELSIF(clock'EVENT AND clock ='1') THEN
IF(STATE = ST_KEY_EXP) THEN
b_reg <= b_circ;
END IF;
END IF;
END PROCESS;

process(clr, clock)
begin
    if(clr = '0') then
	   l(0) <= (OTHERS=>'0');
	   l(1) <= (OTHERS=>'0');
	   l(2) <= (OTHERS=>'0');
	   l(3) <= (OTHERS=>'0');
	elsif (clock'event and clock = '1') then
		if(STATE =  ST_KEY_IN) then
			l(0) <= key(31 downto 0);
			l(1) <= key(63 downto 32);
			l(2) <= key(95 downto 64);
			l(3) <= key(127 downto 96);
		elsif(STATE = ST_KEY_EXP) then
			l(conv_integer(j_cnt)) <= b_circ;
		end if;
	end if;
end process;

process(clr, clock)
begin
	if (clr = '0') then
		s(0) <= X"b7e15163"; s(1) <= X"5618cb1c";s(2) <= X"f45044d5";
		s(3) <= X"9287be8e";s(4) <= X"30bf3847";s(5) <= X"cef6b200";
		s(6) <= X"6d2e2bb9";s(7) <= X"0b65a572";s(8) <= X"a99d1f2b";
		s(9) <= X"47d498e4";s(10) <= X"e60c129d";s(11) <= X"84438c56";
		s(12) <= X"227b060f";s(13) <= X"c0b27fc8";s(14) <= X"5ee9f981";
		s(15) <= X"fd21733a";s(16) <= X"9b58ecf3";s(17) <= X"399066ac";
		s(18) <= X"d7c7e065";s(19) <= X"75ff5a1e";s(20) <= X"1436d3d7";
		s(21) <= X"b26e4d90";s(22) <= X"50a5c749";s(23) <= X"eedd4102";
		s(24) <= X"8d14babb";s(25) <= X"2b4c3474";
	elsif (clock'event and clock = '1') then
	if(STATE =  ST_KEY_IN) then
	s(0) <= X"b7e15163"; s(1) <= X"5618cb1c";s(2) <= X"f45044d5";
		s(3) <= X"9287be8e";s(4) <= X"30bf3847";s(5) <= X"cef6b200";
		s(6) <= X"6d2e2bb9";s(7) <= X"0b65a572";s(8) <= X"a99d1f2b";
		s(9) <= X"47d498e4";s(10) <= X"e60c129d";s(11) <= X"84438c56";
		s(12) <= X"227b060f";s(13) <= X"c0b27fc8";s(14) <= X"5ee9f981";
		s(15) <= X"fd21733a";s(16) <= X"9b58ecf3";s(17) <= X"399066ac";
		s(18) <= X"d7c7e065";s(19) <= X"75ff5a1e";s(20) <= X"1436d3d7";
		s(21) <= X"b26e4d90";s(22) <= X"50a5c749";s(23) <= X"eedd4102";
		s(24) <= X"8d14babb";s(25) <= X"2b4c3474";
	elsif (STATE = ST_KEY_EXP) then
			 s(conv_integer(i_cnt)) <= a_circ;
		end if;
	end if;
end process;





---a values
a<= s(conv_integer(i_cnt)) + a_reg + b_reg;
a_circ <= a(28 downto 0) & a(31 downto 29);

--- b values
b <= l(conv_integer(j_cnt)) + a_circ + b_reg;
temp <= a_circ + b_reg;
WITH temp(4 DOWNTO 0) SELECT
     b_circ <=  b(30 DOWNTO 0) & b(31) WHEN"00001",
		b(29 DOWNTO 0) & b(31 DOWNTO 30) WHEN"00010",
		b(28 DOWNTO 0) & b(31 DOWNTO 29) WHEN"00011",
		b(27 DOWNTO 0) & b(31 DOWNTO 28) WHEN"00100",
		b(26 DOWNTO 0) & b(31 DOWNTO 27) WHEN"00101",
		b(25 DOWNTO 0) & b(31 DOWNTO 26) WHEN"00110",
		b(24 DOWNTO 0) & b(31 DOWNTO 25) WHEN"00111",
		b(23 DOWNTO 0) & b(31 DOWNTO 24) WHEN"01000",
		b(22 DOWNTO 0) & b(31 DOWNTO 23) WHEN"01001",
		b(21 DOWNTO 0) & b(31 DOWNTO 22) WHEN"01010",
		b(20 DOWNTO 0) & b(31 DOWNTO 21) WHEN"01011",
		b(19 DOWNTO 0) & b(31 DOWNTO 20) WHEN"01100",
		b(18 DOWNTO 0) & b(31 DOWNTO 19) WHEN"01101",
		b(17 DOWNTO 0) & b(31 DOWNTO 18) WHEN"01110",
		b(16 DOWNTO 0) & b(31 DOWNTO 17) WHEN"01111",
		b(15 DOWNTO 0) & b(31 DOWNTO 16) WHEN"10000",
		b(14 DOWNTO 0) & b(31 DOWNTO 15) WHEN"10001",
		b(13 DOWNTO 0) & b(31 DOWNTO 14) WHEN"10010",
		b(12 DOWNTO 0) & b(31 DOWNTO 13) WHEN"10011",
		b(11 DOWNTO 0) & b(31 DOWNTO 12) WHEN"10100",
		b(10 DOWNTO 0) & b(31 DOWNTO 11) WHEN"10101",
		b(09 DOWNTO 0) & b(31 DOWNTO 10) WHEN"10110",
		b(08 DOWNTO 0) & b(31 DOWNTO 09) WHEN"10111", 
		b(07 DOWNTO 0) & b(31 DOWNTO 08) WHEN"11000",
		b(06 DOWNTO 0) & b(31 DOWNTO 07) WHEN"11001",
		b(05 DOWNTO 0) & b(31 DOWNTO 06) WHEN"11010",
		b(04 DOWNTO 0) & b(31 DOWNTO 05) WHEN"11011",
		b(03 DOWNTO 0) & b(31 DOWNTO 04) WHEN"11100",
		b(02 DOWNTO 0) & b(31 DOWNTO 03) WHEN"11101",
		b(01 DOWNTO 0) & b(31 DOWNTO 02) WHEN"11110",
		b(0) & b(31 DOWNTO 01) WHEN "11111",
	        b WHEN OTHERS;

WITH STATE SELECT							--WHEN ALL THE STATES ARE COMPLETED THEN OUTPUT SIGNAL key_rdy BECOMES ACTIVE
key_rdy <= '1' WHEN ST_RDY,
            '0' WHEN OTHERS;

process(clock)
begin
if(state = ST_RDY) then
skey<=s;
 end if;
end process;
END df;
