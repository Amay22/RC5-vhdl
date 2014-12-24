--------------------------------------------------------------------------------
-- Company: 
-- Engineer:
--
-- Create Date:   13:54:19 12/03/2014
-- Design Name:   
-- Module Name:   C:/Xilinx/projects/fpgarc5complete/tbrc5.vhd
-- Project Name:  fpgarc5complete
-- Target Device:  
-- Tool versions:  
-- Description:   
-- 
-- VHDL Test Bench Created by ISE for module: kt
-- 
-- Dependencies:
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
--
-- Notes: 
-- This testbench has been automatically generated using types std_logic and
-- std_logic_vector for the ports of the unit under test.  Xilinx recommends
-- that these types always be used for the top-level I/O of a design in order
-- to guarantee that the testbench will bind correctly to the post-implementation 
-- simulation model.
--------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
 
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--USE ieee.numeric_std.ALL;
 
ENTITY tbrc5 IS
END tbrc5;
 
ARCHITECTURE behavior OF tbrc5 IS 
 
    -- Component Declaration for the Unit Under Test (UUT)
 
    COMPONENT kt
    PORT(
         clock : IN  std_logic;
         outsel : IN  std_logic;
         halfSel : IN  std_logic_vector(1 downto 0);
         clr : IN  std_logic;
         key_rdy : OUT  std_logic;
         key_vld : IN  std_logic;
         di_vld : IN  std_logic;
         do_rdy : OUT  std_logic;
         inputSel : IN  std_logic_vector(1 downto 0);
         currentValue : OUT  std_logic_vector(0 to 6);
         digit3en : OUT  std_logic;
         digit2en : OUT  std_logic;
         digit1en : OUT  std_logic;
         digit0en : OUT  std_logic;
         dot : OUT  std_logic
        );
    END COMPONENT;
    

   --Inputs
   signal clock : std_logic := '0';
   signal outsel : std_logic := '0';
   signal halfSel : std_logic_vector(1 downto 0) := (others => '0');
   signal clr : std_logic := '0';
   signal key_vld : std_logic := '0';
   signal di_vld : std_logic := '0';
   signal inputSel : std_logic_vector(1 downto 0) := (others => '0');

 	--Outputs
   signal key_rdy : std_logic;
   signal do_rdy : std_logic;
   signal currentValue : std_logic_vector(0 to 6);
   signal digit3en : std_logic;
   signal digit2en : std_logic;
   signal digit1en : std_logic;
   signal digit0en : std_logic;
   signal dot : std_logic;

   -- Clock period definitions
   constant clock_period : time := 10 ns;
 
BEGIN
 
	-- Instantiate the Unit Under Test (UUT)
   uut: kt PORT MAP (
          clock => clock,
          outsel => outsel,
          halfSel => halfSel,
          clr => clr,
          key_rdy => key_rdy,
          key_vld => key_vld,
          di_vld => di_vld,
          do_rdy => do_rdy,
          inputSel => inputSel,
          currentValue => currentValue,
          digit3en => digit3en,
          digit2en => digit2en,
          digit1en => digit1en,
          digit0en => digit0en,
          dot => dot
        );

   -- Clock process definitions
   clock_process :process
   begin
		clock <= '0';
		wait for clock_period/2;
		clock <= '1';
		wait for clock_period/2;
   end process;
 

   -- Stimulus process
   stim_proc: process
   begin		
      -- hold reset state for 100 ns.
      wait for 100 ns;	

      wait for clock_period*10;

      -- insert stimulus here 

      wait;
   end process;

END;
