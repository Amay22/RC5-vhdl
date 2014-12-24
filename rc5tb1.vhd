--------------------------------------------------------------------------------
-- Company: 
-- Engineer:
--
-- Create Date:   14:00:12 12/03/2014
-- Design Name:   
-- Module Name:   C:/Xilinx/projects/fpgarc5complete/rc5tb1.vhd
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
library std;
--Slibrary textutil;
USE ieee.std_logic_1164.ALL;

use std.textio.all;
USE WORK.rc5_pkg.ALL;

use work.abc.all;
 
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--USE ieee.numeric_std.ALL;
 
ENTITY rc5tb1 IS
END rc5tb1;
 
ARCHITECTURE behavior OF rc5tb1 IS 
 
    -- Component Declaration for the Unit Under Test (UUT)
 
    COMPONENT kt
    PORT(
         clock : IN  std_logic;
         outsel : IN  std_logic;
         clr : IN  std_logic;
	--		skey: inout ROM1;
         tempVal : IN  std_logic_vector(63 downto 0);
         key_rdy : OUT  std_logic;
         key_vld : IN  std_logic;
         di_vld : IN  std_logic;
         rc5Val : OUT  std_logic_vector(63 downto 0);
         do_rdy : OUT  std_logic
        );
    END COMPONENT;
    

   --Inputs
   signal clock : std_logic := '0';
   signal outsel : std_logic := '0';
   signal clr : std_logic := '0';
   signal tempVal : std_logic_vector(63 downto 0) := (others => '0');
   signal key_vld : std_logic := '0';
   signal di_vld : std_logic := '0';

 	--Outputs
   signal key_rdy : std_logic;
   signal rc5Val : std_logic_vector(63 downto 0);
   signal do_rdy : std_logic;
	--signal skey: work.rc5_pkg.ROM1;
   -- Clock period definitions
   constant clock_period : time := 20 ns;
 
BEGIN
 
	-- Instantiate the Unit Under Test (UUT)
   uut: kt PORT MAP (
          clock => clock,
          outsel => outsel,
          clr => clr,
--			 skey=>skey,
          tempVal => tempVal,
          key_rdy => key_rdy,
          key_vld => key_vld,
          di_vld => di_vld,
          rc5Val => rc5Val,
          do_rdy => do_rdy
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
    -- hold reset state for 100 ns.
         file cmdfile: TEXT;       -- Define the file 'handle'
        variable L: Line;         -- Define the line buffer
        variable good: boolean; --status of the read operation
variable count : integer := 1;
 --       variable d: std_logic;
       variable din : std_logic_vector(63 downto 0);
		 variable dout: std_logic_vector(63 downto 0); 
   --     variable S: std_logic_vector(31 downto 0); 

      -- insert stimulus here s

	begin	
clr<='0';	
clr<='1';
key_vld<='1';


outsel<='0';

wait for 85*clock_period;
key_vld<='0';
wait for clock_period;
di_vld<='1';
FILE_OPEN(cmdfile,"C:\Users\Amay\Desktop\Testbench\rc5_10k_test_vectors_ct1.txt",READ_MODE);
        loop

            if endfile(cmdfile) then  -- Check EOF
                assert false
                    report "End of file encountered; exiting."
                    severity NOTE;
                exit;
            end if;

           readline(cmdfile,L);           -- Read the line
            next when L'length = 0;  -- Skip empty lines
          hread(L,din,good);     -- Read the A argument as hex value
            assert good
                report "Text I/O read error"
                severity ERROR;
    --  HREAD(L,din,good);     -- Read the A argument as hex value
      --      assert good
        --        report "Text I/O read error"
          --      severity ERROR;

            hread(L,dout,good);     -- Read the B argument
            assert good
              report "Text I/O read error"
            severity ERROR;

            --hread(L,S,good);     -- Read the Sum expected resulted
            --assert good
              --  report "Text I/O read error"
      
		--severity ERROR;

		tempVal<=din ;
		clr<='0';
		clr<='1';
		
		--rc5Val<=dout;
		 wait for 15*clock_period;

            assert (rc5Val = dout)
                report "Check failed!" &  integer'image(count)
                    severity ERROR;
assert (rc5Val /= dout)
                report "Check Passed!" &  integer'image(count)
                    severity NOTE;
--             assert (cout = CO)
  --              report "Check failed!"
    --                severity ERROR;
        count := count +1;
		  end loop;

      wait;
   end process;

END;
