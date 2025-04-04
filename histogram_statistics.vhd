-- ------------------------------------------------------------------------------------------------------------
-- IP Name:             histogram_statistics
-- Author:              Yifeng Wang (yifenwan@phys.ethz.ch)
-- =========================================================================
-- Revision:            1.0
-- Date:                Jul 25, 2024 (file created)
-- +-----------------------------------------------------------------------+
-- Revision:            1.1
-- Date:                Oct 22, 2024 (formatted)
-- =========================================================================
-- Description:         Host online histogram to collect statistics infomation on the snooped data stream
--
-- Implementation:                
--			1) BRAM (M10K): best for building histogram with large number of bins and low concurrency requirement
--				pro: resource usage is small, as each M10K can host 256 bins
--				con: max count of a bin is limited at 40 bit; max throughput is 1; flushing takes time
--			2) MLAB: best for high concurrency and precise timing scenario
--				pro: high concurrency, all bins can be updated at the same cycle; sclr is cycle-accurate; arbitary max count
--				con: high resource usage, 32-bit counter consumes 17 ALMs. 
-- Mode:
--          operation mode:
--              1) "free-running" mode: read <hist_bin> will get the current value after last clear.
--              2) "rate" mode: read <hist_bin> will yield the value before the last clear.
--                  - ex: if you connect clear to a 1 Hz periodic counter, this histogram will display the rate of data stream.
--          debug mode:
--              1) "debug-off" mode: no effect; the input stream will fill the histogram.
--              2) "timestamp(ts)-difference" mode: difference of the input stream and fpga local counter will be displayed.
--                  - use case: debugging the latency of the whole DAQ system, i.e. the time delay of event registered by 
--                              MuTRiG TDC and received by this IP. A good way to quantitatively study the system flow control
--                              and architectural designs.
--
-- Usage:
--          data flow:
--              - connect <hist_fill_in> to the data to snoop from upstream
--              - connect <hist_fill_out> to the downstream as intact of upstream (this IP is transparent to data)
--
--          control flow:
--              - connect avmm master to <csr> to dynamically control this IP (see "csr_address_map")
--              - read <hist_bin> to get the bin counts of histogram (see "Mode")             
--              - (*optional) connect <ctrl> to start a global sync counter, if you need to debug in "time-difference" mode. 
--
-- ------------------------------------------------------------------------------------------------------------
--	
-- ================ synthsizer configuration =================== 		
-- altera vhdl_input_version vhdl_2008
-- =============================================================

-- basic libraries
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
-- additional libraries
use ieee.std_logic_misc.or_reduce;
use ieee.std_logic_misc.and_reduce;
use IEEE.math_real.log2;
use IEEE.math_real.ceil;
use IEEE.math_real.floor;
-- altera-specific libraries
library altera; 
use altera.altera_syn_attributes.all;
LIBRARY lpm; 
USE lpm.lpm_components.all;

entity histogram_statistics is 
generic (
	-- config input update and filter key 
	UPDATE_KEY_BIT_HI	: natural := 29; -- tcc8n
	UPDATE_KEY_BIT_LO	: natural := 17;
	UPDATE_KEY_REPRESENTATION : string := "UNSIGNED";
	FILTER_KEY_BIT_HI	: natural := 38; -- asic
	FILTER_KEY_BIT_LO	: natural := 35;
	SAR_TICK_WIDTH		: natural := 32; -- for calculating the ticks, must be larger than or equal to SAR_KEY_WIDTH
	SAR_KEY_WIDTH		: natural := 16; -- the comparator width 
	-- histogram config
	N_BINS				: natural := 256; 
	MAX_COUNT_BITS		: natural := 40;
	--N_WIDTH				: natural := 16; -- in unit of the update key and must be power of 2		
	DEF_LEFT_BOUND		: integer := -1000;
	DEF_BIN_WIDTH		: natural := 16;
--	RIGHT_BOUND			: signed(31 downto 0)  := 4000; -- set by N_WIDTH;
	
	-- miscellaneous
	AVS_ADDR_WIDTH		: natural := 8; -- log2(N_BINS)
	ENABLE_REPLAY		: boolean := False;
	AVST_DATA_WIDTH		: natural := 39;
	AVST_CHANNEL_WIDTH	: natural := 4;
	ENABLE_PACKET		: boolean := True;
	SNOOP_EN			: boolean := True;
	DEBUG				: natural := 1
);
port (
	-- AVST sink <hist_bin>
	-- (read histogram bins)
	avs_hist_bin_readdata				: out std_logic_vector(31 downto 0); 
	avs_hist_bin_read					: in  std_logic;
	avs_hist_bin_address				: in  std_logic_vector(AVS_ADDR_WIDTH-1 downto 0);
	avs_hist_bin_waitrequest			: out std_logic; 
	avs_hist_bin_write					: in  std_logic;
	avs_hist_bin_writedata				: in  std_logic_vector(31 downto 0); -- write 0 to flush/clear the histogram
	-- burst and pipeline transaction
	avs_hist_bin_burstcount				: in  std_logic_vector(AVS_ADDR_WIDTH-1 downto 0); -- user can burst for the whole thing
	avs_hist_bin_readdatavalid			: out std_logic;
	avs_hist_bin_writeresponsevalid		: out std_logic;
	avs_hist_bin_response				: out std_logic_vector(1 downto 0);
	
	-- AVMM slave <csr>
    -- (dynamically set the bin width, 
	avs_csr_readdata					: out std_logic_vector(31 downto 0);
	avs_csr_read						: in  std_logic;
	avs_csr_address						: in  std_logic_vector(3 downto 0);
	avs_csr_waitrequest					: out std_logic; 
	avs_csr_write						: in  std_logic;
	avs_csr_writedata					: in  std_logic_vector(31 downto 0);
	
	-- AVST sink <hist_fill_in>
	-- (fill histogram)
	asi_hist_fill_in_ready					: out std_logic; -- not needed if replay buffer is enabled 
	asi_hist_fill_in_valid					: in  std_logic;
	asi_hist_fill_in_data					: in  std_logic_vector(AVST_DATA_WIDTH-1 downto 0);
	asi_hist_fill_in_startofpacket			: in  std_logic;
	asi_hist_fill_in_endofpacket			: in  std_logic;
	asi_hist_fill_in_channel				: in  std_logic_vector(AVST_CHANNEL_WIDTH-1 downto 0);
    asi_hist_fill_in_error                  : in  std_logic_vector(0 downto 0);
	
	-- AVST source <hist_fill_out>
	-- (mirror the hist_fill_in port)
	aso_hist_fill_out_ready					: in  std_logic := '1'; -- NOTE: if dangling, must be set to one, otherwise no data to deassembly
	aso_hist_fill_out_valid					: out std_logic;
	aso_hist_fill_out_data					: out std_logic_vector(AVST_DATA_WIDTH-1 downto 0);
	aso_hist_fill_out_startofpacket			: out std_logic;
	aso_hist_fill_out_endofpacket			: out std_logic;
	aso_hist_fill_out_channel				: out std_logic_vector(AVST_CHANNEL_WIDTH-1 downto 0);
    aso_hist_fill_out_error                 : out std_logic_vector(0 downto 0);
	
    -- AVST sink <ctrl>
	-- (run control management agent)
	asi_ctrl_data						: in  std_logic_vector(8 downto 0); 
	asi_ctrl_valid						: in  std_logic;
	asi_ctrl_ready						: out std_logic;
	
	-- debug ts port (gts - mts) <debug_1>
	asi_debug_1_valid					: in  std_logic;
	asi_debug_1_data					: in  std_logic_vector(15 downto 0);
    
    -- fillness pdf <debug_2>
    asi_debug_2_valid	    			: in  std_logic;
	asi_debug_2_data					: in  std_logic_vector(15 downto 0);
    
    -- fillness pdf <debug_3>
    asi_debug_3_valid	    			: in  std_logic;
	asi_debug_3_data					: in  std_logic_vector(15 downto 0);
    
    -- fillness pdf <debug_4>
    asi_debug_4_valid	    			: in  std_logic;
	asi_debug_4_data					: in  std_logic_vector(15 downto 0);
    
    -- fillness pdf <debug_5>
    asi_debug_5_valid	    			: in  std_logic;
	asi_debug_5_data					: in  std_logic_vector(15 downto 0);
    
    -- burstiness <debug_6>
    asi_debug_6_valid	    			: in  std_logic;
	asi_debug_6_data					: in  std_logic_vector(15 downto 0);
	
	-- reset and clock interfaces
	i_rst								: in  std_logic;
	i_clk								: in  std_logic
	
	
);
end entity histogram_statistics;

architecture rtl of histogram_statistics is

	attribute preserve : boolean;
	attribute altera_attribute of histogram_statistics : entity is "-name PRESERVE_REGISTER on";

	-------------------------------------
	-- global constant 
	-------------------------------------
	--constant HIST_LEFT_BOUND			: signed(SAR_TICK_WIDTH-1 downto 0)  := LEFT_BOUND;
	--constant HIST_RIGHT_BOUND			: signed(SAR_TICK_WIDTH-1 downto 0)  := LEFT_BOUND + to_signed(N_BINS*N_WIDTH,SAR_TICK_WIDTH); -- -1000 + 256*16 = 3096
	constant SAR_PIPELINE_N_STAGES		: natural := integer(ceil(log2(real(N_BINS))));
	constant BIN_INDEX_WIDTH			: natural := integer(ceil(log2(real(N_BINS))));
	
	-- ----------------------
	-- flushing arbiter
	-- ----------------------
	signal flushing_req						: std_logic;
	
	-- ---------------------------------
	-- avmm hist agent interface
	-- ---------------------------------
	type avmm_hist_rd_flow_t 				is (IDLE,INIT,BURST_READ,RESET);
	signal avmm_hist_rd_flow				: avmm_hist_rd_flow_t;
	type avmm_hist_wr_flow_t				is (IDLE,INIT,FLUSHING,RESET);
	signal avmm_hist_wr_flow				: avmm_hist_wr_flow_t;
	
	signal avmm_hist_read_start				: std_logic;
	signal avmm_hist_read_addr_begin		: unsigned(AVS_ADDR_WIDTH-1 downto 0);
	signal avmm_hist_read_burstcount		: unsigned(AVS_ADDR_WIDTH-1 downto 0);
	signal avmm_hist_read_cnt				: unsigned(AVS_ADDR_WIDTH-1 downto 0);
	
	signal avmm_hist_flushing_req			: std_logic;
	signal avmm_hist_read_addr				: unsigned(AVS_ADDR_WIDTH-1 downto 0);
	
	-- --------------------------------------
	-- avmm csr agent interface 
	-- --------------------------------------
	
	constant SAR_KEY_LOC_WIDTH	: natural := 8;
	constant CSR_COUNTER_WIDTH	: natural := avs_csr_readdata'length;
	type csr_t is record 
		commit					: std_logic;
		mode					: std_logic_vector(3 downto 0);
		update_key_is_unsigned	: std_logic;
		filter_enable			: std_logic;
		filter_is_reject		: std_logic;
		error					: std_logic;
		error_info				: std_logic_vector(3 downto 0);
		left_bound				: signed(SAR_TICK_WIDTH-1 downto 0); 
		right_bound				: signed(SAR_TICK_WIDTH-1 downto 0);
		bin_width				: unsigned(15 downto 0);
		update_key_low			: unsigned(SAR_KEY_LOC_WIDTH-1 downto 0); -- fixed width
		update_key_high			: unsigned(SAR_KEY_LOC_WIDTH-1 downto 0); -- fixed width
		filter_key_low			: unsigned(SAR_KEY_LOC_WIDTH-1 downto 0); -- fixed width
		filter_key_high			: unsigned(SAR_KEY_LOC_WIDTH-1 downto 0); -- fixed width
		update_key				: unsigned(SAR_KEY_WIDTH-1 downto 0);
		filter_key				: unsigned(SAR_KEY_WIDTH-1 downto 0);
		underflow_cnt			: unsigned(CSR_COUNTER_WIDTH-1 downto 0); 
		overflow_cnt			: unsigned(CSR_COUNTER_WIDTH-1 downto 0); 
	end record;
	signal csr					: csr_t;
	signal avmm_csr_flushing_req	: std_logic;
	
	-----------------------------
	-- bin width calculator
	------------------------------
	signal calc_bin_done			: std_logic;
	type calc_t is record
		left_bound					: signed(SAR_TICK_WIDTH-1 downto 0); 
		right_bound					: signed(SAR_TICK_WIDTH-1 downto 0); 
		bin_width					: unsigned(15 downto 0);
		error						: std_logic;
		error_info					: std_logic_vector(3 downto 0);
	end record;
	signal calc						: calc_t;
	-- helper check
	signal helper_check_ok			: std_logic;
	signal helper_error_symdrome	: std_logic_vector(3 downto 0);
	
	
	-- -----------------------------
	-- deassembly
	-- -----------------------------
	
	signal deassembly_update_key_raw		: std_logic_vector(AVST_DATA_WIDTH-1 downto 0);
	signal deassembly_update_key_valid		: std_logic;
	signal deassembly_update_key_error		: std_logic_vector(1 downto 0);
	
	signal deassembly_filter_key_raw		: std_logic_vector(AVST_DATA_WIDTH-1 downto 0);
	signal deassembly_filter_key_valid		: std_logic;
	signal deassembly_filter_key_error		: std_logic_vector(1 downto 0);
	
	signal deassembly_filtered_data			: std_logic_vector(SAR_KEY_WIDTH-1 downto 0);
	signal deassembly_filtered_valid		: std_logic;
    
    signal latency_out_valid                : std_logic;
    signal latency_out_data                 : std_logic_vector(SAR_KEY_WIDTH-1 downto 0);
	
	signal deassembly_out_data				: std_logic_vector(SAR_KEY_WIDTH-1 downto 0);
	signal deassembly_out_valid				: std_logic;
	
	
	
	
	-- ------------------------------------------------------
	-- Successive Approximation Register (SAR) pipeline
	-- ------------------------------------------------------
	type sar_pipeline_single_t is record
		valid				: std_logic;
		key					: std_logic_vector(SAR_KEY_WIDTH-1 downto 0);
		high_tick			: signed(SAR_TICK_WIDTH-1 downto 0);
		high_tick_comb		: signed(SAR_TICK_WIDTH-1 downto 0);
		--middle_tick			: signed(SAR_TICK_WIDTH-1 downto 0);
		middle_tick_comb	: signed(SAR_TICK_WIDTH-1 downto 0);
		low_tick			: signed(SAR_TICK_WIDTH-1 downto 0);
		low_tick_comb		: signed(SAR_TICK_WIDTH-1 downto 0);
		box_index			: unsigned(SAR_PIPELINE_N_STAGES-1 downto 0);
		box_index_comb		: unsigned(SAR_PIPELINE_N_STAGES-1 downto 0);
	end record;
	type sar_pipeline_t is array (0 to SAR_PIPELINE_N_STAGES-1) of sar_pipeline_single_t;
	signal sar_pipeline			: sar_pipeline_t;
	
--	attribute preserve of sar_pipeline(SAR_PIPELINE_N_STAGES-1).key 				: signal is true;
--	attribute preserve of sar_pipeline(SAR_PIPELINE_N_STAGES-1).high_tick 			: signal is true;
--	attribute preserve of sar_pipeline(SAR_PIPELINE_N_STAGES-1).low_tick 			: signal is true;
	
	
	-- -----------------------------------------
	-- --	lpm divider for sar pipeline
	-- -----------------------------------------
	component LPM_DIVIDE
	generic (
		LPM_WIDTHN 			: natural;
        LPM_WIDTHD 			: natural;
		LPM_NREPRESENTATION : string := "UNSIGNED";
		LPM_DREPRESENTATION : string := "UNSIGNED";
		LPM_PIPELINE 		: natural := 0;
		MAXIMIZE_SPEED		: integer := 9;
		LPM_TYPE 			: string := L_DIVIDE;
		--INTENDED_DEVICE_FAMILY	: string;
		LPM_HINT 			: string := "UNUSED"
	);
	port (
		NUMER 			: in  std_logic_vector(LPM_WIDTHN-1 downto 0);
		DENOM 			: in  std_logic_vector(LPM_WIDTHD-1 downto 0);
		ACLR 			: in  std_logic := '0';
		CLOCK 			: in  std_logic := '0';
		CLKEN 			: in  std_logic := '1';
		QUOTIENT 		: out std_logic_vector(LPM_WIDTHN-1 downto 0);
		REMAIN 			: out std_logic_vector(LPM_WIDTHD-1 downto 0)
	);
	end component LPM_DIVIDE; 
	type sar_tick_t is array (0 to SAR_PIPELINE_N_STAGES-1) of std_logic_vector(SAR_TICK_WIDTH-1 downto 0);
	signal div_in_num		: sar_tick_t; -- No. one more bit
	signal div_out_quo		: sar_tick_t;
	
	
	-- -----------------------------------------
	-- box filler 
	-- -----------------------------------------
	type box_filler_thread_a_t			is (POST_ADDR, POST_DATA, FLUSHING);
	type box_filler_thread_b_t			is (POST_ADDR, POST_DATA, AV_READ);
	signal box_filler_thread_a			: box_filler_thread_a_t;
	signal box_filler_thread_b			: box_filler_thread_b_t;
	
	signal box_filler_input_valid		: std_logic;
	signal box_filler_input_valid_a		: std_logic;
	signal box_filler_input_valid_b		: std_logic;
	signal box_filler_write_addr		: unsigned(SAR_PIPELINE_N_STAGES-1 downto 0);
	signal box_filler_write_addr_a_reg	: unsigned(SAR_PIPELINE_N_STAGES-1 downto 0);
	signal box_filler_write_addr_b_reg	: unsigned(SAR_PIPELINE_N_STAGES-1 downto 0);
	signal box_filler_thread_b_busy		: std_logic;
	signal box_filler_flushing_cnt		: unsigned(SAR_PIPELINE_N_STAGES-1 downto 0);
	signal box_filler_flushing_done		: std_logic;
	
	
	-- ----------------------------------
	-- histogram ram 
	-- ----------------------------------
	signal ram_addr_a		: unsigned(BIN_INDEX_WIDTH-1 downto 0);
	signal ram_addr_b		: unsigned(BIN_INDEX_WIDTH-1 downto 0);
	signal ram_data_a		: std_logic_vector(MAX_COUNT_BITS-1 downto 0);
	signal ram_data_b		: std_logic_vector(MAX_COUNT_BITS-1 downto 0);
	signal ram_we_a			: std_logic;
	signal ram_we_b			: std_logic;
	signal ram_q_a			: std_logic_vector(MAX_COUNT_BITS-1 downto 0);
	signal ram_q_b			: std_logic_vector(MAX_COUNT_BITS-1 downto 0);
	
	
	-- ------------------------------------
	-- helper functions
	-- ------------------------------------
--	
--	
--	function or_reduce(a : my_array) return std_logic_vector is
--    variable ret : std_logic_vector(31 downto 0) := (others => '0');
--	begin
--		for i in a'range loop
--			ret := ret or a(i);
--		end loop;
--	
--		return ret;
--	end function or_reduce;


	-- ----------------------------------
	-- histogram ram 
	-- ----------------------------------
	
	component alt_dpram_true
		PORT
		(
			address_a		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
			address_b		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
			clock		: IN STD_LOGIC  := '1';
			data_a		: IN STD_LOGIC_VECTOR (39 DOWNTO 0);
			data_b		: IN STD_LOGIC_VECTOR (39 DOWNTO 0);
			wren_a		: IN STD_LOGIC  := '0';
			wren_b		: IN STD_LOGIC  := '0';
			q_a		: OUT STD_LOGIC_VECTOR (39 DOWNTO 0);
			q_b		: OUT STD_LOGIC_VECTOR (39 DOWNTO 0)
		);
	end component;
	
	--------------------------------
	-- run management
	--------------------------------
	type run_state_t is (IDLE, RUN_PREPARE, SYNC, RUNNING, TERMINATING, LINK_TEST, SYNC_TEST, RESET, OUT_OF_DAQ, ERROR);
	signal run_state_cmd				: run_state_t;
	
	-- gts counter
	signal gts_counter_rst		: std_logic;
	signal gts_8n				: unsigned(47 downto 0);
	
	
begin

	-- ------------------------------------
	-- validation (compile time)
	-- ------------------------------------
	assert not (UPDATE_KEY_REPRESENTATION = "UNSIGNED") report "default of update key representation is 'UNSIGNED'" severity note;
	assert not (UPDATE_KEY_REPRESENTATION = "SIGNED") report "default of update key representation is 'SIGNED'" severity note;
	assert UPDATE_KEY_REPRESENTATION = "UNSIGNED" or UPDATE_KEY_REPRESENTATION = "SIGNED" report "wrong type for the update key" severity failure;
	assert SAR_TICK_WIDTH >= SAR_KEY_WIDTH report "the csr keys must be smaller than (or equal to) the width of the sar pipeline tick width (you may manually change this)" severity warning;
	
	
	
	-- ----------------------
	-- flushing arbiter
	-- ----------------------
	proc_flushing_arbiter : process (all)
	begin
		flushing_req		<= avmm_hist_flushing_req or avmm_csr_flushing_req;
		avmm_csr_flushing_req		<= '0'; -- TODO: add this to csr 
	end process;
	
	
	
	-- ---------------------------------
	-- avmm hist agent interface
	-- ---------------------------------
	proc_avmm_hist_agent : process (i_clk)
	begin
		if (rising_edge(i_clk)) then
			-- @@ SYNC RESET @@
			if (i_rst = '1') then 
				avmm_hist_rd_flow		<= RESET;
				avmm_hist_wr_flow		<= RESET;
			else
				-- read flow
				case avmm_hist_rd_flow is 
					when IDLE =>
						if (avs_hist_bin_read = '1') then
							if (box_filler_thread_b_busy = '0') then -- maybe thread b is working on another hit, wait for at most one cycle.
								avmm_hist_rd_flow		<= INIT;
							end if;
							avmm_hist_read_start	<= '1';
							-- latch the command 
							avmm_hist_read_addr_begin	<= unsigned(avs_hist_bin_address);
							avmm_hist_read_burstcount	<= unsigned(avs_hist_bin_burstcount);
						end if;
					when INIT => -- thread b in cycle of av read
						avmm_hist_rd_flow				<= BURST_READ;
					when BURST_READ =>
						if (avmm_hist_read_cnt + 1	< avmm_hist_read_burstcount) then
							avmm_hist_read_cnt		<= avmm_hist_read_cnt + 1;
						else
							avmm_hist_rd_flow		<= RESET;
							avmm_hist_read_start	<= '0';
						end if;
					when RESET =>
						avmm_hist_read_cnt		<= (others => '0');
						avmm_hist_rd_flow		<= IDLE;
					when others =>
				end case;
				
				-- write flow
				case avmm_hist_wr_flow is 
					when IDLE =>
						if (avs_hist_bin_write = '1') then 
							avmm_hist_wr_flow		<= INIT;
						end if;
					when INIT =>
						if (avs_hist_bin_writedata = (avs_hist_bin_writedata'high downto 0 => '0')) then 
							avmm_hist_wr_flow		<= FLUSHING;
						else
							avmm_hist_wr_flow		<= RESET; -- do not reset when write data is not 0
						end if;
					when FLUSHING =>
						-- inter-fsm communication with box-filler to start flush
						if (box_filler_flushing_done = '0' and avmm_hist_flushing_req = '0') then -- start
							avmm_hist_flushing_req		<= '1';
						elsif (box_filler_flushing_done = '0' and avmm_hist_flushing_req = '1') then -- in progress
							-- idle
						elsif (box_filler_flushing_done = '1' and avmm_hist_flushing_req = '1') then -- ack the box filler
							avmm_hist_flushing_req		<= '0';
						else -- box filler ack
							-- no need to do anything here, job complete
							avmm_hist_wr_flow			<= RESET;
						end if;
					when RESET =>
						avmm_hist_flushing_req			<= '0';
						avmm_hist_wr_flow				<= IDLE;
					when others =>
				end case;
			end if;
		end if;
	end process;
	
	proc_avmm_agent_comb : process (all)
	begin	
		-- default 
		avs_hist_bin_waitrequest		<= '1';
		avmm_hist_read_addr				<= (others => '0');
		avs_hist_bin_readdatavalid		<= '0';
		avs_hist_bin_readdata			<= (others => '0');
		avs_hist_bin_response			<= (others => '0');
		avs_hist_bin_writeresponsevalid	<= '0'; -- B channel is always "good", for now	
		-- read flow
		case avmm_hist_rd_flow is 
			when INIT => -- post the first addr
				avs_hist_bin_waitrequest		<= '0'; -- ack the master in the second cycle 
				avmm_hist_read_addr				<= avmm_hist_read_addr_begin; -- post the first address
			when BURST_READ => -- post the second+ addr, assert valid for first and second+ data
				avmm_hist_read_addr				<= avmm_hist_read_addr_begin + avmm_hist_read_cnt + 1;
				avs_hist_bin_readdata			<= ram_q_b(avs_hist_bin_readdata'high downto 0);
				if (avmm_hist_read_cnt < avmm_hist_read_burstcount) then
					avs_hist_bin_readdatavalid		<= '1'; -- the first data is valid
				end if;
			when others =>
		end case;
		
		-- write flow
		case avmm_hist_wr_flow is 
			when IDLE =>
			when INIT =>
				avs_hist_bin_waitrequest		<= '0';
			when FLUSHING =>
				if (box_filler_flushing_done = '0' and avmm_hist_flushing_req = '0') then -- B channel 
					avs_hist_bin_writeresponsevalid		<= '1';
				end if;
			when others =>
		end case;
	end process;
	

	
	-- --------------------------------------
	-- avmm csr agent interface 
	-- --------------------------------------
	proc_csr_agent : process (i_clk)
	-- avalon memory-mapped interface for accessing the control and status registers
	-- -----------------------------------------
	-- 		address map: (word)
	-- -----------------------------------------
	-- 		0: control and status (see code)
	--				Hex: IE XX FR MC
	--				I: error info ("0000" is no error; "0001" is key out-of-range; TBD...)
	--				E: error flag (0x0 is no error; 0x1 indicating an error)
	--				F: filter setting
	--					bit: 0 ('0' is disable; '1' is enable)
	--					bit: 1 ('0' is accept; '1' is reject)
	--				R: representation of update key (0x0 is signed [default], 0x1 is unsigned) 
	--				M: op mode. see below
    --                  
	--				C: commit (write 0x1 to commit all the settings, if read is 0x0 means setting is completed) 
	-- 
	-- 		1: left bound (signed)
	--
	--		2: right bound (signed)
	--				You don't need to set this, as it will be automatically calculated based on left bound and bin width.
	--				NOTE: If you truly wish to set this, you must set the bin width to 0.
	--
	--		3: bin width (unsigned) (limited to 16 bit)
	--				In case of conflict, the bin width will overdrive right bound.
	--
	--		4: keys bit location in the input data stream (unsigned) 
	--				Byte: FH FL UH UL
	--				FH: filter key high
	--				FL: filter key low
	--				UH: update key high
	--				UL: update key low
	--
	--		5: value of keys
	--				Half-word: UK FK
	--				UK: update key
	--				FK: filter key
	--		
	--		6: underflow count (unsigned)
	--
	--		7: overflow count (unsigned)
	
	begin
		
		if (rising_edge(i_clk)) then
			-- @@ SYNC RESET @@
			if (i_rst = '1') then 
				avs_csr_waitrequest		<= '0'; -- release the bus
				-- init reg map
				-- =========================
				-- word 0 (csr)
				csr.commit				<= '0';
				csr.mode				<= (others => '0');
				if (UPDATE_KEY_REPRESENTATION = "SIGNED") then
					csr.update_key_is_unsigned		<= '0';
				else
					csr.update_key_is_unsigned		<= '1';
				end if;
				csr.filter_enable		<= '0';
				csr.filter_is_reject	<= '0';
				csr.error				<= '0';
				csr.error_info			<= (others => '0');
				-- word 1, 2, 3, 4 (misc. hist settings)
				csr.left_bound			<= to_signed(DEF_LEFT_BOUND,SAR_TICK_WIDTH);
				csr.bin_width			<= to_unsigned(DEF_BIN_WIDTH,16);
				csr.right_bound			<= to_signed(DEF_LEFT_BOUND + DEF_BIN_WIDTH*N_BINS,SAR_TICK_WIDTH);
				csr.update_key_low		<= to_unsigned(UPDATE_KEY_BIT_LO,SAR_KEY_LOC_WIDTH);
				csr.update_key_high		<= to_unsigned(UPDATE_KEY_BIT_HI,SAR_KEY_LOC_WIDTH);
				csr.filter_key_low		<= to_unsigned(FILTER_KEY_BIT_LO,SAR_KEY_LOC_WIDTH);
				csr.filter_key_high		<= to_unsigned(FILTER_KEY_BIT_HI,SAR_KEY_LOC_WIDTH);
				-- word 5 (input setting)
				csr.update_key			<= (others => '0');
				csr.filter_key			<= (others => '0');
				-- word 6 (counters)
				--csr.underflow_cnt		<= (others => '0');
				--csr.overflow_cnt		<= (others => '0');
			else 
				-- default
				avs_csr_readdata		<= (others => '0');
				if (avs_csr_read = '1') then 
					avs_csr_waitrequest		<= '0';
					case to_integer(unsigned(avs_csr_address)) is
						when 0 => 
							avs_csr_readdata(0)					<= csr.commit; -- write to 
							avs_csr_readdata(7 downto 4)		<= csr.mode;
							avs_csr_readdata(8)					<= csr.update_key_is_unsigned;
							avs_csr_readdata(12)				<= csr.filter_enable;
							avs_csr_readdata(13)				<= csr.filter_is_reject;
							avs_csr_readdata(24)				<= csr.error;
							avs_csr_readdata(31 downto 28)		<= csr.error_info;
						when 1 =>
							avs_csr_readdata		<= std_logic_vector(to_signed(to_integer(csr.left_bound),avs_csr_readdata'length)); -- dynamic width
						when 2 =>
							avs_csr_readdata		<= std_logic_vector(to_signed(to_integer(csr.right_bound),avs_csr_readdata'length)); -- dynamic width
						when 3 =>
							avs_csr_readdata(csr.bin_width'high downto 0)		<= std_logic_vector(csr.bin_width);
						when 4 =>
							avs_csr_readdata(7 downto 0)		<= std_logic_vector(csr.update_key_low);
							avs_csr_readdata(15 downto 8)		<= std_logic_vector(csr.update_key_high);
							avs_csr_readdata(23 downto 16)		<= std_logic_vector(csr.filter_key_low);
							avs_csr_readdata(31 downto 24)		<= std_logic_vector(csr.filter_key_high);
						when 5 =>
							avs_csr_readdata(15 downto 0)		<= std_logic_vector(csr.update_key);
							avs_csr_readdata(31 downto 16)		<= std_logic_vector(csr.filter_key);
						when 6 =>
							avs_csr_readdata					<= std_logic_vector(csr.underflow_cnt);
						when 7 =>
							avs_csr_readdata					<= std_logic_vector(csr.overflow_cnt);
						when 8 => -- debug
							
						when others =>
					end case;
				elsif (avs_csr_write = '1') then 
					avs_csr_waitrequest		<= '0';
					case to_integer(unsigned(avs_csr_address)) is
						when 0 => 
							csr.commit							<= avs_csr_writedata(0);
							csr.mode							<= avs_csr_writedata(7 downto 4);
							csr.update_key_is_unsigned			<= avs_csr_writedata(8);
							csr.filter_enable					<= avs_csr_writedata(12);
							csr.filter_is_reject				<= avs_csr_writedata(13);
							-- RO
							--csr.error							<= avs_csr_writedata(24); 
							--csr.error_info					<= avs_csr_writedata(31 downto 28);
						when 1 =>
							csr.left_bound			<= to_signed(to_integer(signed(avs_csr_writedata)),csr.left_bound'length); -- dynamic width
						when 2 =>
							csr.right_bound			<= to_signed(to_integer(signed(avs_csr_writedata)),csr.right_bound'length); -- dynamic width
						when 3 =>
							csr.bin_width			<= unsigned(avs_csr_writedata(csr.bin_width'high downto 0));
						when 4 =>
							csr.update_key_low		<= unsigned(avs_csr_writedata(7 downto 0));
							csr.update_key_high		<= unsigned(avs_csr_writedata(15 downto 8));
							csr.filter_key_low		<= unsigned(avs_csr_writedata(23 downto 16));
							csr.filter_key_high		<= unsigned(avs_csr_writedata(31 downto 24)); -- TODO: validate key high must be larger or equal to key low
						when 5 =>
							csr.update_key			<= unsigned(avs_csr_writedata(15 downto 0)); -- no use for now
							csr.filter_key			<= unsigned(avs_csr_writedata(31 downto 16));
						when 6 =>
							-- RO
						when 7 =>
							-- RO
						when 8 =>
						
						when others =>
					end case;
					
					-- TODO: add flush
				else  -- idle 
					avs_csr_waitrequest		<= '1'; 
					if (calc_bin_done = '1' and csr.commit = '1') then -- ack the bin calculator
						csr.commit		<= '0'; -- latch the result from bin width calculator
						csr.left_bound	<= calc.left_bound;
						csr.right_bound	<= calc.right_bound;
						csr.bin_width	<= calc.bin_width;
					elsif (calc_bin_done = '1' and csr.commit = '0') then -- let the bin to ack
						-- idle
					elsif (calc_bin_done = '0' and csr.commit = '0') then 
						-- we can flush here
					else
						-- idle (wait for calculator)
					end if;
					
					-- error
					 csr.error			<= calc.error;
					 csr.error_info		<= calc.error_info;
				end if; -- end of avalon fsm
			end if;
		end if;
	
	end process;
	
	

	-----------------------------
	-- bin width calculator
	------------------------------
	proc_bin_width_calculator : process (i_clk)
	begin
		if (rising_edge(i_clk)) then
			if (i_rst = '1') then 
				calc_bin_done	<= '0';
				calc.error_info	<= (others => '0');
				calc.error		<= '0';
			else 
				if (csr.commit = '1' and calc_bin_done = '0') then
					if (to_integer(csr.bin_width) = 0) then -- no bin width
						-- check if within range
						if (helper_check_ok = '1') then -- ok
							calc.left_bound		<= csr.left_bound;
							calc.right_bound	<= csr.right_bound;
							calc.bin_width		<= to_unsigned((to_integer(csr.right_bound) - to_integer(csr.left_bound)) / N_BINS, calc.bin_width'length); -- derive
							calc.error_info		<= "0000"; -- no error
							calc.error			<= '0';
						else -- throw an exception
							calc.error_info		<= helper_error_symdrome; -- capture 
							calc.error			<= '1';
						end if;
					else -- with bin width, calcuate right bound
						if (helper_check_ok = '1') then -- ok
							calc.left_bound		<= csr.left_bound;
							calc.right_bound	<= to_signed(to_integer(csr.left_bound) + to_integer(csr.bin_width) * N_BINS, calc.right_bound'length); -- derive
							calc.bin_width		<= csr.bin_width;
							calc.error_info		<= "0000"; -- no error
							calc.error			<= '0';
						else -- throw an exception
							calc.error_info		<= helper_error_symdrome; -- capture
							calc.error			<= '1';
						end if;
					end if;
					calc_bin_done		<= '1';
				elsif (csr.commit = '1' and calc_bin_done = '1') then -- wait the master
					-- idle
				elsif (csr.commit = '0' and calc_bin_done = '1') then -- master ack
					calc_bin_done	<= '0';
				else 
					-- pure idle, job complete
				end if;
			end if;
		end if;
	end process;
	
	proc_helper_range_checker : process (all)
		variable bound_check_ok		: std_logic;
		variable bin_width_check_ok	: std_logic;
		variable key_check_ok		: std_logic;
	begin
		if (to_integer(csr.left_bound)	<= to_integer(csr.right_bound)) then -- left bound must be smaller or equal to right bound
			bound_check_ok	:= '1';
		else 
			bound_check_ok	:= '0';
		end if;
		
		 -- bin width is not overflowed 
		bin_width_check_ok	:= '1';
		
	
		
		if (unsigned(deassembly_update_key_error) = 0 and unsigned(deassembly_filter_key_error) = 0) then
			key_check_ok		:= '1';
		else
			key_check_ok		:= '0';
		end if;
		
		helper_check_ok				<= bound_check_ok and bin_width_check_ok and key_check_ok;
		helper_error_symdrome		<= (0 => not bound_check_ok, 1 => bin_width_check_ok, 2 => not key_check_ok, 3 => '0');
	
	end process;
	
	
	
	-- -----------------------------
	-- deassembly
	-- -----------------------------
	
	deassembly_update_key : entity work.shift_reg_with_dsp
	generic map (
		DATA_WIDTH						=> AVST_DATA_WIDTH,
		SELECTION_WIDTH					=> SAR_KEY_LOC_WIDTH,
		LPM_MULT_PIPELINE				=> 2,
		DEBUG							=> 1
	)
	port map (
		-- input data stream to be processed
		i_stream_data					=> asi_hist_fill_in_data,
		i_stream_valid					=> asi_hist_fill_in_valid and asi_hist_fill_in_ready, -- NOTE: If downstream assert backpressure, the valid will be stuck. So, only grant if real data transaction is made.
		-- segment settings (unsigned)
		i_bit_location_high				=> std_logic_vector(csr.update_key_high),
		i_bit_location_low				=> std_logic_vector(csr.update_key_low),
		-- output stream, selected segment shifted to low bits, with upper bits all zeros
		o_stream_data_masked			=> deassembly_update_key_raw,
		o_stream_data_valid				=> deassembly_update_key_valid,
		o_error_info					=> deassembly_update_key_error, -- bit 0: low out-of-range, bit 1: high out-of-range
		-- clock and reset interface (if pipeline=0, clock is not used, TODO: test this)
		i_rst							=> i_rst,
		i_clk							=> i_clk
	);
	
	deassembly_filter_key : entity work.shift_reg_with_dsp
	generic map (
		DATA_WIDTH						=> AVST_DATA_WIDTH,
		SELECTION_WIDTH					=> SAR_KEY_LOC_WIDTH,
		LPM_MULT_PIPELINE				=> 2,
		DEBUG							=> 1
	)
	port map (
		-- input data stream to be processed
		i_stream_data					=> asi_hist_fill_in_data,
		i_stream_valid					=> asi_hist_fill_in_valid and asi_hist_fill_in_ready, -- NOTE: If downstream assert backpressure, the valid will be stuck. So, only grant if real data transaction is made.
		-- segment settings (unsigned)
		i_bit_location_high				=> std_logic_vector(csr.filter_key_high),
		i_bit_location_low				=> std_logic_vector(csr.filter_key_low),
		-- output stream, selected segment shifted to low bits, with upper bits all zeros
		o_stream_data_masked			=> deassembly_filter_key_raw,
		o_stream_data_valid				=> deassembly_filter_key_valid,
		o_error_info					=> deassembly_filter_key_error, -- bit 0: low out-of-range, bit 1: high out-of-range
		-- clock and reset interface (if pipeline=0, clock is not used, TODO: test this)
		i_rst							=> i_rst,
		i_clk							=> i_clk
	);
	
	
	
	proc_deassembly : process (i_clk)
		variable update_key			: std_logic_vector(SAR_KEY_WIDTH-1 downto 0);
		variable filter_key			: std_logic_vector(SAR_KEY_WIDTH-1 downto 0);
		variable update_key_valid	: std_logic;
		variable filter_key_valid	: std_logic;
	begin
		if (rising_edge(i_clk)) then
            -- ----------------------------------
            -- 1.a) input mux  
            -- ----------------------------------
            -- stream[:] or debug<n> -> update_key
            -- ---------------------------------------
            -- mode[3:0] : -1       - debug input1
            --             -2       - debug input2
            --             ...
            --             -n       - debug input<n>, n up to 2^3=8 (4'b1000)
            --             ---------------------------
            --             *0       - stream-raw
            --              1       - stream-tdf
            --             ---------------------------
            --
            -- -----------------------------------
            -- default 
            update_key          := (others => '0');
            update_key_valid    := '0';
            case to_integer(signed(csr.mode)) is 
                -- stream inputs: 
                when 0 => -- stream-raw
                    update_key			:= deassembly_update_key_raw(update_key'high downto 0); -- truncate from 39 bit -> 16 bit
                    -- reg the output data and valid (error_free) of deassembly
                    if (unsigned(deassembly_update_key_error) = 0) then -- mask the valid if error is reported by the shift register module
                        update_key_valid	:= deassembly_update_key_valid;
                    end if;
                when 1 => -- stream-tdf
                    update_key          := deassembly_update_key_raw(update_key'high downto 0);
                    update_key_valid    := deassembly_update_key_valid;
                -- debug inputs: 
                when -1 => -- debug-1 (in modes below please turn off filter on the stream, as it is not meaningful)
                    -- gts - input = diff (signed) 0~2000 
                    update_key			:= asi_debug_1_data;
                    update_key_valid	:= asi_debug_1_valid;
                when -2 => -- debug-2
                    -- fillness PDF of any storage unit (cam=0)
                    update_key			:= asi_debug_2_data;
                    update_key_valid	:= asi_debug_2_valid;
                when -3 =>
                    -- fillness PDF of any storage unit (cam=1)
                    update_key			:= asi_debug_3_data;
                    update_key_valid	:= asi_debug_3_valid;
                when -4 =>
                    -- fillness PDF of any storage unit (cam=2)
                    update_key			:= asi_debug_4_data;
                    update_key_valid	:= asi_debug_4_valid;
                when -5 =>
                    -- fillness PDF of any storage unit (cam=3)
                    update_key			:= asi_debug_5_data;
                    update_key_valid	:= asi_debug_5_valid;
                when -6 =>
                    -- burstiness
                    -- format: [XX] [YY] (see processor IP)
                    -- XX := timestamp (higher 8 bit). ex: 10 bit, range is -512 to 511, triming 2 bits yields -> -128 to 127
                    -- YY := interarrival time (higher 8 bit). ex 10 bit, range is 0 to 1023, triming 2 bits yields -> 0 to 255
                    update_key			:= asi_debug_6_data;
                    update_key_valid	:= asi_debug_6_valid;
                when others => 
                    null;
            end case;
            
			-- ----------------------------------
            -- 1.b) stream[:] -> filter_key
            -- ----------------------------------
            -- default 
            filter_key_valid	:= '0';
            -- error:
			if (unsigned(deassembly_filter_key_error) = 0) then
				filter_key_valid	:= deassembly_filter_key_valid;
			end if;
            -- get the filter key from trimed lower bits
			filter_key			:= deassembly_filter_key_raw(filter_key'high downto 0);
            
			-- --------------------------------------
			-- 2) filtering -> deassembly_filtered
            -- --------------------------------------
            -- default 
            deassembly_filtered_valid           <= update_key_valid;
            deassembly_filtered_data            <= update_key;
            -- filter function enabled by csr
            if (csr.filter_enable = '1') then  
                case csr.filter_is_reject is 
                    when '0' =>
                        if (to_integer(unsigned(filter_key)) = to_integer(csr.filter_key)) then 
                            deassembly_filtered_valid		<= update_key_valid;
                            deassembly_filtered_data		<= update_key;
                        end if;
                    when '1' =>
                        if (to_integer(unsigned(filter_key)) /= to_integer(csr.filter_key)) then 
                            deassembly_filtered_valid		<= update_key_valid;
                            deassembly_filtered_data		<= update_key;
                        end if;
                    when others =>
                        null;
                end case;
            end if;
            
            -- ----------------------------------------------------
            -- 3) tdf (time difference) calculation -> latency_out
			-- ----------------------------------------------------
            -- default 
            latency_out_valid       <= deassembly_filtered_valid;
            latency_out_data        <= deassembly_filtered_data;
            -- gts - hts = 64-1072 at output of processor. higher at cam infifo.out
            if (to_integer(signed(csr.mode)) = 1) then 
                latency_out_valid       <= deassembly_filtered_valid;
                latency_out_data        <= std_logic_vector(to_unsigned(to_integer(gts_8n) - to_integer(unsigned(deassembly_filtered_data)), latency_out_data'length));
            end if;
            
			-- --------------------------------------------------------------
			-- 4) key range check (overflow or underflow) -> deassembly_out
            -- --------------------------------------------------------------
			if (i_rst = '1') then 
				csr.overflow_cnt		<= (others => '0');
				csr.underflow_cnt		<= (others => '0');
                deassembly_out_valid	<= '0';
                deassembly_out_data     <= (others => '0');
			else 
                -- default 
                deassembly_out_valid	<= '0';
                -- logic
				if (latency_out_valid = '1') then
					if (csr.update_key_is_unsigned = '1') then -- for UNSIGNED key. NOTE: in speical mode the key must be signed!
						if (to_integer(unsigned(latency_out_data)) >= to_integer(csr.right_bound)) then 
							csr.overflow_cnt	<= csr.overflow_cnt + 1;
						elsif (to_integer(unsigned(latency_out_data)) < to_integer(csr.left_bound)) then
							csr.underflow_cnt	<= csr.underflow_cnt + 1;
						else -- passed
							deassembly_out_valid		<= '1';
							deassembly_out_data			<= latency_out_data;
						end if;
					else -- for SIGNED key
						if (to_integer(signed(latency_out_data)) >= to_integer(csr.right_bound)) then 
							csr.overflow_cnt	<= csr.overflow_cnt + 1;
						elsif (to_integer(signed(latency_out_data)) < to_integer(csr.left_bound)) then
							csr.underflow_cnt	<= csr.underflow_cnt + 1;
						else -- passed
							deassembly_out_valid		<= '1';
							deassembly_out_data			<= latency_out_data;
						end if;
					end if;
				end if;
			end if;
            -- -----
            -- end
			-- -----
		end if;
	end process;
	
	-- +-----------------+
	-- |     NOTE:       |
	-- +-----------------+
	-- Timing consideration:
	--
	-- Direct division implementation with (key - lower_band) / bin_width = (16 bit - 16 bit) / 16 bit = 16 * 16 stages of carry-chain
	-- timing (10 carry-chain per cycle) is 25 cycles.
	--
	-- Sar implementation with each stage div (high_tick - low_tick) / 2 and compare the result with key = 1 pipeline per stage
	-- timing is N cycles, where N (8) is the bit width of the total bin count, which is fixed at compile time. 
	--
	-- Therefore, SAR implementation is perferred for large range width of the key and bin width.
	--
	-- Example: for monitoring the timestamp delay, which can be a few thousands. While, for monitoring the asic rate, the bin is only 4 bit.
	--          The bin width needs to vary dramatically to interplay both large range and fine-grain distribution. 
	
	
	-- ------------------------------------------------------
	-- Successive Approximation Register (SAR) pipeline
	-- ------------------------------------------------------
	proc_sar_single_stage : process (i_clk)
	begin
		if (rising_edge(i_clk)) then 
			if (i_rst = '1') then
				sar_pipeline(0).valid							<= '0';
				sar_pipeline(SAR_PIPELINE_N_STAGES-1).valid		<= '0';
			else
				-- pipeline input port
				if (deassembly_out_valid = '1') then 
					sar_pipeline(0).valid		<= '1';
					sar_pipeline(0).key			<= deassembly_out_data;
					sar_pipeline(0).high_tick	<= to_signed(to_integer(csr.right_bound),SAR_TICK_WIDTH); -- auto truncate
					sar_pipeline(0).low_tick	<= to_signed(to_integer(csr.left_bound),SAR_TICK_WIDTH); -- auto truncate
					sar_pipeline(0).box_index	<= (others => '0');
				else
					sar_pipeline(0).valid		<= '0';
					sar_pipeline(0).key			<= (others => '0');
					sar_pipeline(0).high_tick	<= (others => '0');
					sar_pipeline(0).low_tick	<= (others => '0');
					sar_pipeline(0).box_index	<= (others => '0');
				end if ;
				-- latch the last stage, until n-1
				gen_sar_pipe: for i in 0 to SAR_PIPELINE_N_STAGES-2 loop -- 8 stages for 256 bins
					sar_pipeline(i+1).valid			<= sar_pipeline(i).valid;
					sar_pipeline(i+1).key			<= sar_pipeline(i).key;
					sar_pipeline(i+1).high_tick		<= sar_pipeline(i).high_tick_comb;
					sar_pipeline(i+1).low_tick		<= sar_pipeline(i).low_tick_comb;
					sar_pipeline(i+1).box_index		<= sar_pipeline(i).box_index_comb;
				end loop gen_sar_pipe;
			end if;
		end if;
	end process;
	
	
	gen_div2_for_sar_pipeline : for i in 0 to SAR_PIPELINE_N_STAGES-1 generate -- 8 stages for 256 bins
		div_out_quo(i)				<= std_logic_vector(shift_right(signed(div_in_num(i)), 1));
	end generate;

	proc_sar_single_stage_comb : process (all)
	-- calculate the next boundary in comb
	-- bin boundary inclusive lower, exclusive higher
		-- ex: this [0 10) [10 20) ... 
	begin
		gen_sar_pipe_comb : for i in 0 to SAR_PIPELINE_N_STAGES-1 loop -- 8 stages for 256 bins
			-- averager: 
			-- mid = avg(high,low)
			div_in_num(i)						<= std_logic_vector(sar_pipeline(i).high_tick + sar_pipeline(i).low_tick); --  32 bit + 32 bit
			sar_pipeline(i).middle_tick_comb 	<= signed(div_out_quo(i)); -- truncate 32 bit into 32 bit
			
			
			
			-- comparator: 
			-- compare if the key is larger or smaller than the calculated middle tick value. 
			-- if smaller: shrink the upper bound (to mid)
			-- if larger or equal: enlarge the lower bound (to mid)
			if (csr.update_key_is_unsigned = '1') then -- for UNSIGNED key
				if (to_integer(unsigned(sar_pipeline(i).key)) < to_integer(sar_pipeline(i).middle_tick_comb)) then 
					sar_pipeline(i).high_tick_comb		<= sar_pipeline(i).middle_tick_comb;
					sar_pipeline(i).low_tick_comb		<= sar_pipeline(i).low_tick;
					sar_pipeline(i).box_index_comb		<= sar_pipeline(i).box_index;
				else
					sar_pipeline(i).high_tick_comb		<= sar_pipeline(i).high_tick;
					sar_pipeline(i).low_tick_comb		<= sar_pipeline(i).middle_tick_comb;
					sar_pipeline(i).box_index_comb		<= sar_pipeline(i).box_index + to_unsigned(N_BINS / natural(2**(i+1)), SAR_PIPELINE_N_STAGES);
				end if;
			else -- for SIGNED key
				if (to_integer(signed(sar_pipeline(i).key)) < to_integer(sar_pipeline(i).middle_tick_comb)) then 
					sar_pipeline(i).high_tick_comb		<= sar_pipeline(i).middle_tick_comb;
					sar_pipeline(i).low_tick_comb		<= sar_pipeline(i).low_tick;
					sar_pipeline(i).box_index_comb		<= sar_pipeline(i).box_index;
				else
					sar_pipeline(i).high_tick_comb		<= sar_pipeline(i).high_tick;
					sar_pipeline(i).low_tick_comb		<= sar_pipeline(i).middle_tick_comb;
					sar_pipeline(i).box_index_comb		<= sar_pipeline(i).box_index + to_unsigned(N_BINS / natural(2**(i+1)), SAR_PIPELINE_N_STAGES);
				end if;
			end if;
		end loop gen_sar_pipe_comb;
	end process;
	
	
	
	
	
	
	
	-- -----------------------------------------
	-- box filler 
	-- -----------------------------------------
	proc_box_filler_comb : process (all)
	-- two write ports and two read ports, box filler can take 2 write port. avalon read will use one of the read port. if avalon write, let the 
	-- box filler to sclr that address (write 0x1) or flush the whole ram (write 0x0). 
	begin
		-- default
		box_filler_input_valid_a		<= '0';
		box_filler_input_valid_b		<= '0';
		ram_addr_a						<= (others => '0');
		ram_addr_b						<= (others => '0');
		ram_data_a						<= (others => '0');
		ram_data_b						<= (others => '0');
		ram_we_a						<= '0';
		ram_we_b						<= '0';
		box_filler_thread_b_busy		<= '0';
		
		-- wire from end reg of pipeline
		--box_filler_input_valid		<= sar_pipeline(SAR_PIPELINE_N_STAGES-1).valid;
		--box_filler_write_addr		<= sar_pipeline(SAR_PIPELINE_N_STAGES-1).box_index;
		-- arbiter 
		-- start thread a if possible. otherwise, when a is busy, start b. 
		if (box_filler_input_valid = '1') then -- assign new hit to thread a or b
			if (flushing_req = '1') then -- do not fill, if flushing is requested
				-- collect the missing hit in the replay buffer
			elsif (box_filler_thread_a = POST_ADDR) then -- a free
				box_filler_input_valid_a 	<= '1';
			elsif (avmm_hist_read_start = '1') then -- b busy (occupied by avmm interface)
				-- collect the missing hit in the replay buffer
			else -- b free
				box_filler_input_valid_b	<= '1';
			end if;
		end if;
		
		case box_filler_thread_a is 
			when POST_ADDR => -- post the addr in comb, expect the data to be reg'd in the next cycle
				ram_addr_a			<= box_filler_write_addr;
			when POST_DATA => 
				ram_we_a			<= '1';
                if (and_reduce(ram_q_a(AVST_DATA_WIDTH-1 downto 0)) = '1') then 
                    -- overflow: keep the max value of the read interface can support
                    ram_data_a          <= ram_q_a; 
                else 
                    -- no overflow: update the value (incr value)
                    ram_data_a			<= std_logic_vector(unsigned(ram_q_a) + 1); -- incr the current count (TODO: make incr amount variable) 
                    -- speical condition: if the thread b was blocked due to atomic (thread a is currently writing). thread a will increase the count for b. (incr 2)
                    if (box_filler_input_valid_b = '1' and box_filler_write_addr = box_filler_write_addr_a_reg and box_filler_thread_a = POST_DATA) then  -- current write address is same as thread a's last write.
                        ram_data_a          <= std_logic_vector(unsigned(ram_q_a) + 2);
                    end if;
                end if;
                ram_addr_a			<= box_filler_write_addr_a_reg;
			when FLUSHING =>
				ram_we_a			<= '1';
				ram_data_a			<= (others => '0');
				ram_addr_a			<= box_filler_flushing_cnt;
			when others =>
		end case;
		-- start thread b, if there is a new hit arriving when it is busy with port a
		case box_filler_thread_b is 
			when POST_ADDR => 
				ram_addr_b			<= box_filler_write_addr;
				box_filler_thread_b_busy	<= '0';
			when POST_DATA => 
				ram_we_b			<= '1';
                if (and_reduce(ram_q_b(AVST_DATA_WIDTH-1 downto 0)) = '1') then 
                    -- overflow: keep the max value
                    ram_data_b          <= ram_q_b; 
                else 
                    -- no overflow: update the value (incr value)
                    ram_data_b			<= std_logic_vector(unsigned(ram_q_b) + 1); -- incr the current count 
                    -- speical condition: if the thread a was blocked due to atomic (thread b is currently writing). thread b will increase the count for a. (incr 2)
                    if (box_filler_input_valid_a = '1' and box_filler_write_addr = box_filler_write_addr_b_reg and box_filler_thread_b = POST_DATA) then  -- current write address is same as thread a's last write.
                        ram_data_b          <= std_logic_vector(unsigned(ram_q_b) + 2);
                    end if;
                end if;
				ram_addr_b			<= box_filler_write_addr_b_reg;
				box_filler_thread_b_busy	<= '1';
			when AV_READ =>
				ram_we_b			<= '0';
				ram_data_b			<= (others => '0');
				ram_addr_b			<= avmm_hist_read_addr;
				box_filler_thread_b_busy	<= '1';
			when others =>
		end case;
	end process;
	
	proc_box_filler : process (i_rst, i_clk)
	begin
		if (rising_edge(i_clk)) then 
			if (i_rst = '1') then 
				box_filler_thread_a		<= POST_ADDR;
				box_filler_thread_b		<= POST_ADDR;
				box_filler_flushing_done	<= '0';
			else 
                -- -----------------------
                -- SAR -> box_filler
                -- -----------------------
                box_filler_input_valid		<= sar_pipeline(SAR_PIPELINE_N_STAGES-1).valid; -- d1 of last stage valid
                box_filler_write_addr		<= sar_pipeline(SAR_PIPELINE_N_STAGES-1).box_index_comb; -- reg the last sar stage 
            
				case box_filler_thread_a is 
					when POST_ADDR =>
						box_filler_flushing_cnt		<= (others => '0');
						if (flushing_req = '1') then
							box_filler_thread_a		<= FLUSHING;
						elsif (box_filler_input_valid_a = '1') then 
							box_filler_thread_a		<= POST_DATA;
							box_filler_write_addr_a_reg		<= box_filler_write_addr; -- remember this for the next cycle
                            if (box_filler_write_addr = box_filler_write_addr_b_reg and ram_we_b = '1') then -- thread b is writing this ram location
                                box_filler_thread_a         <= POST_ADDR; -- block thread a from starting!
                            end if;
						end if;
					when POST_DATA => 
						box_filler_thread_a		<= POST_ADDR;
					when FLUSHING =>
						if (to_integer(box_filler_flushing_cnt)	< N_BINS-1) then 
							box_filler_flushing_cnt		<= box_filler_flushing_cnt + 1;
						else -- job complete
							-- inter-fsm communication with avmm_hist or avmm_csr
							if (box_filler_flushing_done = '0' and flushing_req = '1') then -- ack the master
								box_filler_flushing_done		<= '1';
							elsif (box_filler_flushing_done = '1' and flushing_req = '1') then -- wait for master to ack
								-- idle
							elsif (box_filler_flushing_done = '1' and flushing_req = '0') then -- master ack'd
								box_filler_flushing_done		<= '0';
								box_filler_thread_a				<= POST_ADDR;
							else -- all done
							end if;
							
						end if;
					when others =>
				end case;
				
				case box_filler_thread_b is 
					when POST_ADDR => 
						if (avs_hist_bin_read = '1') then 
							box_filler_thread_b					<= AV_READ;
						elsif (box_filler_input_valid_b = '1') then 
							box_filler_thread_b					<= POST_DATA;
                            -- NOTE: to the same address: mask the another thread when this write is immedate after the thread a write.
                            -- -----------------------------------------------------------------------------------------------------------------
                            -- this is a RDW speical case for the true-dp ram, to the same address, you cannot write and read across port.
                            -- for example: you might expect to the same port, the write value will be updated in the next cycle (latency=1).
                            --              however, this is only true for the same port. the write value will be updated to another port in 2 cycles after (latency=2).
                            --              if you attempt to read in the next cycle after one port is written, an glitch could happen, which data is un-defined. 
                            -- in this cycle, thread a is post data (wr_en = 1) and thread b is post address. in this case, the read value (q) will not be valid in the next cycle.
                            if (box_filler_write_addr = box_filler_write_addr_a_reg and ram_we_a = '1') then 
                                box_filler_thread_b                 <= POST_ADDR; -- block thread b from starting!
                            end if;
							box_filler_write_addr_b_reg			<= box_filler_write_addr; -- remember this for the next cycle
						end if;
					when POST_DATA => 
						box_filler_thread_b		<= POST_ADDR;
					when AV_READ =>
						if (avmm_hist_read_start = '0') then 
							box_filler_thread_b					<= POST_ADDR;
						end if;
					when others =>
				end case;
			end if;
		end if;
	end process;

	
	
	-- ----------------------------------
	-- histogram ram 
	-- ----------------------------------
--	hist_ram : entity work.true_dual_port_ram_single_clock
--	-- this dual port ram hosts the value of the histogram,
--	-- with its address as bin index and its data as count in that histogram bin. 
--	generic map (
--		DATA_WIDTH		=> MAX_COUNT_BITS,
--		ADDR_WIDTH		=> BIN_INDEX_WIDTH
--	)
--	port map(
--		addr_a			=> to_integer(ram_addr_a),
--		addr_b			=> to_integer(ram_addr_b),
--		data_a			=> ram_data_a,
--		data_b			=> ram_data_b,
--		we_a			=> ram_we_a,
--		we_b			=> ram_we_b,
--		q_a				=> ram_q_a,
--		q_b				=> ram_q_b,
--		clk				=> i_clk
--	);
	
	alt_dpram_true_inst : alt_dpram_true PORT MAP (
		address_a	 => std_logic_vector(ram_addr_a),
		address_b	 => std_logic_vector(ram_addr_b),
		clock	 => i_clk,
		data_a	 => ram_data_a,
		data_b	 => ram_data_b,
		wren_a	 => ram_we_a,
		wren_b	 => ram_we_b,
		q_a	 => ram_q_a,
		q_b	 => ram_q_b
	);
	
	
	
	-- ----------------------
	-- stream assembly
	-- ----------------------
	asi_hist_fill_in_ready			<= aso_hist_fill_out_ready; -- be careful, when dangling. in the upper level, set this to '1'.
	aso_hist_fill_out_valid			<= asi_hist_fill_in_valid;
	aso_hist_fill_out_data			<= asi_hist_fill_in_data;
	aso_hist_fill_out_startofpacket	<= asi_hist_fill_in_startofpacket;
	aso_hist_fill_out_endofpacket	<= asi_hist_fill_in_endofpacket;
	aso_hist_fill_out_channel		<= asi_hist_fill_in_channel;
    aso_hist_fill_out_error         <= asi_hist_fill_in_error;
	
	-- -----------------------
	-- run control management
	-- -----------------------
	asi_ctrl_ready			<= '1'; -- for now always ready
	
	proc_run_management_agent : process (i_clk)
	begin
		if (rising_edge(i_clk)) then 
			if (i_rst = '1') then 
			-- valid
				run_state_cmd						<= IDLE;
			else 
				if (asi_ctrl_valid = '1') then 
					-- payload of run control to run cmd
					case asi_ctrl_data is 
						when "000000001" =>
							run_state_cmd		<= IDLE;
						when "000000010" => 
							run_state_cmd		<= RUN_PREPARE;
						when "000000100" =>
							run_state_cmd		<= SYNC;
						when "000001000" =>
							run_state_cmd		<= RUNNING;
						when "000010000" =>
							run_state_cmd		<= TERMINATING;
						when "000100000" => 
							run_state_cmd		<= LINK_TEST;
						when "001000000" =>
							run_state_cmd		<= SYNC_TEST;
						when "010000000" =>
							run_state_cmd		<= RESET;
						when "100000000" =>
							run_state_cmd		<= OUT_OF_DAQ;
						when others =>
							run_state_cmd		<= ERROR;
					end case;
				else 
					run_state_cmd		<= run_state_cmd;
				end if;
			end if;
		end if;
	end process;
	
	proc_run_state : process (i_clk)
	begin
		if (rising_edge(i_clk)) then 
			case run_state_cmd is 
				when SYNC =>
					gts_counter_rst		<= '1';
				when others =>
					gts_counter_rst		<= '0';
			end case;
		end if;
	end process;
	
	proc_gts_counter : process (i_clk)
	-- counter of the global timestamp on the FPGA
		-- needs to be 48 bit at 125 MHz
	begin
		if rising_edge(i_clk) then
			if (gts_counter_rst = '1') then 
				 -- reset counter
				gts_8n		<= (others => '0');
			else
				-- begin counter
				gts_8n		<= gts_8n + 1;
			end if;
		end if;
	end process;
	
	
	
end architecture rtl;










