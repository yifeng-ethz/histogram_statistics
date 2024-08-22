-- File name: shift_reg_with_dsp.vhd 
-- Author: Yifeng Wang (yifenwan@phys.ethz.ch)
-- =======================================
-- Revision: 1.0 (file created)
--		Date: Jul 31, 2024
-- =========
-- Description:	[Shift-register based on DSP (lpm_multiplier)]
--	
--			Functional Description:
--				Get a selected bit segment of the input data stream, set by bit location high and bit location low. 
--
--			Work flow:
--				1) shift the input data, in pipeline
--				2) shift to get the mask, in pipeline
--				3) combine the two to get the output

-- ================ synthsizer configuration =================== 		
-- altera vhdl_input_version vhdl_2008
-- ============================================================= 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
LIBRARY lpm; 
USE lpm.lpm_components.all;

entity shift_reg_with_dsp is 
generic (
	DATA_WIDTH						: natural := 39;
	SELECTION_WIDTH					: natural := 8; -- 0 up to 255, more than enough to cover data width of 39, selection is the bit location 
	LPM_MULT_PIPELINE				: natural := 5;
	DEBUG							: natural := 1
);
port (
	-- input data stream to be processed
	i_stream_data					: in  std_logic_vector(DATA_WIDTH-1 downto 0);
	i_stream_valid					: in  std_logic;
	-- segment settings (unsigned)
	i_bit_location_high				: in  std_logic_vector(SELECTION_WIDTH-1 downto 0);
	i_bit_location_low				: in  std_logic_vector(SELECTION_WIDTH-1 downto 0);
	-- output stream, selected segment shifted to low bits, with upper bits all zeros
	o_stream_data_masked			: out std_logic_vector(DATA_WIDTH-1 downto 0);
	o_stream_data_valid				: out std_logic; 
	o_error_info					: out std_logic_vector(1 downto 0); -- bit 0: low out-of-range, bit 1: high out-of-range
	-- clock and reset interface (if pipeline=0, clock is not used)
	i_rst							: in  std_logic;
	i_clk							: in  std_logic
);
end entity shift_reg_with_dsp;

architecture rtl of shift_reg_with_dsp is 
	
	
	signal stream_data_rev_in				: std_logic_vector(DATA_WIDTH-1 downto 0);
	signal stream_data_rev_in_reg			: std_logic_vector(DATA_WIDTH-1 downto 0);
	signal mask_shift_datab					: std_logic_vector(DATA_WIDTH-1 downto 0);
	signal mask_shift_datab_comb			: std_logic_vector(DATA_WIDTH-1 downto 0);
	signal mask_lsr							: std_logic_vector(DATA_WIDTH-1 downto 0);
	signal stream_data_masked				: std_logic_vector(DATA_WIDTH-1 downto 0);
	signal stream_data_lsr					: std_logic_vector(DATA_WIDTH-1 downto 0);
	signal stream_data_rev_out				: std_logic_vector(DATA_WIDTH-1 downto 0);
	signal stream_data_in					: std_logic_vector(DATA_WIDTH-1 downto 0);
	signal stream_data_in_reg				: std_logic_vector(DATA_WIDTH-1 downto 0);
	
	signal data_shift_datab_comb			: std_logic_vector(DATA_WIDTH-1 downto 0);
	signal data_shift_datab					: std_logic_vector(DATA_WIDTH-1 downto 0);
	
	signal bit_location_low_reg				: std_logic_vector(SELECTION_WIDTH-1 downto 0);
	signal bit_location_high_reg			: std_logic_vector(SELECTION_WIDTH-1 downto 0);
	signal mask_shift_bits_binary			: std_logic_vector(SELECTION_WIDTH-1 downto 0);
	signal data_shift_bits_binary_comb		: std_logic_vector(SELECTION_WIDTH-1 downto 0);
	
	
	type error_msg_single_t is record
		low_oor		: std_logic;
		high_oor	: std_logic;
	end record;
	type error_msg_t			is array (0 to LPM_MULT_PIPELINE) of error_msg_single_t;
	signal error_msg			: error_msg_t;
	
	signal valid_ppl			: std_logic_vector(LPM_MULT_PIPELINE downto 0);
	
	-- --------------------------------------
	-- shift right multiplier
	-- --------------------------------------
	component LPM_MULT
	generic ( 	
		LPM_WIDTHA 		: natural; 
		LPM_WIDTHB 		: natural;
		LPM_WIDTHS 		: natural := 1;
		LPM_WIDTHP 		: natural;
		LPM_REPRESENTATION 	: string := "UNSIGNED";
		LPM_PIPELINE 	: natural := 0;
		LPM_TYPE		: string := L_MULT;
		LPM_HINT 		: string := "UNUSED"
	);
	port (
		DATAA 	: in std_logic_vector(LPM_WIDTHA-1 downto 0);
		DATAB 	: in std_logic_vector(LPM_WIDTHB-1 downto 0);
		ACLR 	: in std_logic := '0';
		CLOCK 	: in std_logic := '0';
		CLKEN 	: in std_logic := '1';
		SUM 	: in std_logic_vector(LPM_WIDTHS-1 downto 0) := (OTHERS => '0');
		RESULT 	: out std_logic_vector(LPM_WIDTHP-1 downto 0)
	);
	end component;
	
	-- derive the 
	component b2o_encoder
	generic(
		INPUT_W		: natural 	:= SELECTION_WIDTH;
		OUTPUT_W	: natural 	:= DATA_WIDTH
	);
	port(
		binary_code		: in  std_logic_vector(SELECTION_WIDTH-1 downto 0);
		onehot_code		: out std_logic_vector(DATA_WIDTH-1 downto 0)
	);
	end component;

begin

	proc_input_register : process (i_clk)
	begin
		if (rising_edge(i_clk)) then
			stream_data_rev_in_reg		<= stream_data_rev_in; -- latch the reversal data
			stream_data_in_reg			<= stream_data_in; -- latch the original data
			bit_location_low_reg		<= i_bit_location_low; 
			bit_location_high_reg		<= i_bit_location_high;
			-- give 1 stage of delay to calculate the mask datab
			mask_shift_datab			<= mask_shift_datab_comb;
			data_shift_datab			<= data_shift_datab_comb;
		end if;
	end process;

	-- --------------------------------------------------
	-- (1) bit-reversal of the input data
	-- --------------------------------------------------
	proc_reverse_data_in : process (all)
	begin
		for i in 0 to i_stream_data'high loop
			stream_data_rev_in(i)		<= i_stream_data(i_stream_data'high-i);
		end loop;
	end process;
	stream_data_in			<= i_stream_data;
	
	
	-- ---------------------------------------------------------
	-- (2a) shift right the data (with the help of dsp block)
	-- ---------------------------------------------------------
	
	proc_derive_data_shift_bit : process (all)
	begin
		data_shift_bits_binary_comb		<= std_logic_vector(to_unsigned(DATA_WIDTH-1,SELECTION_WIDTH) - unsigned(i_bit_location_low)+1);
	-- 38 - 17 = needed to shift right
	end process;
	
	b2o_enc_data : b2o_encoder
	port map (
		binary_code		=> data_shift_bits_binary_comb, -- 17
		onehot_code		=> data_shift_datab_comb
	);
		
	lpm_mult_shift_left_data : LPM_MULT -- this is actually right shift, by the amount of 39 - binary, so we feed binary = 39 - amount
	-- where amount is bit loc low
	generic map ( 	
		LPM_WIDTHA 			=> DATA_WIDTH, -- data reversed
		LPM_WIDTHB 			=> DATA_WIDTH+1, -- 2 to the power of shifts in bit (=lower bit loc) 
		--LPM_WIDTHS 		=> 0, -- not needed
		LPM_WIDTHP 			=> DATA_WIDTH, -- data shifted
		LPM_REPRESENTATION 	=> "UNSIGNED",
		LPM_PIPELINE 		=> LPM_MULT_PIPELINE
	)
	port map (
		DATAA 		=> stream_data_in_reg,
		DATAB(DATA_WIDTH downto 1) 		=> data_shift_datab, -- binary + 1 here
		DATAB(0)			=> '0',
		ACLR 		=> '0',
		CLOCK 		=> i_clk,
		CLKEN 		=> '1',
		--SUM 		=> (14 downto 0 => '0'),
		RESULT 		=> stream_data_lsr
	);
	
	-- ---------------------------------------------------------
	-- (2b) shift right the mask (with the help of dsp block)
	-- ---------------------------------------------------------
	proc_derive_mask_shift_bit : process (all)
		subtype int_range		is integer range 0 to DATA_WIDTH;
		variable diff			: int_range;
		variable shift_bits		: int_range;
	begin -- mask need to shift right by 39 - diff
		diff					:= to_integer(unsigned(i_bit_location_high)) - to_integer(unsigned(i_bit_location_low)); -- 29 - 17 = 12
		--shift_bits				:= diff-1; -- 39 - 1 - 12 = 26 -- should be : 38 - diff
		shift_bits				:= DATA_WIDTH-1-diff;
		mask_shift_bits_binary	<= std_logic_vector(to_unsigned(diff,SELECTION_WIDTH)+1); -- should be just diff? after a lengthy calculation
		--mask_shift_bits_binary	<= std_logic_vector(to_unsigned(shift_bits,mask_shift_bits_binary'length)); 
	end process;
	
	b2o_enc_mask : b2o_encoder
	port map (
		binary_code		=> mask_shift_bits_binary, -- 26
		onehot_code		=> mask_shift_datab_comb -- bit 26 is '1', others '0', meaning 2^26 to feed multiplier, which will shift 26 bit left
	);
		
	
	lpm_mult_shift_left_mask : LPM_MULT
	generic map ( 	
		LPM_WIDTHA 			=> DATA_WIDTH, -- all ones (all mask), after shift low bits are zeros
		LPM_WIDTHB 			=> DATA_WIDTH+1, -- 2 to the power of shifts in bit (=lower bit loc) shift amount (with auto-compensation)
		--LPM_WIDTHS 		=> 0, -- not needed
		LPM_WIDTHP 			=> DATA_WIDTH, -- data shifted
		LPM_REPRESENTATION 	=> "UNSIGNED",
		LPM_PIPELINE 		=> LPM_MULT_PIPELINE
	)
	port map (
		DATAA 		=> (DATA_WIDTH-1 downto 0 => '1'), -- all ones
		DATAB(DATA_WIDTH downto 1) 		=> mask_shift_datab, 
		DATAB(0)				=> '0',
		ACLR 		=> '0',
		CLOCK 		=> i_clk,
		CLKEN 		=> '1',
		--SUM 		=> (14 downto 0 => '0'),
		RESULT 		=> mask_lsr
	);
	
	-- ---------------------------------------------------------
	-- (3) mask the lower bits with zeros
	-- ---------------------------------------------------------
	
	proc_apply_mask_on_data : process (all)
	begin
		for i in 0 to DATA_WIDTH-1 loop
			if (mask_lsr(i) = '1') then
				stream_data_masked(i)		<= stream_data_lsr(i);
			else
				stream_data_masked(i)		<= '0';
			end if;
		end loop;
	end process;
	
	
	
	-- ---------------------------------------------------------
	-- (4) bit-reversal to the key resides in the lower bits
	-- ---------------------------------------------------------
	
	proc_reverse_data_out : process (all)
	begin
		for i in 0 to stream_data_masked'high loop
			stream_data_rev_out(i)		<= stream_data_masked(stream_data_masked'high-i);
		end loop;
	end process;
	
	

	
	
	
	proc_range_checker : process (i_clk,i_rst)
-- 		+-------------------------------------------------------------------------------
-- 		| Function: Check if key bit location is within the range of the max bit width of data.
-- 		| 			The lpm div is non-stopping, but the set range will overflow, which renders the data non-sense. 
-- 		| 			As a result, the error_info is asserted, while valid is unaffected.
-- 		+-------------------------------------------------------------------------------
	begin
		if (rising_edge(i_clk)) then 
			-- @@ SYNC RESET @@
			if (i_rst = '1') then -- only sync reset the first and last stage of pipeline, preparing for Hyperflex architecture. 
				error_msg(0).low_oor		<= '0';
				error_msg(0).high_oor		<= '0';
				error_msg(LPM_MULT_PIPELINE).low_oor		<= '0';
				error_msg(LPM_MULT_PIPELINE).high_oor		<= '0';
			else 
				-- feed error to pipeline (reg-in)
				if (to_integer(unsigned(i_bit_location_low)) >= DATA_WIDTH) then 
					error_msg(0).low_oor		<= '1'; -- oor := out-of-range
				else
					error_msg(0).low_oor		<= '0';
				end if;
				
				if (to_integer(unsigned(i_bit_location_high)) >= DATA_WIDTH) then 
					error_msg(0).high_oor		<= '1';
				else
					error_msg(0).high_oor		<= '0';
				end if;
				-- last stage
				error_msg(LPM_MULT_PIPELINE).low_oor		<= error_msg(LPM_MULT_PIPELINE-1).low_oor;
				error_msg(LPM_MULT_PIPELINE).high_oor		<= error_msg(LPM_MULT_PIPELINE-1).high_oor;
			end if;
			
			-- @@ NO RESET @@
			-- pipeline error
			gen_error_ppl : for i in 0 to LPM_MULT_PIPELINE-2 loop 
				error_msg(i+1)		<= error_msg(i);
			end loop gen_error_ppl;
		
		end if;
	end process;
	
	
	
	proc_valid_pipeline : process (i_clk,i_rst)
	begin
		if (rising_edge(i_clk)) then -- only sync reset the first and last stage of pipeline, preparing for Hyperflex architecture. 
			-- @@ SYNC RESET @@
			if (i_rst = '1') then 
				valid_ppl(0)					<= '0';
				valid_ppl(LPM_MULT_PIPELINE)	<= '0';
			else
				-- feed valid to pipeline (reg-in)
				if (i_stream_valid = '1') then 
					valid_ppl(0)			<= '1';
				else
					valid_ppl(0)			<= '0';
				end if;
				-- last stage
				valid_ppl(LPM_MULT_PIPELINE)	<= valid_ppl(LPM_MULT_PIPELINE-1);
			end if;
			
			-- @@ NO RESET @@
			-- pipeline valid
			gen_valid_ppl : for i in 0 to LPM_MULT_PIPELINE-2 loop 
				valid_ppl(i+1)		<= valid_ppl(i);
			end loop gen_valid_ppl;
		end if;
	end process;
	
	proc_output_assembly : process (i_clk,i_rst)
	begin
		if (rising_edge(i_clk)) then
			-- @@ NO RESET @@
			-- catch data at the output
			o_stream_data_masked		<= stream_data_masked;
			-- catch valid at the output
			o_stream_data_valid			<= valid_ppl(LPM_MULT_PIPELINE);
			-- catch error at the output
			o_error_info				<= (0 => error_msg(LPM_MULT_PIPELINE).low_oor, 1 => error_msg(LPM_MULT_PIPELINE).high_oor);
		end if;
	end process;
	


end architecture rtl;

















