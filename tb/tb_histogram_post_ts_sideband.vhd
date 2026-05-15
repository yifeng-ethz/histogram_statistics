-- File name: tb_histogram_post_ts_sideband.vhd
-- Description: Standalone smoke test for post-rbCAM timestamp sideband adapter.

library ieee;
use ieee.std_logic_1164.all;
use std.env.all;

entity tb_histogram_post_ts_sideband is
end entity tb_histogram_post_ts_sideband;

architecture sim of tb_histogram_post_ts_sideband is

    constant CLK_PERIOD_CONST          : time                         := 8 ns;
    constant ZERO_TS_CONST             : std_logic_vector(47 downto 0) := x"000000000000";
    constant EXPECT_TS_0_CONST         : std_logic_vector(47 downto 0) := x"0123456789AB";
    constant EXPECT_TS_1_CONST         : std_logic_vector(47 downto 0) := x"0123456789AC";

    constant HEADER_WORD_CONST         : std_logic_vector(35 downto 0) := "0001" & x"000000BC";
    constant HEADER_TS_HI_WORD_CONST   : std_logic_vector(35 downto 0) := "0000" & x"01234567";
    constant HEADER_TS_LO_WORD_CONST   : std_logic_vector(35 downto 0) := "0000" & x"8" & x"0000000";
    constant PRE_SUBHEADER_WORD_CONST  : std_logic_vector(35 downto 0) := "0000" & x"70000000";
    constant SUBHEADER_WORD_CONST      : std_logic_vector(35 downto 0) := "0001" & x"9A0000F7";
    constant HIT_WORD_0_CONST          : std_logic_vector(35 downto 0) := "0000" & x"B" & x"0000001";
    constant HIT_WORD_1_CONST          : std_logic_vector(35 downto 0) := "0000" & x"C" & x"0000002";
    constant TRAILER_WORD_CONST        : std_logic_vector(35 downto 0) := "0001" & x"0000009C";

    signal clk                         : std_logic := '0';
    signal rst                         : std_logic := '1';
    signal in_data                     : std_logic_vector(35 downto 0) := (others    => '0');
    signal in_valid                    : std_logic := '0';
    signal in_ready                    : std_logic;
    signal in_sop                      : std_logic := '0';
    signal in_eop                      : std_logic := '0';
    signal out_data                    : std_logic_vector(83 downto 0);
    signal out_valid                   : std_logic;
    signal out_ready                   : std_logic := '1';
    signal out_sop                     : std_logic;
    signal out_eop                     : std_logic;

    procedure drive_and_check(
        signal clk_s          : in  std_logic;
        signal data_s         : out std_logic_vector(35 downto 0);
        signal valid_s        : out std_logic;
        signal ready_s        : in  std_logic;
        signal sop_s          : out std_logic;
        signal eop_s          : out std_logic;
        signal out_data_s     : in  std_logic_vector(83 downto 0);
        signal out_valid_s    : in  std_logic;
        constant word         : in  std_logic_vector(35 downto 0);
        constant expected_ts  : in  std_logic_vector(47 downto 0);
        constant sop          : in  std_logic := '0';
        constant eop          : in  std_logic := '0'
    ) is
    begin
        wait until rising_edge(clk_s);
        data_s  <= word;
        valid_s <= '1';
        sop_s   <= sop;
        eop_s   <= eop;
        wait for 1 ps;

        assert out_valid_s = '1'
            report "adapter output valid did not follow input valid"
            severity failure;
        assert out_data_s = expected_ts & word
            report "adapter timestamp sideband mismatch"
            severity failure;

        while ready_s /= '1' loop
            wait until rising_edge(clk_s);
        end loop;
        wait until rising_edge(clk_s);
        valid_s <= '0';
        data_s  <= (others    => '0');
        sop_s   <= '0';
        eop_s   <= '0';
        wait for 1 ps;
    end procedure drive_and_check;

begin

    clk <= not clk after CLK_PERIOD_CONST / 2;

    dut : entity work.histogram_post_ts_sideband
        port map (
            asi_post_data             => in_data,
            asi_post_valid            => in_valid,
            asi_post_ready            => in_ready,
            asi_post_startofpacket    => in_sop,
            asi_post_endofpacket      => in_eop,
            aso_post_data             => out_data,
            aso_post_valid            => out_valid,
            aso_post_ready            => out_ready,
            aso_post_startofpacket    => out_sop,
            aso_post_endofpacket      => out_eop,
            rsi_reset_reset           => rst,
            csi_clock_clk             => clk
        );

    tb_main : process
    begin
        repeat_reset : for idx in 0 to 4 loop
            wait until rising_edge(clk);
        end loop repeat_reset;
        rst <= '0';
        wait until rising_edge(clk);

        drive_and_check(clk, in_data, in_valid, in_ready, in_sop, in_eop, out_data, out_valid,
                        HEADER_WORD_CONST, ZERO_TS_CONST, '1', '0');
        drive_and_check(clk, in_data, in_valid, in_ready, in_sop, in_eop, out_data, out_valid,
                        HEADER_TS_HI_WORD_CONST, ZERO_TS_CONST);
        drive_and_check(clk, in_data, in_valid, in_ready, in_sop, in_eop, out_data, out_valid,
                        HEADER_TS_LO_WORD_CONST, ZERO_TS_CONST);
        drive_and_check(clk, in_data, in_valid, in_ready, in_sop, in_eop, out_data, out_valid,
                        PRE_SUBHEADER_WORD_CONST, ZERO_TS_CONST);
        drive_and_check(clk, in_data, in_valid, in_ready, in_sop, in_eop, out_data, out_valid,
                        SUBHEADER_WORD_CONST, ZERO_TS_CONST);
        drive_and_check(clk, in_data, in_valid, in_ready, in_sop, in_eop, out_data, out_valid,
                        HIT_WORD_0_CONST, EXPECT_TS_0_CONST);
        drive_and_check(clk, in_data, in_valid, in_ready, in_sop, in_eop, out_data, out_valid,
                        HIT_WORD_1_CONST, EXPECT_TS_1_CONST);
        drive_and_check(clk, in_data, in_valid, in_ready, in_sop, in_eop, out_data, out_valid,
                        TRAILER_WORD_CONST, ZERO_TS_CONST, '0', '1');

        report "tb_histogram_post_ts_sideband PASS" severity note;
        finish;
    end process tb_main;

end architecture sim;
