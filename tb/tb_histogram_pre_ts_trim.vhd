-- File name: tb_histogram_pre_ts_trim.vhd
-- Description: Standalone smoke test for pre-rbCAM timestamp sideband trim.

library ieee;
use ieee.std_logic_1164.all;
use std.env.all;

entity tb_histogram_pre_ts_trim is
end entity tb_histogram_pre_ts_trim;

architecture sim of tb_histogram_pre_ts_trim is

    constant CLK_PERIOD_CONST    : time := 8 ns;
    constant TS_CONST            : std_logic_vector(47 downto 0) := x"FEDCBA987654";
    constant PAYLOAD_CONST       : std_logic_vector(38 downto 0) := "101" & x"123456789";

    signal clk                   : std_logic := '0';
    signal rst                   : std_logic := '1';
    signal in_data               : std_logic_vector(86 downto 0) := (others => '0');
    signal in_valid              : std_logic := '0';
    signal in_ready              : std_logic;
    signal in_sop                : std_logic := '0';
    signal in_eop                : std_logic := '0';
    signal in_channel            : std_logic_vector(3 downto 0) := (others => '0');
    signal in_empty              : std_logic := '0';
    signal in_error              : std_logic := '0';
    signal out_data              : std_logic_vector(38 downto 0);
    signal out_valid             : std_logic;
    signal out_ready             : std_logic := '1';
    signal out_sop               : std_logic;
    signal out_eop               : std_logic;
    signal out_channel           : std_logic_vector(3 downto 0);
    signal out_empty             : std_logic;
    signal out_error             : std_logic;

begin

    clk <= not clk after CLK_PERIOD_CONST / 2;

    dut : entity work.histogram_pre_ts_trim
        port map (
            asi_hit_data             => in_data,
            asi_hit_valid            => in_valid,
            asi_hit_ready            => in_ready,
            asi_hit_startofpacket    => in_sop,
            asi_hit_endofpacket      => in_eop,
            asi_hit_channel          => in_channel,
            asi_hit_empty            => in_empty,
            asi_hit_error            => in_error,
            aso_hit_data             => out_data,
            aso_hit_valid            => out_valid,
            aso_hit_ready            => out_ready,
            aso_hit_startofpacket    => out_sop,
            aso_hit_endofpacket      => out_eop,
            aso_hit_channel          => out_channel,
            aso_hit_empty            => out_empty,
            aso_hit_error            => out_error,
            rsi_reset_reset          => rst,
            csi_clock_clk            => clk
        );

    tb_main : process
    begin
        for idx in 0 to 4 loop
            wait until rising_edge(clk);
        end loop;
        rst <= '0';
        wait until rising_edge(clk);

        in_data       <= TS_CONST & PAYLOAD_CONST;
        in_valid      <= '1';
        in_sop        <= '1';
        in_eop        <= '1';
        in_channel    <= x"A";
        in_empty      <= '1';
        in_error      <= '1';
        wait for 1 ps;

        assert in_ready = '1'
            report "trim input ready did not follow output ready"
            severity failure;
        assert out_valid = '1'
            report "trim output valid did not follow input valid"
            severity failure;
        assert out_data = PAYLOAD_CONST
            report "trim output did not drop timestamp sideband"
            severity failure;
        assert out_sop = '1' and out_eop = '1' and out_channel = x"A" and
               out_empty = '1' and out_error = '1'
            report "trim did not preserve Avalon-ST sideband signals"
            severity failure;

        out_ready <= '0';
        wait for 1 ps;
        assert in_ready = '0'
            report "trim input ready did not deassert with output backpressure"
            severity failure;

        report "tb_histogram_pre_ts_trim PASS" severity note;
        finish;
    end process tb_main;

end architecture sim;
