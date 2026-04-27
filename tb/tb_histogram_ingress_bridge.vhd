-- File name: tb_histogram_ingress_bridge.vhd
-- Author: OpenAI Codex
-- =======================================
-- Revision: 26.0.2
--     Date: 20260425
--     Change: Directed regression for post-hit-stack hit-word filtering.
-- =========

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_histogram_ingress_bridge is
end entity tb_histogram_ingress_bridge;

architecture sim of tb_histogram_ingress_bridge is

    constant CLK_PERIOD_CONST : time := 10 ns;

    signal clk                 : std_logic := '0';
    signal rst                 : std_logic := '1';
    signal csr_address         : std_logic_vector(1 downto 0) := (others => '0');
    signal csr_read            : std_logic := '0';
    signal csr_write           : std_logic := '0';
    signal csr_writedata       : std_logic_vector(31 downto 0) := (others => '0');
    signal csr_readdata        : std_logic_vector(31 downto 0);
    signal csr_waitrequest     : std_logic;

    signal pre_data            : std_logic_vector(38 downto 0) := (others => '0');
    signal pre_valid           : std_logic := '0';
    signal pre_ready           : std_logic;
    signal pre_sop             : std_logic := '0';
    signal pre_eop             : std_logic := '0';
    signal pre_channel         : std_logic_vector(3 downto 0) := (others => '0');
    signal pre_empty           : std_logic := '0';
    signal pre_error           : std_logic := '0';
    signal pre_out_data        : std_logic_vector(38 downto 0);
    signal pre_out_valid       : std_logic;
    signal pre_out_ready       : std_logic := '1';
    signal pre_out_sop         : std_logic;
    signal pre_out_eop         : std_logic;
    signal pre_out_channel     : std_logic_vector(3 downto 0);
    signal pre_out_empty       : std_logic;
    signal pre_out_error       : std_logic;

    signal post_data           : std_logic_vector(35 downto 0) := (others => '0');
    signal post_valid          : std_logic := '0';
    signal post_ready          : std_logic;
    signal post_sop            : std_logic := '0';
    signal post_eop            : std_logic := '0';
    signal post_out_data       : std_logic_vector(35 downto 0);
    signal post_out_valid      : std_logic;
    signal post_out_ready      : std_logic := '1';
    signal post_out_sop        : std_logic;
    signal post_out_eop        : std_logic;

    signal hist_data           : std_logic_vector(38 downto 0);
    signal hist_valid          : std_logic;
    signal hist_ready          : std_logic := '1';
    signal hist_sop            : std_logic;
    signal hist_eop            : std_logic;
    signal hist_channel        : std_logic_vector(3 downto 0);
    signal hist_count          : unsigned(7 downto 0) := (others => '0');

    procedure drive_post_word(
        signal data_s  : out std_logic_vector(35 downto 0);
        signal valid_s : out std_logic;
        signal sop_s   : out std_logic;
        signal eop_s   : out std_logic;
        signal ready_s : in  std_logic;
        constant word  : in  std_logic_vector(35 downto 0);
        constant sop   : in  std_logic := '0';
        constant eop   : in  std_logic := '0'
    ) is
    begin
        data_s  <= word;
        sop_s   <= sop;
        eop_s   <= eop;
        valid_s <= '1';
        loop
            wait until rising_edge(clk);
            exit when ready_s = '1';
        end loop;
        valid_s <= '0';
        sop_s   <= '0';
        eop_s   <= '0';
        wait until rising_edge(clk);
    end procedure drive_post_word;

begin

    clk <= not clk after CLK_PERIOD_CONST / 2;

    dut : entity work.histogram_ingress_bridge
        generic map (
            DEFAULT_SELECT_POST    => 1,
            ENABLE_POST_FORWARD    => 0,
            FILTER_POST_HIT_WORDS  => 1
        )
        port map (
            avs_csr_address            => csr_address,
            avs_csr_read               => csr_read,
            avs_csr_write              => csr_write,
            avs_csr_writedata          => csr_writedata,
            avs_csr_readdata           => csr_readdata,
            avs_csr_waitrequest        => csr_waitrequest,
            asi_pre_data               => pre_data,
            asi_pre_valid              => pre_valid,
            asi_pre_ready              => pre_ready,
            asi_pre_startofpacket      => pre_sop,
            asi_pre_endofpacket        => pre_eop,
            asi_pre_channel            => pre_channel,
            asi_pre_empty              => pre_empty,
            asi_pre_error              => pre_error,
            aso_pre_data               => pre_out_data,
            aso_pre_valid              => pre_out_valid,
            aso_pre_ready              => pre_out_ready,
            aso_pre_startofpacket      => pre_out_sop,
            aso_pre_endofpacket        => pre_out_eop,
            aso_pre_channel            => pre_out_channel,
            aso_pre_empty              => pre_out_empty,
            aso_pre_error              => pre_out_error,
            asi_post_data              => post_data,
            asi_post_valid             => post_valid,
            asi_post_ready             => post_ready,
            asi_post_startofpacket     => post_sop,
            asi_post_endofpacket       => post_eop,
            aso_post_data              => post_out_data,
            aso_post_valid             => post_out_valid,
            aso_post_ready             => post_out_ready,
            aso_post_startofpacket     => post_out_sop,
            aso_post_endofpacket       => post_out_eop,
            aso_hist_data              => hist_data,
            aso_hist_valid             => hist_valid,
            aso_hist_ready             => hist_ready,
            aso_hist_startofpacket     => hist_sop,
            aso_hist_endofpacket       => hist_eop,
            aso_hist_channel           => hist_channel,
            rsi_reset_reset            => rst,
            csi_clock_clk              => clk
        );

    hist_mon : process (clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                hist_count <= (others => '0');
            elsif hist_valid = '1' and hist_ready = '1' then
                assert hist_data(38 downto 36) = "000"
                    report "post stream must be zero-extended into histogram data"
                    severity error;
                assert hist_data(35 downto 32) = "0000"
                    report "non-hit post word reached histogram output"
                    severity error;
                assert hist_sop = '0' and hist_eop = '0'
                    report "filtered post hit stream must not emit frame packet markers"
                    severity error;
                hist_count <= hist_count + 1;
            end if;
        end if;
    end process hist_mon;

    stim : process
    begin
        wait for 5 * CLK_PERIOD_CONST;
        rst <= '0';
        wait until rising_edge(clk);

        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"1000000BC", '1', '0');
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"000000000");
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"000000001");
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"000000000");
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"000000000");
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"1000000F7");
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"10000009C", '0', '1');
        wait for 3 * CLK_PERIOD_CONST;
        assert hist_count = 0
            report "zero-hit frame produced histogram beats"
            severity error;

        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"1000000BC", '1', '0');
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"000000000");
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"000000001");
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"000000000");
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"000000000");
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"1000002F7");
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"001234567");
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"000000222");
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"1000000F7");
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"10000009C", '0', '1');
        wait for 3 * CLK_PERIOD_CONST;
        assert hist_count = 2
            report "data-bearing frame did not forward exactly two hit words"
            severity error;

        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"1000000BC", '1', '0');
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"000000000");
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"000000001");
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"000000000");
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"000000000");
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"1000001F7");

        hist_ready <= '0';
        post_data  <= x"000000033";
        post_valid <= '1';
        wait until rising_edge(clk);
        assert post_ready = '0' and hist_valid = '1'
            report "real hit word did not backpressure on histogram not-ready"
            severity error;
        hist_ready <= '1';
        wait until rising_edge(clk);
        post_valid <= '0';
        wait until rising_edge(clk);
        drive_post_word(post_data, post_valid, post_sop, post_eop, post_ready, x"10000009C", '0', '1');
        wait for 3 * CLK_PERIOD_CONST;
        assert hist_count = 3
            report "backpressured hit word was not eventually forwarded"
            severity error;

        report "tb_histogram_ingress_bridge PASS" severity note;
        wait;
    end process stim;

end architecture sim;
