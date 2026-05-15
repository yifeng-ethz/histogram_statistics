library ieee;
use ieee.std_logic_1164.all;

entity hs0_board_instance_top is
    port (
        i_clk                           : in  std_logic;
        i_rst                           : in  std_logic;
        i_interval_reset                : in  std_logic;

        avs_hist_bin_address            : in  std_logic_vector(7 downto 0);
        avs_hist_bin_read               : in  std_logic;
        avs_hist_bin_write              : in  std_logic;
        avs_hist_bin_writedata          : in  std_logic_vector(31 downto 0);
        avs_hist_bin_readdata           : out std_logic_vector(31 downto 0);
        avs_hist_bin_readdatavalid      : out std_logic;
        avs_hist_bin_waitrequest        : out std_logic;
        avs_hist_bin_burstcount         : in  std_logic_vector(8 downto 0);
        avs_hist_bin_response           : out std_logic_vector(1 downto 0);
        avs_hist_bin_writeresponsevalid : out std_logic;

        avs_csr_address                 : in  std_logic_vector(4 downto 0);
        avs_csr_read                    : in  std_logic;
        avs_csr_write                   : in  std_logic;
        avs_csr_writedata               : in  std_logic_vector(31 downto 0);
        avs_csr_readdata                : out std_logic_vector(31 downto 0);
        avs_csr_waitrequest             : out std_logic;

        avs_bridge_csr_address          : in  std_logic_vector(2 downto 0);
        avs_bridge_csr_read             : in  std_logic;
        avs_bridge_csr_write            : in  std_logic;
        avs_bridge_csr_writedata        : in  std_logic_vector(31 downto 0);
        avs_bridge_csr_readdata         : out std_logic_vector(31 downto 0);
        avs_bridge_csr_waitrequest      : out std_logic;

        asi_ctrl_data                   : in  std_logic_vector(8 downto 0);
        asi_ctrl_valid                  : in  std_logic;

        asi_pre_data                    : in  std_logic_vector(86 downto 0);
        asi_pre_valid                   : in  std_logic;
        asi_pre_ready                   : out std_logic;
        asi_pre_startofpacket           : in  std_logic;
        asi_pre_endofpacket             : in  std_logic;
        asi_pre_channel                 : in  std_logic_vector(3 downto 0);
        asi_pre_empty                   : in  std_logic;
        asi_pre_error                   : in  std_logic;

        aso_pre_data                    : out std_logic_vector(38 downto 0);
        aso_pre_valid                   : out std_logic;
        aso_pre_ready                   : in  std_logic;
        aso_pre_startofpacket           : out std_logic;
        aso_pre_endofpacket             : out std_logic;
        aso_pre_channel                 : out std_logic_vector(3 downto 0);
        aso_pre_empty                   : out std_logic;
        aso_pre_error                   : out std_logic;

        asi_post_data                   : in  std_logic_vector(35 downto 0);
        asi_post_valid                  : in  std_logic;
        asi_post_ready                  : out std_logic;
        asi_post_startofpacket          : in  std_logic;
        asi_post_endofpacket            : in  std_logic;

        aso_post_data                   : out std_logic_vector(35 downto 0);
        aso_post_valid                  : out std_logic;
        aso_post_ready                  : in  std_logic;
        aso_post_startofpacket          : out std_logic;
        aso_post_endofpacket            : out std_logic;

        aso_hist_fill_out_ready         : in  std_logic;
        aso_hist_fill_out_valid         : out std_logic;
        aso_hist_fill_out_data          : out std_logic_vector(86 downto 0);
        aso_hist_fill_out_startofpacket : out std_logic;
        aso_hist_fill_out_endofpacket   : out std_logic;
        aso_hist_fill_out_channel       : out std_logic_vector(3 downto 0);

        asi_debug_1_valid               : in  std_logic;
        asi_debug_1_data                : in  std_logic_vector(15 downto 0);
        asi_debug_2_valid               : in  std_logic;
        asi_debug_2_data                : in  std_logic_vector(15 downto 0);
        asi_debug_3_valid               : in  std_logic;
        asi_debug_3_data                : in  std_logic_vector(15 downto 0);
        asi_debug_4_valid               : in  std_logic;
        asi_debug_4_data                : in  std_logic_vector(15 downto 0);
        asi_debug_5_valid               : in  std_logic;
        asi_debug_5_data                : in  std_logic_vector(15 downto 0);
        asi_debug_6_valid               : in  std_logic;
        asi_debug_6_data                : in  std_logic_vector(15 downto 0)
    );
end entity hs0_board_instance_top;

architecture rtl of hs0_board_instance_top is

    signal post_sideband_data        : std_logic_vector(83 downto 0);
    signal post_sideband_valid       : std_logic;
    signal post_sideband_ready       : std_logic;
    signal post_sideband_sop         : std_logic;
    signal post_sideband_eop         : std_logic;

    signal hist_ingress_data         : std_logic_vector(86 downto 0);
    signal hist_ingress_valid        : std_logic;
    signal hist_ingress_ready        : std_logic;
    signal hist_ingress_sop          : std_logic;
    signal hist_ingress_eop          : std_logic;
    signal hist_ingress_channel      : std_logic_vector(3 downto 0);

begin

    post_ts_sideband_0 : entity work.histogram_post_ts_sideband
        port map (
            asi_post_data             => asi_post_data,
            asi_post_valid            => asi_post_valid,
            asi_post_ready            => asi_post_ready,
            asi_post_startofpacket    => asi_post_startofpacket,
            asi_post_endofpacket      => asi_post_endofpacket,
            aso_post_data             => post_sideband_data,
            aso_post_valid            => post_sideband_valid,
            aso_post_ready            => post_sideband_ready,
            aso_post_startofpacket    => post_sideband_sop,
            aso_post_endofpacket      => post_sideband_eop,
            rsi_reset_reset           => i_rst,
            csi_clock_clk             => i_clk
        );

    ingress_bridge_0 : entity work.histogram_ingress_bridge
        generic map (
            VERSION_MAJOR         => 26,
            VERSION_MINOR         => 0,
            VERSION_PATCH         => 9,
            BUILD                 => 515,
            VERSION_DATE          => 20260515,
            DEFAULT_SELECT_POST   => 0,
            ENABLE_POST_FORWARD   => 0,
            FILTER_POST_HIT_WORDS => 1
        )
        port map (
            avs_csr_address          => avs_bridge_csr_address,
            avs_csr_read             => avs_bridge_csr_read,
            avs_csr_write            => avs_bridge_csr_write,
            avs_csr_writedata        => avs_bridge_csr_writedata,
            avs_csr_readdata         => avs_bridge_csr_readdata,
            avs_csr_waitrequest      => avs_bridge_csr_waitrequest,
            asi_pre_data             => asi_pre_data,
            asi_pre_valid            => asi_pre_valid,
            asi_pre_ready            => asi_pre_ready,
            asi_pre_startofpacket    => asi_pre_startofpacket,
            asi_pre_endofpacket      => asi_pre_endofpacket,
            asi_pre_channel          => asi_pre_channel,
            asi_pre_empty            => asi_pre_empty,
            asi_pre_error            => asi_pre_error,
            aso_pre_data             => aso_pre_data,
            aso_pre_valid            => aso_pre_valid,
            aso_pre_ready            => aso_pre_ready,
            aso_pre_startofpacket    => aso_pre_startofpacket,
            aso_pre_endofpacket      => aso_pre_endofpacket,
            aso_pre_channel          => aso_pre_channel,
            aso_pre_empty            => aso_pre_empty,
            aso_pre_error            => aso_pre_error,
            asi_post_data            => post_sideband_data,
            asi_post_valid           => post_sideband_valid,
            asi_post_ready           => post_sideband_ready,
            asi_post_startofpacket   => post_sideband_sop,
            asi_post_endofpacket     => post_sideband_eop,
            aso_post_data            => aso_post_data,
            aso_post_valid           => aso_post_valid,
            aso_post_ready           => aso_post_ready,
            aso_post_startofpacket   => aso_post_startofpacket,
            aso_post_endofpacket     => aso_post_endofpacket,
            aso_hist_data            => hist_ingress_data,
            aso_hist_valid           => hist_ingress_valid,
            aso_hist_ready           => hist_ingress_ready,
            aso_hist_startofpacket   => hist_ingress_sop,
            aso_hist_endofpacket     => hist_ingress_eop,
            aso_hist_channel         => hist_ingress_channel,
            rsi_reset_reset          => i_rst,
            csi_clock_clk            => i_clk
        );

    histogram_statistics_0 : entity work.histogram_statistics_v2
        generic map (
            N_BINS                    => 256,
            MAX_COUNT_BITS            => 32,
            DEF_LEFT_BOUND            => 0,
            DEF_BIN_WIDTH             => 1,
            UPDATE_KEY_BIT_HI         => 86,
            UPDATE_KEY_BIT_LO         => 39,
            UPDATE_KEY_REPRESENTATION => "UNSIGNED",
            LOCK_KEY_RANGES           => true,
            FILTER_KEY_BIT_HI         => 38,
            FILTER_KEY_BIT_LO         => 35,
            SAR_TICK_WIDTH            => 32,
            SAR_KEY_WIDTH             => 32,
            N_PORTS                   => 1,
            CHANNELS_PER_PORT         => 32,
            COAL_QUEUE_DEPTH          => 256,
            AVST_DATA_WIDTH           => 87,
            AVST_CHANNEL_WIDTH        => 4,
            ENABLE_PINGPONG           => true,
            DEF_INTERVAL_CLOCKS       => 125000000,
            SNOOP_EN                  => false,
            ENABLE_PACKET             => false,
            AVS_ADDR_WIDTH            => 8,
            N_DEBUG_INTERFACE         => 6,
            DEBUG                     => 0,
            VERSION_MAJOR             => 26,
            VERSION_MINOR             => 2,
            VERSION_PATCH             => 3,
            BUILD                     => 514,
            IP_UID                    => 1212765012,
            VERSION_DATE              => 20260514,
            VERSION_GIT               => 72231161,
            INSTANCE_ID               => 0
        )
        port map (
            i_clk                           => i_clk,
            i_rst                           => i_rst,
            i_interval_reset                => i_interval_reset,
            avs_hist_bin_address            => avs_hist_bin_address,
            avs_hist_bin_read               => avs_hist_bin_read,
            avs_hist_bin_write              => avs_hist_bin_write,
            avs_hist_bin_writedata          => avs_hist_bin_writedata,
            avs_hist_bin_readdata           => avs_hist_bin_readdata,
            avs_hist_bin_readdatavalid      => avs_hist_bin_readdatavalid,
            avs_hist_bin_waitrequest        => avs_hist_bin_waitrequest,
            avs_hist_bin_burstcount         => avs_hist_bin_burstcount,
            avs_hist_bin_response           => avs_hist_bin_response,
            avs_hist_bin_writeresponsevalid => avs_hist_bin_writeresponsevalid,
            avs_csr_address                 => avs_csr_address,
            avs_csr_read                    => avs_csr_read,
            avs_csr_write                   => avs_csr_write,
            avs_csr_writedata               => avs_csr_writedata,
            avs_csr_readdata                => avs_csr_readdata,
            avs_csr_waitrequest             => avs_csr_waitrequest,
            asi_ctrl_data                   => asi_ctrl_data,
            asi_ctrl_valid                  => asi_ctrl_valid,
            asi_hist_fill_in_valid          => hist_ingress_valid,
            asi_hist_fill_in_ready          => hist_ingress_ready,
            asi_hist_fill_in_data           => hist_ingress_data,
            asi_hist_fill_in_startofpacket  => hist_ingress_sop,
            asi_hist_fill_in_endofpacket    => hist_ingress_eop,
            asi_hist_fill_in_channel        => hist_ingress_channel,
            asi_fill_in_1_valid             => '0',
            asi_fill_in_1_ready             => open,
            asi_fill_in_1_data              => (others => '0'),
            asi_fill_in_1_startofpacket     => '0',
            asi_fill_in_1_endofpacket       => '0',
            asi_fill_in_1_channel           => (others => '0'),
            asi_fill_in_2_valid             => '0',
            asi_fill_in_2_ready             => open,
            asi_fill_in_2_data              => (others => '0'),
            asi_fill_in_2_startofpacket     => '0',
            asi_fill_in_2_endofpacket       => '0',
            asi_fill_in_2_channel           => (others => '0'),
            asi_fill_in_3_valid             => '0',
            asi_fill_in_3_ready             => open,
            asi_fill_in_3_data              => (others => '0'),
            asi_fill_in_3_startofpacket     => '0',
            asi_fill_in_3_endofpacket       => '0',
            asi_fill_in_3_channel           => (others => '0'),
            asi_fill_in_4_valid             => '0',
            asi_fill_in_4_ready             => open,
            asi_fill_in_4_data              => (others => '0'),
            asi_fill_in_4_startofpacket     => '0',
            asi_fill_in_4_endofpacket       => '0',
            asi_fill_in_4_channel           => (others => '0'),
            asi_fill_in_5_valid             => '0',
            asi_fill_in_5_ready             => open,
            asi_fill_in_5_data              => (others => '0'),
            asi_fill_in_5_startofpacket     => '0',
            asi_fill_in_5_endofpacket       => '0',
            asi_fill_in_5_channel           => (others => '0'),
            asi_fill_in_6_valid             => '0',
            asi_fill_in_6_ready             => open,
            asi_fill_in_6_data              => (others => '0'),
            asi_fill_in_6_startofpacket     => '0',
            asi_fill_in_6_endofpacket       => '0',
            asi_fill_in_6_channel           => (others => '0'),
            asi_fill_in_7_valid             => '0',
            asi_fill_in_7_ready             => open,
            asi_fill_in_7_data              => (others => '0'),
            asi_fill_in_7_startofpacket     => '0',
            asi_fill_in_7_endofpacket       => '0',
            asi_fill_in_7_channel           => (others => '0'),
            aso_hist_fill_out_ready         => aso_hist_fill_out_ready,
            aso_hist_fill_out_valid         => aso_hist_fill_out_valid,
            aso_hist_fill_out_data          => aso_hist_fill_out_data,
            aso_hist_fill_out_startofpacket => aso_hist_fill_out_startofpacket,
            aso_hist_fill_out_endofpacket   => aso_hist_fill_out_endofpacket,
            aso_hist_fill_out_channel       => aso_hist_fill_out_channel,
            asi_debug_1_valid               => asi_debug_1_valid,
            asi_debug_1_data                => asi_debug_1_data,
            asi_debug_2_valid               => asi_debug_2_valid,
            asi_debug_2_data                => asi_debug_2_data,
            asi_debug_3_valid               => asi_debug_3_valid,
            asi_debug_3_data                => asi_debug_3_data,
            asi_debug_4_valid               => asi_debug_4_valid,
            asi_debug_4_data                => asi_debug_4_data,
            asi_debug_5_valid               => asi_debug_5_valid,
            asi_debug_5_data                => asi_debug_5_data,
            asi_debug_6_valid               => asi_debug_6_valid,
            asi_debug_6_data                => asi_debug_6_data
        );

end architecture rtl;
