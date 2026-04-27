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
        asi_ctrl_data                   : in  std_logic_vector(8 downto 0);
        asi_ctrl_valid                  : in  std_logic;
        asi_ctrl_ready                  : out std_logic;
        asi_hist_fill_in_valid          : in  std_logic;
        asi_hist_fill_in_ready          : out std_logic;
        asi_hist_fill_in_data           : in  std_logic_vector(38 downto 0);
        asi_hist_fill_in_startofpacket  : in  std_logic;
        asi_hist_fill_in_endofpacket    : in  std_logic;
        asi_hist_fill_in_channel        : in  std_logic_vector(3 downto 0);
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
begin

    histogram_statistics_0 : entity work.histogram_statistics_v2
        generic map (
            N_BINS                    => 256,
            MAX_COUNT_BITS            => 32,
            DEF_LEFT_BOUND            => 0,
            DEF_BIN_WIDTH             => 1,
            UPDATE_KEY_BIT_HI         => 29,
            UPDATE_KEY_BIT_LO         => 17,
            UPDATE_KEY_REPRESENTATION => "SIGNED",
            FILTER_KEY_BIT_HI         => 38,
            FILTER_KEY_BIT_LO         => 35,
            SAR_TICK_WIDTH            => 16,
            SAR_KEY_WIDTH             => 8,
            N_PORTS                   => 1,
            CHANNELS_PER_PORT         => 32,
            COAL_QUEUE_DEPTH          => 256,
            AVST_DATA_WIDTH           => 39,
            AVST_CHANNEL_WIDTH        => 4,
            ENABLE_PINGPONG           => true,
            DEF_INTERVAL_CLOCKS       => 125000000,
            SNOOP_EN                  => false,
            ENABLE_PACKET             => false,
            AVS_ADDR_WIDTH            => 8,
            N_DEBUG_INTERFACE         => 6,
            DEBUG                     => 0,
            VERSION_MAJOR             => 26,
            VERSION_MINOR             => 0,
            VERSION_PATCH             => 0,
            BUILD                     => 0,
            IP_UID                    => 1212765012,
            VERSION_DATE              => 20260416,
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
            asi_ctrl_ready                  => asi_ctrl_ready,
            asi_hist_fill_in_valid          => asi_hist_fill_in_valid,
            asi_hist_fill_in_ready          => asi_hist_fill_in_ready,
            asi_hist_fill_in_data           => asi_hist_fill_in_data,
            asi_hist_fill_in_startofpacket  => asi_hist_fill_in_startofpacket,
            asi_hist_fill_in_endofpacket    => asi_hist_fill_in_endofpacket,
            asi_hist_fill_in_channel        => asi_hist_fill_in_channel,
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
            aso_hist_fill_out_ready         => '1',
            aso_hist_fill_out_valid         => open,
            aso_hist_fill_out_data          => open,
            aso_hist_fill_out_startofpacket => open,
            aso_hist_fill_out_endofpacket   => open,
            aso_hist_fill_out_channel       => open,
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
