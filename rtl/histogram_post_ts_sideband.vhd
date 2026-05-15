-- File name: histogram_post_ts_sideband.vhd
-- Author: OpenAI Codex
-- =======================================
-- Revision: 26.0.0
--     Date: 20260514
--     Change: Add Type3/FEB-frame timestamp sideband adapter for histogram
--             post-rbCAM delay mode.
-- =========
-- Description:
--     Parses the post-rbCAM/FEB-framed 36-bit stream and appends a 48-bit
--     true hit timestamp sideband above each word. Hit timestamps are derived
--     from the active frame header timestamp high bits, K23.7 subheader
--     timestamp bits [11:4], and the hit word timestamp nibble [3:0].
--     Non-hit words carry a zero sideband; downstream hit filtering in the
--     ingress bridge suppresses those words for histogram updates.
--
-- ================ synthesizer configuration ===================
-- altera vhdl_input_version vhdl_2008
-- =============================================================

library ieee;
use ieee.std_logic_1164.all;

entity histogram_post_ts_sideband is
    generic (
        VERSION_MAJOR    : natural    := 26;
        VERSION_MINOR    : natural    := 0;
        VERSION_PATCH    : natural    := 0;
        BUILD            : natural    := 514;
        VERSION_DATE     : natural    := 20260514
    );
    port (
        asi_post_data             : in  std_logic_vector(35 downto 0);
        asi_post_valid            : in  std_logic;
        asi_post_ready            : out std_logic;
        asi_post_startofpacket    : in  std_logic;
        asi_post_endofpacket      : in  std_logic;

        aso_post_data             : out std_logic_vector(83 downto 0);
        aso_post_valid            : out std_logic;
        aso_post_ready            : in  std_logic;
        aso_post_startofpacket    : out std_logic;
        aso_post_endofpacket      : out std_logic;

        rsi_reset_reset    : in  std_logic;
        csi_clock_clk      : in  std_logic
    );
end entity histogram_post_ts_sideband;

architecture rtl of histogram_post_ts_sideband is

    type parser_state_t is (WAITING_HEADER, CAPTURING_TS_HI, CAPTURING_TS_LO, TRACKING_FRAME);

    constant K_FLAG_CONST    : std_logic_vector(3 downto 0)    := "0001";
    constant K237_CONST      : std_logic_vector(7 downto 0)    := x"F7";
    constant K284_CONST      : std_logic_vector(7 downto 0)    := x"9C";
    constant K285_CONST      : std_logic_vector(7 downto 0)    := x"BC";

    signal parser_state              : parser_state_t                   := WAITING_HEADER;
    signal frame_ts_high             : std_logic_vector(35 downto 0)    := (others    => '0');
    signal subheader_ts              : std_logic_vector(7 downto 0)     := (others    => '0');
    signal current_hit_ts            : std_logic_vector(47 downto 0);
    signal hit_region                : std_logic := '0';
    signal post_handshake            : std_logic;
    signal post_word_is_k            : std_logic;
    signal post_word_is_hit          : std_logic;
    signal post_word_is_header       : std_logic;
    signal post_word_is_trailer      : std_logic;
    signal post_word_is_subheader    : std_logic;

begin

    post_word_is_k            <= '1' when asi_post_data(35 downto 32) = K_FLAG_CONST else '0';

    post_word_is_hit    <= '1' when post_word_is_k = '0' and
                                    parser_state = TRACKING_FRAME and
                                    hit_region = '1' else '0';

    post_word_is_header       <= '1' when post_word_is_k = '1' and asi_post_data(7 downto 0) = K285_CONST else '0';
    post_word_is_trailer      <= '1' when post_word_is_k = '1' and asi_post_data(7 downto 0) = K284_CONST else '0';
    post_word_is_subheader    <= '1' when post_word_is_k = '1' and asi_post_data(7 downto 0) = K237_CONST else '0';
    current_hit_ts            <= frame_ts_high & subheader_ts & asi_post_data(31 downto 28);

    asi_post_ready    <= aso_post_ready;
    aso_post_valid    <= asi_post_valid;

    aso_post_data             <= current_hit_ts & asi_post_data when post_word_is_hit = '1' else
                                 x"000000000000" & asi_post_data;
    aso_post_startofpacket    <= asi_post_startofpacket;
    aso_post_endofpacket      <= asi_post_endofpacket;

    post_handshake <= asi_post_valid and aso_post_ready;

    proc_parser : process (csi_clock_clk)
    begin
        if rising_edge(csi_clock_clk) then
            if rsi_reset_reset = '1' then
                parser_state     <= WAITING_HEADER;
                frame_ts_high    <= (others    => '0');
                subheader_ts     <= (others    => '0');
                hit_region       <= '0';
            elsif post_handshake = '1' then
                case parser_state is
                    when WAITING_HEADER =>
                        if post_word_is_header = '1' then
                            parser_state     <= CAPTURING_TS_HI;
                            frame_ts_high    <= (others    => '0');
                            subheader_ts     <= (others    => '0');
                            hit_region       <= '0';
                        end if;

                    when CAPTURING_TS_HI =>
                        frame_ts_high(35 downto 4)    <= asi_post_data(31 downto 0);
                        parser_state                  <= CAPTURING_TS_LO;

                    when CAPTURING_TS_LO =>
                        frame_ts_high(3 downto 0)    <= asi_post_data(31 downto 28);
                        parser_state                 <= TRACKING_FRAME;

                    when TRACKING_FRAME =>
                        if post_word_is_header = '1' then
                            parser_state     <= CAPTURING_TS_HI;
                            frame_ts_high    <= (others    => '0');
                            subheader_ts     <= (others    => '0');
                            hit_region       <= '0';
                        elsif post_word_is_trailer = '1' then
                            parser_state    <= WAITING_HEADER;
                            hit_region      <= '0';
                        elsif post_word_is_subheader = '1' then
                            subheader_ts    <= asi_post_data(31 downto 24);
                            hit_region      <= '1';
                        end if;
                end case;
            end if;
        end if;
    end process proc_parser;

end architecture rtl;
