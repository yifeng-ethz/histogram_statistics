-- File name: histogram_pre_ts_trim.vhd
-- Author: OpenAI Codex
-- =======================================
-- Revision: 26.0.0
--     Date: 20260514
--     Change: Trim the pre-rbCAM true timestamp sideband before rbCAM.
-- =========
-- Description:
--     The mutrig timestamp processor emits Type-1 hits as
--     {ts[47:0], payload[38:0]} so the histogram delay mode can subtract
--     against a full true hit timestamp. rbCAM still consumes the legacy
--     39-bit Type-1 payload, so this adapter preserves the Avalon-ST sideband
--     signals and forwards only data[38:0].
--
-- ================ synthesizer configuration ===================
-- altera vhdl_input_version vhdl_2008
-- =============================================================

library ieee;
use ieee.std_logic_1164.all;

entity histogram_pre_ts_trim is
    generic (
        VERSION_MAJOR    : natural    := 26;
        VERSION_MINOR    : natural    := 0;
        VERSION_PATCH    : natural    := 0;
        BUILD            : natural    := 514;
        VERSION_DATE     : natural    := 20260514
    );
    port (
        asi_hit_data             : in  std_logic_vector(86 downto 0);
        asi_hit_valid            : in  std_logic;
        asi_hit_ready            : out std_logic;
        asi_hit_startofpacket    : in  std_logic;
        asi_hit_endofpacket      : in  std_logic;
        asi_hit_channel          : in  std_logic_vector(3 downto 0);
        asi_hit_empty            : in  std_logic;
        asi_hit_error            : in  std_logic;

        aso_hit_data             : out std_logic_vector(38 downto 0);
        aso_hit_valid            : out std_logic;
        aso_hit_ready            : in  std_logic;
        aso_hit_startofpacket    : out std_logic;
        aso_hit_endofpacket      : out std_logic;
        aso_hit_channel          : out std_logic_vector(3 downto 0);
        aso_hit_empty            : out std_logic;
        aso_hit_error            : out std_logic;

        rsi_reset_reset    : in  std_logic;
        csi_clock_clk      : in  std_logic
    );
end entity histogram_pre_ts_trim;

architecture rtl of histogram_pre_ts_trim is

begin

    asi_hit_ready            <= aso_hit_ready;
    aso_hit_data             <= asi_hit_data(38 downto 0);
    aso_hit_valid            <= asi_hit_valid;
    aso_hit_startofpacket    <= asi_hit_startofpacket;
    aso_hit_endofpacket      <= asi_hit_endofpacket;
    aso_hit_channel          <= asi_hit_channel;
    aso_hit_empty            <= asi_hit_empty;
    aso_hit_error            <= asi_hit_error;

end architecture rtl;
