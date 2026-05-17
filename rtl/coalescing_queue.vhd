-- File name: coalescing_queue.vhd
-- Author: Yifeng Wang (yifenwan@phys.ethz.ch)
-- =======================================
-- Revision: 1.0 (file created)
--		Date: Mar 20, 2026
-- Revision: 1.1
--      Date: Apr 27, 2026
--      Change: Register the peak occupancy update from the previous
--              cycle's queue level. This keeps the max statistic exact
--              while removing the hit-bin decode from the timing path
--              into queue_level_max.
-- Revision: 1.2
--      Date: May 15, 2026
--      Change: Register the queue-overflow event before the saturating
--              diagnostic counter. The counter remains exact but no longer
--              puts hit-bin decode on the counter enable timing path.
-- Revision: 1.3
--      Date: May 16, 2026
--      Change: Replace the generated per-bin kick/queued register bank with
--              one indexed owner process. This preserves the coalescing
--              contract while removing the 256-way replicated update cone.
-- Revision: 1.4
--      Date: May 16, 2026
--      Change: Replace the per-bin resident kick table with a bounded
--              live-cell coalescer. This is the V3 "bunched" architecture:
--              it compares an incoming bin against the live cells, coalesces
--              on match, allocates a FIFO cell on miss, and exposes overflow
--              when all live cells are occupied by other bins.
-- Revision: 1.5
--      Date: May 16, 2026
--      Change: Remove the redundant post-reset scrub FSM. Reset and clear
--              already own every live-cell register, so the scrub path only
--              added a high-fanout clear_active mux to the hot update cone.
-- =========
-- Description:	[Queued post-divider coalescer for histogram bin updates]
--
--			Accepts single-increment hits from the bin_divider and coalesces
--			repeated hits to the same bin into a single (bin, count) pair.
--			A circular queue tracks which bins are "in-flight"; a per-bin
--			kick counter accumulates hits until the queue head is drained.
--			Reports queue occupancy, peak occupancy, and overflow count.
--

-- ================ synthsizer configuration ===================
-- altera vhdl_input_version vhdl_2008
-- =============================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.histogram_statistics_v2_pkg.all;

entity coalescing_queue is
    generic (
        N_BINS          : natural := 256;
        QUEUE_DEPTH     : natural := 256;
        KICK_WIDTH      : natural := 8;
        OVERFLOW_WIDTH  : natural := 16
    );
    port (
        i_clk            : in  std_logic;
        i_rst            : in  std_logic;
        i_clear          : in  std_logic;
        i_hit_valid      : in  std_logic;
        i_hit_bank       : in  std_logic;
        i_hit_bin        : in  unsigned(clog2(N_BINS) - 1 downto 0);
        i_drain_ready    : in  std_logic;
        o_drain_valid    : out std_logic;
        o_drain_bank     : out std_logic;
        o_drain_bin      : out unsigned(clog2(N_BINS) - 1 downto 0);
        o_drain_count    : out unsigned(KICK_WIDTH - 1 downto 0);
        o_occupancy      : out unsigned(clog2(QUEUE_DEPTH + 1) - 1 downto 0);
        o_occupancy_max  : out unsigned(clog2(QUEUE_DEPTH + 1) - 1 downto 0);
        o_overflow_count : out unsigned(OVERFLOW_WIDTH - 1 downto 0)
    );
end entity coalescing_queue;

architecture rtl of coalescing_queue is

    constant BIN_INDEX_WIDTH_CONST   : natural := clog2(N_BINS);
    constant QUEUE_ADDR_WIDTH_CONST  : natural := clog2(QUEUE_DEPTH);
    constant OCC_WIDTH_CONST         : natural := clog2(QUEUE_DEPTH + 1);

    subtype bin_index_t  is unsigned(BIN_INDEX_WIDTH_CONST - 1 downto 0);
    subtype kick_t       is unsigned(KICK_WIDTH - 1 downto 0);
    subtype occ_t        is unsigned(OCC_WIDTH_CONST - 1 downto 0);
    subtype queue_addr_t is unsigned(QUEUE_ADDR_WIDTH_CONST - 1 downto 0);
    constant KICK_MAX    : kick_t := (others => '1');

    type live_bin_t   is array (0 to QUEUE_DEPTH - 1) of bin_index_t;
    type live_count_t is array (0 to QUEUE_DEPTH - 1) of kick_t;

    signal live_valid       : std_logic_vector(QUEUE_DEPTH - 1 downto 0) := (others => '0');
    signal live_bank        : std_logic_vector(QUEUE_DEPTH - 1 downto 0) := (others => '0');
    signal live_bin         : live_bin_t := (others => (others => '0'));
    signal live_count       : live_count_t := (others => (others => '0'));
    signal queue_rd_ptr     : queue_addr_t := (others => '0');
    signal queue_wr_ptr     : queue_addr_t := (others => '0');
    signal queue_level      : occ_t := (others => '0');
    signal queue_level_max  : occ_t := (others => '0');
    signal overflow_count_q : unsigned(OVERFLOW_WIDTH - 1 downto 0) := (others => '0');
    signal overflow_event_q : std_logic := '0';
    signal drain_valid_q      : std_logic := '0';
    signal drain_bank_q       : std_logic := '0';
    signal drain_bin_q        : bin_index_t := (others => '0');
    signal drain_count_q      : kick_t := (others => '0');
    signal drain_fire_c       : std_logic := '0';
    signal head_count_c       : kick_t := (others => '0');
    signal queue_room_c       : std_logic := '0';

    function next_queue_ptr_f(ptr : queue_addr_t) return queue_addr_t is
    begin
        if to_integer(ptr) = QUEUE_DEPTH - 1 then
            return (others => '0');
        end if;
        return ptr + 1;
    end function next_queue_ptr_f;

begin

    o_drain_valid    <= drain_valid_q;
    o_drain_bank     <= drain_bank_q;
    o_drain_bin      <= drain_bin_q;
    o_drain_count    <= drain_count_q;
    o_occupancy      <= queue_level;
    o_occupancy_max  <= queue_level_max;
    o_overflow_count <= overflow_count_q;
    drain_fire_c <= '1' when (i_drain_ready = '1') and (queue_level /= 0) else '0';
    head_count_c <= live_count(to_integer(queue_rd_ptr));
    queue_room_c <= '1' when (to_integer(queue_level) < QUEUE_DEPTH) or (drain_fire_c = '1') else '0';

    queue_reg : process (i_clk)
        variable hit_bin_v        : bin_index_t;
        variable level_v          : occ_t;
        variable overflow_v       : unsigned(OVERFLOW_WIDTH - 1 downto 0);
        variable overflow_event_v : std_logic;
        variable rd_ptr_v         : queue_addr_t;
        variable wr_ptr_v         : queue_addr_t;
        variable match_valid_v    : boolean;
        variable match_idx_v      : natural;
        variable search_valid_v   : boolean;
    begin
        if rising_edge(i_clk) then
            drain_valid_q <= '0';

            if i_rst = '1' then
                queue_rd_ptr     <= (others => '0');
                queue_wr_ptr     <= (others => '0');
                queue_level      <= (others => '0');
                queue_level_max  <= (others => '0');
                overflow_count_q <= (others => '0');
                overflow_event_q <= '0';
                drain_bank_q     <= '0';
                drain_bin_q      <= (others => '0');
                drain_count_q    <= (others => '0');
                live_valid       <= (others => '0');
                live_bank        <= (others => '0');
                live_bin         <= (others => (others => '0'));
                live_count       <= (others => (others => '0'));
            elsif i_clear = '1' then
                queue_rd_ptr     <= (others => '0');
                queue_wr_ptr     <= (others => '0');
                queue_level      <= (others => '0');
                queue_level_max  <= (others => '0');
                overflow_count_q <= (others => '0');
                overflow_event_q <= '0';
                drain_bank_q     <= '0';
                drain_bin_q      <= (others => '0');
                drain_count_q    <= (others => '0');
                live_valid       <= (others => '0');
                live_bank        <= (others => '0');
                live_bin         <= (others => (others => '0'));
                live_count       <= (others => (others => '0'));
            else
                hit_bin_v    := (others => '0');
                level_v      := queue_level;
                overflow_v   := overflow_count_q;
                overflow_event_v := '0';
                rd_ptr_v     := queue_rd_ptr;
                wr_ptr_v     := queue_wr_ptr;
                match_valid_v := false;
                match_idx_v   := 0;
                drain_valid_q <= drain_fire_c;
                drain_bank_q  <= live_bank(to_integer(queue_rd_ptr));
                drain_bin_q   <= live_bin(to_integer(queue_rd_ptr));
                drain_count_q <= head_count_c;

                if overflow_event_q = '1' then
                    overflow_v := sat_inc(overflow_v);
                end if;

                if drain_fire_c = '1' then
                    live_valid(to_integer(rd_ptr_v)) <= '0';
                    live_bank(to_integer(rd_ptr_v))  <= '0';
                    live_bin(to_integer(rd_ptr_v))   <= (others => '0');
                    live_count(to_integer(rd_ptr_v)) <= (others => '0');
                    rd_ptr_v     := next_queue_ptr_f(rd_ptr_v);
                    level_v      := level_v - 1;
                end if;

                if i_hit_valid = '1' then
                    hit_bin_v := i_hit_bin;

                    for cell_idx in 0 to QUEUE_DEPTH - 1 loop
                        search_valid_v := live_valid(cell_idx) = '1';
                        if (drain_fire_c = '1') and (cell_idx = to_integer(queue_rd_ptr)) then
                            search_valid_v := false;
                        end if;

                        if search_valid_v and (live_bank(cell_idx) = i_hit_bank) and
                           (live_bin(cell_idx) = hit_bin_v) and (not match_valid_v) then
                            match_valid_v := true;
                            match_idx_v   := cell_idx;
                        end if;
                    end loop;

                    if match_valid_v then
                        if live_count(match_idx_v) = KICK_MAX then
                            overflow_event_v := '1';
                        else
                            live_count(match_idx_v) <= live_count(match_idx_v) + 1;
                        end if;
                    elsif queue_room_c = '1' then
                        live_valid(to_integer(wr_ptr_v)) <= '1';
                        live_bank(to_integer(wr_ptr_v))  <= i_hit_bank;
                        live_bin(to_integer(wr_ptr_v))   <= hit_bin_v;
                        live_count(to_integer(wr_ptr_v)) <= to_unsigned(1, KICK_WIDTH);
                        wr_ptr_v := next_queue_ptr_f(wr_ptr_v);
                        level_v  := level_v + 1;
                    else
                        overflow_event_v := '1';
                    end if;
                end if;

                queue_rd_ptr <= rd_ptr_v;
                queue_wr_ptr <= wr_ptr_v;
                queue_level  <= level_v;
                overflow_count_q <= overflow_v;
                overflow_event_q <= overflow_event_v;

                if level_v > queue_level_max then
                    queue_level_max <= level_v;
                end if;
            end if;
        end if;
    end process queue_reg;

end architecture rtl;
