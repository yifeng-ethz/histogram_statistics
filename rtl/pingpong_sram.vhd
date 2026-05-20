-- altera vhdl_input_version vhdl_2008
-- File name: pingpong_sram.vhd
-- Author: Yifeng Wang (yifenwan@phys.ethz.ch)
-- =======================================
-- Revision: 1.0 (file created)
--		Date: Mar 20, 2026
-- Revision: 1.1
--      Date: May 17, 2026
--      Change: Register update-ready as a one-cycle-ahead prediction so
--              interval compare and host readout state do not feed the
--              coalescer drain/update cone in the same cycle.
-- Revision: 1.2
--      Date: May 17, 2026
--      Change: Add a force-interval input so run-control termination can
--              freeze the final partial bank after pending updates drain.
-- Revision: 1.3
--      Date: May 17, 2026
--      Change: Latch force-interval requests and defer them while a host
--              histogram read is active, preventing a termination flush from
--              clearing a frozen bank mid-read.
-- Revision: 1.4
--      Date: May 17, 2026
--      Change: Backpressure histogram read commands while the read engine is
--              busy; generated Qsys may decompose a host burst into one-beat
--              slave reads.
-- Revision: 1.5
--      Date: May 17, 2026
--      Change: Export update-pipeline busy so run termination can wait until
--              accepted coalescer drains have committed to SRAM.
-- Revision: 1.6
--      Date: May 17, 2026
--      Change: Accept a same-cycle old-bank update at a ping-pong interval
--              switch so the one-cycle-ahead update-ready prediction cannot
--              drain and drop the final coalescer update of a window.
-- Revision: 1.7
--      Date: May 20, 2026
--      Change: Seamless bank-follow host read. A host burst that straddles an
--              interval bank swap no longer parks in hist_read_pending and
--              holds waitrequest (sc_hub watchdog -> 0xEEEEEEEE padding).
--              The host-read source bank now follows hist_wait_bank
--              dynamically through the burst, so when active_bank flips
--              mid-burst the read continues from the just-frozen bank
--              (interval N+1) instead of the bank being cleared. The
--              hist_wait_bank_busy / hist_read_pending contribution to
--              waitrequest is removed; only the short per-beat burst pacing
--              remains. An in-flight beat issued to the pre-swap bank is
--              reissued to the new frozen bank so every burstcount beat
--              returns valid OKAY data with no missing/duplicated beat. The
--              update/accumulation engine is unchanged.
-- =========
-- Description:	[Dual-bank histogram storage with interval freeze/readout]
--
--			Two dual-port RAM banks hold histogram bin counts. In ping-pong mode
--			the active write bank flips on each interval boundary, while the host
--			reads the frozen bank from the previous interval. The newly active
--			bank is cleared before new updates are accepted so the update path can
--			use a deterministic RAM read/modify/write pipeline.
--

-- ================ synthsizer configuration ===================
-- altera vhdl_input_version vhdl_2008
-- =============================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.histogram_statistics_v2_pkg.all;

entity pingpong_sram is
    generic (
        N_BINS         : natural := 256;
        COUNT_WIDTH    : natural := HS_COUNT_W_CONST;
        UPDATE_WIDTH   : natural := HS_KICK_W_CONST
    );
    port (
        i_clk                : in  std_logic;
        i_rst                : in  std_logic;
        i_clear              : in  std_logic;
        i_enable_pingpong    : in  std_logic;
        i_interval_clocks    : in  unsigned(31 downto 0);
        i_force_interval     : in  std_logic := '0';
        i_upd_valid          : in  std_logic;
        i_upd_bank           : in  std_logic;
        i_upd_bin            : in  unsigned(clog2(N_BINS) - 1 downto 0);
        i_upd_count          : in  unsigned(UPDATE_WIDTH - 1 downto 0);
        i_bank_pending       : in  std_logic_vector(1 downto 0) := (others => '0');
        o_upd_ready          : out std_logic;
        i_hist_read          : in  std_logic;
        i_hist_address       : in  unsigned(clog2(N_BINS) - 1 downto 0);
        i_hist_burstcount    : in  unsigned(clog2(N_BINS) downto 0);
        o_hist_readdata      : out std_logic_vector(31 downto 0);
        o_hist_readdatavalid : out std_logic;
        o_hist_waitrequest   : out std_logic;
        o_update_busy        : out std_logic;
        o_active_bank        : out std_logic;
        o_flushing           : out std_logic;
        o_flush_addr         : out unsigned(clog2(N_BINS) - 1 downto 0);
        o_interval_pulse     : out std_logic
    );
end entity pingpong_sram;

architecture rtl of pingpong_sram is

    constant ADDR_WIDTH_CONST : natural := clog2(N_BINS);
    constant RAM_DEPTH_CONST  : natural := 2 ** ADDR_WIDTH_CONST;

    subtype count_t     is unsigned(COUNT_WIDTH - 1 downto 0);
    subtype count_slv_t is std_logic_vector(COUNT_WIDTH - 1 downto 0);
    subtype addr_t      is unsigned(ADDR_WIDTH_CONST - 1 downto 0);

    signal bank_a_addr_a : natural range 0 to RAM_DEPTH_CONST - 1 := 0;
    signal bank_a_addr_b : natural range 0 to RAM_DEPTH_CONST - 1 := 0;
    signal bank_b_addr_a : natural range 0 to RAM_DEPTH_CONST - 1 := 0;
    signal bank_b_addr_b : natural range 0 to RAM_DEPTH_CONST - 1 := 0;
    signal bank_a_data_a : count_slv_t := (others => '0');
    signal bank_a_data_b : count_slv_t := (others => '0');
    signal bank_b_data_a : count_slv_t := (others => '0');
    signal bank_b_data_b : count_slv_t := (others => '0');
    signal bank_a_we_a   : std_logic := '0';
    signal bank_a_we_b   : std_logic := '0';
    signal bank_b_we_a   : std_logic := '0';
    signal bank_b_we_b   : std_logic := '0';
    signal bank_a_q_a    : count_slv_t;
    signal bank_a_q_b    : count_slv_t;
    signal bank_b_q_a    : count_slv_t;
    signal bank_b_q_b    : count_slv_t;
    signal bank_a_valid   : std_logic_vector(RAM_DEPTH_CONST - 1 downto 0) := (others => '0');
    signal bank_b_valid   : std_logic_vector(RAM_DEPTH_CONST - 1 downto 0) := (others => '0');

    signal active_bank        : std_logic := '0';
    signal clear_active       : std_logic := '1';
    signal clear_bank         : std_logic := '0';
    signal clear_both         : std_logic := '1';
    signal clear_addr         : addr_t := (others => '0');
    signal timer_count        : unsigned(31 downto 0) := (others => '0');
    signal interval_pulse     : std_logic := '0';
    signal force_interval_pending : std_logic := '0';

    signal upd_issue_valid    : std_logic := '0';
    signal upd_issue_bank     : std_logic := '0';
    signal upd_issue_bin      : addr_t := (others => '0');
    signal upd_issue_count    : count_t := (others => '0');
    signal upd_issue_old_valid: std_logic := '0';
    signal upd_read_valid     : std_logic := '0';
    signal upd_read_bank      : std_logic := '0';
    signal upd_read_bin       : addr_t := (others => '0');
    signal upd_read_count     : count_t := (others => '0');
    signal upd_read_old_valid : std_logic := '0';
    signal upd_add_valid      : std_logic := '0';
    signal upd_add_bank       : std_logic := '0';
    signal upd_add_bin        : addr_t := (others => '0');
    signal upd_add_data       : count_t := (others => '0');
    signal upd_add_count      : count_t := (others => '0');
    signal upd_sum_valid      : std_logic := '0';
    signal upd_sum_bank       : std_logic := '0';
    signal upd_sum_bin        : addr_t := (others => '0');
    signal upd_sum_value      : count_t := (others => '0');
    signal upd_write_valid    : std_logic := '0';
    signal upd_write_bank     : std_logic := '0';
    signal upd_write_bin      : addr_t := (others => '0');
    signal upd_write_value    : count_t := (others => '0');
    signal upd_read_data      : count_t;
    signal upd_ready_int      : std_logic := '0';

    signal burst_active       : std_logic := '0';
    signal burst_addr         : addr_t := (others => '0');
    signal burst_remaining    : unsigned(ADDR_WIDTH_CONST downto 0) := (others => '0');
    signal hist_issue_valid   : std_logic := '0';
    signal hist_issue_bank    : std_logic := '0';
    signal hist_issue_addr    : addr_t := (others => '0');
    signal hist_issue_bin_valid : std_logic := '0';
    signal hist_read_valid    : std_logic := '0';
    signal hist_read_bank     : std_logic := '0';
    signal hist_read_bin_valid : std_logic := '0';
    signal hist_readdata      : std_logic_vector(31 downto 0) := (others => '0');
    signal hist_readdatavalid : std_logic := '0';
    signal hist_waitrequest   : std_logic := '0';
    signal update_busy        : std_logic := '0';

begin

    o_upd_ready          <= upd_ready_int;
    o_hist_readdata      <= hist_readdata;
    o_hist_readdatavalid <= hist_readdatavalid;
    o_hist_waitrequest   <= hist_waitrequest;
    o_update_busy        <= update_busy;
    o_active_bank        <= active_bank;
    o_flushing           <= '1' when (clear_active = '1') or
                                      ((i_enable_pingpong = '1') and
                                       (((active_bank = '0') and (i_bank_pending(1) = '1')) or
                                        ((active_bank = '1') and (i_bank_pending(0) = '1'))))
                             else '0';
    o_flush_addr         <= clear_addr;
    o_interval_pulse     <= interval_pulse;

    upd_read_data   <= unsigned(bank_a_q_a) when upd_read_bank = '0' else unsigned(bank_b_q_a);

    bank_a_we_a   <= '0';
    bank_b_we_a   <= '0';
    bank_a_data_a <= (others => '0');
    bank_b_data_a <= (others => '0');

    -- Frozen readout bank. Under ping-pong this is the bank NOT being written;
    -- it flips the same cycle active_bank flips at an interval swap. The host
    -- read follows this dynamically (see ram_ctrl) so a burst straddling a swap
    -- continues from the just-frozen interval rather than the bank being
    -- cleared.
    -- The seamless bank-follow read never stalls on a bank swap or a transient
    -- update touch of the readout bank: it follows to the frozen bank instead
    -- (frozen bank = not active_bank under ping-pong, tracked cycle-by-cycle in
    -- ram_ctrl). Only the short per-beat burst pacing (burst_active) gates new
    -- commands, which the sc_hub watchdog tolerates.
    hist_waitrequest <= '1' when (burst_active = '1')
                        else '0';
    update_busy <= i_upd_valid or
                   upd_issue_valid or
                   upd_read_valid or
                   upd_add_valid or
                   upd_sum_valid or
                   upd_write_valid;

    bank_a_addr_a <= to_integer(hist_issue_addr) when (hist_issue_valid = '1') and (hist_issue_bank = '0')
                else to_integer(upd_issue_bin)   when (upd_issue_valid = '1') and (upd_issue_bank = '0')
                else 0;

    bank_b_addr_a <= to_integer(hist_issue_addr) when (hist_issue_valid = '1') and (hist_issue_bank = '1')
                else to_integer(upd_issue_bin)   when (upd_issue_valid = '1') and (upd_issue_bank = '1')
                else 0;

    bank_a_we_b   <= '1' when ((clear_active = '1') and (clear_bank = '0'))
                              or ((upd_write_valid = '1') and (upd_write_bank = '0'))
                     else '0';
    bank_b_we_b   <= '1' when ((clear_active = '1') and (clear_bank = '1'))
                              or ((upd_write_valid = '1') and (upd_write_bank = '1'))
                     else '0';

    bank_a_addr_b <= to_integer(clear_addr)     when (clear_active = '1') and (clear_bank = '0')
                else to_integer(upd_write_bin)  when (upd_write_valid = '1') and (upd_write_bank = '0')
                else 0;

    bank_b_addr_b <= to_integer(clear_addr)     when (clear_active = '1') and (clear_bank = '1')
                else to_integer(upd_write_bin)  when (upd_write_valid = '1') and (upd_write_bank = '1')
                else 0;

    bank_a_data_b <= (others => '0')              when (clear_active = '1') and (clear_bank = '0')
                else std_logic_vector(upd_write_value);

    bank_b_data_b <= (others => '0')              when (clear_active = '1') and (clear_bank = '1')
                else std_logic_vector(upd_write_value);

    bank_a_ram : entity work.true_dual_port_ram_single_clock
        generic map (
            DATA_WIDTH => COUNT_WIDTH,
            ADDR_WIDTH => ADDR_WIDTH_CONST
        )
        port map (
            clk    => i_clk,
            addr_a => bank_a_addr_a,
            addr_b => bank_a_addr_b,
            data_a => bank_a_data_a,
            data_b => bank_a_data_b,
            we_a   => bank_a_we_a,
            we_b   => bank_a_we_b,
            q_a    => bank_a_q_a,
            q_b    => bank_a_q_b
        );

    bank_b_ram : entity work.true_dual_port_ram_single_clock
        generic map (
            DATA_WIDTH => COUNT_WIDTH,
            ADDR_WIDTH => ADDR_WIDTH_CONST
        )
        port map (
            clk    => i_clk,
            addr_a => bank_b_addr_a,
            addr_b => bank_b_addr_b,
            data_a => bank_b_data_a,
            data_b => bank_b_data_b,
            we_a   => bank_b_we_a,
            we_b   => bank_b_we_b,
            q_a    => bank_b_q_a,
            q_b    => bank_b_q_b
        );

    ram_ctrl : process (i_clk)
        variable ram_v_switch_fire   : boolean;
        variable ram_v_next_bank     : std_logic;
        variable ram_v_hist_issue    : std_logic;
        variable ram_v_hist_bank     : std_logic;
        variable ram_v_hist_addr     : addr_t;
        variable ram_v_hist_bin_valid : std_logic;
        variable ram_v_burst_count   : unsigned(burst_remaining'range);
        variable ram_v_read_data     : count_t;
        variable ram_v_accept_update : boolean;
        variable ram_v_upd_forward   : count_t;
        variable ram_v_update_old_valid : std_logic;
        variable ram_v_read_target_bank : std_logic;
        variable ram_v_next_clear_active : std_logic;
        variable ram_v_next_burst_active : std_logic;
        variable ram_v_next_timer_count : unsigned(31 downto 0);
        variable ram_v_ready_block_switch : boolean;
        variable ram_v_next_ready : std_logic;
        variable ram_v_force_fire : boolean;
        variable ram_v_read_busy_any : boolean;
        variable ram_v_read_stale : boolean;
        variable ram_v_read_inflight : boolean;
    begin
        if rising_edge(i_clk) then
            hist_readdatavalid <= '0';
            interval_pulse     <= '0';

            if i_rst = '1' then
                upd_ready_int     <= '0';
                active_bank       <= '0';
                clear_active      <= '0';
                clear_bank        <= '0';
                clear_both        <= '0';
                clear_addr        <= (others => '0');
                bank_a_valid      <= (others => '0');
                bank_b_valid      <= (others => '0');
                timer_count       <= (others => '0');
                interval_pulse    <= '0';
                force_interval_pending <= '0';
                upd_issue_valid   <= '0';
                upd_issue_bank    <= '0';
                upd_issue_bin     <= (others => '0');
                upd_issue_count   <= (others => '0');
                upd_issue_old_valid <= '0';
                upd_read_valid    <= '0';
                upd_read_bank     <= '0';
                upd_read_bin      <= (others => '0');
                upd_read_count    <= (others => '0');
                upd_read_old_valid <= '0';
                upd_add_valid     <= '0';
                upd_add_bank      <= '0';
                upd_add_bin       <= (others => '0');
                upd_add_data      <= (others => '0');
                upd_add_count     <= (others => '0');
                upd_sum_valid     <= '0';
                upd_sum_bank      <= '0';
                upd_sum_bin       <= (others => '0');
                upd_sum_value     <= (others => '0');
                upd_write_valid   <= '0';
                upd_write_bank    <= '0';
                upd_write_bin     <= (others => '0');
                upd_write_value   <= (others => '0');
                burst_active      <= '0';
                burst_addr        <= (others => '0');
                burst_remaining   <= (others => '0');
                hist_issue_valid  <= '0';
                hist_issue_bank   <= '0';
                hist_issue_addr   <= (others => '0');
                hist_issue_bin_valid <= '0';
                hist_read_valid   <= '0';
                hist_read_bank    <= '0';
                hist_read_bin_valid <= '0';
                hist_readdata     <= (others => '0');
            elsif i_clear = '1' then
                upd_ready_int     <= '0';
                active_bank       <= '0';
                clear_active      <= '0';
                clear_bank        <= '0';
                clear_both        <= '0';
                clear_addr        <= (others => '0');
                bank_a_valid      <= (others => '0');
                bank_b_valid      <= (others => '0');
                timer_count       <= (others => '0');
                interval_pulse    <= '0';
                force_interval_pending <= '0';
                upd_issue_valid   <= '0';
                upd_issue_bank    <= '0';
                upd_issue_bin     <= (others => '0');
                upd_issue_count   <= (others => '0');
                upd_issue_old_valid <= '0';
                upd_read_valid    <= '0';
                upd_read_bank     <= '0';
                upd_read_bin      <= (others => '0');
                upd_read_count    <= (others => '0');
                upd_read_old_valid <= '0';
                upd_add_valid     <= '0';
                upd_add_bank      <= '0';
                upd_add_bin       <= (others => '0');
                upd_add_data      <= (others => '0');
                upd_add_count     <= (others => '0');
                upd_sum_valid     <= '0';
                upd_sum_bank      <= '0';
                upd_sum_bin       <= (others => '0');
                upd_sum_value     <= (others => '0');
                upd_write_valid   <= '0';
                upd_write_bank    <= '0';
                upd_write_bin     <= (others => '0');
                upd_write_value   <= (others => '0');
                burst_active      <= '0';
                burst_addr        <= (others => '0');
                burst_remaining   <= (others => '0');
                hist_issue_valid  <= '0';
                hist_issue_bank   <= '0';
                hist_issue_addr   <= (others => '0');
                hist_issue_bin_valid <= '0';
                hist_read_valid   <= '0';
                hist_read_bank    <= '0';
                hist_read_bin_valid <= '0';
                hist_readdata     <= (others => '0');
            else
                ram_v_next_bank     := active_bank;
                ram_v_switch_fire   := false;
                ram_v_hist_issue    := '0';
                ram_v_hist_bank     := active_bank;
                ram_v_hist_addr     := (others => '0');
                ram_v_hist_bin_valid := '0';
                ram_v_accept_update := false;
                ram_v_update_old_valid := '0';
                ram_v_read_target_bank := active_bank;
                ram_v_next_clear_active := clear_active;
                ram_v_next_burst_active := burst_active;
                ram_v_next_timer_count := timer_count;
                ram_v_read_busy_any := (burst_active = '1') or
                                       (hist_issue_valid = '1') or
                                       (hist_read_valid = '1') or
                                       (i_hist_read = '1');
                if i_force_interval = '1' then
                    force_interval_pending <= '1';
                end if;
                ram_v_force_fire := (force_interval_pending = '1') and
                                    (not ram_v_read_busy_any);

                -- Read (q-valid) stage with seamless bank-follow staleness check.
                -- The host read keeps a SINGLE beat outstanding (issue -> q-valid
                -- -> emit) so beat accounting stays exact across a swap. A beat
                -- is stale when, under ping-pong, an interval swap landed in the
                -- one-cycle RAM read latency: the bank this beat read is now the
                -- active (being-cleared) bank, not the frozen readout bank.
                -- active_bank here is the registered value in effect this cycle
                -- (the swap assignment for this cycle happens later in the
                -- process), so hist_read_bank = active_bank precisely flags a
                -- beat that targeted the bank now being flushed. A stale beat is
                -- NOT emitted and the burst address is NOT advanced: the same
                -- address is reissued next cycle against the new frozen bank, so
                -- every burstcount beat returns valid OKAY data with no missing
                -- or duplicated beat. A fresh beat is emitted and advances the
                -- burst.
                ram_v_read_stale := (hist_read_valid = '1') and (i_enable_pingpong = '1') and
                                    (hist_read_bank = active_bank);
                if (hist_read_valid = '1') and (not ram_v_read_stale) then
                    if hist_read_bank = '0' then
                        ram_v_read_data := unsigned(bank_a_q_a);
                    else
                        ram_v_read_data := unsigned(bank_b_q_a);
                    end if;
                    if hist_read_bin_valid = '1' then
                        hist_readdata <= std_logic_vector(resize(ram_v_read_data, 32));
                    else
                        hist_readdata <= (others => '0');
                    end if;
                    hist_readdatavalid <= '1';
                end if;

                if i_enable_pingpong = '1' then
                    if ((i_interval_clocks /= 0) and (timer_count = (i_interval_clocks - 1))) or
                       ram_v_force_fire then
                        ram_v_switch_fire := true;
                        ram_v_next_bank   := not active_bank;
                        active_bank       <= not active_bank;
                        timer_count       <= (others => '0');
                        ram_v_next_timer_count := (others => '0');
                        interval_pulse    <= '1';
                        clear_active      <= '0';
                        ram_v_next_clear_active := '0';
                        clear_bank        <= not active_bank;
                        clear_both        <= '0';
                        clear_addr        <= (others => '0');
                        if active_bank = '1' then
                            bank_a_valid <= (others => '0');
                        else
                            bank_b_valid <= (others => '0');
                        end if;
                        if ram_v_force_fire then
                            force_interval_pending <= '0';
                        end if;
                    elsif i_interval_clocks /= 0 then
                        timer_count <= timer_count + 1;
                        ram_v_next_timer_count := timer_count + 1;
                    else
                        timer_count <= (others => '0');
                        ram_v_next_timer_count := (others => '0');
                    end if;
                else
                    timer_count <= (others => '0');
                    ram_v_next_timer_count := (others => '0');
                end if;

                clear_active <= '0';
                clear_both   <= '0';
                clear_addr   <= (others => '0');
                ram_v_next_clear_active := '0';

                -- Current frozen readout bank. Under ping-pong this is the bank
                -- NOT being written and tracks active_bank cycle-by-cycle, so a
                -- burst in progress automatically follows to the just-frozen
                -- bank (interval N+1) when active_bank flips mid-burst. In
                -- non-ping-pong mode the single bank is read directly.
                if i_enable_pingpong = '1' then
                    ram_v_read_target_bank := not active_bank;
                else
                    ram_v_read_target_bank := active_bank;
                end if;

                -- A host read beat is outstanding while it occupies the issue or
                -- the q-valid stage. The single-outstanding rule keeps beat
                -- accounting exact across a swap.
                ram_v_read_inflight := (hist_issue_valid = '1') or (hist_read_valid = '1');

                -- Command acceptance never stalls on a bank swap or a transient
                -- update touch of the readout bank: accept whenever no burst is
                -- in progress. The seamless bank-follow issue logic below sources
                -- the live frozen bank, so there is no need to park in a pending
                -- slot. Only burst_active (per-beat pacing) gates new commands.
                if (i_hist_read = '1') and (hist_waitrequest = '0') and (burst_active = '0') then
                    ram_v_burst_count := resize(i_hist_burstcount, burst_remaining'length);
                    if ram_v_burst_count = 0 then
                        ram_v_burst_count := to_unsigned(1, ram_v_burst_count'length);
                    end if;
                    burst_active    <= '1';
                    burst_addr      <= i_hist_address;
                    burst_remaining <= ram_v_burst_count;
                    ram_v_next_burst_active := '1';
                end if;

                -- Advance / retire on the q-valid stage:
                --   fresh beat -> advance the burst address (or finish);
                --   stale beat -> hold the address for a reissue against the new
                --   frozen bank (no advance, no emit).
                if (hist_read_valid = '1') and (not ram_v_read_stale) then
                    if burst_remaining = 1 then
                        burst_active    <= '0';
                        burst_remaining <= (others => '0');
                        ram_v_next_burst_active := '0';
                    else
                        burst_addr      <= burst_addr + 1;
                        burst_remaining <= burst_remaining - 1;
                        ram_v_next_burst_active := '1';
                    end if;
                end if;

                -- Issue stage. Issue the current burst address whenever the burst
                -- is active and no beat is already outstanding (single beat in
                -- flight). The address is sourced from ram_v_read_target_bank,
                -- the live frozen bank, so a swap mid-burst is followed
                -- seamlessly; a stale beat simply reissues the same address next
                -- cycle against the new frozen bank.
                if (burst_active = '1') and (not ram_v_read_inflight) then
                    ram_v_hist_issue := '1';
                    ram_v_hist_bank  := ram_v_read_target_bank;
                    ram_v_hist_addr  := burst_addr;
                    if ram_v_read_target_bank = '0' then
                        ram_v_hist_bin_valid := bank_a_valid(to_integer(burst_addr));
                    else
                        ram_v_hist_bin_valid := bank_b_valid(to_integer(burst_addr));
                    end if;
                end if;

                if (upd_ready_int = '1') and (i_upd_valid = '1') then
                    if not ram_v_switch_fire then
                        ram_v_accept_update := true;
                    elsif (i_enable_pingpong = '1') and (i_upd_bank = active_bank) then
                        ram_v_accept_update := true;
                    end if;
                end if;

                hist_issue_valid <= ram_v_hist_issue;
                hist_issue_bank  <= ram_v_hist_bank;
                hist_issue_addr  <= ram_v_hist_addr;
                hist_issue_bin_valid <= ram_v_hist_bin_valid;
                hist_read_valid  <= hist_issue_valid;
                hist_read_bank   <= hist_issue_bank;
                hist_read_bin_valid <= hist_issue_bin_valid;
                upd_read_valid   <= upd_issue_valid;
                upd_read_bank    <= upd_issue_bank;
                upd_read_bin     <= upd_issue_bin;
                upd_read_count   <= upd_issue_count;
                upd_read_old_valid <= upd_issue_old_valid;
                if upd_read_valid = '1' then
                    if upd_read_old_valid = '1' then
                        ram_v_upd_forward := upd_read_data;
                    else
                        ram_v_upd_forward := (others => '0');
                    end if;

                    if (upd_add_valid = '1') and (upd_add_bank = upd_read_bank) and (upd_add_bin = upd_read_bin) then
                        ram_v_upd_forward := sat_add(upd_add_data, upd_add_count);
                    elsif (upd_sum_valid = '1') and (upd_sum_bank = upd_read_bank) and (upd_sum_bin = upd_read_bin) then
                        ram_v_upd_forward := upd_sum_value;
                    elsif (upd_write_valid = '1') and (upd_write_bank = upd_read_bank) and (upd_write_bin = upd_read_bin) then
                        ram_v_upd_forward := upd_write_value;
                    end if;

                    upd_add_valid  <= '1';
                    upd_add_bank   <= upd_read_bank;
                    upd_add_bin    <= upd_read_bin;
                    upd_add_data   <= ram_v_upd_forward;
                    upd_add_count  <= upd_read_count;
                else
                    upd_add_valid  <= '0';
                    upd_add_bank   <= '0';
                    upd_add_bin    <= (others => '0');
                    upd_add_data   <= (others => '0');
                    upd_add_count  <= (others => '0');
                end if;
                if upd_add_valid = '1' then
                    upd_sum_valid  <= '1';
                    upd_sum_bank   <= upd_add_bank;
                    upd_sum_bin    <= upd_add_bin;
                    upd_sum_value  <= sat_add(upd_add_data, upd_add_count);
                else
                    upd_sum_valid  <= '0';
                    upd_sum_bank   <= '0';
                    upd_sum_bin    <= (others => '0');
                    upd_sum_value  <= (others => '0');
                end if;
                upd_write_valid  <= upd_sum_valid;
                upd_write_bank   <= upd_sum_bank;
                upd_write_bin    <= upd_sum_bin;
                upd_write_value  <= upd_sum_value;

                if upd_write_valid = '1' then
                    if upd_write_bank = '0' then
                        bank_a_valid(to_integer(upd_write_bin)) <= '1';
                    else
                        bank_b_valid(to_integer(upd_write_bin)) <= '1';
                    end if;
                end if;

                if ram_v_accept_update then
                    if i_upd_bank = '0' then
                        ram_v_update_old_valid := bank_a_valid(to_integer(i_upd_bin));
                    else
                        ram_v_update_old_valid := bank_b_valid(to_integer(i_upd_bin));
                    end if;
                    upd_issue_valid <= '1';
                    upd_issue_bank  <= i_upd_bank;
                    upd_issue_bin   <= i_upd_bin;
                    upd_issue_count <= resize(i_upd_count, COUNT_WIDTH);
                    upd_issue_old_valid <= ram_v_update_old_valid;
                else
                    upd_issue_valid <= '0';
                    upd_issue_old_valid <= '0';
                end if;

                ram_v_ready_block_switch := false;
                if (i_enable_pingpong = '1') and (i_interval_clocks /= 0) then
                    ram_v_ready_block_switch := ram_v_next_timer_count = (i_interval_clocks - 1);
                end if;

                ram_v_next_ready := '0';
                if (ram_v_next_clear_active = '0') and (not ram_v_ready_block_switch) and
                   not ((i_enable_pingpong = '0') and (ram_v_next_burst_active = '1')) then
                    ram_v_next_ready := '1';
                end if;
                upd_ready_int <= ram_v_next_ready;
            end if;
        end if;
    end process ram_ctrl;

end architecture rtl;
