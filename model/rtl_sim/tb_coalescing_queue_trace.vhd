library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use std.env.all;

library work;
use work.histogram_statistics_v2_pkg.all;

entity tb_coalescing_queue_trace is
    generic (
        N_BINS_G       : natural := 256;
        QUEUE_DEPTH_G  : natural := 160;
        KICK_WIDTH_G   : natural := 8;
        STIM_FILE_G    : string := "queue_trace_stimulus.csv";
        OUT_FILE_G     : string := "queue_trace_observed_rtl.csv"
    );
end entity tb_coalescing_queue_trace;

architecture sim of tb_coalescing_queue_trace is
    constant BIN_W_C : natural := clog2(N_BINS_G);
    constant OCC_W_C : natural := clog2(QUEUE_DEPTH_G + 1);

    signal i_clk            : std_logic := '0';
    signal i_rst            : std_logic := '1';
    signal i_clear          : std_logic := '0';
    signal i_hit_valid      : std_logic := '0';
    signal i_hit_bin        : unsigned(BIN_W_C - 1 downto 0) := (others => '0');
    signal i_drain_ready    : std_logic := '1';
    signal o_drain_valid    : std_logic;
    signal o_drain_bin      : unsigned(BIN_W_C - 1 downto 0);
    signal o_drain_count    : unsigned(KICK_WIDTH_G - 1 downto 0);
    signal o_occupancy      : unsigned(OCC_W_C - 1 downto 0);
    signal o_occupancy_max  : unsigned(OCC_W_C - 1 downto 0);
    signal o_overflow_count : unsigned(15 downto 0);
begin
    i_clk <= not i_clk after 5 ns;

    dut : entity work.coalescing_queue
        generic map (
            N_BINS         => N_BINS_G,
            QUEUE_DEPTH    => QUEUE_DEPTH_G,
            KICK_WIDTH     => KICK_WIDTH_G,
            OVERFLOW_WIDTH => 16
        )
        port map (
            i_clk            => i_clk,
            i_rst            => i_rst,
            i_clear          => i_clear,
            i_hit_valid      => i_hit_valid,
            i_hit_bin        => i_hit_bin,
            i_drain_ready    => i_drain_ready,
            o_drain_valid    => o_drain_valid,
            o_drain_bin      => o_drain_bin,
            o_drain_count    => o_drain_count,
            o_occupancy      => o_occupancy,
            o_occupancy_max  => o_occupancy_max,
            o_overflow_count => o_overflow_count
        );

    driver : process
        file stim_f : text;
        file out_f  : text;
        variable in_l          : line;
        variable out_l         : line;
        variable comma_v       : character;
        variable cycle_i       : integer;
        variable hit_valid_i   : integer;
        variable hit_bin_i     : integer;
        variable drain_ready_i : integer;
    begin
        file_open(stim_f, STIM_FILE_G, read_mode);
        file_open(out_f, OUT_FILE_G, write_mode);

        write(out_l, string'("cycle,hit_valid,hit_bin,drain_ready,drain_valid,drain_bin,drain_count,occupancy,occupancy_max,overflow_count"));
        writeline(out_f, out_l);

        for i in 0 to 4 loop
            wait until rising_edge(i_clk);
        end loop;
        i_rst <= '0';

        -- The queue clears its per-bin `queued` and `kick_ram` state for N_BINS
        -- cycles after reset. Start trace stimulus only after that walk.
        for i in 0 to N_BINS_G + 4 loop
            wait until rising_edge(i_clk);
        end loop;

        readline(stim_f, in_l); -- CSV header
        while not endfile(stim_f) loop
            readline(stim_f, in_l);
            read(in_l, cycle_i);
            read(in_l, comma_v);
            read(in_l, hit_valid_i);
            read(in_l, comma_v);
            read(in_l, hit_bin_i);
            read(in_l, comma_v);
            read(in_l, drain_ready_i);

            if hit_valid_i = 0 then
                i_hit_valid <= '0';
            else
                i_hit_valid <= '1';
            end if;
            i_hit_bin <= to_unsigned(hit_bin_i, i_hit_bin'length);
            if drain_ready_i = 0 then
                i_drain_ready <= '0';
            else
                i_drain_ready <= '1';
            end if;

            wait until rising_edge(i_clk);
            wait for 1 ns;

            write(out_l, cycle_i);
            write(out_l, string'(","));
            write(out_l, hit_valid_i);
            write(out_l, string'(","));
            write(out_l, hit_bin_i);
            write(out_l, string'(","));
            write(out_l, drain_ready_i);
            write(out_l, string'(","));
            if o_drain_valid = '1' then
                write(out_l, 1);
            else
                write(out_l, 0);
            end if;
            write(out_l, string'(","));
            write(out_l, to_integer(o_drain_bin));
            write(out_l, string'(","));
            write(out_l, to_integer(o_drain_count));
            write(out_l, string'(","));
            write(out_l, to_integer(o_occupancy));
            write(out_l, string'(","));
            write(out_l, to_integer(o_occupancy_max));
            write(out_l, string'(","));
            write(out_l, to_integer(o_overflow_count));
            writeline(out_f, out_l);
        end loop;

        i_hit_valid <= '0';
        i_drain_ready <= '1';
        for i in 0 to 8 loop
            wait until rising_edge(i_clk);
        end loop;

        file_close(stim_f);
        file_close(out_f);
        finish(0);
    end process driver;
end architecture sim;
