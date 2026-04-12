module hist_pipeline_sva (
  input logic clk,
  input logic rst,
  hist_probe_if probe_if
);
  localparam int unsigned POST_CLEAR_DRAIN_CYCLES = 12;

  int unsigned post_clear_drain;

  always_ff @(posedge clk) begin
    if (rst) begin
      post_clear_drain <= 0;
    end else if (probe_if.measure_clear_pulse) begin
      // The scoreboard suppresses stale divider/queue observations for the
      // 3-stage pre-divider pipe + 8-stage divider + 1 registered queue-hit hop.
      post_clear_drain <= POST_CLEAR_DRAIN_CYCLES;
    end else if (post_clear_drain != 0) begin
      post_clear_drain <= post_clear_drain - 1;
    end
  end

  property p_apply_blocks_ingress;
    @(posedge clk) disable iff (rst)
      probe_if.cfg_apply_pending |-> (probe_if.ingress_accept == '0);
  endproperty

  property p_divider_bin_to_queue_hit;
    @(posedge clk) disable iff (rst || probe_if.measure_clear_pulse || (post_clear_drain != 0))
      probe_if.divider_valid && !probe_if.divider_underflow && !probe_if.divider_overflow
      |=> probe_if.queue_hit_valid && (probe_if.queue_hit_bin == $past(probe_if.divider_bin_index));
  endproperty

  property p_divider_exception_no_queue_hit;
    @(posedge clk) disable iff (rst || probe_if.measure_clear_pulse || (post_clear_drain != 0))
      probe_if.divider_valid && (probe_if.divider_underflow || probe_if.divider_overflow)
      |=> !probe_if.queue_hit_valid;
  endproperty

  property p_interval_toggles_bank;
    @(posedge clk) disable iff (rst)
      probe_if.interval_pulse |-> (probe_if.active_bank != $past(probe_if.active_bank));
  endproperty

  property p_measure_clear_starts_flush;
    @(posedge clk) disable iff (rst)
      probe_if.measure_clear_pulse |=> probe_if.flushing;
  endproperty

  assert property (p_apply_blocks_ingress)
    else $error("hist_pipeline_sva: ingress_accept seen while cfg_apply_pending");

  assert property (p_divider_bin_to_queue_hit)
    else $error("hist_pipeline_sva: valid divider bin did not produce queue_hit");

  assert property (p_divider_exception_no_queue_hit)
    else $error("hist_pipeline_sva: divider underflow/overflow still produced queue_hit");

  assert property (p_interval_toggles_bank)
    else $error("hist_pipeline_sva: interval_pulse without active_bank toggle");

  assert property (p_measure_clear_starts_flush)
    else $error("hist_pipeline_sva: measure_clear_pulse did not lead to flushing");
endmodule
