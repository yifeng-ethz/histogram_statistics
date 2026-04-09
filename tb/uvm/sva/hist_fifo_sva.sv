module hist_fifo_sva #(
  int N_PORTS = 8
) (
  input logic clk,
  input logic rst,
  hist_probe_if probe_if
);
  genvar port_idx;

  generate
    for (port_idx = 0; port_idx < N_PORTS; port_idx++) begin : g_fifo_chk
      property p_drop_implies_full;
        @(posedge clk) disable iff (rst)
          probe_if.drop_pulse[port_idx] |-> probe_if.fifo_full[port_idx] && !probe_if.fifo_write[port_idx];
      endproperty

      property p_write_not_full;
        @(posedge clk) disable iff (rst)
          probe_if.fifo_write[port_idx] |-> !probe_if.fifo_full[port_idx];
      endproperty

      property p_read_not_empty;
        @(posedge clk) disable iff (rst)
          probe_if.fifo_read[port_idx] |-> !probe_if.fifo_empty[port_idx];
      endproperty

      property p_no_empty_and_full;
        @(posedge clk) disable iff (rst)
          !(probe_if.fifo_empty[port_idx] && probe_if.fifo_full[port_idx]);
      endproperty

      assert property (p_drop_implies_full)
        else $error("hist_fifo_sva: drop_pulse without fifo_full on port %0d", port_idx);

      assert property (p_write_not_full)
        else $error("hist_fifo_sva: fifo_write while fifo_full on port %0d", port_idx);

      assert property (p_read_not_empty)
        else $error("hist_fifo_sva: fifo_read while fifo_empty on port %0d", port_idx);

      assert property (p_no_empty_and_full)
        else $error("hist_fifo_sva: fifo_empty and fifo_full both set on port %0d", port_idx);
    end
  endgenerate
endmodule
