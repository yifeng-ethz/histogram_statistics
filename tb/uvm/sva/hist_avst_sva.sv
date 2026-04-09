module hist_avst_sva #(
  int DATA_W = 39,
  int CH_W   = 4
) (
  input logic clk,
  input logic rst,
  hist_fill_if stream_if
);
  property p_valid_holds_until_ready;
    @(posedge clk) disable iff (rst)
      stream_if.valid && !stream_if.ready
      |=> stream_if.valid && $stable({stream_if.data, stream_if.channel, stream_if.sop, stream_if.eop});
  endproperty

  assert property (p_valid_holds_until_ready)
    else $error("hist_avst_sva: payload changed while backpressured");
endmodule
