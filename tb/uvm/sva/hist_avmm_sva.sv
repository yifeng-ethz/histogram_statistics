module hist_avmm_sva (
  input logic clk,
  input logic rst,
  hist_csr_if csr_if,
  hist_bin_if bin_if
);
  property p_csr_waitrequest_low;
    @(posedge clk) disable iff (rst)
      csr_if.waitrequest == 1'b0;
  endproperty

  property p_bin_waitrequest_low;
    @(posedge clk) disable iff (rst)
      bin_if.waitrequest == 1'b0;
  endproperty

  property p_csr_read_known;
    @(posedge clk) disable iff (rst)
      csr_if.read |=> !$isunknown(csr_if.readdata);
  endproperty

  property p_bin_write_response;
    @(posedge clk) disable iff (rst)
      bin_if.write |=> bin_if.writeresponsevalid;
  endproperty

  assert property (p_csr_waitrequest_low)
    else $error("hist_avmm_sva: csr waitrequest unexpectedly asserted");

  assert property (p_bin_waitrequest_low)
    else $error("hist_avmm_sva: hist_bin waitrequest unexpectedly asserted");

  assert property (p_csr_read_known)
    else $error("hist_avmm_sva: csr readdata unknown after read");

  assert property (p_bin_write_response)
    else $error("hist_avmm_sva: hist_bin write missing writeresponsevalid");

  int unsigned burst_remaining_q;
  bit          burst_active_q;
  int unsigned read_timeout_q;

  always_ff @(posedge clk) begin
    if (rst) begin
      burst_remaining_q <= 0;
      burst_active_q    <= 1'b0;
      read_timeout_q    <= 0;
    end else begin
      if (bin_if.read) begin
        if (burst_active_q) begin
          $error("hist_avmm_sva: overlapping hist_bin bursts detected");
        end
        burst_active_q    <= 1'b1;
        burst_remaining_q <= (bin_if.burstcount == 0) ? 1 : bin_if.burstcount;
        read_timeout_q    <= 0;
      end else if (burst_active_q) begin
        read_timeout_q <= read_timeout_q + 1;
        if (read_timeout_q > 4096) begin
          $error("hist_avmm_sva: hist_bin burst timed out waiting for readdatavalid");
          burst_active_q    <= 1'b0;
          burst_remaining_q <= 0;
          read_timeout_q    <= 0;
        end
      end

      if (bin_if.readdatavalid) begin
        if (!burst_active_q) begin
          $error("hist_avmm_sva: stray readdatavalid without active burst");
        end else if (burst_remaining_q == 0) begin
          $error("hist_avmm_sva: readdatavalid exceeded requested burstcount");
        end else if (burst_remaining_q == 1) begin
          burst_active_q    <= 1'b0;
          burst_remaining_q <= 0;
          read_timeout_q    <= 0;
        end else begin
          burst_remaining_q <= burst_remaining_q - 1;
          read_timeout_q    <= 0;
        end
      end
    end
  end
endmodule
