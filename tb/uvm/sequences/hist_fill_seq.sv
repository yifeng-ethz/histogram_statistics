class hist_fill_seq extends uvm_sequence #(hist_fill_txn);
  `uvm_object_utils(hist_fill_seq)

  rand int unsigned                port_index;
  rand int unsigned                inter_gap_cycles;
  rand logic [HS_AVST_DATA_W-1:0]  raw_words[$];
  rand bit [HS_AVST_CH_W-1:0]      channels[$];
  rand bit                         sops[$];
  rand bit                         eops[$];
  rand int signed                  key_values[$];
  rand int unsigned                filter_values[$];
  bit                              use_raw_words;
  int unsigned                     update_lo;
  int unsigned                     update_hi;
  int unsigned                     filter_lo;
  int unsigned                     filter_hi;

  function new(string name = "hist_fill_seq");
    super.new(name);
    port_index       = 0;
    inter_gap_cycles = 0;
    use_raw_words    = 1'b0;
    update_lo        = HS_DEF_UPDATE_LO;
    update_hi        = HS_DEF_UPDATE_HI;
    filter_lo        = HS_DEF_FILTER_LO;
    filter_hi        = HS_DEF_FILTER_HI;
  endfunction

  task body();
    int          num_items_v;
    hist_fill_txn req;

    if (use_raw_words) begin
      num_items_v = raw_words.size();
    end else begin
      num_items_v = key_values.size();
    end

    if (num_items_v == 0) begin
      `uvm_warning(get_type_name(), "hist_fill_seq called with no items")
      return;
    end

    for (int idx = 0; idx < num_items_v; idx++) begin
      req = hist_fill_txn::type_id::create($sformatf("req_%0d", idx));
      start_item(req);
      req.port_index = port_index[2:0];
      req.data = use_raw_words ? raw_words[idx] : hist_build_fill_word(
        key_values[idx],
        update_lo,
        update_hi,
        (idx < filter_values.size()) ? filter_values[idx] : 0,
        filter_lo,
        filter_hi
      );
      req.channel = (idx < channels.size()) ? channels[idx] : '0;
      req.sop     = (idx < sops.size()) ? sops[idx] : 1'b0;
      req.eop     = (idx < eops.size()) ? eops[idx] : 1'b0;
      req.idle_cycles_after = inter_gap_cycles;
      finish_item(req);
    end
  endtask
endclass
