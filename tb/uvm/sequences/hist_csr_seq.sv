class hist_csr_write_seq extends uvm_sequence #(hist_csr_txn);
  `uvm_object_utils(hist_csr_write_seq)

  rand bit [3:0]  address;
  rand bit [31:0] writedata;

  function new(string name = "hist_csr_write_seq");
    super.new(name);
  endfunction

  task body();
    hist_csr_txn req;
    req = hist_csr_txn::type_id::create("req");
    start_item(req);
    req.write     = 1'b1;
    req.address   = address;
    req.writedata = writedata;
    finish_item(req);
  endtask
endclass

class hist_csr_read_seq extends uvm_sequence #(hist_csr_txn);
  `uvm_object_utils(hist_csr_read_seq)

  rand bit [3:0] address;
  bit [31:0]     readdata;

  function new(string name = "hist_csr_read_seq");
    super.new(name);
  endfunction

  task body();
    hist_csr_txn req;
    hist_csr_txn rsp;

    req = hist_csr_txn::type_id::create("req");
    start_item(req);
    req.write     = 1'b0;
    req.address   = address;
    req.writedata = '0;
    finish_item(req);

    get_response(rsp);
    readdata = rsp.readdata;
  endtask
endclass
