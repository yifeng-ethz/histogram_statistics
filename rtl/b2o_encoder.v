// File name: b2o_encoder.vhd 
// Author: Yifeng Wang (yifenwan@phys.ethz.ch)
// =======================================
// Revision: 1.0 (file created)
//		Date: Aug 6, 2024
// =========
// Description:	[Binary to Onehot code encoder] 
//				Encode arbitary binary code into onehot code.
//				The input and output ports width must be set correctly by the user.

module b2o_encoder 
#(parameter INPUT_W=6, OUTPUT_W=64)
(
	input  [INPUT_W-1:0] binary_code,
	output wire [OUTPUT_W-1:0] onehot_code
);

	
	assign	onehot_code = 1 << binary_code;
	
endmodule 