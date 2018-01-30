module calc_num_blocks(input logic [31:0] size,
                       output logic [15:0] num_blocks);
 
 //A block for our purposes will be 64 bytes
 //In addition to storing the "bytes" specified by "size", we also need to accomodate the following:
 //***1-bit delimter***
 //***64-bit(8 byte) representation of "size"***
 //This makes 8 and 1/8 bytes of additions, for now it is acceptable to round to 9 bytes for convenience
 //ex, a 126 byte input would have to be stored in 3 blocks, not 2, because of these necessary additions
  
  function logic [15:0] determine_num_blocks(input logic [31:0] size);
     logic [15:0] base_blocks;
	  logic [15:0] remainder;
	  logic [15:0] additional_block;
	  logic [15:0] block_for_additional_bytes;
	  
	  //This gives us how many whole blocks
	  base_blocks = size/64;
	
	  //This gives us how much of a block we have left over. Add 9 bytes for delimeter and representation of size
	  remainder = (size%64) + 9;
	  
	  //Now convert this to see if we have an additional block
	  additional_block = remainder/64;
	  
	  //If we don't have exactly one more block, we need to add another block to allocate space for the additional bytes
	  block_for_additional_bytes = 0;
	  if (remainder > 64) begin
	     block_for_additional_bytes = 1;
	  end
	  
	  
	  determine_num_blocks = base_blocks + additional_block + block_for_additional_bytes;
	  //$display("My block: %f", block_calc);
	  
  endfunction
  
  assign num_blocks = determine_num_blocks(size);
endmodule