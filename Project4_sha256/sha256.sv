//SHA Encryption
module sha256(input logic clk, reset_n, start, 
					input logic [31:0] message_addr, size, output_addr,
					output logic done, mem_clk, mem_we, 
					output logic [15:0] mem_addr, 
					output logic [31:0] mem_write_data,
					input logic [31:0] mem_read_data);

logic [31:0] num_words;
assign num_words = size/4;
logic [3:0] counter;
logic [31:0] read_data;
enum logic [2:0] {IDLE=3'b000, STEP1=3'b001, STEP2=3'b010, STEP3=3'b011, STEP4=3'b100, DONE=3'b101} state;

logic   [255:0] sha256_hash; // results here



int             message_size = 120; // in bytes // change this number to test your design
int             pad_length;

int             t, m;
int             outloop;
int             cycles;

logic   [255:0] sha256_digest;

logic   [31:0] h0;
logic   [31:0] h1;
logic   [31:0] h2;
logic   [31:0] h3;
logic   [31:0] h4;
logic   [31:0] h5;
logic   [31:0] h6;
logic   [31:0] h7;

logic   [31:0] a, b, c, d, e, f, g, h;
logic   [31:0] s1, s0;

logic   [31:0] w[0:63];

logic [15:0] rc, wc; // read and write counters

// right rotation
function logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [7:0] r);
begin
    rightrotate = (x >> r) | (x << (32-r));
end
endfunction

//wtnew function
function logic [31:0] wtnew; // function with no inputs
 logic [31:0] s0, s1;
 s0 = rightrotate(w[t-15],7)^rightrotate(w[t-15],18)^(w[t-15]>>3);
 s1 = rightrotate(w[t-2],17)^rightrotate(w[t-2],19)^(w[t-2]>>10);
 wtnew = w[t-16] + s0 + w[t-7] + s1;
endfunction


  assign mem_clk = clk;

  always_ff @(posedge clk, negedge reset_n)
  begin
    if (!reset_n) begin
      state <= IDLE;
    end else
      case (state)
      IDLE: // start
          if (start) begin 
            mem_we <= 0;
            mem_addr <= message_addr;
            rc <= 1;
            state <= STEP2;
          end
      STEP1: begin // READ 0
          mem_we <= 0;
          mem_addr <= message_addr + rc;
          state <= STEP2;
        end
      STEP2: begin // READ 1
          mem_we <= 0;
          //mem_addr <= message_addr + rc;
          rc <= rc + 1;
          state <= STEP3;
        end
      
		// READ 3, data on mem_read_data is available to use
		//mem_read_data just contains a 32 bit message
		STEP3: begin 
          mem_we <= 1;
			 $display("STEP 3");
			 $display("%d\n", mem_read_data);
			 $display(rc);
			 
			
			  
					if (t < 16) begin
						 w[t] <= mem_read_data;
					end else begin
						 //s0 <= rightrotate(w[t-15], 7) ^ rightrotate(w[t-15], 18) ^ (w[t-15] >> 3);
						 //s1 <= rightrotate(w[t-2], 17) ^ rightrotate(w[t-2], 19) ^ (w[t-2] >> 10);
						 //w[t] <= w[t-16] + s0 + w[t-7] + s1;
						 w[t] = wtnew;
						 $display("w[%d] is %d", t,w[t]);
					end
					
					t <= t + 1;
					

          if (t < 64) begin
          //if ((rc) < (size >> 2)) begin
              state <= STEP1;
          end else begin
              state <= DONE;
          end
        end
		  
		
			
      
      DONE: begin
		    $display("Printing W array here, should be 64 32-bit words WORKED");
			 $display("%p", w);
          done <= 1;
          state <= IDLE;
        end
      endcase
  end


//parameter int sha256_k[0:63] = '{
//   32'h428a2f98, 32'h71374491, 32'hb5c0fbcf, 32'he9b5dba5, 32'h3956c25b, 32'h59f111f1, 32'h923f82a4, 32'hab1c5ed5,
//   32'hd807aa98, 32'h12835b01, 32'h243185be, 32'h550c7dc3, 32'h72be5d74, 32'h80deb1fe, 32'h9bdc06a7, 32'hc19bf174,
//   32'he49b69c1, 32'hefbe4786, 32'h0fc19dc6, 32'h240ca1cc, 32'h2de92c6f, 32'h4a7484aa, 32'h5cb0a9dc, 32'h76f988da,
//   32'h983e5152, 32'ha831c66d, 32'hb00327c8, 32'hbf597fc7, 32'hc6e00bf3, 32'hd5a79147, 32'h06ca6351, 32'h14292967,
//   32'h27b70a85, 32'h2e1b2138, 32'h4d2c6dfc, 32'h53380d13, 32'h650a7354, 32'h766a0abb, 32'h81c2c92e, 32'h92722c85,
//   32'ha2bfe8a1, 32'ha81a664b, 32'hc24b8b70, 32'hc76c51a3, 32'hd192e819, 32'hd6990624, 32'hf40e3585, 32'h106aa070,
//   32'h19a4c116, 32'h1e376c08, 32'h2748774c, 32'h34b0bcb5, 32'h391c0cb3, 32'h4ed8aa4a, 32'h5b9cca4f, 32'h682e6ff3,
//   32'h748f82ee, 32'h78a5636f, 32'h84c87814, 32'h8cc70208, 32'h90befffa, 32'ha4506ceb, 32'hbef9a3f7, 32'hc67178f2
//};
//
//	h0 = 32'h6a09e667;
//	h1 = 32'hbb67ae85;
//	h2 = 32'h3c6ef372;
//	h3 = 32'ha54ff53a;
//	h4 = 32'h510e527f;
//	h5 = 32'h9b05688c;
//	h6 = 32'h1f83d9ab;
//	h7 = 32'h5be0cd19;
//	
//	//Initial message digest
//	sha256_digest = {h0, h1, h2, h3, h4, h5, h6, h7};
//	
//	// INITIAL HASH AT ROUND K
//
//   a = h0;
//   b = h1;
//   c = h2;
//   d = h3;
//   e = h4;
//   f = h5;
//   g = h6;
//   h = h7;
//	
//	//There are 64 processing rounds per block, m is number of blocks
//	//w[0] --> w[15] will just be the 16 32-bit words respectively
//	//The rest of the w[t]'s will be calculated with the below formula
//	
//	//Asked question on piazza about padding. You do not need to read in all the words for
//	//a block and then do processing.
//	
//	//"you can store mem_read_data/pad for t=0 to t=15, all the while we do {a,b,c,d,e,f,g} <= ... 
//	//There are multiple ways to fill in ... but you do not need to fill in w[0] to w[15] all at once to fill it in
//   //One way to pad on the fly is catch the last word with an if(last_word) and use something like the case statement in lecture 7 last slide."
//	
//	for (m = 0; m < outloop; m = m + 1) begin
//        // W ARRAY EXPANSION
//
//        for (t = 0; t < 64; t = t + 1) begin
//            if (t < 16) begin
//                w[t] = //32-bit word;
//            end else begin
//                s0 = rightrotate(w[t-15], 7) ^ rightrotate(w[t-15], 18) ^ (w[t-15] >> 3);
//                s1 = rightrotate(w[t-2], 17) ^ rightrotate(w[t-2], 19) ^ (w[t-2] >> 10);
//                w[t] = w[t-16] + s0 + w[t-7] + s1;
//            end
//        end
//
//        
//
//// HASH ROUNDS
////Actually need to implement the "sha256_op" function
//function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
//                                 input logic [7:0] t);
//
//logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
//begin
//    S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
//    ch = (e & f) ^ ((~e) & g);
//    t1 = h + S1 + ch + sha256_k[t] + w;
//    S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
//    maj = (a & b) ^ (a & c) ^ (b & c);
//    t2 = S0 + maj;
//
//    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
//end
//endfunction
//
//
//        for (t = 0; t < 64; t = t + 1) begin
//            {a, b, c, d, e, f, g, h} = sha256_op(a, b, c, d, e, f, g, h, w[t], t);
//				
//				// FINAL HASH
//
//				h0 = h0 + a;
//				h1 = h1 + b;
//				h2 = h2 + c;
//				h3 = h3 + d;
//				h4 = h4 + e;
//				h5 = h5 + f;
//				h6 = h6 + g;
//				h7 = h7 + h;
//        end
//
//	end
//	 
//	sha256_digest = {h0, h1, h2, h3, h4, h5, h6, h7};
		



endmodule 