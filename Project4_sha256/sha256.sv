 //SHA Encryption
module sha256(input logic clk, reset_n, start, 
                    input logic [31:0] message_addr, size, output_addr,
                    output logic done, mem_clk, mem_we, 
                    output logic [15:0] mem_addr, 
                    output logic [31:0] mem_write_data,
                    input logic [31:0] mem_read_data);
//begin variables
enum logic [3:0] {IDLE=4'b0000, STEP1=4'b0001, STEP2=4'b0010, STEP3=4'b0011, STEP4=4'b0100, STEP5=4'b0101, STEP6=4'b0110, STEP7=4'b0111, DONE=4'b1000} state;

logic   [31:0] h0;
logic   [31:0] h1;
logic   [31:0] h2;
logic   [31:0] h3;
logic   [31:0] h4;
logic   [31:0] h5;
logic   [31:0] h6;
logic   [31:0] h7;

logic   [31:0] a, b, c, d, e, f, g, h;

logic [255:0] sha256_digest;

logic    [31:0] w[64];
logic    [31:0] num_blocks;
logic    [31:0] total_length;
logic    [31:0] current_block;
logic    [31:0] last;


logic    [7:0] t;
logic    [31:0] temp[64];
logic    [15:0] rc, wc; // read and write counters

int pad_length;
int curr,j, v, deal_first_word;
int num_words;

assign pad_length = 512 - (((size << 3) + 65) % 512); //size of a block - ((size * 8) +65) % 512
assign num_words = size / 4;
logic    [5000:0] total;
//logic    [5000:0] full_message;
assign mem_clk = clk;
            
parameter int sha256_k[0:63] = '{
   32'h428a2f98, 32'h71374491, 32'hb5c0fbcf, 32'he9b5dba5, 32'h3956c25b, 32'h59f111f1, 32'h923f82a4, 32'hab1c5ed5,
   32'hd807aa98, 32'h12835b01, 32'h243185be, 32'h550c7dc3, 32'h72be5d74, 32'h80deb1fe, 32'h9bdc06a7, 32'hc19bf174,
   32'he49b69c1, 32'hefbe4786, 32'h0fc19dc6, 32'h240ca1cc, 32'h2de92c6f, 32'h4a7484aa, 32'h5cb0a9dc, 32'h76f988da,
   32'h983e5152, 32'ha831c66d, 32'hb00327c8, 32'hbf597fc7, 32'hc6e00bf3, 32'hd5a79147, 32'h06ca6351, 32'h14292967,
   32'h27b70a85, 32'h2e1b2138, 32'h4d2c6dfc, 32'h53380d13, 32'h650a7354, 32'h766a0abb, 32'h81c2c92e, 32'h92722c85,
   32'ha2bfe8a1, 32'ha81a664b, 32'hc24b8b70, 32'hc76c51a3, 32'hd192e819, 32'hd6990624, 32'hf40e3585, 32'h106aa070,
   32'h19a4c116, 32'h1e376c08, 32'h2748774c, 32'h34b0bcb5, 32'h391c0cb3, 32'h4ed8aa4a, 32'h5b9cca4f, 32'h682e6ff3,
   32'h748f82ee, 32'h78a5636f, 32'h84c87814, 32'h8cc70208, 32'h90befffa, 32'ha4506ceb, 32'hbef9a3f7, 32'hc67178f2
};

function logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [7:0] r);
begin
    rightrotate = (x >> r) | (x << (32-r));
end
endfunction

function logic [31:0] wtnew; // function with no inputs
 logic [31:0] s0, s1;
 s0 = rightrotate(w[t-15],7)^rightrotate(w[t-15],18)^(w[t-15]>>3);
 s1 = rightrotate(w[t-2],17)^rightrotate(w[t-2],19)^(w[t-2]>>10);
 wtnew = w[t-16] + s0 + w[t-7] + s1;
endfunction

function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                                 input logic [7:0] t);

logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
begin
    S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
    ch = (e & f) ^ ((~e) & g);
    t1 = h + S1 + ch + sha256_k[t] + w;
    S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
    maj = (a & b) ^ (a & c) ^ (b & c);
    t2 = S0 + maj;

    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
end
endfunction

initial begin
    total_length = (size << 3) + 65 + pad_length;
    num_blocks = total_length / 512;
end

always_ff @(posedge clk, negedge reset_n)
begin
    if (!reset_n) begin
        state <= IDLE;
    end else begin
        case (state)
            IDLE:
                if (start) begin 
                    h0 <= 32'h6a09e667;
                    h1 <= 32'hbb67ae85;
                    h2 <= 32'h3c6ef372;
                    h3 <= 32'ha54ff53a;
                    h4 <= 32'h510e527f;
                    h5 <= 32'h9b05688c;
                    h6 <= 32'h1f83d9ab;
                    h7 <= 32'h5be0cd19;
						  
						  //*****TESTING RESULTS: IF YOU COMMENT a-h BELOW OUT, NO OUTPUT, WEIRD...***** Its like a-h never get initially set otherwise even though they should
                    a <= 32'h6a09e667;
                    b <= 32'hbb67ae85;
                    c <= 32'h3c6ef372;
                    d <= 32'ha54ff53a;
                    e <= 32'h510e527f;
                    f <= 32'h9b05688c;
                    g <= 32'h1f83d9ab;
                    h <= 32'h5be0cd19;
						  
						  
                    t <= 0;
                    curr <= 0;
                    mem_we <= 0;
                    //mem_addr <= message_addr;
                    rc <= 0;
						  deal_first_word <= 0;
                    state <= STEP1;
                end
            //if not start STATE stays IDLE
            STEP1: begin
				
					if (curr > 0 && deal_first_word == 1) begin
						w[0] <= last;
						deal_first_word <= 0;
						state <= STEP2;
						//continue;
					end
				   else begin
                   mem_addr <= message_addr + rc;
                   rc <= rc + 1;
				   end
					
				
                state <= STEP2;
            end
				
				STEP2: begin
				    state <= STEP3;
				end
				
            STEP3: begin //reader

                //$display("INPUT DATA");
					 //$display(mem_addr);
                //$display(rc);
					 
					 //$display("w[0] is %x", w[0]);
					 

                 if (t < 64) begin
                    if(t < 16 && rc < num_words) begin		//we only want to read words we are given, no more make sense?
//                        if (curr > 0) begin
//									w[0] <= last;
//								end
								w[t] <= mem_read_data;
                        state <= STEP1;
                    end
						  
						  
						  else if (rc == num_words) begin
								case (size % 4)
								  0: w[t] <= 32'h80000000;
								  1: w[t] <= (mem_read_data & 32'hff000000) | 32'h00800000;
								  2: w[t] <= (mem_read_data & 32'hffff0000) | 32'h00008000;
								  3: w[t] <= (mem_read_data & 32'hffffff00) | 32'h00000080;
								endcase
								
								if (t == 14 && rc == num_words) begin
								 w[t] <= {29'd0, size[31:29]};
							   end
							   else if (t == 15 && rc == num_words) begin
								 w[t] <= {size[28:0], 3'd0};
							   end
							   else begin
								 w[t] <= 32'd0;   //pad with zeros
							   end
								
								
						  end
							
						  
                    if(t > 15) begin
                        if(t == 16) begin
								    //$display("AM I EVEN GETTING HERE!!!!!!!!!!!");
                            last <= mem_read_data;
                        end
                        w[t] <= wtnew;
                        state <= STEP3;
                    end
                    t <= t + 1;
                    
                end 
                else begin
					     
						 //$display("t is %d", t);
				       if(t == 63) begin  //*****TESTING RESULTS: CHANGING THIS "t" DOES NOT CHANGE FINAL RESULT, WEIRD...*****
						   a <= h0;
					      b <= h1;
					      c <= h2;
					      d <= h3;
					      e <= h4;
					      f <= h5;
					      g <= h6;
					      h <= h7;
						 end
					     
                    t <= 0;
                    curr <= curr + 1;
						  $display("PROCESSING BLOCK %d", curr);
                    state <= STEP4;
                end 

            end
            
				
				STEP4: begin
				    
					 //Just testing w display for each block
				    for(int i = 0; i < 64; i++) begin
                    $display("w[t]: %h", w[i]); 
                end
                
					 //Setting 64 computed w's to temp. Use "temp" for sha256_op fucntion 
                for(int i = 0; i < 64; i++) begin
                    temp[i] <= w[i];
                    w[i] <= 0;
                end
					 
					 //NOT SURE IF ABOVE FOR LOOP WORKS HERE, SO DID STATE CALLBACK INSTEAD. GET RID OF IF NOT NEEDED. I think they do the same thing but double check
//					 if (v < 64) begin
//					     temp[v] <= w[v];
//						  w[v] <= 0;
//						  v <= v+1;
//						  state <= STEP3;
//					 end
//					 
//					 else begin
//					    v <= 0;
//					    state <= STEP4;
//					 end

                state <= STEP5;
			   end
				
				STEP5: begin
				    //$display("IN STEP 4");
					 //$display("DISPLAYING TEMP BLOCK");
					 for(int i = 0; i < 64; i++) begin
                    //$display("temp[j]: %h", temp[i]); 
                end
				    if(j < 64) begin       //*****TESTING REULTS: Changing J from <64 to <60 changes the final hash, so sha256_op is computing something...*****
							{a, b, c, d, e, f, g, h} <= sha256_op(a, b, c, d, e, f, g, h, temp[j], j);
							j <= j + 1;
							state <= STEP5;
					 end else begin
					      j <= 0;
							state <= STEP6;
					 end
			   end
				
				STEP6: begin
					 
                if(curr != 3) begin
                    //$display("Missed value is %x", last);
						  
                    //w[0] <= last;
						  
						  h0 <= h0 + a;
						  h1 <= h1 + b;
					     h2 <= h2 + c;
					     h3 <= h3 + d;
					     h4 <= h4 + e;
					     h5 <= h5 + f;
					     h6 <= h6 + g;
					     h7 <= h7 + h;
						  
						  deal_first_word <= 1;

                    state <= STEP1;
                    
                    
						  
                end else begin
					 
					     
                    sha256_digest <= {h0, h1, h2, h3, h4, h5, h6, h7};
						  
						  //Need to set write count here for testbench dpsram
						  wc <= 0;	
						  mem_we <= 1;
						  state <= STEP7;
						  
                    //state <= DONE;
                end
            end
				
				STEP7: begin
				  $display("***IN STEP 6***");
				  $display("Data is %x", mem_write_data);
				  
				  if (wc == 0) begin
						mem_addr <= output_addr + wc;
						mem_write_data <= h0;
						wc <= wc +1;
						state <= STEP7;
				  end
				  
				  else if (wc == 1) begin
						mem_addr <= output_addr + wc;
						mem_write_data <= h1;
						wc <= wc +1;
						state <= STEP7;
				  end
				  
				  else if (wc == 2) begin
						mem_addr <= output_addr + wc;
						mem_write_data <= h2;
						wc <= wc +1;
						state <= STEP7;
				  end
				  
				  else if (wc == 3) begin
						mem_addr <= output_addr + wc;
						mem_write_data <= h3;
						wc <= wc +1;
						state <= STEP7;
				  end
				  
				  else if (wc == 4) begin
						mem_addr <= output_addr + wc;
						mem_write_data <= h4;
						wc <= wc +1;
						state <= STEP7;
				  end
				  
				  else if (wc == 5) begin
						mem_addr <= output_addr + wc;
						mem_write_data <= h5;
						wc <= wc +1;
						state <= STEP7;
				  end
				  
				  else if (wc == 6) begin
						mem_addr <= output_addr + wc;
						mem_write_data <= h6;
						wc <= wc +1;
						state <= STEP7;
				  end
				  
				  else if (wc == 7) begin
						mem_addr <= output_addr + wc;
						mem_write_data <= h7;
						wc <= wc +1;
						state <= STEP7;
				  end
				  
				  else begin
						mem_we <= 0;
						state <= DONE;
				  end
				
				end
				
				
            DONE: begin
				
				    $display("Printing sha256_digest here");
					 $display("%x", sha256_digest);
                
                done <= 1;
                state <= IDLE;
            end    
        endcase
    end
end

endmodule 