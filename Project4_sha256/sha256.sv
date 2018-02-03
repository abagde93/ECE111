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

/*logic   [255:0] sha256_hash; // results here

logic   [31:0] dpsram[0:16383]; // each row has 32 bits
logic   [31:0] dpsram_tb[0:16383]; // for result testing, testbench only

logic   [31:0] message_seed = 32'h01234567; // modify message_seed to test your design

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

	h0 = 32'h6a09e667;
	h1 = 32'hbb67ae85;
	h2 = 32'h3c6ef372;
	h3 = 32'ha54ff53a;
	h4 = 32'h510e527f;
	h5 = 32'h9b05688c;
	h6 = 32'h1f83d9ab;
	h7 = 32'h5be0cd19;
	
	for (m = 0; m < outloop; m = m + 1) begin
        // W ARRAY EXPANSION

        for (t = 0; t < 64; t = t + 1) begin
            if (t < 16) begin
                w[t] = dpsram_tb[t+m*16];
            end else begin
                s0 = rightrotate(w[t-15], 7) ^ rightrotate(w[t-15], 18) ^ (w[t-15] >> 3);
                s1 = rightrotate(w[t-2], 17) ^ rightrotate(w[t-2], 19) ^ (w[t-2] >> 10);
                w[t] = w[t-16] + s0 + w[t-7] + s1;
            end
        end

        // INITIAL HASH AT ROUND K

        a = h0;
        b = h1;
        c = h2;
        d = h3;
        e = h4;
        f = h5;
        g = h6;
        h = h7;

        // HASH ROUNDS

        for (t = 0; t < 64; t = t + 1) begin
            {a, b, c, d, e, f, g, h} = sha256_op(a, b, c, d, e, f, g, h, w[t], t);
        end

        // FINAL HASH

        h0 = h0 + a;
        h1 = h1 + b;
        h2 = h2 + c;
        h3 = h3 + d;
        h4 = h4 + e;
        h5 = h5 + f;
        h6 = h6 + g;
        h7 = h7 + h;
	end
	 
	sha256_digest = {h0, h1, h2, h3, h4, h5, h6, h7};*/
		
	/*always_ff @(posedge clk, negedge reset_n)
	begin
		if(!reset_n) begin
			state <= IDLE;
		end
		else begin
			case(state)
				IDLE:
					if (start) begin
						counter <= 0;
						state <= READ_1;        
					end
				//add more cases
				READ_1: begin
					mem_addr <= message_addr[15:0] + counter;
					mem_we <= 0;
					state <= READ_2;
				end
				
				READ_2:
					if(!mem_we) begin
						//$display("in read2");
						mem_we <= 1;
						state <= WRITE_1;
					end
					
				WRITE_1:
					if(mem_we) begin
						//$display("IN WRITE_1, setting address");
						//num_words <= (size >> 4);
						read_data <= mem_read_data;
						//$display(read_data);
						counter <= counter + 1;
						//mem_addr <= output_addr[15:0] + counter;
						//state <= WRITE_2;
						state <= WRITE_2;
					end
				WRITE_2: begin
					$display("In Write 2 displaying read_data");
					//$display(read_data);
					state <= READ_1;
				end
	
			 endcase
		end

	end//end ff block*/
logic [15:0] rc, wc; // read and write counters

  // byte rotation
  function logic [31:0] byte_rotate(input logic [31:0] value);
    byte_rotate = {value[23:16], value[15:8], value[7:0], value[31:24]};
  endfunction

  assign mem_clk = clk;

  always_ff @(posedge clk, negedge reset_n)
  begin
    if (!reset_n) begin
      state <= IDLE;
    end else
      case (state)
      IDLE: // start
          if (start) begin // READ 0
            mem_we <= 0;
            mem_addr <= message_addr;
            rc <= 1;
            wc <= 0;
            state <= STEP2;
          end
      STEP1: begin // READ 0
          mem_we <= 0;
          mem_addr <= message_addr + rc;
          rc <= rc + 1;
          state <= STEP2;
        end
      STEP2: begin // READ 1
          mem_we <= 0;
          mem_addr <= message_addr + rc;
          rc <= rc + 1;
          state <= STEP3;
        end
      STEP3: begin // WRITE 0
          mem_we <= 1;
          mem_addr <= output_addr + wc;
			 $display("STEP 3");
			 $display("%h, %b, %d, %x\n", mem_read_data,mem_read_data, mem_read_data, mem_read_data);
          mem_write_data <= byte_rotate(mem_read_data);
          wc <= wc + 1;
          if ((wc + 1) < (size >> 2)) begin
              state <= STEP4;
          end else begin
              state <= DONE;
          end
        end
      STEP4: begin // WRITE 1
          mem_we <= 1;
          mem_addr <= output_addr + wc;
          mem_write_data <= byte_rotate(mem_read_data);
          wc <= wc + 1;
          if ((wc + 1) < (size >>2)) begin
              state <= STEP1;
          end else begin
              state <= DONE;
          end
        end
      DONE: begin
          done <= 1;
          state <= IDLE;
        end
      endcase
  end


endmodule 