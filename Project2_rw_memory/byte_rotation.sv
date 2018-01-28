module byte_rotation(input logic clk, reset_n, start,
 input logic [31:0] message_addr, size, output_addr,
 output logic done, mem_clk, mem_we,
 output logic [15:0] mem_addr,
 output logic [31:0] mem_write_data,
 input logic [31:0] mem_read_data);
 
 enum logic [2:0] {IDLE=3'b000, READ_1=3'b001, READ_2=3'b010, WRITE_1=3'b011, WRITE_2=3'b100} state;
 
 assign mem_clk = clk;
 logic [31:0] read_data;
 
function logic [31:0] byte_rotate(input logic [31:0] value);
 byte_rotate = {value[23:16], value[15:8], value[7:0], value[31:24]};
endfunction



 
 
 always_ff @(posedge clk, negedge reset_n)
 begin
   if (!reset_n) begin
	  state <= IDLE;
	end
 
   else 
	  case (state)
      IDLE: 
 		 if (start) begin
		      state <= READ_1;        
       end 
		
		 
		READ_1: begin
		   $display("IN READ_1");
			mem_addr <= message_addr[31:16];
			$display(mem_addr);
			mem_we <= 0;
			state <= READ_2;
		end
		
		
		READ_2: 
		  if(!mem_we) begin
		    $display("IN READ_2, reading data");
			 $display(mem_addr);
		    //read_data <= mem_read_data;
			 mem_we <= 1;
			 state <= WRITE_1;
		  end
		 
			 
			
		
		WRITE_1: 
		 if(mem_we) begin
		      $display("IN WRITE_1, setting address");
				read_data <= mem_read_data;
				$display(read_data);
				mem_addr <= 4;
				state <= WRITE_2;
		 end
		
		 
		WRITE_2:
		 if(mem_we) begin
		      $display("IN WRITE_2, doing rotation");
				//read_data <= mem_read_data;
				$display("read_data: %x", read_data);
				mem_write_data <= byte_rotate(read_data);
				mem_we <= 0;
				done <= 1;
		 end
		
	  endcase     
      
 end
endmodule