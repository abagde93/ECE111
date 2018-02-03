module tb_sha256();

logic           clk, reset_n, start;
logic   [ 31:0] message_addr, size, output_addr;
logic           done, mem_clk, mem_we;
logic   [ 15:0] mem_addr;
logic   [ 31:0] mem_write_data;
logic   [ 31:0] mem_read_data;

logic   [255:0] sha256_hash; // results here

logic   [ 31:0] dpsram[0:16383]; // each row has 32 bits
logic   [ 31:0] dpsram_tb[0:16383]; // for result testing, testbench only

logic   [ 31:0] message_seed = 32'h01234567; // modify message_seed to test your design

int             message_size = 120; // in bytes // change this number to test your design
int             pad_length;

int             t, m;
int             outloop;
int             cycles;

logic   [255:0] sha256_digest;

logic   [ 31:0] h0;
logic   [ 31:0] h1;
logic   [ 31:0] h2;
logic   [ 31:0] h3;
logic   [ 31:0] h4;
logic   [ 31:0] h5;
logic   [ 31:0] h6;
logic   [ 31:0] h7;

logic   [ 31:0] a, b, c, d, e, f, g, h;
logic   [ 31:0] s1, s0;

logic   [ 31:0] w[0:63];

// instantiate your design
sha256 sha256_inst (clk, reset_n, start, message_addr, size, output_addr, done,
    mem_clk, mem_we, mem_addr, mem_write_data, mem_read_data);

// SHA256 K constants
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

// SHA256 hash round
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

// right rotation
function logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [7:0] r);
begin
    rightrotate = (x >> r) | (x << (32-r));
end
endfunction

// clock generator
always begin
    #10;
    clk = 1'b1;
    #10
    clk = 1'b0;
end

// main testbench
initial
begin
    // RESET HASH CO-PROCESSOR

    @(posedge clk) reset_n = 0;
    for (m = 0; m < 2; m = m + 1) @(posedge clk);
    reset_n = 1;
    for (m = 0; m < 2; m = m + 1) @(posedge clk);

    // SET MESSAGE LOCATION

    size = message_size;
    message_addr = 32'd0;
    output_addr = message_addr + ((message_size-1))/4 + 1;

    // CREATE AND DISPLAY MESSAGETEXT

    $display("-----------\n");
    $display("Messagetext\n");
    $display("-----------\n");

    dpsram[message_addr+0] = message_seed;
    dpsram_tb[0] = message_seed;

    $display("%x\n", dpsram[0]);

    for (m = 1; m < (message_size-1)/4+1; m = m + 1) begin // data generation
        dpsram[message_addr+m] = (dpsram[message_addr+m-1]<<1)|(dpsram[message_addr+m-1]>>31);
        dpsram_tb[m] = dpsram[message_addr+m];
        $display("%x\n", dpsram[message_addr+m]);
    end

    // START PROCESSOR

    start = 1'b1;
    for (m = 0; m < 2; m = m + 1) @(posedge clk);
    start = 1'b0;

    // PERFORM PADDING OF MESSAGE

    // calculate total number of bytes after padding (before appending total length)
    if ((message_size + 1) % 64 <= 56 && (message_size + 1) % 64 > 0)
        pad_length = (message_size/64)*64 + 56;
    else
        pad_length = (message_size/64+1)*64 + 56;

    case (message_size % 4) // pad bit 1
    0: dpsram_tb[message_size/4] = 32'h80000000;
    1: dpsram_tb[message_size/4] = dpsram_tb[message_size/4] & 32'h FF000000 | 32'h 00800000;
    2: dpsram_tb[message_size/4] = dpsram_tb[message_size/4] & 32'h FFFF0000 | 32'h 00008000;
    3: dpsram_tb[message_size/4] = dpsram_tb[message_size/4] & 32'h FFFFFF00 | 32'h 00000080;
    endcase

    for (m = message_size/4+1; m < pad_length/4; m = m + 1) begin
        dpsram_tb[m] = 32'h00000000;
    end

    dpsram_tb[pad_length/4] = message_size >> 29; // append length of message in bits (before pre-processing)
    dpsram_tb[pad_length/4+1] = message_size * 8;
    pad_length = pad_length + 8; // final length after pre-processing

    outloop = pad_length/64; // break message into 512-bit chunks (64 bytes)

    // SET INITIAL HASH

    h0 = 32'h6a09e667;
    h1 = 32'hbb67ae85;
    h2 = 32'h3c6ef372;
    h3 = 32'ha54ff53a;
    h4 = 32'h510e527f;
    h5 = 32'h9b05688c;
    h6 = 32'h1f83d9ab;
    h7 = 32'h5be0cd19;

    // COMPUTE SHA256 HASH

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

    sha256_digest = {h0, h1, h2, h3, h4, h5, h6, h7};

    // WAIT UNTIL ENTIRE FRAME IS HASHED, THEN DISPLAY HASH RESULT

    wait (done == 1);

    // DISPLAY HASH RESULT

    $display("-----------------------\n");
    $display("correct hash result is:\n");
    $display("-----------------------\n");
    $display("%x\n", sha256_digest);

    sha256_hash = {
        dpsram[output_addr],
        dpsram[output_addr+1],
        dpsram[output_addr+2],
        dpsram[output_addr+3],
        dpsram[output_addr+4],
        dpsram[output_addr+5],
        dpsram[output_addr+6],
        dpsram[output_addr+7]
    };

    $display("-----------------------\n");
    $display("Your result is:        \n");
    $display("-----------------------\n");
    $display("%x\n", sha256_hash);

    $display("***************************\n");

    if (sha256_digest === sha256_hash) begin
        $display("Congratulations! You have the correct hash result!\n");
        $display("Total number of cycles: %d\n\n", cycles);
    end else begin
        $display("Error! The hash result is wrong!\n");
    end

    $display("***************************\n");

    $stop;
end

// memory model
always @(posedge mem_clk)
begin
    if (mem_we) // write
        dpsram[mem_addr] = mem_write_data;
    else // read
        mem_read_data = dpsram[mem_addr];
end

// track # of cycles
always @(posedge clk)
begin
    if (!reset_n)
        cycles = 0;
    else
        cycles = cycles + 1;
end

endmodule
