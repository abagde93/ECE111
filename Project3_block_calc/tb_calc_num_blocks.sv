module tb_calc_num_blocks;

logic [31:0] size;
logic [15:0] num_blocks;
logic [31:0] sz[0:1];
int tb[0:1];
int error_flag;

calc_num_blocks cb(size, num_blocks);

initial
begin
  error_flag = 0;

  sz[0] = 16'd120; tb[0] = 3;
  sz[1] = 16'd511; tb[1] = 9;

  for (int i = 0; i < 2; i++) begin
    #10
    size = sz[i];
    #10
    $display("Your result for size = %3d: %1d", sz[i], num_blocks);
    if (num_blocks !== tb[i]) begin
        $display("          ERROR! Should be: %1d", tb[i]);
        error_flag = error_flag + 1;
    end
  end

  if (error_flag == 1)
    $display("\nERROR! %1d result is wrong!", error_flag);
  else if (error_flag > 1)
    $display("\nERROR! %1d results are wrong!", error_flag);
  else
    $display("\nCongratulations! All results are correct!");
end
endmodule