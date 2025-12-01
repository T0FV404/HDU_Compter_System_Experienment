`timescale 1ns / 1ps

module tb_SingleCycleCPU;

    // ============================================================
    // 1. 信号定义 (对应 SingleCycleCPU 的端口)
    // ============================================================
    reg  clock;
    reg  reset;
    
    // CPU 输出信号连接
    wire        io_zf;
    wire        io_of;
    wire [31:0] io_aluResult;
    wire [31:0] io_currentInst;
    wire [31:0] io_currentPC;
    wire        io_isSignedOp;
    wire        io_dmemWen;
    wire [31:0] io_dmemAddr;
    wire [31:0] io_dmemWData;
    wire [31:0] io_dmemRData;

    // ============================================================
    // 2. 实例化顶层模块 (DUT - Device Under Test)
    // ============================================================
    // 端口名必须严格匹配 cputest.v 中的 SingleCycleCPU 模块定义
    SingleCycleCPU u_cpu (
        .clock          (clock),
        .reset          (reset),
        .io_zf          (io_zf),
        .io_of          (io_of),
        .io_aluResult   (io_aluResult),
        .io_currentInst (io_currentInst),
        .io_currentPC   (io_currentPC),
        .io_isSignedOp  (io_isSignedOp),
        .io_dmemWen     (io_dmemWen),
        .io_dmemAddr    (io_dmemAddr),
        .io_dmemWData   (io_dmemWData),
        .io_dmemRData   (io_dmemRData)
    );

    // ============================================================
    // 3. 时钟生成
    // ============================================================
    initial begin
        clock = 0;
        forever #5 clock = ~clock; // 周期 10ns (100MHz)
    end

    // ============================================================
    // 4. 测试流程控制
    // ============================================================
    initial begin
        // 打印表头
        $display("\n============================================================================================================");
        $display("                                   Lab 5 Simulation (Vivado / Generated Verilog)");
        $display("============================================================================================================");
        $display("| Cyc |    PC    |   Inst   | Disassembly          | ALU Result | MemWr? | Mem WData  | Mem RData  |");
        $display("|-----|----------|----------|----------------------|------------|--------|------------|------------|");

        // 复位序列
        reset = 1;
        #20;       // 保持复位 2 个时钟周期
        reset = 0; // 释放复位，CPU 开始运行

        // 运行足够的时间以覆盖所有测试指令 (20条指令 * 10ns = 200ns)
        #250; 

        $display("============================================================================================================\n");
        $display("Simulation Finished.");
        $stop; // 停止仿真
    end

    // ============================================================
    // 5. 辅助逻辑：简易反汇编器 (用于显示)
    // ============================================================
    // 定义一个字符串变量用于显示汇编指令 (SystemVerilog 特性)
    string asmStr;
    
    // 计数器
    integer cycle_count = 0;

    // 根据指令码生成字符串 (对应 AsyncInstMem 中的初始化数据)
    always @(*) begin
        case (io_currentInst)
            32'hfff00093: asmStr = "addi x1, x0, -1";
            32'h08102023: asmStr = "sw   x1, 128(x0)";
            32'h08002103: asmStr = "lw   x2, 128(x0)";
            32'hff810113: asmStr = "addi x2, x2, -8";
            32'h08202223: asmStr = "sw   x2, 132(x0)";
            32'h08402183: asmStr = "lw   x3, 132(x0)";
            32'h08002203: asmStr = "lw   x4, 128(x0)";
            32'h876542b7: asmStr = "lui  x5, 0x87654"; 
            32'h32128293: asmStr = "addi x5, x5, 0x321";
            32'h10502023: asmStr = "sw   x5, 256(x0)";
            32'h10002303: asmStr = "lw   x6, 256(x0)";
            32'h00000013: asmStr = "nop";
            default:      asmStr = "unknown";
        endcase
    end

    // ============================================================
    // 6. 监控打印 (Monitor)
    // ============================================================
    // 在时钟下降沿采样并打印状态，确保信号已稳定
    always @(negedge clock) begin
        if (!reset) begin
            // 格式化打印
            // %h: 16进制
            // %s: 字符串
            // %08x: 8位宽16进制补0
            $display("| %3d | %08x | %08x | %-20s |  %08x  |   %s    |  %08x  |  %08x  |", 
                cycle_count,
                io_currentPC, 
                io_currentInst, 
                asmStr, 
                io_aluResult, 
                (io_dmemWen ? "YES" : " - "), // MemWr?
                (io_dmemWen ? io_dmemWData : 32'hxxxxxxxx), // WData (仅写时显示数值，否则显示x)
                io_dmemRData
            );
            
            // ------------------------------------------------------------
            // 自动检查逻辑 (Assertions) - 帮你验证实验是否成功
            // ------------------------------------------------------------
            
            // 检查 SW 指令: sw x1, 128(x0) -> 应该把 -1 (ffffffff) 写入地址 128
            if (io_currentInst == 32'h08102023) begin 
                if (io_dmemWData !== 32'hffffffff) 
                    $display("Error: [SW Failed] Write Data should be ffffffff (-1), got %h", io_dmemWData);
                else
                    $display("       >>> [CHECK PASS] SW Instruction Correct!");
            end
            
            // 检查 LW 指令: lw x2, 128(x0) -> 应该读回 -1 (ffffffff)
            if (io_currentInst == 32'h08002103) begin 
                // 注意：由于是组合逻辑读，当指令为 LW 时，io_dmemRData 应该已经有了正确的数据
                if (io_dmemRData !== 32'hffffffff)
                    $display("Error: [LW Failed] Read Data should be ffffffff (-1), got %h", io_dmemRData);
                else
                    $display("       >>> [CHECK PASS] LW Instruction Correct!");
            end

            cycle_count = cycle_count + 1;
        end
    end

endmodule