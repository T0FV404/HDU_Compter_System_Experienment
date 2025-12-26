import chisel3._
import chisel3.util._

// ==========================================
// 0. 配置参数与工具类
// ==========================================
class CPUConfig {
  val XLEN = 32           // 数据位宽
  val ADDR_WIDTH = 32     // 地址位宽  
  val REG_NUM = 32        // 寄存器数量
  val REG_ADDR_WID = 5    // 寄存器地址位宽
  val IMEM_SIZE = 1024    // 指令存储器大小（字） 
  val DMEM_SIZE = 1024    // 数据存储器大小（字）
  val PC_START = 0x0      // PC 起始地址
  var enableDebug = true  // 控制debug
}

object ALUOps extends ChiselEnum {
  val ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND = Value
}

// ==========================================
// 1.数据包定义
// ==========================================

// 控制信号包 (包含在各级数据包中)
class CtrlInfo(implicit config: CPUConfig) extends Bundle {
  val regWen   = Bool()
  val regWAddr = UInt(config.REG_ADDR_WID.W)
  val aluOp    = ALUOps()
  val memWen   = Bool()      // R型暂时用不到，保留接口
  val wbSrc    = UInt(2.W)   // 0=ALU, 1=Mem
}

// IF -> ID 级间数据
class IfIdData(implicit config: CPUConfig) extends Bundle {
  val pc   = UInt(config.ADDR_WIDTH.W)
  val inst = UInt(config.XLEN.W)
  val valid = Bool()
}

// ID -> EXE 级间数据
class IdExeData(implicit config: CPUConfig) extends Bundle {
  val pc      = UInt(config.ADDR_WIDTH.W)
  val info    = new CtrlInfo
  val rs1Data = UInt(config.XLEN.W)
  val rs2Data = UInt(config.XLEN.W)
  val imm     = UInt(config.XLEN.W) // R型不需要，但保留结构
  val valid   = Bool()
}

// EXE -> MEM 级间数据
class ExeMemData(implicit config: CPUConfig) extends Bundle {
  val pc       = UInt(config.ADDR_WIDTH.W)
  val info     = new CtrlInfo
  val aluOut   = UInt(config.XLEN.W)
  val memWData = UInt(config.XLEN.W) // 也就是 rs2Data
  val valid    = Bool()
}

// MEM -> WB 级间数据
class MemWbData(implicit config: CPUConfig) extends Bundle {
  val pc       = UInt(config.ADDR_WIDTH.W)
  val info     = new CtrlInfo
  val wbData   = UInt(config.XLEN.W) // 最终要写回寄存器的数据
  val valid    = Bool()
}

// ==========================================
// 2. 基础组件 (ALU, RegFile, Imem, Dmem)
// ==========================================

class ALU(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val a      = Input(UInt(config.XLEN.W))
    val b      = Input(UInt(config.XLEN.W))
    val aluOp  = Input(ALUOps())
    val result = Output(UInt(config.XLEN.W))
  })

  val shamt = io.b(log2Ceil(config.XLEN) - 1, 0) // 动态获取位宽
  val isSub = (io.aluOp === ALUOps.SUB) || 
              (io.aluOp === ALUOps.SLT) || 
              (io.aluOp === ALUOps.SLTU)
  val operandB = Mux(isSub, ~io.b, io.b) 
  val sum = io.a + operandB + isSub.asUInt

  io.result := MuxLookup(io.aluOp.asUInt, 0.U)(Seq(
    ALUOps.ADD.asUInt  -> sum,
    ALUOps.SUB.asUInt  -> sum,
    ALUOps.SLL.asUInt  -> (io.a << shamt),
    ALUOps.SLT.asUInt  -> Mux(io.a.asSInt < io.b.asSInt, 1.U, 0.U),
    ALUOps.SLTU.asUInt -> Mux(io.a < io.b, 1.U, 0.U),
    ALUOps.XOR.asUInt  -> (io.a ^ io.b),
    ALUOps.SRL.asUInt  -> (io.a >> shamt),
    ALUOps.SRA.asUInt  -> (io.a.asSInt >> shamt).asUInt,
    ALUOps.OR.asUInt   -> (io.a | io.b),
    ALUOps.AND.asUInt  -> (io.a & io.b)
  ))
}

class RegisterFile(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val rs1Addr = Input(UInt(config.REG_ADDR_WID.W))
    val rs2Addr = Input(UInt(config.REG_ADDR_WID.W))
    val rs1Data = Output(UInt(config.XLEN.W))
    val rs2Data = Output(UInt(config.XLEN.W))
    
    // 写端口 (来自 WB 阶段)
    val wbReq   = Input(new Bundle {
      val wen   = Bool()
      val waddr = UInt(config.REG_ADDR_WID.W)
      val wdata = UInt(config.XLEN.W)
    })
  })

  val initValues = Seq.fill(config.REG_NUM)(0.U(config.XLEN.W))
  val finalInitValues = initValues.updated(1, 10.U).updated(2, 20.U)

  // 寄存器堆初始化
  val regs = RegInit(VecInit(finalInitValues))

  
  // 读逻辑
  io.rs1Data := Mux(io.rs1Addr === 0.U, 0.U, regs(io.rs1Addr))
  io.rs2Data := Mux(io.rs2Addr === 0.U, 0.U, regs(io.rs2Addr))

  // 写逻辑
  when(io.wbReq.wen && io.wbReq.waddr =/= 0.U) {
    regs(io.wbReq.waddr) := io.wbReq.wdata
  }
}

class InstructionMemory(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(config.ADDR_WIDTH.W))
    val inst = Output(UInt(config.XLEN.W))
  })
  // 简单的 R 型指令测试程序
  val program = Seq(
  // 1. add x3, x1, x2  (x1=10, x2=20 -> x3=30)
  "h002081b3".U, 
  
  // 插入 3 个 NOP 等待 x3 写回 (WB阶段)
  "h00000013".U, // nop (addi x0, x0, 0)
  "h00000013".U, // nop
  "h00000013".U, // nop

  // 2. sub x4, x3, x1  (x3=30, x1=10 -> x4=20)
  "h40118233".U,

  // 插入 3 个 NOP 等待 x4 写回
  "h00000013".U, 
  "h00000013".U, 
  "h00000013".U, 

  // 3. or  x5, x4, x3  (x4=20, x3=30 -> x5=30|20=30)
  "h003262b3".U,
  
  // 插入 3 个 NOP
  "h00000013".U, 
  "h00000013".U, 
  "h00000013".U, 

  // 4. and x6, x5, x4
  "h0042f333".U
  )
  val romContent = program ++ Seq.fill(config.IMEM_SIZE - program.length)("h00000013".U) // nop
  val wordAddr = io.addr >> 2
  io.inst := VecInit(romContent)(wordAddr)
}

// ==========================================
// 3. 流水线阶段单元 (Fetch, Decode, Execute, Memory, WriteBack)
// ==========================================

// --- 取指阶段 (Fetch) ---
class FetchUnit(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val imemAddr = Output(UInt(config.ADDR_WIDTH.W))
    val imemInst = Input(UInt(config.XLEN.W))
    val out      = Output(new IfIdData)
  })

  val pc = RegInit(config.PC_START.U(config.ADDR_WIDTH.W))
  
  // 简单的 PC + 4 逻辑 (理想流水线暂不考虑分支预测和跳转冲突)
  pc := pc + 4.U

  io.imemAddr := pc
  
  // 输出到下一级
  io.out.pc    := pc
  io.out.inst  := io.imemInst
  io.out.valid := true.B // 简单起见，始终有效
}

// --- 译码阶段 (Decode) ---
class DecodeUnit(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val in       = Input(new IfIdData) // 来自 Fetch
    val rfRead   = new Bundle {
      val rs1Addr = Output(UInt(config.REG_ADDR_WID.W))
      val rs2Addr = Output(UInt(config.REG_ADDR_WID.W))
      val rs1Data = Input(UInt(config.XLEN.W))
      val rs2Data = Input(UInt(config.XLEN.W))
    }
    val out      = Output(new IdExeData)
  })

  val inst = io.in.inst
  val opcode = inst(6, 0)
  val funct3 = inst(14, 12)
  val funct7 = inst(31, 25)
  val rd     = inst(11, 7)
  val rs1    = inst(19, 15)
  val rs2    = inst(24, 20)

  // 连接寄存器堆读地址
  io.rfRead.rs1Addr := rs1
  io.rfRead.rs2Addr := rs2

  // --- 控制信号解码 ---
  val ctrl = Wire(new CtrlInfo)
  
  // 默认值
  ctrl.regWen   := false.B
  ctrl.memWen   := false.B
  ctrl.aluOp    := ALUOps.ADD
  ctrl.wbSrc    := 0.U
  ctrl.regWAddr := rd

  // R-Type 识别
  val isRType = opcode === "b0110011".U
  
  when(isRType) {
    ctrl.regWen := true.B
    switch(funct3) {
      is("b000".U) { ctrl.aluOp := Mux(funct7 === "b0100000".U, ALUOps.SUB, ALUOps.ADD) }
      is("b001".U) { ctrl.aluOp := ALUOps.SLL }
      is("b010".U) { ctrl.aluOp := ALUOps.SLT }
      is("b011".U) { ctrl.aluOp := ALUOps.SLTU }
      is("b100".U) { ctrl.aluOp := ALUOps.XOR }
      is("b101".U) { ctrl.aluOp := Mux(funct7 === "b0100000".U, ALUOps.SRA, ALUOps.SRL) }
      is("b110".U) { ctrl.aluOp := ALUOps.OR }
      is("b111".U) { ctrl.aluOp := ALUOps.AND }
    }
  }

  // 输出打包
  io.out.pc      := io.in.pc
  io.out.info    := ctrl
  io.out.rs1Data := io.rfRead.rs1Data
  io.out.rs2Data := io.rfRead.rs2Data
  io.out.imm     := 0.U // R型无立即数
  io.out.valid   := io.in.valid && isRType // 只有R型才标记有效(针对本实验)
}

// --- 执行阶段 (Execute) ---
class ExecuteUnit(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val in  = Input(new IdExeData)
    val out = Output(new ExeMemData)
  })

  val alu = Module(new ALU)

  alu.io.a     := io.in.rs1Data
  alu.io.b     := io.in.rs2Data
  alu.io.aluOp := io.in.info.aluOp

  // 输出
  io.out.pc       := io.in.pc
  io.out.info     := io.in.info
  io.out.aluOut   := alu.io.result
  io.out.memWData := io.in.rs2Data
  io.out.valid    := io.in.valid
}

// --- 访存阶段 (Memory) ---
class MemoryUnit(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val in  = Input(new ExeMemData)
    // 这里预留数据存储器接口，虽然R型指令不用
    // val dmem = ... 
    val out = Output(new MemWbData)
  })

  // R型指令直通，不访问内存
  // 如果是 Load 指令，这里会从 Dmem 读数据
  val memResult = 0.U 

  val finalWbData = Mux(io.in.info.wbSrc === 1.U, memResult, io.in.aluOut)

  io.out.pc     := io.in.pc
  io.out.info   := io.in.info
  io.out.wbData := finalWbData
  io.out.valid  := io.in.valid
}

// --- 写回阶段 (WriteBack) ---
class WriteBackUnit(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val in = Input(new MemWbData)
    // 输出给 RegFile 的写端口
    val wbReq = Output(new Bundle {
      val wen   = Bool()
      val waddr = UInt(config.REG_ADDR_WID.W)
      val wdata = UInt(config.XLEN.W)
    })
    // 调试信号
    val debug_pc = Output(UInt(config.ADDR_WIDTH.W))
    val debug_valid = Output(Bool())
  })

  // 生成写回请求
  io.wbReq.wen   := io.in.info.regWen && io.in.valid
  io.wbReq.waddr := io.in.info.regWAddr
  io.wbReq.wdata := io.in.wbData

  // Debug 输出
  io.debug_pc    := io.in.pc
  io.debug_valid := io.in.valid
}

// ==========================================
// 4. 五级流水线 CPU 顶层
// ==========================================
class PipelineCPU(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    // 简单的顶层 Debug 接口
    val diffTest = if (config.enableDebug) Some(new Bundle {
      val pc       = Output(UInt(config.ADDR_WIDTH.W))
      val inst     = Output(UInt(config.XLEN.W))
      val wbValid  = Output(Bool())
      val wbData   = Output(UInt(config.XLEN.W))
    }) else None
  })

  // --- 实例化各阶段单元 ---
  val fetch  = Module(new FetchUnit)
  val decode = Module(new DecodeUnit)
  val execute = Module(new ExecuteUnit)
  val memory = Module(new MemoryUnit)
  val writeback = Module(new WriteBackUnit)

  // --- 实例化存储器与寄存器堆 ---
  val imem = Module(new InstructionMemory)
  val regFile = Module(new RegisterFile)

  // --- 连接 Fetch 阶段 ---
  imem.io.addr   := fetch.io.imemAddr
  fetch.io.imemInst := imem.io.inst

  // --- 流水线寄存器 IF/ID ---
  // 使用 RegNext 将 Fetch 的输出锁存一个周期传递给 Decode
  val if_id_reg = RegNext(fetch.io.out, 0.U.asTypeOf(new IfIdData))
  
  // --- 连接 Decode 阶段 ---
  decode.io.in <> if_id_reg
  decode.io.rfRead.rs1Data <> regFile.io.rs1Data
  decode.io.rfRead.rs2Data <> regFile.io.rs2Data
  // 注意：地址连接已经在 DecodeUnit 内部完成，连接到了 rfRead

  // --- 流水线寄存器 ID/EXE ---
  val id_exe_reg = RegNext(decode.io.out, 0.U.asTypeOf(new IdExeData))

  // --- 连接 Execute 阶段 ---
  execute.io.in <> id_exe_reg

  // --- 流水线寄存器 EXE/MEM ---
  val exe_mem_reg = RegNext(execute.io.out, 0.U.asTypeOf(new ExeMemData))

  // --- 连接 Memory 阶段 ---
  memory.io.in <> exe_mem_reg

  // --- 流水线寄存器 MEM/WB ---
  val mem_wb_reg = RegNext(memory.io.out, 0.U.asTypeOf(new MemWbData))

  // --- 连接 WriteBack 阶段 ---
  writeback.io.in <> mem_wb_reg
  
  // --- 写回路径 (WB -> ID/RegFile) ---
  // 这是流水线的闭环关键：WB 阶段产生的写信号连接回 RegFile
  regFile.io.wbReq <> writeback.io.wbReq
  // 同时也需要连接 RegFile 的读地址端口（由 Decode 单元控制）
  regFile.io.rs1Addr := decode.io.rfRead.rs1Addr
  regFile.io.rs2Addr := decode.io.rfRead.rs2Addr

  // --- 调试信号输出 ---
  if (config.enableDebug) {
    val dbg = io.diffTest.get
    // 这里输出的是 WB 阶段的信息，代表指令完成
    dbg.pc      := writeback.io.debug_pc
    dbg.inst    := 0.U // 简化处理，流水线若要传Inst需要一路RegNext下来，这里省略
    dbg.wbValid := writeback.io.debug_valid
    dbg.wbData  := writeback.io.wbReq.wdata
  }
}

// ==========================================
// 5. 生成 Verilog
// ==========================================
object CPUGen extends App {
  val config = new CPUConfig
  emitVerilog(new PipelineCPU()(config), Array("--target-dir", "generated"))
}