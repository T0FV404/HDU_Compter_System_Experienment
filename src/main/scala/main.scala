import chisel3._
import chisel3.util._

// ==========================================
// 1. ALU 操作码枚举（ChiselEnum, Chisel 7 用法）
// ==========================================
object ALUOps extends ChiselEnum {
  val ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND = Value
  val ERROR = Value
}

// ==========================================
// 1. 控制器 (Control Unit) 
// ==========================================
class ControlUnit extends Module {
  val io = IO(new Bundle {
    val opcode  = Input(UInt(7.W))
    val funct3  = Input(UInt(3.W))
    val funct7  = Input(UInt(7.W))

    // 控制信号输出
    val aluOp   = Output(ALUOps())
    val regWen  = Output(Bool())   // 寄存器写使能
    val memWen  = Output(Bool())   // 内存写使能 (Store)
    val src1Sel = Output(UInt(2.W))// 0: RS1, 1: PC, 2: 0 (for LUI)
    val src2Sel = Output(UInt(1.W))// 0: RS2, 1: Imm
    val wbSel   = Output(UInt(1.W))// 0: ALU, 1: Mem (Load)
    val isSigned = Output(Bool())
  })

  // 默认值 (防止 latch)
  io.aluOp   := ALUOps.ADD
  io.regWen  := false.B
  io.memWen  := false.B
  io.src1Sel := 0.U // RS1
  io.src2Sel := 0.U // RS2
  io.wbSel   := 0.U // ALU

  val isRType = io.opcode === "b0110011".U
  val isIType = io.opcode === "b0010011".U
  val isLoad  = io.opcode === "b0000011".U 
  val isStore = io.opcode === "b0100011".U 
  val isLUI   = io.opcode === "b0110111".U
  val isAUIPC = io.opcode === "b0010111".U
  
  //生成控制信号
  when(isRType || isIType) {
    switch(io.funct3) {
      is("b000".U) { // ADD or SUB
        // R-Type 且 funct7=0x20 时为 SUB
        when(isRType && io.funct7 === "b0100000".U) { io.aluOp := ALUOps.SUB }
        .otherwise { io.aluOp := ALUOps.ADD }
      }
      is("b001".U) { io.aluOp := ALUOps.SLL }
      is("b010".U) { io.aluOp := ALUOps.SLT }
      is("b011".U) { io.aluOp := ALUOps.SLTU }
      is("b100".U) { io.aluOp := ALUOps.XOR }
      is("b101".U) { // SRL or SRA
        when(io.funct7 === "b0100000".U) { io.aluOp := ALUOps.SRA }
        .otherwise { io.aluOp := ALUOps.SRL }
      }
      is("b110".U) { io.aluOp := ALUOps.OR }
      is("b111".U) { io.aluOp := ALUOps.AND }
    }
  }
  io.isSigned := (io.aluOp === ALUOps.ADD) || 
                 (io.aluOp === ALUOps.SUB) || 
                 (io.aluOp === ALUOps.SLT) || 
                 (io.aluOp === ALUOps.SRA)

  //生成其他控制信号
  when(isRType) {
    io.regWen  := true.B
    io.src1Sel := 0.U // RS1
    io.src2Sel := 0.U // RS2
    io.wbSel   := 0.U // ALU Result
  } .elsewhen(isIType) {
    io.regWen  := true.B
    io.src1Sel := 0.U // RS1
    io.src2Sel := 1.U // Imm
    io.wbSel   := 0.U // ALU Result
  } .elsewhen(isLoad) { // LW
    io.regWen  := true.B
    io.src1Sel := 0.U // RS1 (Base)
    io.src2Sel := 1.U // Imm (Offset)
    io.wbSel   := 1.U // Mem Data 
    io.aluOp   := ALUOps.ADD // 计算地址: Base + Offset
  } .elsewhen(isStore) { // SW
    io.memWen  := true.B
    io.src1Sel := 0.U // RS1 (Base)
    io.src2Sel := 1.U // Imm (Offset)
    io.aluOp   := ALUOps.ADD // 计算地址
  } .elsewhen(isLUI) {
    io.regWen  := true.B
    io.src1Sel := 2.U // 0
    io.src2Sel := 1.U // ImmU
    io.aluOp   := ALUOps.ADD
  } .elsewhen(isAUIPC) {
    io.regWen  := true.B
    io.src1Sel := 1.U // PC
    io.src2Sel := 1.U // ImmU
    io.aluOp   := ALUOps.ADD
  }
}

// ==========================================
// 2. 立即数生成器 (ImmGen)
// ==========================================
class ImmGen extends Module {
  val io = IO(new Bundle {
    val inst = Input(UInt(32.W))
    val out  = Output(UInt(32.W))
  })

  val inst = io.inst
  val opcode = inst(6, 0)

  // 各种格式的立即数解码 (符号扩展)
  val immI = Cat(Fill(20, inst(31)), inst(31, 20)).asSInt // I-Type (LW, ADDI)
  val immS = Cat(Fill(20, inst(31)), inst(31, 25), inst(11, 7)).asSInt // S-Type (SW)
  val immU = Cat(inst(31, 12), 0.U(12.W)).asSInt // U-Type (LUI, AUIPC)

  // 选择输出
  io.out := MuxCase(0.S, Seq(
    (opcode === "b0100011".U) -> immS, // Store 使用 S-Type
    (opcode === "b0110111".U || opcode === "b0010111".U) -> immU, // LUI/AUIPC 使用 U-Type
    (true.B) -> immI // 默认 I-Type (Load, Arith-I)
  )).asUInt
}

// ==========================================
// 3. ALU 模块
// ==========================================
class ALU extends Module {
  val io = IO(new Bundle {
    val a      = Input(UInt(32.W))
    val b      = Input(UInt(32.W))
    val aluOp  = Input(ALUOps())
    val out    = Output(UInt(32.W))
    val zf     = Output(Bool())
    val of     = Output(Bool())
  })

  val shamt = io.b(4, 0)
  val res   = WireDefault(0.U(32.W))
  
  //减法复用加法器
  val isSub = (io.aluOp === ALUOps.SUB) || (io.aluOp === ALUOps.SLT) || (io.aluOp === ALUOps.SLTU)
  val adderB = Mux(isSub, (~io.b).asUInt, io.b)
  val sum    = io.a + adderB + isSub.asUInt

  switch(io.aluOp) {
    is(ALUOps.ADD)  { res := sum }
    is(ALUOps.SUB)  { res := sum }
    is(ALUOps.SLL)  { res := io.a << shamt }
    is(ALUOps.SLT)  { res := Mux(io.a.asSInt < io.b.asSInt, 1.U, 0.U) }
    is(ALUOps.SLTU) { res := Mux(io.a < io.b, 1.U, 0.U) }
    is(ALUOps.XOR)  { res := io.a ^ io.b }
    is(ALUOps.OR)   { res := io.a | io.b }
    is(ALUOps.AND)  { res := io.a & io.b }
    is(ALUOps.SRL)  { res := io.a >> shamt }
    is(ALUOps.SRA)  { res := (io.a.asSInt >> shamt).asUInt }
  }

  io.out := res
  io.zf  := (res === 0.U)
  
  val signA = io.a(31)
  val signB = adderB(31)
  val signR = sum(31)
  io.of := (io.aluOp === ALUOps.ADD || io.aluOp === ALUOps.SUB) && (signA === signB) && (signR =/= signA)
}


// ==========================================
// 4. 寄存器堆
// ==========================================
class RegisterFile extends Module {
  val io = IO(new Bundle {
    val rs1   = Input(UInt(5.W))
    val rs2   = Input(UInt(5.W))
    val waddr = Input(UInt(5.W))
    val wdata = Input(UInt(32.W))
    val wen   = Input(Bool())
    val rdata1 = Output(UInt(32.W))
    val rdata2 = Output(UInt(32.W))
  })

  val regs = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))

  io.rdata1 := Mux(io.rs1 === 0.U, 0.U, regs(io.rs1))
  io.rdata2 := Mux(io.rs2 === 0.U, 0.U, regs(io.rs2))

  when(io.wen && io.waddr =/= 0.U) {
    regs(io.waddr) := io.wdata
  }
}

// ==========================================
// 5. 异步指令存储器 
// ==========================================
class AsyncInstMem extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(8.W))
    val inst = Output(UInt(32.W))
  })

  val initProg = Seq(
    "hfff00093".U, // 00: addi x1, x0, -1
    "h08102023".U, // 04: sw x1, 128(x0)
    "h08002103".U, // 08: lw x2, 128(x0)
    "hff810113".U, // 0C: addi x2, x2, -8
    "h08202223".U, // 10: sw x2, 132(x0)
    "h08402183".U, // 14: lw x3, 132(x0)
    "h08002203".U, // 18: lw x4, 128(x0)
    "h876542b7".U, // 1C: lui x5, 0x87654
    "h32128293".U, // 20: addi x5, x5, 0x321
    "h10502023".U, // 24: sw x5, 256(x0)
    "h10002303".U  // 28: lw x6, 256(x0)
  ) ++ Seq.fill(245)("h00000013".U) // NOP

  val mem = VecInit(initProg)
  io.inst := mem(io.addr)
}

// ==========================================
// 6. 异步数据存储器 (Data Memory)
// ==========================================
class AsyncDataMem extends Module {
  val io = IO(new Bundle {
    val addr  = Input(UInt(32.W)) // 字节地址
    val wdata = Input(UInt(32.W))
    val wen   = Input(Bool())     // 写使能
    val rdata = Output(UInt(32.W))
  })

  // 调整深度以完成FPGA
  val depth = 128
  val mem = RegInit(VecInit(Seq.fill(depth)(0.U(32.W))))

  val wordAddr = io.addr(8, 2)

  io.rdata := mem(wordAddr)

  when(io.wen) {
    mem(wordAddr) := io.wdata
  }
}

// ==========================================
// 7. 单周期 CPU 顶层 (结构化连线)
// ==========================================
class SingleCycleCPU extends Module {
  val io = IO(new Bundle {
    // 调试端口
    val zf          = Output(Bool())
    val of          = Output(Bool())
    val aluResult   = Output(UInt(32.W))
    val currentInst = Output(UInt(32.W))
    val currentPC   = Output(UInt(32.W))
    val isSignedOp  = Output(Bool())
    // 访存监控端口
    val dmemWen     = Output(Bool())
    val dmemAddr    = Output(UInt(32.W))
    val dmemWData   = Output(UInt(32.W))
    val dmemRData   = Output(UInt(32.W))
  })

  // 1. 实例化子模块
  val pcReg   = RegInit(0.U(32.W))
  val control = Module(new ControlUnit)
  val regFile = Module(new RegisterFile)
  val immGen  = Module(new ImmGen)
  val alu     = Module(new ALU)
  val imem    = Module(new AsyncInstMem)
  val dmem    = Module(new AsyncDataMem)

  // 2. PC 更新逻辑
  pcReg := pcReg + 4.U
  io.currentPC := pcReg

  // 3. 取指 (Fetch)
  imem.io.addr := pcReg(9, 2)
  val inst = imem.io.inst
  io.currentInst := inst

  // 4. 控制器连线
  control.io.opcode := inst(6, 0)
  control.io.funct3 := inst(14, 12)
  control.io.funct7 := inst(31, 25)

  // 5. 立即数生成
  immGen.io.inst := inst

  // 6. 寄存器堆读
  regFile.io.rs1   := inst(19, 15)
  regFile.io.rs2   := inst(24, 20)
  regFile.io.waddr := inst(11, 7)
  regFile.io.wen   := control.io.regWen

  // 7. ALU 操作数选择 Mux
  val src1 = MuxLookup(control.io.src1Sel, 0.U)(Seq(
    0.U -> regFile.io.rdata1,
    1.U -> pcReg,
    2.U -> 0.U
  ))

  val src2 = Mux(control.io.src2Sel === 1.U, immGen.io.out, regFile.io.rdata2)

  // 8. ALU 执行
  alu.io.a     := src1
  alu.io.b     := src2
  alu.io.aluOp := control.io.aluOp

  // 9. 访存
  dmem.io.addr  := alu.io.out
  dmem.io.wdata := regFile.io.rdata2
  dmem.io.wen   := control.io.memWen

  // 10. 写回
  // wbSel: 0 -> ALU, 1 -> Mem
  regFile.io.wdata := Mux(control.io.wbSel === 1.U, dmem.io.rdata, alu.io.out)

  // 11. 输出调试信号
  io.aluResult  := alu.io.out
  io.zf         := alu.io.zf
  io.of         := alu.io.of
  io.isSignedOp := control.io.isSigned
  
  // 输出访存调试信号
  io.dmemWen   := control.io.memWen
  io.dmemAddr  := alu.io.out
  io.dmemWData := regFile.io.rdata2
  io.dmemRData := dmem.io.rdata
}

// 生成 Verilog
object CPUGen extends App {
  emitVerilog(new SingleCycleCPU, Array("--target-dir", "generated"))
}