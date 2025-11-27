import chisel3._
import chisel3.util._

// ==========================================
// 1. ALU 操作码枚举（ChiselEnum, Chisel 7 用法）
// ==========================================
object ALUOps extends ChiselEnum {
  val ADD  = Value
  val SLL  = Value
  val SLT  = Value
  val SLTU = Value
  val XOR  = Value
  val SRL  = Value
  val OR   = Value
  val AND  = Value
  val SUB  = Value
  val SRA  = Value
  val ERROR = Value
}

// ==========================================
// 1. ALU 模块
// ==========================================
class ALU extends Module {
  val io = IO(new Bundle {
    val a      = Input(UInt(32.W))        // 操作数 A
    val b      = Input(UInt(32.W))        // 操作数 B
    val aluOp  = Input(ALUOps())          // 注意：Chisel 7 用 ALUOps()
    val out    = Output(UInt(32.W))       // 运算结果
    val zf     = Output(Bool())           // Zero Flag
    val of     = Output(Bool())           // Overflow Flag（ADD/SUB）
  })

  val shamt = io.b(4, 0)                  // 移位量
  val res   = WireDefault(0.U(32.W))

  switch(io.aluOp) {
    is(ALUOps.ADD)  { res := io.a + io.b }
    is(ALUOps.SUB)  { res := io.a - io.b }
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

  // 溢出（有符号 ADD/SUB）
  val signA = io.a(31)
  val signB = io.b(31)
  val signR = res(31)

  io.of := false.B
  when (io.aluOp === ALUOps.ADD) {
    io.of := (signA === signB) && (signR =/= signA)
  } .elsewhen (io.aluOp === ALUOps.SUB) {
    io.of := (signA =/= signB) && (signR =/= signA)
  }
}

// ==========================================
// 2. 寄存器堆
// ==========================================
class RegisterFile extends Module {
  val io = IO(new Bundle {
    val rs1    = Input(UInt(5.W))
    val rs2    = Input(UInt(5.W))
    val waddr  = Input(UInt(5.W))
    val wdata  = Input(UInt(32.W))
    val wen    = Input(Bool())
    val rdata1 = Output(UInt(32.W))
    val rdata2 = Output(UInt(32.W))
  })

  val regs = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))

  when (io.wen && io.waddr =/= 0.U) {
    regs(io.waddr) := io.wdata
  }

  io.rdata1 := Mux(io.rs1 === 0.U, 0.U, regs(io.rs1))
  io.rdata2 := Mux(io.rs2 === 0.U, 0.U, regs(io.rs2))
}

// ==========================================
// 3. 异步指令存储器 (ROM)
// ==========================================
class AsyncInstMem extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(8.W))   // 字地址
    val inst = Output(UInt(32.W))
  })

  val initProg = Seq(
    "h123450b7".U, // 00: lui x1, 0x12345
    "h67808093".U, // 04: addi x1, x1, 0x678
    "h87654137".U, // 08: lui x2, 0x87654
    "h32110113".U, // 0C: addi x2, x2, 0x321
    "h002081b3".U, // 10: add x3, x1, x2
    "h40218233".U, // 14: sub x4, x3, x2
    "h111112b7".U, // 18: lui x5, 0x11111
    "h11128293".U, // 1C: addi x5, x5, 0x111
    "h0042f333".U, // 20: and x6, x5, x4
    "h0042e3b3".U, // 24: or  x7, x5, x4
    "h0042c433".U, // 28: xor x8, x5, x4
    "h0042b4b3".U, // 2C: sltu x9, x5, x4
    "h00629533".U, // 30: sll x10, x5, x6
    "hfff00593".U, // 34: addi x11, x0, -1
    "h0ff5f613".U, // 38: andi x12, x11, 0xff
    "h0ff5c693".U, // 3C: xori x13, x11, 0xff
    "h00359713".U, // 40: slli x14, x11, 3
    "h00500793".U, // 44: addi x15, x0, 5
    "h0ff7a813".U, // 48: slti x16, x15, 0xff
    "h00000897".U  // 4C: auipc x17, 0
  ) ++ Seq.fill(236)("h00000013".U) // NOP

  val mem = VecInit(initProg)
  io.inst := mem(io.addr)
}

// ==========================================
// 4. 单周期 CPU 顶层
// ==========================================
class SingleCycleCPU extends Module {
  val io = IO(new Bundle {
    val zf          = Output(Bool())
    val of          = Output(Bool())
    val aluResult   = Output(UInt(32.W))
    val aluSignResult = Output(SInt(32.W))
    val currentInst = Output(UInt(32.W))
    val currentPC   = Output(UInt(32.W))
  })

  // 1. PC 与取指
  val pc     = RegInit(0.U(32.W))
  val pcNext = pc + 4.U
  pc := pcNext

  val imem = Module(new AsyncInstMem)
  imem.io.addr := pc(9, 2) //存储器容量为2**8  pc转化为存储器下标需要除4（右移2位)
  val inst = imem.io.inst

  io.currentInst := inst
  io.currentPC   := pc

  // 2. 译码
  val opcode = inst(6, 0)
  val rd     = inst(11, 7)
  val funct3 = inst(14, 12)
  val rs1    = inst(19, 15)
  val rs2    = inst(24, 20)
  val funct7 = inst(31, 25)

  val immI = Cat(Fill(20, inst(31)), inst(31, 20)).asSInt
  val immU = Cat(inst(31, 12), 0.U(12.W))

  val isRType = (opcode === "b0110011".U)
  val isIType = (opcode === "b0010011".U)
  val isLUI   = (opcode === "b0110111".U)
  val isAUIPC = (opcode === "b0010111".U)

  // Chisel 7：枚举类型的 Wire 这样声明
  val aluOp = Wire(ALUOps())
  aluOp := ALUOps.ERROR

  // 3. 生成 aluOp
  when (isRType) {
    switch (funct3) {
      is ("b000".U) { // ADD / SUB
        when (funct7 === "b0100000".U) {
          aluOp := ALUOps.SUB
        } .otherwise {
          aluOp := ALUOps.ADD
        }
      }
      is ("b001".U) { aluOp := ALUOps.SLL  }
      is ("b010".U) { aluOp := ALUOps.SLT  }
      is ("b011".U) { aluOp := ALUOps.SLTU }
      is ("b100".U) { aluOp := ALUOps.XOR  }
      is ("b110".U) { aluOp := ALUOps.OR   }
      is ("b111".U) { aluOp := ALUOps.AND  }
    }
  } .elsewhen (isIType) {
    switch (funct3) {
      is ("b000".U) { aluOp := ALUOps.ADD } // addi
      is ("b001".U) { aluOp := ALUOps.SLL } // slli
      is ("b010".U) { aluOp := ALUOps.SLT } // slti
      // 其他 I-type 逻辑可继续扩展
    }
  } .elsewhen (isLUI || isAUIPC) {
    aluOp := ALUOps.ADD // LUI: 0+immU, AUIPC: PC+immU
  } .otherwise {
    aluOp := ALUOps.ERROR
  }

  // 4. 寄存器堆 & 操作数
  val regFile = Module(new RegisterFile)
  regFile.io.rs1   := rs1
  regFile.io.rs2   := rs2
  regFile.io.waddr := rd
  regFile.io.wen   := true.B

  val src1 = Wire(UInt(32.W))
  val src2 = Wire(UInt(32.W))

  when (isAUIPC) {
    src1 := pc
  } .elsewhen (isLUI) {
    src1 := 0.U
  } .otherwise {
    src1 := regFile.io.rdata1
  }

  when (isRType) {
    src2 := regFile.io.rdata2
  } .elsewhen (isLUI || isAUIPC) {
    src2 := immU
  } .otherwise {
    src2 := immI.asUInt
  }

  // 5. ALU 执行
  val alu = Module(new ALU)
  alu.io.a     := src1
  alu.io.b     := src2
  alu.io.aluOp := aluOp

  // 6. 写回
  regFile.io.wdata := alu.io.out

  io.aluResult := alu.io.out
  io.zf        := alu.io.zf
  io.of        := alu.io.of
  io.aluSignResult := alu.io.out.asSInt
}

// ==========================================
// 5. 生成 Verilog
// ==========================================
object CPUGen extends App {
  emitVerilog(new SingleCycleCPU, Array(
    "--target-dir", "generated",
    ))
}
