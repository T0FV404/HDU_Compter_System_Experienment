import chisel3._
import chisel3.util._

// ==========================================
// HDU-XL-01 专用数码管驱动模块
// ==========================================
class Seg7LEDCtrl_HDU extends Module {
  val io = IO(new Bundle {
    val data   = Input(UInt(32.W))  // 要显示的 32 位数据
    val seg    = Output(UInt(8.W))  // 段选 (CA-CG, DP)
    val which  = Output(UInt(3.W))  // 位选 (3:8 译码器输入)
    val enable = Output(Bool())     // 译码器使能
  })

  // 扫描计数器：20MHz / 20000 = 1kHz 扫描频率
  val scanCounter = RegInit(0.U(15.W))
  val digitSelect = RegInit(0.U(3.W))

  scanCounter := scanCounter + 1.U
  when(scanCounter === 19999.U) {
    scanCounter := 0.U
    digitSelect := digitSelect + 1.U
  }

  // 译码器控制
  io.enable := true.B
  io.which  := digitSelect

  // 数据切片映射 (从左到右显示 High -> Low)
  // TB7(左) -> TB0(右) 对应 data[31:28] -> data[3:0]
  val shiftAmount = (7.U - digitSelect) << 2
  val currentNibble = (io.data >> shiftAmount)(3, 0)

  // 段选译码 (共阳极: 0亮1灭)
  io.seg := MuxLookup(currentNibble, "hFF".U)(Seq(
    0x0.U -> "h03".U, 0x1.U -> "h9F".U, 0x2.U -> "h25".U, 0x3.U -> "h0D".U,
    0x4.U -> "h99".U, 0x5.U -> "h49".U, 0x6.U -> "h41".U, 0x7.U -> "h1F".U,
    0x8.U -> "h01".U, 0x9.U -> "h09".U, 0xA.U -> "h11".U, 0xB.U -> "hC1".U,
    0xC.U -> "h63".U, 0xD.U -> "h85".U, 0xE.U -> "h61".U, 0xF.U -> "h71".U
  ))
}

// ==========================================
// 板级顶层 BoardTop
// ==========================================
class BoardTop(sim: Boolean = false)(implicit config: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val sys_clk = Input(Clock())     // 20MHz 系统时钟 (H4)
    val rst_n   = Input(Bool())      // 复位按键 (低电平有效, R4)
    val sw      = Input(UInt(32.W))  // 32个拨码开关
    val swb     = Input(UInt(8.W))   // 8个按键
    
    val led     = Output(UInt(32.W)) // 32个LED
    val seg     = Output(UInt(8.W))  // 数码管段选
    val which   = Output(UInt(3.W))  // 数码管位选
    val enable  = Output(Bool())     // 数码管使能
  })

  // ----------------------------------------------------------
  // 1. 时钟分频逻辑 (产生慢速 CPU 时钟)
  // ----------------------------------------------------------
  val cpuClkReg = withClockAndReset(io.sys_clk, false.B) {
    // 仿真: 20MHz/10=2MHz, 上板: 20MHz/10000000=2Hz
    val countMax = if (sim) 10 else 10000000
    val counter = RegInit(0.U(32.W))
    val clkReg = RegInit(false.B)

    counter := counter + 1.U
    when(counter === (countMax - 1).U) {
      counter := 0.U
      clkReg := ~clkReg
    }
    clkReg
  }

  // ----------------------------------------------------------
  // 2. 实例化 CPU（使用慢时钟）
  // ----------------------------------------------------------
  val cpuReset = !io.rst_n  // 转换为高电平有效
  val cpu = withClockAndReset(cpuClkReg.asClock, cpuReset) {
    Module(new SingleCycleCPU)
  }

  // ----------------------------------------------------------
  // 3. 数据显示选择 (使用拨码开关 sw[2:0] 选择)
  // ----------------------------------------------------------
  val displayData = MuxLookup(io.sw(2, 0), cpu.io.debug.get.pc)(Seq(
    0.U -> cpu.io.debug.get.pc,       // 000: PC
    1.U -> cpu.io.debug.get.inst,     // 001: 指令
    2.U -> cpu.io.debug.get.aluOut,   // 010: ALU 结果
    3.U -> cpu.io.debug.get.memRData, // 011: 内存读数据
    4.U -> cpu.io.debug.get.memWData, // 100: 内存写数据
    5.U -> cpu.io.debug.get.memAddr,  // 101: 内存地址
    6.U -> Cat(Fill(31, 0.U), cpuClkReg),      // 110: 时钟心跳
    7.U -> Cat(Fill(31, 0.U), cpuReset.asUInt) // 111: 复位状态
  ))

  // ----------------------------------------------------------
  // 4. 数码管驱动（使用快时钟，不复位）
  // ----------------------------------------------------------
  val segDriver = withClockAndReset(io.sys_clk, false.B) {
    Module(new Seg7LEDCtrl_HDU)
  }
  
  segDriver.io.data := displayData
  io.seg    := segDriver.io.seg
  io.which  := segDriver.io.which
  io.enable := segDriver.io.enable

  // ----------------------------------------------------------
  // 5. LED 状态指示（功耗优化：仅点亮必要的 LED）
  // ----------------------------------------------------------
  io.led := Cat(
    Fill(28, 0.U),              // LED[31:4] 关闭
    cpu.io.debug.get.memWen,        // LED[3]: 内存写指示
    cpuReset.asUInt,            // LED[2]: 复位状态
    io.swb(0),                  // LED[1]: 按键状态
    cpuClkReg                   // LED[0]: CPU 时钟心跳
  )
}

// ==========================================
// 生成 Verilog
// ==========================================
object BoardGen extends App {
  implicit val config = new CPUConfig  // ✅ 使用 main.scala 中的定义
  
  emitVerilog(new BoardTop(sim = false), Array("--target-dir", "generated_board"))
  
  println("✅ 板级顶层模块生成完成！")
  println("📁 输出目录: generated_board/")
  println("📄 Verilog: BoardTop.v")
}

object BoardSimGen extends App {
  implicit val config = new CPUConfig
  
  emitVerilog(new BoardTop(sim = true), Array("--target-dir", "generated_sim"))
  println("✅ 仿真版本生成完成！")
}