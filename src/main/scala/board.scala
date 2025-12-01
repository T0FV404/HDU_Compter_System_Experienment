import chisel3._
import chisel3.util._


// ==========================================
// 8. 辅助模块：按键消抖 (Button Debounce)
// ==========================================
class ButtonDebounce extends Module {
  val io = IO(new Bundle {
    val btnIn  = Input(Bool())
    val btnOut = Output(Bool()) // 输出一个周期的脉冲
  })

  // 假设主时钟 100MHz，消抖时间 20ms -> 2,000,000 个周期
  val CNT_MAX = 2000000.U
  val cnt     = RegInit(0.U(32.W))
  val stable  = RegInit(false.B)
  
  // 1. 跨时钟域同步 (打两拍)
  val sync0 = RegNext(io.btnIn, false.B)
  val sync1 = RegNext(sync0, false.B)

  // 2. 计数消抖
  when (sync1 === stable) {
    cnt := 0.U
  } .otherwise {
    cnt := cnt + 1.U
    when (cnt === CNT_MAX) {
      stable := sync1
      cnt := 0.U
    }
  }

  // 3. 上升沿检测 (Generating a single-cycle pulse)
  // 当稳定信号从 0 变 1 时，输出一个周期的高电平
  io.btnOut := stable && !RegNext(stable)
}

// ==========================================
// 9. 辅助模块：8位数码管驱动 (Seg7 Driver)
// ==========================================
class Seg7Driver extends Module {
  val io = IO(new Bundle {
    val dataIn = Input(UInt(32.W)) // 要显示的 32 位数据
    val seg    = Output(UInt(8.W)) // 段选 (A-G + DP)
    val an     = Output(UInt(8.W)) // 位选
  })

  // 1. 分频产生扫描时钟 (约 1kHz)
  // 100MHz / 100,000 = 1kHz
  val MAX_COUNT = 100000.U
  val timer     = RegInit(0.U(20.W))
  val tick      = Wire(Bool())

  when(timer === MAX_COUNT) {
    timer := 0.U
    tick  := true.B
  } .otherwise {
    timer := timer + 1.U
    tick  := false.B
  }

  // 2. 位选扫描计数器 (0-7)
  val digitSel = RegInit(0.U(3.W))
  when(tick) {
    digitSel := digitSel + 1.U
  }

  // 3. 当前位数据选择 (Hex Digit Selection)
  val currentHex = Wire(UInt(4.W))
  // 根据 digitSel 选择 dataIn 的哪 4 位
  currentHex := MuxLookup(digitSel, 0.U)(Seq(
    0.U -> io.dataIn(3, 0),
    1.U -> io.dataIn(7, 4),
    2.U -> io.dataIn(11, 8),
    3.U -> io.dataIn(15, 12),
    4.U -> io.dataIn(19, 16),
    5.U -> io.dataIn(23, 20),
    6.U -> io.dataIn(27, 24),
    7.U -> io.dataIn(31, 28)
  ))

  // 4. 段码译码 (Common Anode 共阳极: 0亮1灭)
  // 格式: DP, G, F, E, D, C, B, A
  val segDecoded = MuxLookup(currentHex, "b11111111".U)(Seq(
    0.U  -> "b11000000".U, // 0
    1.U  -> "b11111001".U, // 1
    2.U  -> "b10100100".U, // 2
    3.U  -> "b10110000".U, // 3
    4.U  -> "b10011001".U, // 4
    5.U  -> "b10010010".U, // 5
    6.U  -> "b10000010".U, // 6
    7.U  -> "b11111000".U, // 7
    8.U  -> "b10000000".U, // 8
    9.U  -> "b10010000".U, // 9
    10.U -> "b10001000".U, // A
    11.U -> "b10000011".U, // b
    12.U -> "b11000110".U, // C
    13.U -> "b10100001".U, // d
    14.U -> "b10000110".U, // E
    15.U -> "b10001110".U  // F
  ))

  io.seg := segDecoded

  // 5. 位选输出 (Active Low, 循环移位)
  // 比如 digitSel=0 -> an=11111110, digitSel=1 -> an=11111101
  io.an := ~(1.U(8.W) << digitSel)
}

// ==========================================
// 10. 板级顶层 (Board Top) - 对应 BoardTop.v
// ==========================================
class BoardTop extends Module {
  val io = IO(new Bundle {
    val sys_clk   = Input(Clock())  // 开发板 100MHz 晶振
    val rst_n_btn = Input(Bool())   // 复位按钮 (低电平有效)
    val step_btn  = Input(Bool())   // 单步按钮
    val sw        = Input(UInt(2.W))// 拨动开关
    
    val led       = Output(UInt(2.W)) // [1]=OF, [0]=ZF
    val seg       = Output(UInt(8.W)) // 段选
    val an        = Output(UInt(8.W)) // 位选
  })

  // 1. 处理复位信号 (转为高电平有效)
  val sys_reset = !io.rst_n_btn

  // 2. 实例化按键消抖
  val debouncer = withClockAndReset(io.sys_clk, sys_reset) {
    Module(new ButtonDebounce)
  }
  debouncer.io.btnIn := io.step_btn

  // 3. 实例化 CPU
  // 关键：使用 withClock 将 CPU 的时钟域切换为“消抖后的按键脉冲”
  // asClock 将 Bool 转换为 Clock 类型
  // 注意：在实际工程中，用组合逻辑驱动时钟(Gated Clock)是不推荐的，但作为教学实验单步调试是标准做法。
  val cpuClk = debouncer.io.btnOut.asClock

  val cpu = withClockAndReset(cpuClk, sys_reset) {
    Module(new SingleCycleCPU)
  }

  // 悬空 CPU 的其他输入（如果有的话，目前 CPU 只有内部存储器）
  
  // 4. LED 显示标志位
  // Cat(OF, ZF) -> LED[1:0]
  io.led := Cat(cpu.io.of, cpu.io.zf)

  // 5. 数据显示选择 (Mux)
  val displayData = MuxLookup(io.sw, 0.U)(Seq(
    0.U -> cpu.io.aluResult,   // 00: ALU 结果
    1.U -> cpu.io.dmemRData,   // 01: 内存读数据
    2.U -> cpu.io.dmemAddr,    // 10: 内存地址 (ALU Out)
    3.U -> cpu.io.currentInst  // 11: 当前指令
  ))

  // 6. 实例化数码管驱动
  // 数码管驱动需要使用高速时钟 (sys_clk) 进行扫描
  val segDriver = withClockAndReset(io.sys_clk, sys_reset) {
    Module(new Seg7Driver)
  }
  segDriver.io.dataIn := displayData
  
  io.seg := segDriver.io.seg
  io.an  := segDriver.io.an
}

// 生成 BoardTop 的 Verilog
object BoardTopGen extends App {
  emitVerilog(new BoardTop, Array("--target-dir", "generated"))
}