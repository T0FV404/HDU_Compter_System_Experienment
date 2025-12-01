import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SingleCycleCPUTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SingleCycleCPU Experiment 5"

  it should "execute memory instructions and display in HEX" in {
    test(new SingleCycleCPU).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      
      // 1. 复位
      c.reset.poke(true.B)
      c.clock.step(1)
      c.reset.poke(false.B)

      println("\n=============================================================================================================")
      println("                                   实验五：访存指令执行结果 (全 Hex 显示)")
      println("=============================================================================================================")
      // 表头：明确标注单位是 Hex
      println(f"| Cyc |    PC    |   Inst   | Disassembly          | ALU(Hex)   | Mem Wr? | WData(Hex) | RData(Hex) |")
      println(f"|-----|----------|----------|----------------------|------------|---------|------------|------------|")

      for (i <- 0 until 15) {
        // --- 读取硬件状态 ---
        val pc       = c.io.currentPC.peek().litValue
        val inst     = c.io.currentInst.peek().litValue.toLong
        val aluRes   = c.io.aluResult.peek().litValue
        val memWen   = c.io.dmemWen.peek().litToBoolean
        val memAddr  = c.io.dmemAddr.peek().litValue
        val memWData = c.io.dmemWData.peek().litValue
        val memRData = c.io.dmemRData.peek().litValue
        
        // --- 数据格式化 (统一用 %08x) ---
        // ALU 结果直接显示 8位 Hex
        val aluResStr = f"$aluRes%08x"
        
        // 访存状态
        val memWrStr   = if (memWen) "YES" else " - "
        
        // 如果写内存，显示写入数据的 Hex；否则显示占位符
        val memWDataStr = if (memWen) f"$memWData%08x" else "   -    "
        
        // 读出数据始终显示 Hex
        val memRDataStr = f"$memRData%08x"

        // --- 反汇编辅助显示 ---
        val asmStr = inst match {
          case 0xfff00093L => "addi x1, x0, -1"
          case 0x08102023L => "sw   x1, 128(x0)"
          case 0x08002103L => "lw   x2, 128(x0)"
          case 0xff810113L => "addi x2, x2, -8"
          case 0x08202223L => "sw   x2, 132(x0)"
          case 0x08402183L => "lw   x3, 132(x0)"
          case 0x08002203L => "lw   x4, 128(x0)"
          case 0x876542b7L => "lui  x5, 0x87654"
          case 0x32128293L => "addi x5, x5, 0x321"
          case 0x10502023L => "sw   x5, 256(x0)"
          case 0x10002303L => "lw   x6, 256(x0)"
          case 0x00000013L => "nop"
          case _           => "unknown"
        }

        // --- 打印 ---
        // %08x 表示：输出16进制，不足8位前面补0
        println(f"| $i%3d | $pc%08x | $inst%08x | $asmStr%-20s |  $aluResStr  | $memWrStr%7s |  $memWDataStr  |  $memRDataStr  |")

        c.clock.step(1)
      }
      println("=============================================================================================================\n")
    }
  }
}