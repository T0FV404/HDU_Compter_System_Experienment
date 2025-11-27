import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class NegativeTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SingleCycleCPU Negative Display"

  it should "show negative values correctly" in {
    test(new SingleCycleCPU).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      
      // 1. 复位 CPU
      c.reset.poke(true.B)
      c.clock.step(1)
      c.reset.poke(false.B)

      println("\n================ 负数结果展示测试 ================")

      // 我们知道代码中有一条指令：addi x11, x0, -1
      // 它的机器码是 0xfff00593，大约在第 14 个周期执行
      
      var found = false
      
      for (cycle <- 0 until 20) {
        val pc   = c.io.currentPC.peek().litValue
        val inst = c.io.currentInst.peek().litValue
        val res  = c.io.aluResult.peek().litValue

        // 检测是否执行到了 addi x11, x0, -1
        if (inst == 0xfff00593L) {
          found = true
          println(f"[Cycle $cycle] 捕获到指令: addi x11, x0, -1 (PC=0x$pc%x)")
          println(s"--------------------------------------------------")
          
          // === 关键步骤：数据类型转换 ===
          
          // 1. 硬件原始值 (UInt -> BigInt)
          // 这是 Chisel peek() 默认返回的类型，它永远是正数
          val unsignedBigInt = res 
          
          // 2. 转换为 32位有符号整数 (Int)
          // Scala 的 .toInt 方法会把 32位数据按补码解释
          val signedInt = res.toInt 

          // 3. 打印对比
          println(f"1. 硬件原始十六进制 (Hex)      : 0x$unsignedBigInt%08x")
          println(f"2. 默认显示 (Unsigned BigInt)  : $unsignedBigInt")
          println(f"3. 转换后显示 (Signed Int)     : $signedInt  <-- 看这里！")
          
          println(s"--------------------------------------------------")
          
          // 验证结果是否正确
          assert(signedInt == -1, "错误：结果应该是 -1")
          assert(unsignedBigInt == BigInt("FFFFFFFF", 16), "错误：十六进制应该是 FFFFFFFF")
        }

        c.clock.step(1)
      }
      
      if (!found) {
        println("错误：未能在前 20 个周期内找到测试指令！")
      }
      println("================ 测试结束 ================\n")
    }
  }
}