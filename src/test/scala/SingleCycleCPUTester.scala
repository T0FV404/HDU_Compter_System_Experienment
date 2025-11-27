import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SingleCycleCPUTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SingleCycleCPU"

  it should "execute instructions and print trace" in {
    test(new SingleCycleCPU).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      // 1. 初始化与复位
      c.reset.poke(true.B)
      c.clock.step(1)
      c.reset.poke(false.B)

      // 2. 打印表头
      println("\n==========================================================================================")
      println("Cycle |    PC    |   Inst   | ALU Res(Hex) | ALU Res(Dec) | ZF | OF | Inst Disasm (Ref)")
      println("------+----------+----------+--------------+--------------+----+----+------------------")

      // 3. 运行 25 个周期 (覆盖所有测试指令)
      for (i <- 0 until 25) {
        val pc   = c.io.currentPC.peek().litValue
        // 【修改点】在这里加上 .toLong，把 BigInt 转为 Long，这样才能和下面的 0x...L 匹配
        val inst = c.io.currentInst.peek().litValue.toLong 
        val res  = c.io.aluResult.peek().litValue
        val zf   = c.io.zf.peek().litValue
        val of   = c.io.of.peek().litValue

        // 【关键步骤】将无符号大整数 (BigInt) 转换为 32位有符号整数 (Int)
        val resSigned = res.toInt

        // 简单的反汇编注释，方便对照
        val asmHelp = inst match {
          case 0x123450b7L => "lui x1, 0x12345"
          case 0x67808093L => "addi x1, x1, 0x678"
          case 0x87654137L => "lui x2, 0x87654"
          case 0x32110113L => "addi x2, x2, 0x321"
          case 0x002081b3L => "add x3, x1, x2"
          case 0x40218233L => "sub x4, x3, x2"
          case 0x111112b7L => "lui x5, 0x11111"
          case 0x11128293L => "addi x5, x5, 0x111"
          case 0x0042f333L => "and x6, x5, x4"
          case 0x0042e3b3L => "or x7, x5, x4"
          case 0x0042c433L => "xor x8, x5, x4"
          case 0x0042b4b3L => "sltu x9, x5, x4"
          case 0x00629533L => "sll x10, x5, x6"
          case 0xfff00593L => "addi x11, x0, -1"     // 重点关注这里，应显示 -1
          case 0x0ff5f613L => "andi x12, x11, 0xff"
          case 0x0ff5c693L => "xori x13, x11, 0xff"
          case 0x00359713L => "slli x14, x11, 3"
          case 0x00500793L => "addi x15, x0, 5"
          case 0x0ff7a813L => "slti x16, x15, 0xff"
          case 0x00000897L => "auipc x17, 0"
          case 0x00000013L => "nop"
          case _           => "unknown"
        }

        // 格式化打印
        println(f"$i%5d | $pc%08x | $inst%08x |   $res%08x   | $resSigned%12d | $zf  | $of  | $asmHelp")

        c.clock.step(1)
      }
      println("==========================================================================================\n")
    }
  }
}