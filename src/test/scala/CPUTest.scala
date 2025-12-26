import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CPUTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "PipelineCPU"

  // 1. 初始化配置
  implicit val config = new CPUConfig()
  config.enableDebug = true // 确保开启 Debug 端口以观察结果

  it should "run the pipeline correctly for R-Type instructions" in {
    // 2. 启动仿真
    // WriteVcdAnnotation 可以在 generated 目录下生成波形文件，用于 GTKWave 查看
    test(new PipelineCPU).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      
      println("--- Start Simulation ---")
      
      // 初始化：复位
      c.reset.poke(true.B)
      c.clock.step(1)
      c.reset.poke(false.B)

      // 3. 运行多个周期
      // 这里的 20 个周期足够跑完你 InstructionMemory 里的那 4 条指令
      // 流水线填满需要 5 个周期，随后每个周期出一条结果
      for (i <- 0 until 50) {
        // 获取 Debug 端口的数据
        val pc      = c.io.diffTest.get.pc.peek().litValue
        val wbValid = c.io.diffTest.get.wbValid.peek().litValue
        val wbData  = c.io.diffTest.get.wbData.peek().litValue
        
        // 只有当写回有效时（wbValid=1），才打印结果
        if (wbValid == 1) {
          println(f"Cycle $i: PC=0x$pc%x | WB_Data=$wbData%d (0x$wbData%x)")
        } else {
          // 可选：打印正在填充流水线的空周期
          // println(f"Cycle $i: Pipeline filling/stalled...")
        }

        // 推进一个时钟周期
        c.clock.step(1)
      }
      
      println("--- End Simulation ---")
    }
  }
}