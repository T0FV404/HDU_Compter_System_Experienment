# 警告
这是杭州电子科技大学机组实验源码，如果你拿去用，要注意是否是同一个实验，总之后果自负。

# 运行

运行测试代码：`sbt "testOnly SingleCycleCPUTester"`

只编译main文件下的代码：`sbt run`

如果你想在vivado里运行我们生成的sv代码:请一定要将他们在vivado里设置为`System Verilog`

(我们vivado里的代码都是 sv)
