PhiPlot
========

**PhiPlot**是为完成XDU编译原理课程设计之要求而实现的一个的简易绘图语言。

PhiPlot的目标特性如下：

+ IS语句作为赋值语句，未声明变量就地声明，同时
+ SCLAE, ROT, ORIGIN实现为预定义变量，以兼容原有语法
+ FOR语句支持嵌套
+ 支持代码块
+ 支持过程定义
+ DRAW**在PhiPlot中是一个接受两个参数的内建函数**
+ 可选的IF语句支持
+ 可选的自定义变量支持
+ 可选的字节码编译器&虚拟机
+ LLVM based JIT (under developing)

> **PhiPlot**的分析综合模块和字节码编译器仅在GHC 7.8.3 下测试通过。
> **PhiPlot**的虚拟机可由完整实现C11标准的编译器编译，但不保证对Windows平台的兼容性。
