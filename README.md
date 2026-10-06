# PhiPlot

## Overview

**PhiPlot** 是为完成 XDU 编译原理课程设计之要求而实现的一个的简易绘图语言。

## 语言特性

- 不区分大小写
- 内置类型
  - 标量类型 `f64`
  - 向量类型 `f64x2`
- 全局变量
  - `scale`
  - `rot`
  - `origin`
- 内置函数
  - `draw(x, y)` 绘制点 `(x, y)` 相对于原点 `origin` 缩放 `scale`, 和旋转 `rot` 之后的结果
  - 数学函数: `sin`, `cos`, `sqrt`, `exp`, `ln`
- 语句
  - 赋值语句: `dst IS src;`
  - 循环语句: `FOR var FROM start TO end STEP stride expr`

## 构建说明

```bash
cabal build all
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j
```
