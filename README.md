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
- 语句
  - 赋值语句: `dst IS src;`
  - 循环语句: `FOR T FROM start TO end STEP stride body DRAW(x, y)`
- 内置函数
  - 数学函数: `sin`, `cos`, `tan`, `sqrt`, `exp`, `log`

## 语言拓展

- 全局变量
  - `canvasSize`: 画布大小, 必须在第一次 `DRAW` 之前设置. 默认值为 1024 * 1024 像素.
- 语句
  - 赋值语句: 支持 C 风格赋值 `dst = src;`
  - 循环语句
    - 循环变量可以具有任意名称
    - 循环体可以是任意块语句
- 内置函数
  - `draw(x, y)` 实现为普通函数
  - `print(x)` 打印 `x` 的值
- 支持自定义函数

## 构建说明

```bash
cabal build all
```

## 运行
