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
  - 数学函数: `sin`, `cos`, `tan`, `sqrt`, `exp`, `log`
- 语句
  - 赋值语句: `dst IS src;`
  - 循环语句: `FOR var FROM start TO end STEP stride body`

## 语言拓展

- 全局变量
  - `canvasSize`: 画布大小, 必须在第一次 `DRAW` 之前设置. 默认值为 1024 * 1024 像素.

- 语句
  - 赋值语句: 支持 C 风格赋值 `dst = src;`

## 构建说明

```bash
cabal build all
```

## 运行
