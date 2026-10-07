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
  - 循环绘图语句: `FOR T FROM start TO end STEP stride body DRAW(x, y)`
- 内置函数
  - 数学函数: `sin`, `cos`, `tan`, `sqrt`, `exp`, `log`

## 语言拓展

- 全局变量
  - 添加 `canvasSize`: 画布大小, 必须在第一次 `DRAW` 之前设置. 默认值为 1024 * 1024 像素.
- 语句
  - 条件语句: 支持 C 风格的条件语句 `if COND { ... } else { ... }`
  - 赋值语句: 支持 C 风格赋值 `dst = src;`
  - 循环语句
    - 循环变量可以具有任意名称
    - 循环体可以是任意块语句, `draw(...)` 作为函数实现
- 内置函数
  - `draw(x, y)`: 执行变换并绘制点 (x, y)
  - `draw(x, y, r, g, b)`: 执行变换并以指定颜色 (r, g, b) 绘制点 (x, y)
  - `print(x)`: 打印 `x` 的值
  - `mod(x, y)`: 计算 `x % y`
  - `abs`, `floor`, `ceiling`
  - `min`/`max`: 计算 2 个 `f64` 之间的最小/最大大值
  - `clamp(x)`: 计算 `clamp(x, 0, 1)`
  - `clamp(x, amin, amax)`: 截断 `x` 到 `(amin, amax)` 之间
- 支持自定义函数

## 构建说明

```bash
cabal build
```

## 运行测试

```bash
cabal test
```
## 运行示例程序

以下代码使用了 `PhiPlot` 的所有拓展特性

- `example/ArchimedeanSpiral.phi`
- `example/activations.phi`

使用下面的命令运行它们:

```
cabal run PhiPlot -- -i example/activations.phi -o activations.png
cabal run PhiPlot -- -i example/ArchimedeanSpiral.phi -o ArchimedeanSpiral.png
```

以渲染图像。
