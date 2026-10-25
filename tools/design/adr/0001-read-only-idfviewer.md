---
status: accepted
date: 2026-10-18
---

# 只读 IDF 查看器使用 Three.js 与 htmlwidgets

查看器只显示已有 IDF 几何，提供对象、Zone、Space 筛选以及已有模拟结果着色。用户确认不在此界面编辑模型，不提供独立 App 或模拟运行入口。因此从主线移入只读 Three.js/htmlwidgets 实现，不引入 bemgeo、OCCT、几何编辑器及 App 的依赖。

R 函数生成 htmlwidget 是主要入口。用户允许按需启动由当前 R 会话管理的轻量本地通信服务，用于更新已打开视图、回读网页选择和读取结果帧。静态 HTML 与报告仍可在没有 R 服务时独立使用；相机、筛选与其他即时交互留在浏览器。

保留原 eplusr 的模型、模拟及结果读取 API，本次不扩大其职责。查看器不以 mesh 身份代替模型对象身份，不把 Zone 数值的表面着色解释为内部空间分布。
