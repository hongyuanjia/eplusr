# 真实 EnergyPlus 结果示例

这份副本来自本机已有的 `large_office_shared_wall_offset/source` 完成记录。IDF 与 SQLite 取自同一运行目录，没有重新运行模拟或改写结果。

- 引擎：EnergyPlus 23.1.0-87ed9199d4；原运行时间 2026-09-30。
- 模型：19 个 Zone，显示时生成 142 个几何对象。
- 变量：`Zone Mean Air Temperature`，逐时，单位 °C。
- 本例时段：Chicago 夏季设计日 `CHICAGO ANN CLG .4% CONDNS WB=>MDB`，7 月 21 日 01:00–24:00，共 24 帧。
- 该时段全模型结果范围：22.24580998–26.59466026 °C。
- SQL 还含冬季设计日，示例显式选取夏季，避免混合两个工况。这里不是全年气象结果。

原日志记录模拟成功完成、4 条 Warning、0 条 Severe；请查看 `eplusout.err`。这些数值用于演示已有模拟结果的可视化，不构成实测验证。`provenance.json` 保存源路径和逐文件 SHA-256，复制后的 SQL、IDF、IDD 与原文件字节一致。

`Energy+.idd` 是与输入版本一致的 23.1 字典；因此只展示几何和结果时，不需要安装 23.1 引擎。该 IDD 来自本机 replus 的对应版本字典夹具。EnergyPlus 输入字典和示例模型的来源信息保留在原文件中。

在包源码根目录的 R 控制台运行：

```r
source("tools/viewer/real-results.R")
```

脚本会加载此 worktree，打开连接当前 R 会话的 htmlwidget，筛选顶层四个周边 Zone，选中其中两个，并显示第 6 个逐时结果（06:00）。对象保留为 `viewer`、`widget`、`idf`、`sql` 和 `zones`，可继续操作。浏览器工具面板的 Select 选为 Zones 后，Shift 点击可增加或取消 Zone 选择。

```r
viewer$filter(zone = zones[1])            # 只显示一个 Zone
viewer$filter(zone = zones)              # 显示四个 Zone
viewer$select(zone = zones[c(1, 3)])      # 选中两个 Zone 的表面
viewer$selection()                       # 读回浏览器实际选择
viewer$update("time", frame = 17L)       # 第 18 帧，索引从 0 开始
viewer$update("play", speed = 2)
viewer$update("pause")
viewer$filter()                          # 清除全部筛选
viewer$close()
```

命令异步执行；`viewer$state()` 的 `sent` 和 `acknowledged` 相等时，浏览器已完成此前命令。运行后保持该 R 会话开启，provider 才能继续按需读取 SQL 结果帧。

脚本优先使用源码目录中的 `output/r-library`（若存在）。首次运行时，可将依赖安装到这个专用目录：

```r
viewer_root <- normalizePath(".")
viewer_lib <- file.path(viewer_root, "output/r-library")
dir.create(viewer_lib, recursive = TRUE, showWarnings = FALSE)
install.packages(
    c("pkgload", "htmlwidgets", "decido", "httpuv", "later"),
    lib = viewer_lib,
    repos = "https://cloud.r-project.org"
)
```

数值和交互检查记录位于 worktree 的 `output/playwright/real-results-20261018/`。数据副本在 `tools/`，不会进入 R 包构建。
