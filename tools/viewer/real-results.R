# 在 R 控制台直接运行本文件。只读取已有 IDF / SQL，不运行 EnergyPlus。
# 本例为 Chicago 夏季设计日，24 个逐时结果，不代表全年气象模拟。
# Resolve source() relative to this file; Rscript may also be run from the package root.
viewer_source <- tryCatch(sys.frame(1)$ofile, error = function(error) NULL)
viewer_root <- if (length(viewer_source)) {
    normalizePath(file.path(dirname(viewer_source), "../.."))
} else {
    normalizePath(".")
}
# 本地三角剖分依赖放在 worktree 的 output/r-library，保留系统 R 库。
.libPaths(c(file.path(viewer_root, "output/r-library"), .libPaths()))
pkgload::load_all(viewer_root, quiet = TRUE)
example_dir <- file.path(viewer_root, "tools/viewer/examples/large-office")

# 使用与原模拟一致的 23.1 IDD；展示已有模型不需要安装或运行该版本引擎。
idf <- read_idf(
    file.path(example_dir, "in.idf"),
    idd = use_idd(file.path(example_dir, "Energy+.idd"))
)
sql <- eplus_sql(file.path(example_dir, "eplusout.sql"))

# 直接关联真实 SQLite 变量；provider 按当前时间读取结果帧。
# 本例来自归档文件，没有当前会话的 last_job，因此显式传入 sql。
# 若 idf 在当前会话已完成模拟，可用 viewer$results() 读取 idf$last_job()；
# 也可用 viewer$results(job)，直接传入已完成的 EplusJob。
viewer <- idf_viewer(idf)
viewer$results(
    sql,
    variable = "Zone Mean Air Temperature",
    reporting_frequency = "Hourly",
    environment_name = "CHICAGO ANN CLG .4% CONDNS WB=>MDB",
    mode = "provider"
)
widget <- viewer$widget(live = TRUE, height = "700px")

# 筛选顶层四个周边 Zone。去掉屋顶，方便看到各 Zone 内部的结果颜色。
zones <- paste0("Perimeter_top_ZN_", 1:4)
viewer$filter(zone = zones, type = c("wall", "window", "floor"))

# 一次选中两个 Zone 的全部可见表面；选中边界不会替换结果颜色。
viewer$select(zone = zones[c(1, 2)])
viewer$update("range_mode", value = "all")
viewer$update("time", frame = 5L) # 从 0 开始，第 6 帧为 06:00，便于观察 Zone 间差异。
viewer$update("fit_model")

# source() 后保留 viewer 和 widget，便于在同一 R 会话继续控制。
if (interactive()) {
    print(widget)
}

# Results 页可修改变量、环境、频率、结果键以及时间条件，再点 Apply result query。
# 初始 R 条件不是永久限制；Reset query 可以恢复查询该源的全部空间结果。
# 数据查询只决定结果着色；Objects 页和 filter() 决定哪些几何对象可见。
# R 端也支持 report_data() 的 name/key_value/year/tz/period/month/day/hour/
# minute/interval/simulation_days/day_type/environment_name/reporting_frequency。
# period 按指定日期或时刻匹配，两个端点不会自动展开为连续区间。
# 后续在控制台逐条运行：
# viewer$results(sql, name = "Zone Mean Air Temperature", hour = 8:18,
#     environment_name = "CHICAGO ANN CLG .4% CONDNS WB=>MDB", mode = "provider")
# viewer$filter(zone = zones[1])                # 只显示一个 Zone
# viewer$filter(zone = zones)                  # 显示四个 Zone
# viewer$select(zone = zones[c(1, 3)])          # 替换为另一组多选
# viewer$selection()                           # 读回浏览器实际选择
# viewer$update("time", frame = 17L)           # 切到第 18 帧
# viewer$update("play", speed = 2)             # 按时间播放着色
# viewer$update("pause")                       # 暂停
# viewer$filter()                              # 清除全部筛选
# viewer$close()                               # 查看结束后释放本地连接
