for (f in list.files("tools/R", "*.R$", full.names = TRUE)) source(f)

eplus_src <- Sys.getenv("ENERGYPLUS_SRC")
update_internal_data(eplus_src)
