for (f in list.files("tools/R", "*.R$", full.names = TRUE)) source(f)

eplus_src <- file.path(Sys.getenv("USERPROFILE"), "Dropbox/repos/EnergyPlus")
update_internal_data(eplus_src)
