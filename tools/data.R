for (f in list.files("tools/R", "*.R$", full.names = TRUE)) source(f)

set_proxy()

eplus_src <- file.path(Sys.getenv("USERPROFILE"), "Dropbox/github_repo/EnergyPlus")
update_internal_data(eplus_src, force = TRUE)

unset_proxy()

