## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# the default output hook
hook_output = knitr::knit_hooks$get('output')
knitr::knit_hooks$set(output = function(x, options) {
  if (!is.null(n <- options$out.lines)) {
    x = unlist(stringr::str_split(x, '\n'))
    if (length(x) > n) {
      # truncate the output
      x = c(head(x, n), '....\n')
    }
    x = paste(x, collapse = '\n') # paste first n lines together
  }
  hook_output(x, options)
})

knitr::opts_knit$set(root.dir = tempdir())

## ----cran-install, eval = FALSE------------------------------------------
#  install.packages("eplusr")

## ----gh-installation, eval = FALSE---------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("hongyuanjia/eplusr")

## ----eplus-install, eval = FALSE-----------------------------------------
#  # install the latest version (currently v8.9.0)
#  eplusr::install_eplus("latest")
#  
#  # OR download the latest version (currently v8.9.0) and run the installer
#  # manually by yourself
#  eplusr::download_eplus("latest", dir = tempdir())

## ----install_eplus, include = FALSE--------------------------------------
if (!eplusr::is_avail_eplus(8.8)) eplusr::install_eplus(8.8)

## ----copy_example, include = FALSE---------------------------------------
library(eplusr)

cfg <- eplus_config(8.8)

example_name <- "5Zone_Transformer.idf"
weather_name <- "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
ddy_name <- "USA_CA_San.Francisco.Intl.AP.724940_TMY3.ddy"

path_example <- file.path(cfg$dir, "ExampleFiles", example_name)
path_weather <- file.path(cfg$dir, "WeatherData", weather_name)
path_ddy <- file.path(cfg$dir, "WeatherData", ddy_name)

file.copy(path_example, tempdir(), overwrite = TRUE)
file.copy(c(path_weather, path_ddy),
  file.path(tempdir(), c("San_Francisco.epw", "San_Francisco.ddy")), overwrite = TRUE)

## ----idd_dl, eval = FALSE------------------------------------------------
#  path_idd <- download_idd(8.8)
#  use_idd(path_idd)
#  
#  # OR
#  use_idd(8.8, download = TRUE)

## ----read, out.lines = 30------------------------------------------------
model <- read_idf(path = "5Zone_Transformer.idf", idd = NULL)

model

## ----idf_methods---------------------------------------------------------
setdiff(ls(model), "initialize")

## ----all_grp, out.lines = 14---------------------------------------------
model$group_name()

## ----all_cls, out.lines = 14---------------------------------------------
model$class_name()

## ----all_field-----------------------------------------------------------
def_mat <- model$definition(class = "Material")[[1]]
def_mat

## ----idd_obj-------------------------------------------------------------
idd <- use_idd(8.8)

idd$Material

# OR
# idd$object("Material")[[1]]

## ----iddobj_methods------------------------------------------------------
setdiff(ls(def_mat), "initialize")

## ----mat_def-------------------------------------------------------------
def_val <- def_mat$field_default()
def_val

## ----def_type------------------------------------------------------------
vapply(def_val, class, character(1))

## ----all_id, out.lines = 20----------------------------------------------
model$object_id(class = c("Material", "Construction"), simplify = FALSE)

## ----obj_nm--------------------------------------------------------------
model$object_name(class = c("Version", "Material", "Construction"), simplify = FALSE)

## ----obj_num-------------------------------------------------------------
model$object_num(c("BuildingSurface:Detailed", "Material", "Output:Variable"))

## ----obj-----------------------------------------------------------------
model$object(c("WD10", "ROOF-1"))

## ----obj_in_cls, out.lines = 30------------------------------------------
model$object_in_class("Material")

## ----obj_in_cls_shortcut_1, out.lines = 30-------------------------------
model$Material_NoMass
# OR
# model[["Material_NoMass"]]

## ----rp------------------------------------------------------------------
rp <- model$RunPeriod[[1]]
rp

## ----search_obj, out.lines = 20------------------------------------------
model$search_object("Demand", class = "Branch")

## ----idfobj_methods------------------------------------------------------
setdiff(ls(rp), "initialize")

## ----s3_obj--------------------------------------------------------------
rp$Begin_Day_of_Month

# OR
rp[["Begin_Day_of_Month"]]
rp[[3]]

## ----chain---------------------------------------------------------------
model$RunPeriod$WinterDay$Begin_Day_of_Month

## ----dup-----------------------------------------------------------------
model$dup_object(c("ROOF-1", "ROOF-1", "WALL-1"))

## ----add_obj-------------------------------------------------------------
model$add_object(rep("RunPeriod", 2),
  value = list(
    list("rp_test_1", 1, 1, 2, 1),

    list(name = "rp_test_2",
         begin_month = 3,
         begin_day_of_month = 1,
         end_month = 4,
         end_day_of_month = 1)
    ),
  comment = list(
    list("Comment for new object 1", "Another comment"),
    list("Comment for new object 2")),
  default = TRUE
)

## ----set_obj-------------------------------------------------------------
model$set_object("rp_test_1", list(name = "rp_test_3", begin_day_of_month = 2),
  comment = list(format(Sys.Date()), "begin day has been changed."))

## ----set_ref-------------------------------------------------------------
mat <- model$Material$CC03

# get other objects referencing this object
mat$ref_by_object()

mat$set_value(name = "CC03_renamed")

mat$ref_by_object()

## ----possible------------------------------------------------------------
mat$possible_value()

## ----ddy, warning=TRUE, out.lines = 20-----------------------------------
# read ddy file as normal IDF
ddy <- read_idf("San_Francisco.ddy", idd = 8.8)

model$ins_object(ddy$SizingPeriod_DesignDay)

## ----ref_by--------------------------------------------------------------
clng <- model$Material_NoMass$MAT_CLNG_1
clng$ref_by_object()

## ----del, error = TRUE---------------------------------------------------
eplusr_option("validate_level")
model$del_object("mat-clng-1")

## ----del_force-----------------------------------------------------------
eplusr_option(validate_level = "draft")
invisible(model$del_object("mat-clng-1", referenced = TRUE))

## ----valid, error = TRUE-------------------------------------------------
eplusr_option(validate_level = "final")
model$validate()

## ----save, eval = FALSE--------------------------------------------------
#  model$save(overwrite = TRUE)
#  
#  model$save("test.idf")

## ----avail_eplus---------------------------------------------------------
avail_eplus()

## ----use_eplus, eval = FALSE---------------------------------------------
#  use_eplus("C:/EnergyPlusV8-8-0")

## ----install, eval = FALSE-----------------------------------------------
#  install_eplus(ver = 8.9)

## ----epw-----------------------------------------------------------------
epw_sf <- read_epw("San_Francisco.epw")
epw_sf

## ----epw_method----------------------------------------------------------
setdiff(ls(epw_sf), "initialize")

## ----epw_data------------------------------------------------------------
epw_data <- epw_sf$get_data()
str(epw_data)

