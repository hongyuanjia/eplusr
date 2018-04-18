devtools::load_all()

# idd_8.0 <- parse_idd("V8-0-0-Energy+.idd")
# idd_8.1 <- parse_idd("V8-1-0-Energy+.idd")
# idd_8.2 <- parse_idd("V8-2-0-Energy+.idd")
# idd_8.3 <- parse_idd("V8-3-0-Energy+.idd")
# idd_8.4 <- parse_idd("V8-4-0-Energy+.idd")
idd_8.5 <- Idd$new("V8-5-0-Energy+.idd")
idd_8.6 <- Idd$new("V8-6-0-Energy+.idd")
idd_8.7 <- Idd$new("V8-7-0-Energy+.idd")
idd_8.8 <- Idd$new("V8-8-0-Energy+.idd")
idd_8.9 <- Idd$new("V8-9-0-Energy+.idd")
devtools::use_data(
    idd_8.5, idd_8.6, idd_8.7, idd_8.8, idd_8.9,
    internal = TRUE, overwrite = TRUE, compress = "xz"
)
