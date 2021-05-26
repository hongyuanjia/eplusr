# Epw Header {{{
test_that("Epw Header", {
    # IDD
    expect_is(idd <- get_epw_idd(), "Idd")

    # PARSE {{{
    expect_error(parse_epw_header("Wrong\n"), class = "eplusr_error_parse_epw_header_name")
    expect_error(parse_epw_header("LOCATION,;;\n"), class = "eplusr_error_parse_epw_header_line")
    expect_error(parse_epw_header(paste0("LOCATION", strrep(",", 11), "\n")), class = "eplusr_error_parse_epw_header_field")

    # can stop if missing header
    expect_is(err <- catch_cnd(parse_epw_header("LOCATION\n")), "eplusr_error_validity_check")
    expect_equal(err$data$missing_object, c(
        "DESIGN CONDITIONS",
        "TYPICAL/EXTREME PERIODS",
        "GROUND TEMPERATURES",
        "HOLIDAYS/DAYLIGHT SAVINGS",
        "COMMENTS 1",
        "COMMENTS 2",
        "DATA PERIODS"
    ))

    # can stop if invalid type
    expect_is(err <- catch_cnd(parse_epw_header("DESIGN CONDITIONS,a\n")), "eplusr_error_validity_check")
    expect_equal(err$data$invalid_numeric$class_name, "DESIGN CONDITIONS")
    expect_equal(err$data$invalid_numeric$field_index, 1L)

    # can fill "0" for empty headers
    expect_is(class = "list",
        h <- parse_epw_header(
            "
            LOCATION,city,state,country,type,wmo,1,2,3,4
            DESIGN CONDITIONS
            TYPICAL/EXTREME PERIODS
            GROUND TEMPERATURES
            HOLIDAYS/DAYLIGHT SAVINGS,yes,0,0,0
            COMMENTS 1
            COMMENTS 2
            DATA PERIODS,1,1, Data, Friday, 2016/01/01, 2016/12/31
            "
        )
    )
    expect_equal(h$value[object_id %in% c(2, 3, 4), value_num], rep(0, 3))
    expect_equal(h$value[object_id %in% c(6, 7), value_chr], rep(NA_character_, 2))

    get_idf_value(get_epw_idd_env(), h, EPW_CLASS[[paste0("comment", 1)]])

    # can fix mismatched extensible group and value of number field
    expect_warning(
        {
            DC <- function (n = 1, m = n) {
                htg <- c("heating", 1:15)
                clg <- c("cooling", 1:32)
                ext <- c("extremes", 1:16)
                grp <- paste0(rep(c(htg, clg, ext), m), collapse = ",")
                paste("DESIGN CONDITIONS", n, "src", "", grp, sep = ",", collapse = ",")
            }

            h <- parse_epw_header(paste0(
                "
                LOCATION,city,state,country,type,wmo,1,2,3,4
                ", DC(0, 1), "
                TYPICAL/EXTREME PERIODS,0,period,typical,1/1,1/2
                GROUND TEMPERATURES,0,0.5,,,,", paste0(1:12, collapse = ","), "
                HOLIDAYS/DAYLIGHT SAVINGS,yes,0,0,0,New year,1/1
                COMMENTS 1
                COMMENTS 2
                DATA PERIODS,1,1,Data,Friday,2016/01/01,2016/12/31,Data1,Friday,2017/01/01,2017/12/31
                "
            ))
        },
        "Number of Design Conditions"
    )
    expect_equal(h$value[object_id == 2, value_num][1], 1L)
    expect_equal(h$value[object_id == 3, value_num][1], 1L)
    expect_equal(h$value[object_id == 4, value_num][1], 1L)
    expect_equal(h$value[object_id == 5, value_num][4], 1L)
    expect_equal(h$value[object_id == 8, value_num][1], 2L)

    # can stop if invalid EpwDate
    expect_error(
        parse_epw_header(paste0(
            "
            LOCATION,city,state,country,type,wmo,1,2,3,4
            DESIGN CONDITIONS
            TYPICAL/EXTREME PERIODS,1,period,typical,a,1/2
            GROUND TEMPERATURES
            HOLIDAYS/DAYLIGHT SAVINGS,yes,0,0,0
            COMMENTS 1
            COMMENTS 2
            DATA PERIODS,1,1,Data,Friday,2016/01/01,2016/12/31
            "
        )),
        class = "eplusr_error_parse_epw"
    )
    expect_error(
        parse_epw_header(paste0(
            "
            LOCATION,city,state,country,type,wmo,1,2,3,4
            DESIGN CONDITIONS
            TYPICAL/EXTREME PERIODS,1,period,typical,1/1,a
            GROUND TEMPERATURES
            HOLIDAYS/DAYLIGHT SAVINGS,yes,0,0,0
            COMMENTS 1
            COMMENTS 2
            DATA PERIODS,1,1,Data,Friday,2016/01/01,2016/12/31
            "
        )),
        class = "eplusr_error_parse_epw"
    )
    expect_error(
        parse_epw_header(paste0(
            "
            LOCATION,city,state,country,type,wmo,1,2,3,4
            DESIGN CONDITIONS
            TYPICAL/EXTREME PERIODS
            GROUND TEMPERATURES
            HOLIDAYS/DAYLIGHT SAVINGS,yes,1/1,0,0
            COMMENTS 1
            COMMENTS 2
            DATA PERIODS,1,1,Data,Friday,2016/01/01,2016/12/31
            "
        )),
        class = "eplusr_error_parse_epw"
    )
    expect_error(
        parse_epw_header(paste0(
            "
            LOCATION,city,state,country,type,wmo,1,2,3,4
            DESIGN CONDITIONS
            TYPICAL/EXTREME PERIODS
            GROUND TEMPERATURES
            HOLIDAYS/DAYLIGHT SAVINGS,yes,0,0,1,new year, 2020/01/01
            COMMENTS 1
            COMMENTS 2
            DATA PERIODS,1,1,Data,Friday,2016/01/01,2016/12/31
            "
        )),
        class = "eplusr_error_parse_epw"
    )
    expect_error(
        parse_epw_header(paste0(
            "
            LOCATION,city,state,country,type,wmo,1,2,3,4
            DESIGN CONDITIONS
            TYPICAL/EXTREME PERIODS
            GROUND TEMPERATURES
            HOLIDAYS/DAYLIGHT SAVINGS,yes,0,0,0
            COMMENTS 1
            COMMENTS 2
            DATA PERIODS,1,7,Data,Friday,2016/01/01,2016/12/31
            "
        )),
        class = "eplusr_error_parse_epw"
    )
    expect_error(
        parse_epw_header(paste0(
            "
            LOCATION,city,state,country,type,wmo,1,2,3,4
            DESIGN CONDITIONS
            TYPICAL/EXTREME PERIODS
            GROUND TEMPERATURES
            HOLIDAYS/DAYLIGHT SAVINGS,yes,0,0,0
            COMMENTS 1
            COMMENTS 2
            DATA PERIODS,2,1,Data,Friday,2016/01/01,2016/1/31,Data,Friday,2016/2/01,2016/12/31
            "
        )),
        class = "eplusr_error_parse_epw"
    )
    expect_error(
        parse_epw_header(paste0(
            "
            LOCATION,city,state,country,type,wmo,1,2,3,4
            DESIGN CONDITIONS
            TYPICAL/EXTREME PERIODS
            GROUND TEMPERATURES
            HOLIDAYS/DAYLIGHT SAVINGS,yes,0,0,0
            COMMENTS 1
            COMMENTS 2
            DATA PERIODS,1,1,Data,Friday,2nd Mon in December,2016/12/31
            "
        )),
        class = "eplusr_error_parse_epw"
    )
    expect_error(
        parse_epw_header(paste0(
            "
            LOCATION,city,state,country,type,wmo,1,2,3,4
            DESIGN CONDITIONS
            TYPICAL/EXTREME PERIODS
            GROUND TEMPERATURES
            HOLIDAYS/DAYLIGHT SAVINGS,yes,0,0,0
            COMMENTS 1
            COMMENTS 2
            DATA PERIODS,1,1,Data,Friday,12/1,0
            "
        )),
        class = "eplusr_error_parse_epw"
    )
    expect_warning(
        h <- parse_epw_header(paste0(
            "
            LOCATION,city,state,country,type,wmo,1,2,3,4
            DESIGN CONDITIONS
            TYPICAL/EXTREME PERIODS
            GROUND TEMPERATURES
            HOLIDAYS/DAYLIGHT SAVINGS,yes,0,0,0
            COMMENTS 1
            COMMENTS 2
            DATA PERIODS,1,1,Data,Friday,2016/01/01,12/31
            "
        ))
    )
    expect_equal(h$value[object_id == 8L, value_chr][6], "2016/12/31")
    expect_error(
        parse_epw_header(paste0(
            "
            LOCATION,city,state,country,type,wmo,1,2,3,4
            DESIGN CONDITIONS
            TYPICAL/EXTREME PERIODS
            GROUND TEMPERATURES
            HOLIDAYS/DAYLIGHT SAVINGS,yes,0,0,0
            COMMENTS 1
            COMMENTS 2
            DATA PERIODS,1,1,Data,Friday,2017/01/01,2/29
            "
        )),
        class = "eplusr_error_parse_epw"
    )
    expect_warning(
        h <- parse_epw_header(paste0(
            "
            LOCATION,city,state,country,type,wmo,1,2,3,4
            DESIGN CONDITIONS
            TYPICAL/EXTREME PERIODS
            GROUND TEMPERATURES
            HOLIDAYS/DAYLIGHT SAVINGS,yes,0,0,0
            COMMENTS 1
            COMMENTS 2
            DATA PERIODS,1,1,Data,Friday,01/29,2016/3/21
            "
        )),
        class = "eplusr_error_parse_epw"
    )
    expect_equal(h$value[object_id == 8L, value_chr][6], " 3/21")
    expect_warning(
        h <- parse_epw_header(paste0(
            "
            LOCATION,city,state,country,type,wmo,1,2,3,4
            DESIGN CONDITIONS
            TYPICAL/EXTREME PERIODS
            GROUND TEMPERATURES
            HOLIDAYS/DAYLIGHT SAVINGS,yes,0,0,0
            COMMENTS 1
            COMMENTS 2
            DATA PERIODS,1,1,Data,Friday,2015/01/29,2015/3/21
            "
        ))
    )
    expect_error(
        h <- parse_epw_header(paste0(
            "
            LOCATION,city,state,country,type,wmo,1,2,3,4
            DESIGN CONDITIONS
            TYPICAL/EXTREME PERIODS
            GROUND TEMPERATURES
            HOLIDAYS/DAYLIGHT SAVINGS,yes,0,0,0
            COMMENTS 1
            COMMENTS 2
            DATA PERIODS,1,1,Data,Friday,1/2,1/1
            "
        )),
        class = "eplusr_error_parse_epw"
    )

    expect_error(
        suppressWarnings(h <- parse_epw_header(paste0(
            "
            LOCATION,city,state,country,type,wmo,1,2,3,4
            DESIGN CONDITIONS
            TYPICAL/EXTREME PERIODS
            GROUND TEMPERATURES
            HOLIDAYS/DAYLIGHT SAVINGS,no,0,0,0
            COMMENTS 1
            COMMENTS 2
            DATA PERIODS,1,1,Data,Friday,2016/2/29,2016/3/1
            "
        ))),
        class = "eplusr_error_parse_epw"
    )

    expect_error(
        parse_epw_header(paste0(
            "
            LOCATION,city,state,country,type,wmo,1,2,3,4
            DESIGN CONDITIONS
            TYPICAL/EXTREME PERIODS
            GROUND TEMPERATURES
            HOLIDAYS/DAYLIGHT SAVINGS,yes,0,0,0
            COMMENTS 1
            COMMENTS 2
            DATA PERIODS,2,1,Data,Friday,2016/01/01,2016/1/31,Data1,Friday,2016/1/1,2016/12/31
            "
        )),
        class = "eplusr_error_parse_epw"
    )
    # }}}

    # VALUE {{{
    idd <- get_epw_idd()
    idd_env <- get_priv_env(idd)$idd_env()
    idf_env <- parse_epw_header(
        "
        LOCATION,city,state,country,type,wmo,1,2,3,4
        DESIGN CONDITIONS
        TYPICAL/EXTREME PERIODS
        GROUND TEMPERATURES
        HOLIDAYS/DAYLIGHT SAVINGS,no,0,0,0
        COMMENTS 1
        COMMENTS 2
        DATA PERIODS,1,1, Data, Friday, 01/01, 12/31
        "
    )
    # }}}

    # FORMAT {{{
    idd_env <- get_priv_env(idd)$idd_env()
    expect_is(class = "list",
        h <- parse_epw_header(
            "
            LOCATION,city,state,country,type,wmo,1,2,3,4
            DESIGN CONDITIONS
            TYPICAL/EXTREME PERIODS
            GROUND TEMPERATURES
            HOLIDAYS/DAYLIGHT SAVINGS,no,0,0,0
            COMMENTS 1
            COMMENTS 2
            DATA PERIODS,1,1, Data, Friday, 01/01, 12/31
            "
        )
    )

    expect_equal(format_epw_header(h),
        c("LOCATION,city,state,country,type,wmo,1.00,2.00,3.0,4.0",
          "DESIGN CONDITIONS,0",
          "TYPICAL/EXTREME PERIODS,0",
          "GROUND TEMPERATURES,0",
          "HOLIDAYS/DAYLIGHT SAVINGS,No,0,0,0",
          "COMMENTS 1,",
          "COMMENTS 2,",
          "DATA PERIODS,1,1,Data,Friday, 1/ 1,12/31"
        )
    )

    expect_equal(format_epw_meta(h),
        c("[Location ]: city, state, country",
          "             {N 1°0'}, {E 2°0'}, {UTC+03:00}",
          "[Elevation]: 4m above see level",
          "[Data Src ]: type",
          "[WMO Stat ]: wmo",
          "[Leap Year]: No",
          "[Interval ]: 60 mins"
        )
    )
    # }}}
})
# }}}

# Epw Data {{{
test_that("Epw Data", {
    idd <- get_epw_idd()
    idd_env <- get_priv_env(idd)$idd_env()
    idf_env <- parse_epw_header(
        "
        LOCATION,city,state,country,type,wmo,1,2,3,4
        DESIGN CONDITIONS
        TYPICAL/EXTREME PERIODS
        GROUND TEMPERATURES
        HOLIDAYS/DAYLIGHT SAVINGS,no,0,0,0
        COMMENTS 1
        COMMENTS 2
        DATA PERIODS,1,1, Data, Friday, 01/01, 12/31
        "
    )

    expect_error(parse_epw_data("\n\n\n\n\n\n\n\n,,,"), class = "eplusr_error_parse_epw_data_column")

    hd <- "1\n2\n3\n4\n5\n6\n7\n8"
    dh <- paste0(rep("a", 35), collapse = ",")
    rw <- init_idf_value(idd_env, idf_env, "WEATHER DATA", property = "type")

    val <- paste0(copy(rw)[type == "integer", value_chr := "a"]$value_chr, collapse = ",")
    expect_error(parse_epw_data(paste(hd, dh, val, sep = "\n")), class = "eplusr_error_parse_epw_data_type")

    val <- paste0(copy(rw)[type == "real", value_chr := "a"]$value_chr, collapse = ",")
    expect_error(parse_epw_data(paste(hd, dh, val, sep = "\n")), class = "eplusr_error_parse_epw_data_type")

    skip_on_cran()
    if (!is_avail_eplus(8.8)) install_eplus(8.8)
    path_epw <- file.path(eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    expect_is(parsed <- parse_epw_file(path_epw), "list")
    expect_equal(names(parsed), c("header", "data", "matched"))
    expect_equal(ncol(parsed$data), 36L)
    expect_equal(parsed$matched, data.table(index = 1L, row = 1L, num = 8760L))

    # FORMAT
    expect_equal(find_nearst_wday_year(make_date(2019, 1, 14), 1, 2019), 2019)
    expect_equal(find_nearst_wday_year(make_date(2019, 1, 14), 2, 2019), 2014)
})
# }}}

# EpwDate Class {{{
test_that("EpwDate Class", {
    expect_equal(get_epw_wday(1), 1)
    expect_equal(get_epw_wday(1, label = TRUE), "Monday")
    expect_equal(get_epw_wday(1, label = TRUE, abbr = TRUE), "Mon")
    expect_equal(get_epw_wday("mon"), 1)
    expect_equal(get_epw_wday("mon", label = TRUE), "Monday")
    expect_equal(get_epw_wday("mon", label = TRUE, abbr = TRUE), "Mon")

    expect_error(epw_date(list()), "Missing method to convert")
    expect_equal(epw_date(""), init_epwdate_vctr(1))
    expect_equal(format(epw_date("")), NA_character_)
    expect_output(print(epw_date("")), "NA")

    expect_equal(epw_date(0L), init_epwdate_vctr(1, "0-01-01"))
    expect_equal(epw_date("0"), init_epwdate_vctr(1, "0-01-01"))

    expect_equal(epw_date(367), init_epwdate_vctr(1))
    expect_equal(format(epw_date(367)), NA_character_)
    expect_output(print(epw_date(367)), "NA")

    expect_equal(epw_date(366), init_epwdate_vctr(1, "4-12-31"))
    expect_equal(format(epw_date(366)), "366")
    expect_output(print(epw_date(366)), "366th day")

    expect_equal(epw_date(3), init_epwdate_vctr(1, "4-01-03"))
    expect_equal(format(epw_date(3)), "3")
    expect_output(print(epw_date(3)), "3rd day")

    expect_equal(epw_date("3.10"), init_epwdate_vctr(1, "8-03-10"))

    expect_equal(epw_date("01/03"), init_epwdate_vctr(1, "8-01-03"))
    expect_equal(format(epw_date("Apr-01")), " 4/ 1")
    expect_output(print(epw_date("Apr-01")), "Apr 01")

    expect_equal(epw_date("01-Apr"), init_epwdate_vctr(1, "8-04-01"))
    expect_equal(format(epw_date("01-Apr")), " 4/ 1")
    expect_output(print(epw_date("01-Apr")), "Apr 01")

    expect_equal(epw_date("2019-01-Apr"), init_epwdate_vctr(1))
    expect_equal(format(epw_date("2019-01-Apr")), NA_character_)
    expect_output(print(epw_date("2019-01-Apr")), "NA")

    expect_equal(epw_date("2019-Apr-01"), init_epwdate_vctr(1))
    expect_equal(format(epw_date("2019-Apr-01")), NA_character_)
    expect_output(print(epw_date("2019-Apr-01")), "NA")

    expect_equal(epw_date("4-01-2019"), init_epwdate_vctr(1, "2019-04-01"))
    expect_equal(format(epw_date("4-01-2019")), "2019/ 4/ 1")
    expect_output(print(epw_date("4-01-2019")), "2019-04-01")

    expect_equal(epw_date("last Mon in Jan"), init_epwdate_vctr(1, "16-01-25"))
    expect_equal(format(epw_date("last Mon in Jan")), "Last Monday in January")
    expect_output(print(epw_date("last Mon in Jan")), "Last Monday in January")

    expect_equal(epw_date("1st Mon in Jan"), init_epwdate_vctr(1, "12-01-02"))
    expect_equal(format(epw_date("1st Mon in Jan")), "1st Monday in January")
    expect_output(print(epw_date("1st Mon in Jan")), "1st Monday in January")

    expect_equal(format(epw_date(c("2nd Sunday in March", "1st Sunday in November"))),
        c("2nd Sunday in March", "1st Sunday in November")
    )

    expect_equal(epw_date("6 Mon in Jan"), init_epwdate_vctr(1))
    expect_equal(format(epw_date("6 Mon in Jan")), NA_character_)
    expect_output(print(epw_date("6 Mon in Jan")), "NA")

    expect_equal(c(epw_date("1/3"), epw_date("3")), epw_date(c("1/3", "3")))
    d <- epw_date(c("1/3", "4"))
    expect_equal(c(d[2], d[1], epw_date(1.1)), epw_date(c("4", "1/3", "1.1")))

    expect_true(is_EpwDate(epw_date("1")))
    expect_false(is_EpwDate(Sys.Date()))
    expect_equal(epw_date(1), as_EpwDate("1"))
    expect_true(is.na(epw_date("")))
    expect_false(is.na(epw_date(1)))
    expect_equal(length(epw_date(1:5)), 5L)
    expect_equal(epw_date(1:5)[2L], epw_date(2))
    expect_equal(epw_date(1:5)[[3L]], epw_date(3))
    expect_equal({d <- epw_date(1:2);d[1] <- epw_date(3);d}, epw_date(c(3, 2)))
    expect_equal({d <- epw_date(1:2);d[[1]] <- epw_date(3);d}, epw_date(c(3, 2)))
})
# }}}
