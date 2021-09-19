# get all TeX files of I/O ref
get_tex_paths <- function (dir_src) {
    dir <- file.path(dir_src, "doc/input-output-reference/src/overview")
    files <- list.files(dir, "group-.+[.]tex", ignore.case = TRUE, full.names = TRUE)

    # for environment factor outputs
    file_envfctr <- file.path(dirname(dir), "input-for-output.tex")

    normalizePath(c(files, file_envfctr))
}

comment_out_lines <- function(path, lines) {
    l <- readLines(path, warn = FALSE)
    l[lines] <- paste("%", l[lines])
    writeLines(l, path)
}

# get paths of Markdown files converted from TeX files using Pandoc
get_md_paths <- function (dir_src, dir = tempdir(), ver = 9.4) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    paths <- get_tex_paths(dir_src)
    vapply(paths, convert_to_md, character(1), dir = dir, ver = ver)
}

# Convert TeX to Markdown using Pandoc
convert_to_md <- function(path, dir = tempdir(), ver = 9.4) {
    stopifnot(file.copy(path, dir, overwrite = TRUE))

    file <- normalizePath(file.path(dir, basename(path)), mustWork = FALSE)
    md <- sprintf("%s.markdown", tools::file_path_sans_ext(file))

    # use pandoc to convert EnergyPlus raw doc in tex to markdown
    status <- -1L
    while (status != 1L) {
        f <- tempfile()
        log <- try(
            processx::run(
                "pandoc", c("-f", "latex", "-t", "markdown", file, "-o", md, "--wrap=preserve"),
                stderr = f
            ),
            silent = TRUE
        )

        if (!inherits(log, "try-error")) {
            status <- log$status
        }
        err <- readLines(f, warn = FALSE)
        unlink(f, force = TRUE)

        if (status == 0L) break

        if (status != 1L) {
            # try to comment out invalid LaTeX lines in the source file
            invld <- stringi::stri_match_first_regex(err, '^Error at "source" \\(line (\\d+), column \\d+\\)')[, 2L]
            invld <- invld[!is.na(invld)]

            if (length(invld)) {
                invld <- as.integer(invld)

                # NOTE: there is a LaTeX error in one \paragraph in
                # "group-exterior-energy-use-equipment.tex" in EnergyPlus v9.0.0
                # have to fix it manually
                if (numeric_version(ver) >= 9.0 && numeric_version(ver) < 9.1 &&
                    basename(file) == "group-exterior-energy-use-equipment.tex") {
                    l <- readLines(file, warn = FALSE)
                    l[185] <- gsub(")", "}", l[185], fixed = TRUE)
                    writeLines(l, file)
                } else {
                    # comment out invald lines
                    comment_out_lines(file, invld)
                }
                cat(sprintf(" --> Attempting to fix LaTeX error in file '%s'...\n", file))
            # unknown error
            } else {
                stop(sprintf("Failed to convert '%s' to Markdown:\n %s",
                    path, paste0(err, collapse = "\n")
                ))
            }
        }
    }

    md
}

# read Markdown file as a data.table using {parsermd} package
read_md_dt <- function(path) {
    dt <- data.table::setDT(parsermd::as_tibble(parsermd::parse_rmd(path, parse_yaml = FALSE)))[]

    # remove labels from headings
    cols <- grep("sec_h\\d+", names(dt))
    for (j in cols) {
        data.table::set(dt, NULL, j, gsub("\\s*{#.+?}", "", dt[[j]], perl = TRUE))
    }

    dt
}

# get the total heading levels
get_md_h_levels <- function(doc) {
    sort(unique(unlist(lapply(doc$ast, function(x) if (!inherits(x, "rmd_heading")) NULL else x$level))))
}

# extract all output variables from Markdown file
extract_outputs <- function (path, ver = 9.4) {
    cat(sprintf(" --> Processing file '%s'...\n", path))
    doc <- read_md_dt(path)[!J("rmd_yaml"), on = "type"]
    data.table::set(doc, NULL, "label", NULL)

    out <- data.table::data.table(group_name = character(), class_name = character(), output = character(), string = character())
    levels <- get_md_h_levels(doc)

    for (lvl in levels) {
        if (!sprintf("sec_h%i", lvl + 1L) %in% names(doc)) next

        out <- data.table::rbindlist(use.names = TRUE,
            list(out, extract_outputs_from_heading_level(doc, lvl)
        ))
    }

    out <- post_process_outputs(path, doc, out, ver)

    # keep the original order
    data.table::set(out, NULL, "index", seq_len(nrow(out)))

    # split variable name and unit
    out <- out[, c("reported_time_step", "report_type", "variable", "units") := {
        m1 <- stringi::stri_match_first_regex(string, "(\\S+?)\\s*,\\s*(.+?)\\s*,")

        m2 <- stringi::stri_match_first_regex(output, "(.+?)\\s*\\\\\\[(.*)\\\\\\]")
        m2[is.na(m2[, 1L]), 2L] <- output[is.na(m2[, 1L])]
        m2[stringi::stri_isempty(m2[, 3L]), 3L] <- NA_character_
        m2[, 2L] <- stringi::stri_replace_all_fixed(m2[, 2L], "\\", "")

        list(m1[, 2L], m1[, 3L], m2[, 2L], m2[, 3L])
    }]

    # make sure bullet points are kept since they have the information about
    # output reported_time_step
    data.table::setorderv(out, c("reported_time_step", "report_type"), na.last = TRUE)

    cols <- c("reported_time_step", "report_type", "units", "variable")
    out[, c(cols) := lapply(.SD, stringi::stri_trim_both), .SDcols = cols]
    out <- unique(out, by = c("class_name", "variable"))

    data.table::setorderv(out, "index")

    # clean
    data.table::set(out, NULL, c("string", "index", "output", "group_name"), NULL)
    data.table::setcolorder(out, c("reported_time_step", "report_type", "variable", "units"))
}

extract_outputs_from_heading_level <- function (doc, level, pattern = "Output", parent = NULL) {
    levels <- get_md_h_levels(doc)
    nm_sec <- function(level) {
        if (any(mis <- !level %in% levels)) {
            level[mis] <- vapply(level[mis], function(x) levels[which.min(abs(x - levels))], integer(1))
        }
        sprintf("sec_h%i", level)
    }

    # subset using parent
    if (is.null(parent)) {
        out <- doc[!grepl("[^A-Za-z\\-\\:]+", doc[[nm_sec(level - 1L)]])]
    } else if (is.character(parent)) {
        out <- doc[doc[[nm_sec(level + 1L)]] %in% parent]
    } else {
        out <- doc
    }

    out <- out[grepl(pattern, out[[nm_sec(level)]])]

    if (!nrow(out)) {
        return(data.table::data.table(
            group_name = character(), class_name = character(),
            output = character(), string = character()
        ))
    }

    # from heading
    h <- out[J("rmd_heading"), on = "type", nomatch = NULL, .SD,
        .SDcols = nm_sec(c(level - 2L, level - 1L, level + 1L))][
        grepl(".+\\\\\\[.*\\\\\\]", get(nm_sec(level + 1L)))]
    data.table::setnames(h, c("group_name", "class_name", "output"))
    h[, string := output]

    # from bullet and code fence
    b <- out[, by = c(nm_sec(c(level - 2L, level - 1L))),
        {
            chk <- is.na(get(nm_sec(level + 1L)))
            if (length(chk) < 2L || !chk[2L]) {
                list(output = NA_character_, string = NA_character_)
            # in case there is no heading
            } else if (type[1L] != "rmd_heading" && which(type == "rmd_markdown")[1L] > which(type == "rmd_heading")[1L]) {
                list(output = NA_character_, string = NA_character_)
            } else {
                s <- ast[which(type == "rmd_markdown")[1L]][[1L]]
                m <- stringi::stri_match_first_regex(s, "^(?:(?:\\s*-\\s{1,})|\\s{4}).+?,.+?,(.+)")
                list(output = m[, 2L], string = m[, 1L])
            }
        }
    ][!is.na(output)]
    data.table::setnames(b, c("group_name", "class_name", "output", "string"))

    data.table::rbindlist(list(h, b), fill = TRUE)
}

post_process_outputs <- function(file, doc, out, ver) {
    if (basename(file) == "group-location-climate-weather-file-access.markdown") {
        # outputs from "## Weather Data Related Outputs" apply to all "Environment"
        post1 <- extract_outputs_from_heading_level(doc, 2, parent = FALSE,
            "Weather Data Related Outputs")
        data.table::set(post1, NULL, "class_name", "Environment")

        # "## Outputs for local temperature/wind speed calculations" apply to
        # all "Environment"
        post2 <- extract_outputs_from_heading_level(doc, 2, parent = FALSE,
            "Outputs for local temperature/wind speed calculations")
        data.table::set(post2, NULL, "class_name", "Environment")
        # further clean up
        post2[, output := gsub("\\s+\\(for OUTDOOR AIR.*", "", output)]

        out <- data.table::rbindlist(list(out, post1, post2))
    }

    if (basename(file) == "group-schedules.markdown") {
        cls <- eplusr::use_idd(ver)$class_name(by_group = TRUE)[["Schedules"]]
        cls <- grep("Schedule:", cls, value = TRUE)
        post <- data.table::data.table(
            group_name = "Group -- Schedules",
            class_name = cls, output = "Schedule Value",
            string = "Zone,Average,Schedule Value []"
        )

        out <- data.table::rbindlist(list(out, post))
    }

    if (basename(file) == "group-surface-construction-elements.markdown") {
        # TODO: Outputs below should be added if Conduction Finite Difference
        # solution algorithm is used:
        # * CondFD Inner Solver Loop Iteration Count[]
        # * ConFD Surface Temperature Node <1> [C]

        # "## Surface Outputs" apply to all "MaterialProperty:HeatAndMoistureTransfer:*"
        cls <- eplusr::use_idd(ver)$class_name(by_group = TRUE)[["Surface Construction Elements"]]
        cls <- grep("MaterialProperty:HeatAndMoistureTransfer:", cls, value = TRUE)
        post <- out[class_name == "MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity", list(
            group_name = rep(group_name, length(cls)),
            class_name = rep(cls, each = .N),
            output = rep(output, length(cls)),
            string = rep(string, length(cls))
        )]
        out <- data.table::rbindlist(list(out[class_name != "MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity"], post))

        # "## Ecoroof / RoofVegetation outputs" apply to "Material:RoofVegetation"
        post <- extract_outputs_from_heading_level(doc, 2, parent = FALSE,
            "Ecoroof / RoofVegetation outputs")
        out <- data.table::rbindlist(list(out, post))
    }

    if (basename(file) == "group-thermal-zone-description-geometry.markdown") {
        # "## Zone Thermal Output(s)" apply to "Zone"
        post1 <- extract_outputs_from_heading_level(doc, 2, parent = FALSE,
            "Zone Thermal Output\\(s\\)")
        data.table::set(post1, NULL, "class_name", "Zone")

        # all valid class names
        cls <- eplusr::use_idd(ver)$class_name(by_group = TRUE)[["Thermal Zones and Surfaces"]]

        # apply to all classes except adiabatic
        post2 <- extract_outputs_from_heading_level(doc, 2, parent = FALSE,
            "Surface Output Variables \\(all heat transfer surfaces\\)")
        cls_ht <- stringi::stri_subset_regex(cls,
            "^(BuildingSurface|FenestrationSurface|Wall|RoofCeiling|Ceiling|Floor|Window|Door|GlazedDoor)($|:.+(?<!Adiabatic)$)")
        post2 <- post2[, list(group_name = rep(group_name, length(cls_ht)),
            class_name = rep(cls_ht, each = .N),
            output = rep(output, length(cls_ht)),
            string = rep(string, length(cls_ht))
        )]

        # apply to all classes except adiabatic, groundcontact, underground, and
        # interzone
        post3 <- extract_outputs_from_heading_level(doc, 2, parent = FALSE,
            "Surface Output Variables \\(exterior heat transfer surfaces\\)")
        cls_ext <- stringi::stri_subset_regex(cls,
            "^(BuildingSurface|FenestrationSurface|Wall|RoofCeiling|Ceiling|Floor|Window|Door|GlazedDoor)($|:.+(?<!(Adiabatic|Underground|Interzone|GroundContact))$)")
        post3 <- post3[, list(
            group_name = rep(group_name, length(cls_ext)),
            class_name = rep(cls_ext, each = .N),
            output = rep(output, length(cls_ext)),
            string = rep(string, length(cls_ext))
        )]

        # apply to all opaque
        post4 <- extract_outputs_from_heading_level(doc, 2, parent = FALSE,
            "Opaque Surface Output Variables")
        cls_opa <- stringi::stri_subset_regex(cls,
            "^(BuildingSurface|Wall|RoofCeiling|Ceiling|Floor|Window|Door)($|:.+(?<!Adiabatic)$)")
        post4 <- post4[, list(
            group_name = rep(group_name, length(cls_opa)),
            class_name = rep(cls_opa, each = .N),
            output = rep(output, length(cls_opa)),
            string = rep(string, length(cls_opa))
        )]

        # apply to windoes or glass doors
        post5 <- extract_outputs_from_heading_level(doc, 2, parent = FALSE,
            "Window Output Variables")
        cls_win <- stringi::stri_subset_regex(cls, "^(Window|GlazedDoor)($|:.+$)")
        post5 <- post5[, list(
            group_name = rep(group_name, length(cls_win)),
            class_name = rep(cls_win, each = .N),
            output = rep(output, length(cls_win)),
            string = rep(string, length(cls_win))
        )]

        # TODO: Thermochromic Window Outputs only applicable with
        # WindowMaterial:GlazingGroup:Thermochromic
        post6 <- extract_outputs_from_heading_level(doc, 2, parent = FALSE,
            "Thermochromic Window Outputs")[!J("Switchable Window Outputs"), on = "output"]
        post6 <- post6[, list(
            group_name = rep(group_name, length(cls_win)),
            class_name = rep(cls_win, .N),
            output = rep(output, length(cls_win)),
            string = rep(string, length(cls_win))
        )]

        out <- data.table::rbindlist(list(out, post1, post2, post3, post4, post5, post6))
    }

    if (basename(file) == "group-internal-gains-people-lights-other.markdown") {
        # Outputs for ElectricEquipment, GasEquipment, HotWaterEquipment,
        # SteamEquipment, and OtherEquipment are all listed under "## OtherEquipment"
        out[class_name == "OtherEquipment" & grepl("Electric Equipment", output), class_name := "ElectricEquipment"]
        out[class_name == "OtherEquipment" & grepl("Gas Equipment", output), class_name := "GasEquipment"]
        out[class_name == "OtherEquipment" & grepl("Hot Water Equipment", output), class_name := "HotWaterEquipment"]
        out[class_name == "OtherEquipment" & grepl("Steam Equipment", output), class_name := "SteamEquipment"]

        # Outputs for ElectricEquipment:ITE:AirCooled are placed after
        # "### Field: Return Temperature Difference Schedule"
        d <- doc[sec_h2 == "ElectricEquipment:ITE:AirCooled" & type == "rmd_heading" & !is.na(sec_h3)]
        if (!nrow(d)) {
            post1 <- out[0]
        } else {
            post1 <- data.table::data.table(
                group_name = out$group_name[1],
                class_name = "ElectricEquipment:ITE:AirCooled",
                output = d[(max(grep("^Field:", sec_h4)) + 1L):.N, sec_h4]
            )
            post1[, string := output]
        }
        d <- doc[sec_h2 == "ElectricEquipment:ITE:AirCooled" & type == "rmd_markdown" & !is.na(sec_h3)]
        if (!nrow(d)) {
            post2 <- out[0]
        } else {
            s <- d[max(grep("^Field:", sec_h4)), ast[[1]]]
            output <- stringi::stri_match_first_regex(s, "^(?:(?:\\s*-\\s{1,})|\\s{4}).+?,.+?,(.+)")[, 2L]
            string <- output
            string[!is.na(output)] <- s[!is.na(output)]
            post2 <- data.table::data.table(
                group_name = out$group_name[1],
                class_name = "ElectricEquipment:ITE:AirCooled",
                output = output, string = string
            )[!is.na(output)]
        }

        out <- data.table::rbindlist(list(out, post1, post2))
    }

    if (basename(file) == "group-daylighting.markdown") {
        # "### Outputs" under Daylighting:DELight:ComplexFenestration apply to
        # Daylighting:Controls objects
        out[class_name == "Daylighting:DELight:ComplexFenestration", class_name := "Daylighting:Controls"]
    }

    if (basename(file) == "group-exterior-energy-use-equipment.markdown") {
        # Some "### Outputs" under Exterior:WaterEquipment applies to
        # Exterior:FuelEquipment
        post <- out[class_name == "Exterior:WaterEquipment" & grepl("Fuel", output)]
        post[, class_name := "Exterior:FuelEquipment"]

        out <- data.table::rbindlist(list(out, post))
    }

    if (basename(file) == "group-airflow.markdown") {
        cls <- eplusr:::use_idd(ver)$class_name(by_group = TRUE)[["Zone Airflow"]]

        # "### Outputs" under ZoneInfiltration:FlowCoefficient apply to all
        # ZoneInfiltration:*
        cls_infil <- grep("ZoneInfiltration:", cls, value = TRUE)
        post1 <- out[class_name == "ZoneInfiltration:FlowCoefficient",
            list(group_name = rep(group_name, length(cls_infil)),
                 class_name = rep(cls_infil, each = .N),
                 output = rep(output, length(cls_infil)),
                 string = rep(string, length(cls_infil))
            )
        ]
        out <- out[class_name != "ZoneInfiltration:FlowCoefficient"]

        # "### Outputs" under ZoneVentilation:WindandStackOpenArea apply to all
        # ZoneVentilation:*
        cls_vent <- grep("ZoneVentilation:", cls, value = TRUE)
        post2 <- out[class_name == "ZoneVentilation:WindandStackOpenArea",
            list(group_name = rep(group_name, length(cls_infil)),
                 class_name = rep(cls_infil, each = .N),
                 output = rep(output, length(cls_infil)),
                 string = rep(string, length(cls_infil))
            )
        ]
        out <- out[class_name != "ZoneVentilation:WindandStackOpenArea"]

        out <- data.table::rbindlist(list(out, post1, post2))
    }

    if (basename(file) == "group-air-distribution.markdown") {
        # AirLoopHVAC outputs are described in several places
        post1 <- extract_outputs_from_heading_level(doc, 2L, "System Loads Outputs", FALSE)
        post2 <- extract_outputs_from_heading_level(doc, 2L, "System Energy Use Outputs", FALSE)
        post3 <- extract_outputs_from_heading_level(doc, 2L, "System Component Loads Outputs", FALSE)
        post4 <- extract_outputs_from_heading_level(doc, 2L, "System Component Energy Use Outputs", FALSE)
        post1 <- data.table::rbindlist(list(post1, post2, post3, post4))
        data.table::set(post1, NULL, "class_name", "AirLoopHVAC")

        # "#### Ventilation Load Reports" is incorrectly placed at the same
        # level as other AirLoopHVAC outputs
        out <- out[output != "Ventilation Load Reports"]

        # OutdoorAir:Node outputs are described in a dedicated section
        post2 <- extract_outputs_from_heading_level(doc, 2L, "Outdoor Air Node outputs", FALSE)
        data.table::set(post2, NULL, "class_name", "OutdoorAir:Node")

        out <- data.table::rbindlist(list(out, post1, post2))
    }

    if (basename(file) == "group-airflow-network.markdown") {
        # AirflowNetwork outputs combine outputs from different classes
        # AirflowNetwork:MultiZone:Zone
        # AirflowNetwork:MultiZone:Surface
        # AirflowNetwork:MultiZone:ExternalNode
        # AirflowNetwork:Distribution:Node
        # AirflowNetwork:Distribution:Linkage
        out[grepl("AFN Zone", output), class_name := "AirflowNetwork:MultiZone:Zone"]
        out[grepl("AFN Surface", output), class_name := "AirflowNetwork:MultiZone:Surface"]
        out[grepl("AFN Node", output), class_name := "AirflowNetwork:MultiZone:ExternalNode"]
        out[grepl("AFN Linkage Node", output), class_name := "AirflowNetwork:Distribution:Node"]
    }

    if (basename(file) == "group-refrigeration.markdown") {
        # "## Refrigeration Case and WalkIn Outputs" apply to
        # * Refrigeration:Case
        # * Refrigeration:Walkin
        post1 <- extract_outputs_from_heading_level(doc, 2, parent = FALSE,
            "Refrigeration Case and WalkIn Outputs")[, list(
            group_name = rep(group_name, 2L),
            class_name = rep(c("Refrigeration:Case", "Refrigeration:Walkin"), each = .N),
            output = rep(output, 2L), string = rep(string, 2L)
        )]

        # "## Additional Refrigeration Outputs for Each Zone" apply to
        # * Refrigeration:Case
        # * Refrigeration:Walkin
        post2 <- extract_outputs_from_heading_level(doc, 2, parent = FALSE,
            "Additional Refrigeration Outputs for Each Zone")[, list(
            group_name = rep(group_name, 2L),
            class_name = rep(c("Refrigeration:Case", "Refrigeration:Walkin"), each = .N),
            output = rep(output, 2L), string = rep(string, 2L)
        )]

        out <- data.table::rbindlist(list(out, post1, post2))
    }

    if (basename(file) == "group-heating-and-cooling-coils.markdown") {
        cls <- eplusr:::use_idd(ver)$class_name(by_group = TRUE)[["Coils"]]

        # "## Heating Coil(Steam) Outputs:" apply to
        # * Coil:Heating:Water
        # * Coil:Heating:Steam
        post1 <- extract_outputs_from_heading_level(doc, 2, parent = FALSE,
            "Heating Coil \\(Steam\\) Outputs:")[!grepl("Steam", output),
            list(group_name, class_name = "Coil:Heating:Water", output, string)
        ]

        # Outputs under Coil:Cooling:DX:MultiSpeed apply to all cooling coil
        # types of Coil:Cooling:DX:*
        cls_clgdx <- grep("Coil:Cooling:DX:(Single|Two|Multi)", cls, value = TRUE)

        post2 <- out[class_name == "Coil:Cooling:DX:MultiSpeed"]
        out <- out[class_name != "Coil:Cooling:DX:MultiSpeed"]

        # 2 variables only apply to Coil:Cooling:DX:TwoStageWithHumidityControlMode
        post3 <- post2[grepl("(Cooling Coil Stage 2 Runtime Fraction)|(Cooling Coil Dehumidification Mode)", output)]
        post2 <- post2[!post3, on = c("class_name", "output")]
        post3[, class_name := "Coil:Cooling:DX:TwoStageWithHumidityControlMode"]
        # 2 variables only apply to Coil:Cooling:DX:MultiSpeed
        post4 <- post2[grepl("DX.*Fuel Type", output)]
        post2 <- post2[!post4, on = c("class_name", "output")]
        post4[, class_name := "Coil:Cooling:DX:MultiSpeed"]

        post2 <- post2[, list(
            group_name = rep(group_name, length(cls_clgdx)),
            class_name = rep(cls_clgdx, each = .N),
            output = rep(output, length(cls_clgdx)),
            string = rep(string, length(cls_clgdx))
        )]

        # Coil:Cooling:WaterToAirHeatPump:ParameterEstimation and
        # Coil:Cooling:WaterToAirHeatPump:EquationFit have the same output
        # variables
        post5 <- out[class_name == "Coil:Cooling:WaterToAirHeatPump:EquationFit"][
            , class_name := "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation"]

        # Coil:Cooling:DX:SingleSpeed, Coil:Heating:DX:SingleSpeed,
        # Coil:Cooling:DX:TwoSpeed, Coil:Cooling:DX:MultiSpeed, and
        # Coil:Heating:DX:MultiSpeed can have Secondary DX Coil Output
        cls_dx <- grep("DX:(Single|Two|Multi)Speed$", cls, value = TRUE)
        post6 <- extract_outputs_from_heading_level(doc, 2, parent = FALSE,
            "Secondary DX Coil Output")[,
            list(group_name = rep(group_name, length(cls_dx)),
                 class_name = rep(cls_dx, each = .N),
                 output = rep(output, length(cls_dx)),
                 string = rep(string, length(cls_dx)))
        ]

        # Coil:Cooling:DX:CurveFit:Speed applies to Coil:Cooling:DX:CurveFix:*
        cls_dxcurve <- grep("Cooling:DX:CurveFit", cls, value = TRUE)
        post7 <- out[, list(
            group_name = rep(group_name, length(cls_dxcurve)),
            class_name = rep(cls_dxcurve, each = .N),
            output = rep(output, length(cls_dxcurve)),
            string = rep(string, length(cls_dxcurve)))
        ]
        out <- out[class_name != "Coil:Cooling:DX:CurveFit:Speed"]

        out <- data.table::rbindlist(list(out, post1, post2, post3, post4, post5, post6, post7))
    }

    if (basename(file) == "group-electric-load-center-generator.markdown") {
        # Outputs for ElectricLoadCenter:Storage:Converter are listed in a
        # dedicated subsection
        post <- extract_outputs_from_heading_level(doc, 2, parent = FALSE,
            "Electric Load Center Converter Outputs")[
            , class_name := "ElectricLoadCenter:Storage:Converter"]

        out <- data.table::rbindlist(list(out, post))
    }

    if (basename(file) == "group-advanced-surface-concepts.markdown") {
        # Outputs under SurfaceProperty:OtherSideConditionsModel do not give
        # units
        out[output == "Surface Other Side Coefficients Exterior Air Drybulb Temperature", string := paste(string, "\\[C\\]")]
    }

    out
}

extract_output_vars <- function (eplus_src, ver = eplusr:::ALL_EPLUS_VER) {
    # get all possible released tags
    tags <- git2r::tags(eplus_src)
    on.exit(git2r::checkout(eplus_src, "develop"), add = TRUE)

    # LaTeX doc was added since v8.5.0
    eplus_ver <- numeric_version(ver)
    eplus_ver <- paste0("v", eplus_ver[eplus_ver >= 8.5])

    if (!any(is_mis <- eplus_ver %in% names(tags))) {
        stop(sprintf("Failed to find EnergyPlus version %s.", paste0(eplus_ver[is_mis], collapse = ", ")))
    }

    tags <- names(tags[names(tags) %in% eplus_ver])

    outputs <- list()

    for (tag in tags) {
        cat("Extracting output variables from EnergyPlus", tag, "\n")

        git2r::checkout(eplus_src, tag)

        unlink("doc_md", recursive = TRUE, force = TRUE)

        paths <- get_md_paths(eplus_src, "doc_md", ver = substring(tag, 2))
        outputs[[tag]] <- data.table::rbindlist(lapply(paths, extract_outputs, ver = substring(tag, 2)))

        unlink("doc_md", recursive = TRUE, force = TRUE)
    }

    names(outputs) <- substring(names(outputs), 2)

    outputs
}
