# text used for testing {{{
idftext <- function(type = c("idf", "idd"), ver = NULL) {
    type <- match.arg(type)
    # idd_text {{{
    idd_text <- c(
        "!IDD_BUILD 7c3bbe4830
         \\group TestGroup1

         TestSimple,
         A1 ; \\field Test Field
           \\reference RefTestSimpleA1

         \\group TestGroup2
         TestSlash,
           \\memo This is just a test
           \\required-object
           \\unique-object
           \\min-fields 3
           \\format singleLine
           \\extensible 4 !all fields are extensible
         A1 , \\field Test Character Field 1
           \\note Test Note Parsing
           \\reference-class-name RefTestSlash
           \\required-field
           \\begin-extensible
           \\object-list RefTestSimpleA1
         N1 , \\field Test Numeric Field 1
           \\units m
           \\ip-units in
           \\unitsbasedonfield A2
           \\minimum 1
           \\maximum< 10
           \\default 2
           \\autosizable
           \\type real
         N2 , \\field Test Numeric Field 2
           \\autocalculatable
           \\type real
         A2 ; \\field Test Character Field 2
           \\type choice
           \\key Key1
           \\key Key2")
    # }}}

    idf_text <- "
        ! this is a test comment for WD01
        Material,
            WD01,                    !- Name
            MediumSmooth,            !- Roughness
            1.9099999E-02,           !- Thickness {m}
            0.1150000,               !- Conductivity {W/m-K}
            513.0000,                !- Density {kg/m3}
            1381.000,                !- Specific Heat {J/kg-K}
            0.9000000,               !- Thermal Absorptance
            0.7800000,               !- Solar Absorptance
            0.7800000;               !- Visible Absorptance

        Construction,
            WALL-1,                  !- Name
            WD01,                    !- Outside Layer
            PW03,                    !- Layer 2
            IN02,                    !- Layer 3
            GP01;                    !- Layer 4

        BuildingSurface:Detailed,
            WALL-1PF,                !- Name
            WALL,                    !- Surface Type
            WALL-1,                  !- Construction Name
            PLENUM-1,                !- Zone Name
            Outdoors,                !- Outside Boundary Condition
            ,                        !- Outside Boundary Condition Object
            SunExposed,              !- Sun Exposure
            WindExposed,             !- Wind Exposure
            0.50000,                 !- View Factor to Ground
            4,                       !- Number of Vertices
            0.0,                     !- Vertex 1 X-coordinate {m}
            0.0,                     !- Vertex 1 Y-coordinate {m}
            3.0,                     !- Vertex 1 Z-coordinate {m}
            0.0,                     !- Vertex 2 X-coordinate {m}
            0.0,                     !- Vertex 2 Y-coordinate {m}
            2.4,                     !- Vertex 2 Z-coordinate {m}
            30.5,                    !- Vertex 3 X-coordinate {m}
            0.0,                     !- Vertex 3 Y-coordinate {m}
            2.4,                     !- Vertex 3 Z-coordinate {m}
            30.5,                    !- Vertex 4 X-coordinate {m}
            0.0,                     !- Vertex 4 Y-coordinate {m}
            3.0,                     !- Vertex 4 Z-coordinate {m}
               ,                     !- Vertex 5 X-coordinate {m}
               ,                     !- Vertex 5 Y-coordinate {m}
               ;                     !- Vertex 5 Z-coordinate {m}

        Material,
            WD02,                    !- Name
            MediumSmooth,            !- Roughness
            1.9099999E-02,           !- Thickness {m}
            0.1150000;               !- Conductivity {W/m-K}
        "

    if (type == "idd") {
        text <- idd_text
        if (!is.null(ver)) text <- paste0("!IDD_Version ", ver, "\n", text)
    } else if (type == "idf") {
        text <- idf_text
        if (!is.null(ver)) text <- paste0(text, "\nVersion, ", ver, ";")
    }
    text
}
# }}}
# example IDF shipped with this package {{{
example <- function () {
    system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr")
}
# }}}
