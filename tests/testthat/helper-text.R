# text used for testing {{{
idftext <- function(type = c("idf", "idd", "schema"), ver = LATEST_EPLUS_VER) {
    type <- match.arg(type)

    # idd_snippet {{{
    idd_snippet <- paste0(c(
          "!IDD_BUILD 87ed9199d4
          \\group Surface Construction Elements

          Version,
            A1 ; \\default 23.1

          Material,
          A1 , \\field Name
               \\required-field
               \\type alpha
               \\reference MaterialName
          A2 , \\field Roughness
               \\required-field
               \\type choice
               \\key VeryRough
               \\key Rough
               \\key MediumRough
               \\key MediumSmooth
               \\key Smooth
               \\key VerySmooth
          N1 , \\field Thickness
               \\required-field
               \\units m
               \\type real
               \\minimum> 0
               \\ip-units in
          N2 , \\field Conductivity
               \\required-field
               \\units W/m-K
               \\type real
               \\minimum> 0
          N3 , \\field Density
               \\required-field
               \\units kg/m3
               \\type real
               \\minimum> 0
          N4 , \\field Specific Heat
               \\required-field
               \\units J/kg-K
               \\type real
               \\minimum 100
          N5 , \\field Thermal Absorptance
               \\type real
               \\minimum> 0
               \\default .9
               \\maximum 0.99999
          N6 , \\field Solar Absorptance
               \\type real
               \\default .7
               \\minimum 0
               \\maximum 1
          N7 ; \\field Visible Absorptance
               \\type real
               \\minimum 0
               \\default .7
               \\maximum 1

          BuildingSurface:Detailed,
            \\extensible:3
            \\format vertices
            \\min-fields 20
            A1 , \\field Name
                 \\required-field
                 \\type alpha
                 \\reference SurfaceNames
                 \\reference SurfAndSubSurfNames
                 \\reference AllHeatTranSurfNames
                 \\reference OutFaceEnvNames
                 \\reference AllHeatTranAngFacNames
                 \\reference RadiantSurfaceNames
                 \\reference AllShadingAndHTSurfNames
                 \\reference FloorSurfaceNames
            A2 , \\field Surface Type
                 \\required-field
                 \\type choice
                 \\key Floor
                 \\key Wall
                 \\key Ceiling
                 \\key Roof
            A3 , \\field Construction Name
                 \\required-field
                 \\type object-list
                 \\object-list ConstructionNames
            A4 , \\field Zone Name
                 \\required-field
                 \\type object-list
            A5 , \\field Space Name
                 \\type object-list
            A6 , \\field Outside Boundary Condition
                 \\required-field
                 \\type choice
                 \\key Adiabatic
                 \\key Surface
                 \\key Zone
                 \\key Outdoors
                 \\key Foundation
                 \\key Ground
                 \\key GroundFCfactorMethod
                 \\key OtherSideCoefficients
                 \\key OtherSideConditionsModel
                 \\key GroundSlabPreprocessorAverage
                 \\key GroundSlabPreprocessorCore
                 \\key GroundSlabPreprocessorPerimeter
                 \\key GroundBasementPreprocessorAverageWall
                 \\key GroundBasementPreprocessorAverageFloor
                 \\key GroundBasementPreprocessorUpperWall
                 \\key GroundBasementPreprocessorLowerWall
            A7,  \\field Outside Boundary Condition Object
                 \\type object-list
                 \\object-list OutFaceEnvNames
            A8 , \\field Sun Exposure
                 \\type choice
                 \\key SunExposed
                 \\key NoSun
                 \\default SunExposed
            A9,  \\field Wind Exposure
                 \\type choice
                 \\key WindExposed
                 \\key NoWind
                 \\default WindExposed
            N1,  \\field View Factor to Ground
                 \\type real
                 \\autocalculatable
                 \\minimum 0.0
                 \\maximum 1.0
                 \\default autocalculate
            N2,  \\field Number of Vertices
                 \\autocalculatable
                 \\minimum 3
                 \\default autocalculate
            N3,  \\field Vertex 1 X-coordinate
                 \\begin-extensible
                 \\required-field
                 \\units m
                 \\type real
            N4 , \\field Vertex 1 Y-coordinate
                 \\required-field
                 \\units m
                 \\type real
            N5,  \\field Vertex 1 Z-coordinate
                 \\required-field
                 \\units m
                 \\type real
            N6,  \\field Vertex 2 X-coordinate
                 \\required-field
                 \\units m
                 \\type real
            N7,  \\field Vertex 2 Y-coordinate
                 \\required-field
                 \\units m
                 \\type real
            N8,  \\field Vertex 2 Z-coordinate
                 \\required-field
                 \\units m
                 \\type real
            N9,  \\field Vertex 3 X-coordinate
                 \\required-field
                 \\units m
                 \\type real
            N10, \\field Vertex 3 Y-coordinate
                 \\required-field
                 \\units m
                 \\type real
            N11; \\field Vertex 3 Z-coordinate
                 \\required-field
                 \\units m
                 \\type real

          Construction,
            A1 , \\field Name
                 \\required-field
                 \\type alpha
                 \\reference ConstructionNames
            A2 , \\field Outside Layer
                 \\required-field
                 \\type object-list
                 \\object-list MaterialName
            A3 , \\field Layer 2
                 \\type object-list
                 \\object-list MaterialName
            A4 , \\field Layer 3
                 \\type object-list
                 \\object-list MaterialName
            A5 , \\field Layer 4
                 \\type object-list
                 \\object-list MaterialName
            A6 , \\field Layer 5
                 \\type object-list
                 \\object-list MaterialName
            A7 , \\field Layer 6
                 \\type object-list
                 \\object-list MaterialName
            A8 , \\field Layer 7
                 \\type object-list
                 \\object-list MaterialName
            A9 , \\field Layer 8
                 \\type object-list
                 \\object-list MaterialName
            A10, \\field Layer 9
                 \\type object-list
                 \\object-list MaterialName
            A11; \\field Layer 10
                 \\type object-list
                 \\object-list MaterialName

          Output:Surfaces:List,
                 \\format singleLine
             A1, \\field Report Type
                 \\required-field
                 \\type choice
                 \\key Details
                 \\key Vertices
                 \\key DetailsWithVertices
                 \\key ViewFactorInfo
                 \\key Lines
                 \\key CostInfo
                 \\key DecayCurvesFromComponentLoadsSummary
             A2; \\field Report Specifications
                 \\type choice
                 \\key IDF
          SurfaceConvectionAlgorithm:Inside,
                 \\unique-object
                 \\format singleLine
            A1 ; \\field Algorithm
                 \\type choice
                 \\key Simple
                 \\key TARP
                 \\key CeilingDiffuser
                 \\key AdaptiveConvectionAlgorithm
                 \\key ASTMC1340
                 \\default TARP
          "
    ))
    # }}}

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

     # idf_text {{{
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
            ,                        !- Space Name
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
     # }}}

    if (type %in% c("idd", "schema")) {
        text <- if (type == "idd") idd_text else idd_snippet
        if (!is.null(ver)) text <- paste0("!IDD_Version ", ver, "\n", text)
    } else if (type == "idf") {
        text <- idf_text
        if (!is.null(ver)) text <- paste0(text, "\nVersion, ", ver, ";")
    }
    text
}
# }}}

# vim: set fdm=marker:
