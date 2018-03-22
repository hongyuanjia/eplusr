context("IDF method")

describe("IDF related", {

    path <- "files/5Zone_Transformer_8.8.idf"

    text_idf <- c(
       "!-Generator eplusr
        !-Option SortedOrder

        !-NOTE: All comments with '!-' are ignored by the IDFEditor and are generated automatically.
        !-      Use '!' comments if they need to be retained when using the IDFEditor.


        !-   ===========  ALL OBJECTS IN CLASS: VERSION ===========

        ! 5Zone_Transformer.idf
        Version,
            8.8;                     !- Version Identifier


        !-   ===========  ALL OBJECTS IN CLASS: SIMULATIONCONTROL ===========

        SimulationControl,
            Yes,                     !- Do Zone Sizing Calculation
            Yes,                     !- Do System Sizing Calculation
            Yes,                     !- Do Plant Sizing Calculation
            No,                      !- Run Simulation for Sizing Periods
            Yes;                     !- Run Simulation for Weather File Run Periods


        !-   ===========  ALL OBJECTS IN CLASS: BUILDING ===========

        Building,
            Building,                !- Name
            30,                      !- North Axis {deg}
            City,                    !- Terrain
            0.04,                    !- Loads Convergence Tolerance Value
            0.4,                     !- Temperature Convergence Tolerance Value {deltaC}
            FullExterior,            !- Solar Distribution
            25,                      !- Maximum Number of Warmup Days
            6;                       !- Minimum Number of Warmup Days


        !-   ===========  ALL OBJECTS IN CLASS: SURFACECONVECTIONALGORITHM:INSIDE ===========

        SurfaceConvectionAlgorithm:Inside,
            Simple;                  !- Algorithm"
    )

    idd <- IDD$new("files/V8-8-0-Energy+.idd")

    describe("Preprocess IDF files"
        it("can read IDF stored in files", {
            expect_silent(idf_str <- read_idd(path))
        })

        it("can read IDF stored in strings", {
            expect_silent(idf_str <- read_idd(text_idf))
        })

        it("can correctly get IDF version", {
            expect_equal(get_idf_ver(idf_str), "8.8")
        })

        it("has not tested what would happen when no version")
    )

    describe("Parse IDF files",
        it("can parse IDF stored in files", {
            expect_silent(idf_parsed <- parse_idf(path, idd))
        })

        it("can parse IDF stored in strings", {
            expect_silent(idf_parsed <- parse_idf(text_idf, idd))
        })
    )

    describe("IDF methods",
        it("can create new IDF object", {
            expect_silent(idf <- IDF$new(text_idf, idd))
        })
    )

    expect_equal(idf$version(), "8.8")

    expect_equal(idf$all_id(), 1:4)
    expect_equal(idf$all_class(), c("Version", "SimulationControl", "Building",
                                    "SurfaceConvectionAlgorithm:Inside"))
    expect_equal(idf$id_of_class("Version"), 1L)
    expect_equal(idf$class_of_id(2L), "SimulationControl")

    expect_equal(idf$is_valid_id(1L), TRUE)
    expect_error(idf$is_valid_id(1L:4L), "id is not a scalar.")
    expect_equal(idf$is_valid_id(5L), FALSE)
    expect_equal(idf$is_valid_class("Version"), TRUE)
    expect_equal(idf$is_valid_class("Wrong"), FALSE)

    expect_silent(idf$object(1))
    expect_equal(class(idf$object(1)), c("IDFObject", "IDDObject", "R6"))
    expect_error(idf$object(1:2))
    expect_error(idf$object("a"))
    expect_error(idf$object(5))

    expect_silent(idf$objects(1:2))
    expect_equal(class(idf$objects(1:2)), "list")
    expect_equal(length(idf$objects(1:2)), 2L)
    expect_error(idf$objects(1:5), "`5` is not a valid object ID in current IDF.")

    expect_silent(idf$objects_in_class("Building"))
    expect_equal(idf$objects_in_class("Building"), idf$objects(3))
    expect_equal(idf$objects_in_class("Building", 1), idf$objects(3))
    expect_error(idf$objects_in_class("Version", 2))

    expect_error(idf$set_object(1))
    expect_equal(idf$set_object(2, "No")$get_value(1), list("No"))
    expect_equal(idf$set_object(2, "Yes")$get_value(1), list("Yes"))

    expect_equal(trimws(idf$out()), idf_str)
})
