
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eplusr <img src="man/figures/logo.svg" align="right" />

[![Travis-CI Build
Status](https://travis-ci.org/hongyuanjia/eplusr.svg?branch=master)](https://travis-ci.org/hongyuanjia/eplusr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/hongyuanjia/eplusr?branch=master&svg=true)](https://ci.appveyor.com/project/hongyuanjia/eplusr)
[![Coverage
Status](https://img.shields.io/codecov/c/github/hongyuanjia/eplusr/master.svg)](https://codecov.io/github/hongyuanjia/eplusr?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/eplusr)](https://cran.r-project.org/package=eplusr)
[![CRAN Download
Badge](https://cranlogs.r-pkg.org/badges/eplusr)](https://cran.rstudio.com/web/packages/eplusr/index.html)

> A Toolkit for Using EnergyPlus in R.

eplusr provides a rich toolkit of using whole building energy simulation
program [EnergyPlus](https://energyplus.net) directly in R, which
enables programmatic navigation, modification of EnergyPlus models and
makes it less painful to do parametric simulations and analysis. More
information about EnergyPlus can be found at [its
website](https://energyplus.net).

A comprehensive introduction to eplusr can be found using
[`vignette("eplusr")`](https://hongyuanjia.github.io/eplusr/articles/eplusr.html).
You can learn more about eplusr at
<https://hongyuanjia.github.io/eplusr>, along with full package
documentation.

## Installation

You can install the latest stable release of eplusr from CRAN.

``` r
install.packages("eplusr")
```

Alternatively, you can install the development version from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("hongyuanjia/eplusr")
```

Since running the IDF files requires EnergyPlus
(<https://energyplus.net>), EnergyPlus has to be installed if you want
to run EnergyPlus models in R. There are helper functions in eplusr to
download and install it automatically on major operating systems
(Windows, macOS and Linux):

``` r
# install the latest version (currently v8.9.0)
eplusr::install_eplus("latest")

# OR download the latest version (currently v8.9.0) and run the installer
# manually by yourself
eplusr::download_eplus("latest", dir = tempdir())
```

Note that the installation process in `install_eplus()` requires
**administrative privileges**. You have to run R with administrator (or
with sudo if you are on macOS or Linux) to make it work if you are not
in interactive mode.

## Features

  - Read, parse and modify EnergyPlus Weather File (EPW)
  - Read, parse and modify EnergyPlus Input Data File (IDF)
  - Query on models, including classes, objects and fields
  - Directly add, modify, duplicate, insert, and delete objects of IDF
  - Automatically change referenced fields when modifying objects
  - Save changed models into standard formats in the same way as
    IDFEditor distributed along with EnergyPlus
  - Run your models and collect the simulation output
  - Conduct parametric energy simulations and collect all results in one
    go

## Usage overview

### Class structure

Below shows the class structure in eplusr.

<p align="center">

<img src="man/figures/class_structure.png"/>

</p>

Basically, eplusr uses `Idf` class to present the whole IDF file and
`IdfObject` class to present a single object in an IDF. Both `Idf` and
`IdfObject` class contain member functions for helping modify the data
in IDF so it complies with the underlying EnergyPlus IDD (Input Data
Dictionary). Similarly, IDD file is wrapped into two classes, i.e. `Idd`
and `IddObject`.

Besides, `Epw` class is used to present EnergyPlus Weather files;
`EplusJob` to run single EnergyPlus simulation and collect outputs,
`ParametricJob` to run parametric EnergyPlus simulations and collect all
outputs.

It is highly recommended to read the documentation to get a thorough
understanding on each class.

### Getting started

``` r
library(eplusr)

idd <- use_idd(8.8, download = "auto")
#> Idd v8.8.0 has not been parsed before. Try to locate `Energy+.idd` in EnergyPlus v8.8.0 installation folder `/usr/local/EnergyPlus-8-8-0`.
#> IDD file found: `/usr/local/EnergyPlus-8-8-0/Energy+.idd`.
#> Start parsing...
#> 
  Parsing IDD (Parsing ) [=========>------------------------]  30% in  0s
  Parsing IDD (Parsing ) [=============>--------------------]  40% in  0s
  Parsing IDD (Parsing ) [================>-----------------]  50% in  1s
  Parsing IDD (Parsing ) [===================>--------------]  60% in  2s
  Parsing IDD (Parsing ) [==========================>-------]  80% in  3s
  Parsing IDD (Parsing ) [===============================>--]  95% in  3s
  Parsing IDD (Complete) [==================================] 100% in  3s

model <- read_idf(system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr"))

model
#> ══ EnergPlus Input Data File ══════════════════════════════════════════════
#> ● Path: `/home/hongyuanjia/R/eplusr/extdata/1ZoneUncontrolled.idf`
#> ● Version: `8.8`
#> 
#> Group: `Simulation Parameters`
#> ───────────────────────────────────────────────────────────────────────────
#> [01] Version
#> [01] SimulationControl
#> [01] Building
#> [01] SurfaceConvectionAlgorithm:Inside
#> [01] SurfaceConvectionAlgorithm:Outside
#> [01] HeatBalanceAlgorithm
#> [01] Timestep
#> 
#> Group: `Location and Climate`
#> ───────────────────────────────────────────────────────────────────────────
#> [01] Site:Location
#> [02] SizingPeriod:DesignDay
#> [01] RunPeriod
#> 
#> Group: `Schedules`
#> ───────────────────────────────────────────────────────────────────────────
#> [02] ScheduleTypeLimits
#> [01] Schedule:Constant
#> 
#> Group: `Surface Construction Elements`
#> ───────────────────────────────────────────────────────────────────────────
#> [01] Material
#> [02] Material:NoMass
#> [03] Construction
....

model$Material_NoMass$R13LAYER
#> IdfObject <<[ID:12] `R13LAYER`>>`Material:NoMass`
#> ──────────────────────────────── * VALUES * ───────────────────────────────
#> ●1: R13LAYER,          !- Name
#> ●2: Rough,             !- Roughness
#> ●3: 2.290965,          !- Thermal Resistance {m2-K/W}
#>  4: 0.9,               !- Thermal Absorptance
#>  5: 0.75,              !- Solar Absorptance
#>  6: 0.75;              !- Visible Absorptance
#> ───────────────────────────────────────────────────────────────────────────

model$RunPeriod[[1]][c("Begin Month", "End Month")]
#> $Begin_Month
#> [1] 1
#> 
#> $End_Month
#> [1] 12

model$add_object("RunPeriod",
    list(name = "run_period", begin_month = 3, begin_day_of_month = 1,
         end_month = 4, end_day_of_month = 1))
#> $run_period
#> IdfObject <<[ID:54] `run_period`>>`RunPeriod`
#> ──────────────────────────────── * VALUES * ───────────────────────────────
#>   1: run_period,        !- Name
#> ● 2: 3,                 !- Begin Month
#> ● 3: 1,                 !- Begin Day of Month
#> ● 4: 4,                 !- End Month
#> ● 5: 1,                 !- End Day of Month
#>   6: UseWeatherFile,    !- Day of Week for Start Day
#>   7: Yes,               !- Use Weather File Holidays and Special Days
#>   8: Yes,               !- Use Weather File Daylight Saving Period
#>   9: No,                !- Apply Weekend Holiday Rule
#>  10: Yes,               !- Use Weather File Rain Indicators
#>  11: Yes;               !- Use Weather File Snow Indicators
#> ───────────────────────────────────────────────────────────────────────────

model$Construction$FLOOR$possible_value("Outside Layer")
#> ── 2: Field `Outside Layer` ───────────────────────────────────────────────
#> ● References:
#>   - `R13LAYER`
#>   - `R31LAYER`
#>   - `C5 - 4 IN HW CONCRETE`

model$save(file.path(tempdir(), "model.idf"), overwrite = TRUE)

job <- model$run(
    weather = file.path(eplus_config(8.8)$dir, "WeatherData/USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"),
    dir = NULL)
#> ── Info ───────────────────────────────────────────────────────────────────
#> Adding object `Output:SQLite` and setting `Option Type` to `SimpleAndTabular`.
#> 
#> ── Info ───────────────────────────────────────────────────────────────────
#> Replace the existing file located  at /tmp/RtmpGWmNQZ/model.idf.
#> 
#> ExpandObjects Started.
#> No expanded file generated.
#> ExpandObjects Finished. Time:     0.005
#> EnergyPlus Starting
#> EnergyPlus, Version 8.8.0-7c3bbe4830, YMD=2018.08.22 17:21
#> Processing Data Dictionary
#> Processing Input File
#> Initializing Simulation
#> Reporting Surfaces
#> Beginning Primary Simulation
#> Initializing New Environment Parameters
#> Warming up {1}
#> Warming up {2}
#> Warming up {3}
#> Warming up {4}
#> Warming up {5}
#> Warming up {6}
#> Warming up {7}
#> Warming up {8}
#> Warming up {9}
#> Warming up {10}
#> Warming up {11}
#> Warming up {12}
#> Warming up {13}
....

job$errors()
#> 
#> ┌───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
#> │Warning[1/3] Weather file location will be used rather than entered (IDF) Location object.                                                 │
#> │  ..Location object=DENVER CENTENNIAL  GOLDEN   N_CO_USA DESIGN_CONDITIONS                                                                 │
#> │  ..Weather File Location=San Francisco Intl Ap CA USA TMY3 WMO#=724940                                                                    │
#> │  ..due to location differences, Latitude difference=[2.12] degrees, Longitude difference=[17.22] degrees.                                 │
#> │  ..Time Zone difference=[1.0] hour(s), Elevation difference=[99.89] percent, [1827.00] meters.                                            │
#> │                                                                                                                                           │
#> │Warning[2/3] SetUpDesignDay: Entered DesignDay Barometric Pressure=81198 differs by more than 10% from Standard Barometric Pressure=101301.│
#> │  ...occurs in DesignDay=DENVER CENTENNIAL  GOLDEN   N ANN HTG 99% CONDNS DB, Standard Pressure (based on elevation) will be used.         │
#> │                                                                                                                                           │
#> │Warning[3/3] SetUpDesignDay: Entered DesignDay Barometric Pressure=81198 differs by more than 10% from Standard Barometric Pressure=101301.│
#> │  ...occurs in DesignDay=DENVER CENTENNIAL  GOLDEN   N ANN CLG 1% CONDNS DB=>MWB, Standard Pressure (based on elevation) will be used.     │
#> └───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
#> 
#> EnergyPlus completed successfully with 3 Warning.

job$report_data(name = "EnergyTransfer:Facility", case = "example")
#>          Case            DateTime KeyValue                    Name Units
#>    1: example 2018-01-01 01:00:00          EnergyTransfer:Facility     J
#>    2: example 2018-01-01 02:00:00          EnergyTransfer:Facility     J
#>    3: example 2018-01-01 03:00:00          EnergyTransfer:Facility     J
#>    4: example 2018-01-01 04:00:00          EnergyTransfer:Facility     J
#>    5: example 2018-01-01 05:00:00          EnergyTransfer:Facility     J
#>   ---                                                                   
#> 9572: example 2018-12-31 20:00:00          EnergyTransfer:Facility     J
#> 9573: example 2018-12-31 21:00:00          EnergyTransfer:Facility     J
#> 9574: example 2018-12-31 22:00:00          EnergyTransfer:Facility     J
#> 9575: example 2018-12-31 23:00:00          EnergyTransfer:Facility     J
#> 9576: example 2019-01-01 00:00:00          EnergyTransfer:Facility     J
#>       Value
#>    1:     0
#>    2:     0
#>    3:     0
#>    4:     0
#>    5:     0
#>   ---      
#> 9572:     0
#> 9573:     0
#> 9574:     0
#> 9575:     0
#> 9576:     0
```

## Acknowledgements

I would like to thank many open source projects who have heavily
inspired the development of eplusr package, especially these below:

  - [OpenStudio](https://www.openstudio.net)
  - [eppy: scripting language for E+,
    Energyplus](https://github.com/santoshphilip/eppy)

## Author

Hongyuan Jia

*Faculty of Urban Construction and Environmental Engineering, Chongqing
University*

## License

The project is released under the terms of the GPLv3.

Copyright © 2016-2018 Hongyuan Jia
