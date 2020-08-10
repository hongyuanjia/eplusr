
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eplusr <img src="man/figures/logo.svg" align="right" />

[![Travis-CI Build
Status](https://travis-ci.com/hongyuanjia/eplusr.svg?branch=master)](https://travis-ci.com/hongyuanjia/eplusr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/hongyuanjia/eplusr?branch=master&svg=true)](https://ci.appveyor.com/project/hongyuanjia/eplusr)
[![codecov](https://codecov.io/gh/hongyuanjia/eplusr/branch/master/graph/badge.svg)](https://codecov.io/gh/hongyuanjia/eplusr)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/eplusr)](https://cran.r-project.org/package=eplusr)
[![CRAN
Checks](https://cranchecks.info/badges/summary/eplusr)](https://cranchecks.info/pkgs/eplusr)
[![CRAN Download
Badge](https://cranlogs.r-pkg.org/badges/eplusr)](https://cran.r-project.org/package=eplusr)

> A Toolkit for Using EnergyPlus in R.

eplusr provides a rich toolkit of using whole building energy simulation
program [EnergyPlus](https://energyplus.net) directly in R, which
enables programmatic navigation, modification of EnergyPlus, conducts
parametric simulations and retrieves outputs. More information about
EnergyPlus can be found at [its website](https://energyplus.net).

A comprehensive introduction to eplusr can be found using
[`vignette("eplusr")`](https://hongyuanjia.github.io/eplusr/articles/eplusr.html).
There is also an online slides here ([Interfacing EnergyPlus Using
R](https://hongyuanjia.github.io/eplusrIntro)). You can learn more about
eplusr at <https://hongyuanjia.github.io/eplusr>, along with full
package documentation.

## Installation

You can install the latest stable release of eplusr from CRAN.

``` r
install.packages("eplusr")
```

Alternatively, you can install the development version from GitHub.

``` r
# install.packages("remotes")
remotes::install_github("hongyuanjia/eplusr")
```

Since running the IDF files requires EnergyPlus
(<https://energyplus.net>), EnergyPlus has to be installed if you want
to run EnergyPlus models in R. There are helper functions in eplusr to
download and install it automatically on major operating systems
(Windows, macOS and Linux):

``` r
# install the latest version (currently v9.1.0)
eplusr::install_eplus("latest")

# OR download the latest version (currently v9.1.0) and run the installer
# manually by yourself
eplusr::download_eplus("latest", dir = tempdir())
```

Note that the installation process in `install_eplus()` requires
**administrative privileges**. You have to run R with administrator (or
with sudo if you are on macOS or Linux) to make it work if you are not
in interactive mode.

## Features

  - Download, install EnergyPlus in R
  - Read, parse and modify EnergyPlus:
      - Input Data File (IDF)
      - Weather File (EPW)
      - Report Data Dictionary (RDD) & Meter Data Dictionary (MDD)
      - Error File (ERR)
  - Modify multiple versions of IDFs and run corresponding EnergyPlus
    both in the background and in the front
  - Rich-featured interfaces to query and modify IDFs
  - Automatically handle referenced fields and validate input during
    modification
  - Take fully advantage of most common used data structure for data
    science in R – data.frame
      - Extract model, weather data into data.frames
      - Modify multiple objects via data.frames input
      - Query output via SQL in Tidy format which is much better for
        data analysis and visualization
  - Provide a simple yet extensible prototype of conducting parametric
    simulations and collect all results in one go
  - A pure R-based version updater which is more than
    [20X](https://hongyuanjia.me/en/2019/08/update-energyplus-using-eplusr-transition)
    faster than VersionUpdater distributed with EnergyPlus

**View IDF geometry in 3D**  
<img src="https://github.com/hongyuanjia/eplusr/blob/master/tools/figures/view_geometry.gif?raw=true" width="60%" />

**Turn RStudio into a model editor via autocompletion**  
<img src="https://github.com/hongyuanjia/eplusr/blob/master/tools/figures/autocomplete.gif?raw=true" width="60%" />

**Query and modify weather file**  
<img src="https://github.com/hongyuanjia/eplusr/blob/master/tools/figures/epw.gif?raw=true" width="60%" />

**Query output via SQL in Tidy format which is much better for data
analysis**  
<img src="https://github.com/hongyuanjia/eplusr/blob/master/tools/figures/job.gif?raw=true" width="60%" />

## Resources

### Articles

  - Hongyuan Jia, Adrian Chong (2020). eplusr: A framework for
    integrating building energy simulation and data-driven analytics.
    doi: 10.13140/RG.2.2.34326.16966
      - [Source code and data to reproduce figures in the
        article](https://github.com/ideas-lab-nus/eplusr-paper)

### Vignettes

Please see these vignettes and articles about {eplusr}

  - [Introduction to
    eplusr](https://hongyuanjia.github.io/eplusr/articles/idf.html)
  - [Run simulation and data
    exploration](https://hongyuanjia.github.io/eplusr/articles/job.html)
  - [Rarametric
    simulations](https://hongyuanjia.github.io/eplusr/articles/param.html)
  - [Work with weather
    files](https://hongyuanjia.github.io/eplusr/articles/epw.html)
  - [Update IDF
    version](https://hongyuanjia.github.io/eplusr/articles/transition.html)
  - [Work with `Schedule:Compact`
    objects](https://hongyuanjia.github.io/eplusr/articles/schedule.html)
  - [Frequently asked
    questions](https://hongyuanjia.github.io/eplusr/articles/faq.html)

### Slides

  - [Slides: Interfacing EnergyPlus using
    R](https://hongyuanjia.github.io/eplusrIntro)

## Getting started

``` r
# read IDF
idf <- read_idf(system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr"))
idf
#> ── EnergPlus Input Data File ────────────────────────────
#>  * Path: '/mnt/c/Users/hongy/Dropbox/github_repo/epl...
#>  * Version: '8.8.0'
#> 
#> Group: <Simulation Parameters>
#> ├─ [01<O>] Class: <Version>
#> │─ [01<O>] Class: <SimulationControl>
#> │─ [01<O>] Class: <Building>
#> │─ [01<O>] Class: <SurfaceConvectionAlgorithm:Inside>
#> │─ [01<O>] Class: <SurfaceConvectionAlgorithm:Outside>
#> │─ [01<O>] Class: <HeatBalanceAlgorithm>
#> └─ [01<O>] Class: <Timestep>
#> 
#> Group: <Location and Climate>
#> ├─ [01<O>] Class: <Site:Location>
#> │─ [02<O>] Class: <SizingPeriod:DesignDay>
#> └─ [01<O>] Class: <RunPeriod>
#> 
#> Group: <Schedules>
#> ├─ [02<O>] Class: <ScheduleTypeLimits>
#> └─ [01<O>] Class: <Schedule:Constant>
#> 
#> Group: <Surface Construction Elements>
#> ├─ [01<O>] Class: <Material>
#> │─ [02<O>] Class: <Material:NoMass>
#> └─ [03<O>] Class: <Construction>
#> 
#> Group: <Thermal Zones and Surfaces>
#> ├─ [01<O>] Class: <GlobalGeometryRules>
#> │─ [01<O>] Class: <Zone>
....

# extract object
idf$Material_NoMass$R13LAYER
#> <IdfObject: 'Material:NoMass'> [ID:12] `R13LAYER`
#> Class: <Material:NoMass>
#> ├─ 1*: "R13LAYER", !- Name
#> │─ 2*: "Rough",    !- Roughness
#> │─ 3*: 2.290965,   !- Thermal Resistance {m2-K/W}
#> │─ 4 : 0.9,        !- Thermal Absorptance
#> │─ 5 : 0.75,       !- Solar Absorptance
#> └─ 6 : 0.75;       !- Visible Absorptance

# get object relation
idf$object_relation("R13LAYER", "all")
#> ── Refer to Others ──────────────────────────────────────
#> Target(s) does not refer to any other field.
#> 
#> ── Referred by Others ───────────────────────────────────
#>  Class: <Material:NoMass>
#>  └─ Object [ID:12] <R13LAYER>
#>      └─ 1: "R13LAYER";    !- Name
#>         ^~~~~~~~~~~~~~~~~~~~~~~~~
#>         └─ Class: <Construction>
#>            └─ Object [ID:15] <R13WALL>
#>               └─ 2: "R13LAYER";    !- Outside Layer
#>     
#> 
#> ── Node Relation ────────────────────────────────────────
#> Target(s) has no node or their nodes have no reference to other object.

# extract field value
idf$RunPeriod[[1]][c("Begin Month", "End Month")]
#> $`Begin Month`
#> [1] 1
#> 
#> $`End Month`
#> [1] 12

# add new object
idf$add(RunPeriod = list("run_period", 3, 1, 4, 1))
#> $run_period
#> <IdfObject: 'RunPeriod'> [ID:54] `run_period`
#> Class: <RunPeriod>
#> ├─ 01 : "run_period",     !- Name
#> │─ 02*: 3,                !- Begin Month
#> │─ 03*: 1,                !- Begin Day of Month
#> │─ 04*: 4,                !- End Month
#> │─ 05*: 1,                !- End Day of Month
#> │─ 06 : "UseWeatherFile", !- Day of Week for Start Day
#> │─ 07 : "Yes",            !- Use Weather File Holida...
#> │─ 08 : "Yes",            !- Use Weather File Daylig...
#> │─ 09 : "No",             !- Apply Weekend Holiday R...
#> │─ 10 : "Yes",            !- Use Weather File Rain I...
#> └─ 11 : "Yes";            !- Use Weather File Snow I...

# purge unused resource objects
idf$purge(group = "Schedules")
#> Object(s) below have been purged:
#>  #1| Object ID [19] (name 'Fraction') in class 'ScheduleTypeLimits'

# get possible values for fields
idf$Construction$FLOOR$value_possible("Outside Layer")
#> ── 2: Outside Layer ─────────────────────────────────────
#> * Auto value: <NA>
#> * Default: <NA>
#> * Choice: <NA>
#> * Source: 
#>   - "C5 - 4 IN HW CONCRETE"
#>   - "R13LAYER"
#>   - "R31LAYER"

# extract object data
idf$to_table(class = "BuildingSurface:Detailed", string_value = FALSE, unit = TRUE)
#>         id          name                    class index
#>      <int>        <char>                   <char> <int>
#>   1:    21 Zn001:Wall001 BuildingSurface:Detailed     1
#>   2:    21 Zn001:Wall001 BuildingSurface:Detailed     2
#>   3:    21 Zn001:Wall001 BuildingSurface:Detailed     3
#>   4:    21 Zn001:Wall001 BuildingSurface:Detailed     4
#>   5:    21 Zn001:Wall001 BuildingSurface:Detailed     5
#>  ---                                                   
#> 128:    26 Zn001:Roof001 BuildingSurface:Detailed    18
#> 129:    26 Zn001:Roof001 BuildingSurface:Detailed    19
#> 130:    26 Zn001:Roof001 BuildingSurface:Detailed    20
#> 131:    26 Zn001:Roof001 BuildingSurface:Detailed    21
#> 132:    26 Zn001:Roof001 BuildingSurface:Detailed    22
#>                           field         value
#>                          <char>        <list>
#>   1:                       Name Zn001:Wall001
#>   2:               Surface Type          Wall
#>   3:          Construction Name       R13WALL
#>   4:                  Zone Name      ZONE ONE
#>   5: Outside Boundary Condition      Outdoors
#>  ---                                         
#> 128:      Vertex 3 Y-coordinate         0 [m]
#> 129:      Vertex 3 Z-coordinate     4.572 [m]
#> 130:      Vertex 4 X-coordinate     15.24 [m]
#> 131:      Vertex 4 Y-coordinate     15.24 [m]
#> 132:      Vertex 4 Z-coordinate     4.572 [m]

# save the IDF
idf$save(file.path(tempdir(), "model.idf"), overwrite = TRUE)

# read EPW
path_epw <- file.path(eplus_config(8.8)$dir, "WeatherData/USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")
epw <- read_epw(path_epw)
epw
#> ══ EnergyPlus Weather File ══════════════════════════════
#> [Location ]: San Francisco Intl Ap, CA, USA
#>              {N 37°37'}, {W 122°24'}, {UTC-08:00}
#> [Elevation]: 2m above see level
#> [Data Src ]: TMY3
#> [WMO Stat ]: 724940
#> [Leap Year]: No
#> [Interval ]: 60 mins
#> 
#> ── Data Periods ─────────────────────────────────────────
#>    Name StartDayOfWeek StartDay EndDay
#> 1: Data         Sunday     1/ 1  12/31
#> 
#> ─────────────────────────────────────────────────────────

# get location
(loc <- epw$location())
#> $city
#> [1] "San Francisco Intl Ap"
#> 
#> $state_province
#> [1] "CA"
#> 
#> $country
#> [1] "USA"
#> 
#> $data_source
#> [1] "TMY3"
#> 
#> $wmo_number
#> [1] "724940"
#> 
#> $latitude
#> [1] 37.62
#> 
#> $longitude
#> [1] -122.4
#> 
#> $time_zone
#> [1] -8
#> 
#> $elevation
#> [1] 2

# extract weather data
(weather <- head(epw$data()))
#>               datetime  year month   day  hour minute
#>                 <POSc> <int> <int> <int> <int>  <int>
#> 1: 2017-01-01 01:00:00  1999     1     1     1      0
#> 2: 2017-01-01 02:00:00  1999     1     1     2      0
#> 3: 2017-01-01 03:00:00  1999     1     1     3      0
#> 4: 2017-01-01 04:00:00  1999     1     1     4      0
#> 5: 2017-01-01 05:00:00  1999     1     1     5      0
#> 6: 2017-01-01 06:00:00  1999     1     1     6      0
#>                                           data_source
#>                                                <char>
#> 1: ?9?9?9?9E0?9?9?9?9?9?9?9?9?9?9?9?9?9?9?9*9*9?9?9?9
#> 2: ?9?9?9?9E0?9?9?9?9?9?9?9?9?9?9?9?9?9?9?9*9*9?9?9?9
#> 3: ?9?9?9?9E0?9?9?9?9?9?9?9?9?9?9?9?9?9?9?9*9*9?9?9?9
#> 4: ?9?9?9?9E0?9?9?9?9?9?9?9?9?9?9?9?9?9?9?9*9*9?9?9?9
#> 5: ?9?9?9?9E0?9?9?9?9?9?9?9?9?9?9?9?9?9?9?9*9*9?9?9?9
#> 6: ?9?9?9?9E0?9?9?9?9?9?9?9?9?9?9?9?9?9?9?9*9*9?9?9?9
#>    dry_bulb_temperature dew_point_temperature
#>                   <num>                 <num>
#> 1:                  7.2                   5.6
#> 2:                  7.2                   5.6
#> 3:                  6.7                   5.0
#> 4:                  6.1                   5.0
#> 5:                  4.4                   3.9
#> 6:                  4.4                   3.9
#>    relative_humidity atmospheric_pressure
#>                <num>                <num>
#> 1:                90               102200
#> 2:                90               102100
#> 3:                89               102200
#> 4:                93               102200
....

# a date time column added with correct start day of week type
epw$period()$start_day_of_week
#> [1] "Sunday"
weekdays(weather$datetime)
#> [1] "Sunday" "Sunday" "Sunday" "Sunday" "Sunday" "Sunday"

# run simulation
job <- idf$run(epw)
#> Adding an object in class 'Output:SQLite' and setting its 'Option Type' to 'SimpleAndTabular' in order to create SQLite output file.
#> Replace the existing IDF located at /tmp/RtmpgL7ywt/model.idf.
#> EnergyPlus Starting
#> EnergyPlus, Version 8.8.0-7c3bbe4830, YMD=2020.08.10 14:34
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
#> Warming up {14}
#> Warming up {15}
#> Warming up {16}
#> Warming up {17}
#> Warming up {18}
#> Warming up {19}
#> Warming up {20}
#> Warming up {21}
#> Warming up {22}
....

# print simulation error
job$errors()
#> ══ EnergyPlus Error File ════════════════════════════════
#>   * EnergyPlus version: 8.8.0 (7c3bbe4830)
#>   * Simulation started: 2020-08-10 14:34:00
#>   * Terminated: FALSE
#>   * Successful: TRUE
#>   * Warning[W]: 3
#> 
#> ── During Simulation Initiation ─────────────────────────
#> [W 1/3] Weather file location will be used rather than
#>         entered (IDF) Location object.
#>         ..Location object=DENVER CENTENNIAL GOLDEN N_CO_USA
#>         DESIGN_CONDITIONS
#>         ..Weather File Location=San Francisco Intl Ap CA
#>         USA TMY3 WMO#=724940
#>         ..due to location differences, Latitude
#>         difference=[2.12] degrees, Longitude
#>         difference=[17.22] degrees.
#>         ..Time Zone difference=[1.0] hour(s), Elevation
#>         difference=[99.89] percent, [1827.00] meters.
#> [W 2/3] SetUpDesignDay: Entered DesignDay Barometric
#>         Pressure=81198 differs by more than 10% from
#>         Standard Barometric Pressure=101301.
#>         ...occurs in DesignDay=DENVER CENTENNIAL GOLDEN N
#>         ANN HTG 99% CONDNS DB, Standard Pressure (based on
#>         elevation) will be used.
#> [W 3/3] SetUpDesignDay: Entered DesignDay Barometric
#>         Pressure=81198 differs by more than 10% from
#>         Standard Barometric Pressure=101301.
#>         ...occurs in DesignDay=DENVER CENTENNIAL GOLDEN N
#>         ANN CLG 1% CONDNS DB=>MWB, Standard Pressure (based
....

# get report data
results <- job$report_data("zone one", "zone mean air temperature",
  case = "example", month = 1:6, hour = 1, day_type = "Monday",
  all = TRUE
)
str(results)
#> Classes 'data.table' and 'data.frame':   29 obs. of  22 variables:
#>  $ case                    : chr  "example" "example" "example" "example" ...
#>  $ datetime                : POSIXct, format: "2017-03-06 01:00:00" ...
#>  $ month                   : int  3 3 3 3 1 1 1 1 2 2 ...
#>  $ day                     : int  6 13 20 27 7 14 21 28 4 11 ...
#>  $ hour                    : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ minute                  : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ dst                     : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ interval                : int  60 60 60 60 60 60 60 60 60 60 ...
#>  $ simulation_days         : int  6 13 20 27 7 14 21 28 35 42 ...
#>  $ day_type                : chr  "Monday" "Monday" "Monday" "Monday" ...
#>  $ environment_name        : chr  "RUN_PERIOD" "RUN_PERIOD" "RUN_PERIOD" "RUN_PERIOD" ...
#>  $ environment_period_index: int  3 3 3 3 4 4 4 4 4 4 ...
#>  $ is_meter                : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ type                    : chr  "Avg" "Avg" "Avg" "Avg" ...
#>  $ index_group             : chr  "Zone" "Zone" "Zone" "Zone" ...
#>  $ timestep_type           : chr  "HVAC System" "HVAC System" "HVAC System" "HVAC System" ...
#>  $ key_value               : chr  "ZONE ONE" "ZONE ONE" "ZONE ONE" "ZONE ONE" ...
#>  $ name                    : chr  "Zone Mean Air Temperature" "Zone Mean Air Temperature" "Zone Mean Air Temperature" "Zone Mean Air Temperature" ...
#>  $ reporting_frequency     : chr  "Hourly" "Hourly" "Hourly" "Hourly" ...
#>  $ schedule_name           : chr  NA NA NA NA ...
#>  $ units                   : chr  "C" "C" "C" "C" ...
#>  $ value                   : num  15.9 18.1 16.2 14.9 10.8 ...
#>  - attr(*, ".internal.selfref")=<externalptr>

# a date time column added with correct day of week type
all(weekdays(results$datetime) == results$day_type)
#> [1] TRUE

# get tabular data
job$tabular_data(table_name = "site and source energy", row_name = "total site energy", wide = TRUE)
#> $`AnnualBuildingUtilityPerformanceSummary.Entire Facility.Site and Source Energy`
#>      case                             report_name
#>    <char>                                  <char>
#> 1:  model AnnualBuildingUtilityPerformanceSummary
#>         report_for             table_name
#>             <char>                 <char>
#> 1: Entire Facility Site and Source Energy
#>             row_name Total Energy [GJ]
#>               <char>             <num>
#> 1: Total Site Energy             89.81
#>    Energy Per Total Building Area [MJ/m2]
#>                                     <num>
#> 1:                                 386.67
#>    Energy Per Conditioned Building Area [MJ/m2]
#>                                           <num>
#> 1:                                           NA
```

## How to cite

To cite eplusr package in publications, please use:

> Hongyuan Jia and Adrian Chong, (2020). eplusr: A framework for
> integrating building energy simulation and data-driven analytics.
> <https://CRAN.R-project.org/package=eplusr>. doi:
> 10.13140/RG.2.2.34326.16966

## Additional resources

  - eplusr manual: <https://hongyuanjia.github.io/eplusr/>
  - eplusr Docker image: <https://github.com/hongyuanjia/eplusr-docker>
  - [epwshiftr](https://CRAN.R-project.org/package=epwshiftr) for
    creating future EnergyPlus weather files using CMIP6 data
  - [epluspar](https://github.com/hongyuanjia/epluspar) for conducting
    parametric analysis on EnergyPlus models, including sensitivity
    analysis, Bayesian calibration and optimization.

## Acknowledgement

I would like to thank many open source projects who have heavily
inspired the development of eplusr package, especially these below:

  - [EnergyPlus](https://www.energyplus.net): A whole building energy
    simulation program.
  - [OpenStudio](https://www.openstudio.net): A cross-platform
    collection of software tools to support whole building energy
    modeling using EnergyPlus and advanced daylight analysis using
    Radiance.
  - [eppy](https://github.com/santoshphilip/eppy): Scripting language
    for E+, EnergyPlus.
  - [JEplus](http://www.jeplus.org): An EnergyPlus simulation manager
    for parametrics.

## Author

Hongyuan Jia

## License

The project is released under the terms of MIT License.

Copyright © 2016-2020 Hongyuan Jia

-----

Please note that the ‘eplusr’ project is released with a [Contributor
Code of
Conduct](https://github.com/hongyuanjia/eplusr/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
