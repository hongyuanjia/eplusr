
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eplusr <img src="man/figures/logo.svg" align="right" />

<!-- badges: start -->

[![R build
status](https://github.com/hongyuanjia/eplusr/workflows/R-CMD-check/badge.svg)](https://github.com/hongyuanjia/eplusr/actions)
[![codecov](https://codecov.io/gh/hongyuanjia/eplusr/branch/master/graph/badge.svg?token=HoBA0Qm6k2)](https://codecov.io/gh/hongyuanjia/eplusr)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/eplusr)](https://cran.r-project.org/package=eplusr)
[![CRAN
Checks](https://cranchecks.info/badges/summary/eplusr)](https://cranchecks.info/pkgs/eplusr)
[![CRAN Download
Badge](https://cranlogs.r-pkg.org/badges/eplusr)](https://cran.r-project.org/package=eplusr)
<!-- badges: end -->

> A Toolkit for Using EnergyPlus in R.

eplusr provides a rich toolkit of using whole building energy simulation
program [EnergyPlus](https://energyplus.net) directly in R, which
enables programmatic navigation, modification of EnergyPlus, conducts
parametric simulations and retrieves outputs. More information about
EnergyPlus can be found at [its website](https://energyplus.net).

A comprehensive introduction to eplusr can be found using
[`vignette("eplusr")`](https://hongyuanjia.github.io/eplusr/articles/eplusr.html).
There is also an online slides here ([Interfacing EnergyPlus Using
R](https://hongyuanjia.github.io/eplusrIntro/)). You can learn more
about eplusr at <https://hongyuanjia.github.io/eplusr/>, along with full
package documentation.

## How to cite

``` r
citation("eplusr")
#> 
#> To cite eplusr in publications use:
#> 
#>   Hongyuan Jia, Adrian Chong (2020). eplusr: A framework for
#>   integrating building energy simulation and data-driven analytics.
#>   Energy and Buildings 237 (April): 110757.
#>   https://doi.org/10.1016/j.enbuild.2021.110757
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {eplusr: A framework for integrating building energy simulation and data-driven analytics},
#>     author = {Hongyuan Jia and Adrian Chong},
#>     year = {2020},
#>     journal = {Energy and Buildings},
#>     volume = {237},
#>     url = {https://CRAN.R-project.org/package=eplusr},
#>     doi = {https://doi.org/10.1016/j.enbuild.2021.110757},
#>   }
```

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
# install the latest version (currently v9.4.0)
eplusr::install_eplus("latest")

# OR download the latest version (currently v9.4.0) and run the installer
# manually by yourself
eplusr::download_eplus("latest", dir = tempdir())
```

Note that the installation process in `install_eplus()` requires
**administrative privileges**. You have to run R with administrator (or
with sudo if you are on macOS or Linux) to make it work if you are not
in interactive mode.

## Features

-   Download, install EnergyPlus in R
-   Read, parse and modify EnergyPlus:
    -   Input Data File (IDF)
    -   Weather File (EPW)
    -   Report Data Dictionary (RDD) & Meter Data Dictionary (MDD)
    -   Error File (ERR)
-   Modify multiple versions of IDFs and run corresponding EnergyPlus
    both in the background and in the front
-   Rich-featured interfaces to query and modify IDFs
-   Automatically handle referenced fields and validate input during
    modification
-   Take fully advantage of most common used data structure for data
    science in R – data.frame
    -   Extract model, weather data into data.frames
    -   Modify multiple objects via data.frames input
    -   Query output via SQL in Tidy format which is much better for
        data analysis and visualization
-   Provide a simple yet extensible prototype of conducting parametric
    simulations and collect all results in one go
-   A pure R-based version updater which is more than
    [20X](https://hongyuanjia.github.io/eplusr/articles/transition.html)
    faster than VersionUpdater distributed with EnergyPlus
-   Fast 3D geometry visualization

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

-   Hongyuan Jia, Adrian Chong (2020). eplusr: A framework for
    integrating building energy simulation and data-driven analytics.
    doi: 10.13140/RG.2.2.34326.16966
    -   [Source code and data to reproduce figures in the
        article](https://github.com/ideas-lab-nus/eplusr-paper)

### Vignettes

Please see these vignettes and articles about {eplusr}

-   [Introduction to
    eplusr](https://hongyuanjia.github.io/eplusr/articles/eplusr.html)
-   [Run simulation and data
    exploration](https://hongyuanjia.github.io/eplusr/articles/job.html)
-   [Parametric
    simulations](https://hongyuanjia.github.io/eplusr/articles/param.html)
-   [Update IDF
    version](https://hongyuanjia.github.io/eplusr/articles/transition.html)
-   [Work with weather
    files](https://hongyuanjia.github.io/eplusr/articles/epw.html)
-   [Work with `Schedule:Compact`
    objects](https://hongyuanjia.github.io/eplusr/articles/schedule.html)
-   [Work with
    geometries](https://hongyuanjia.github.io/eplusr/articles/geom.html)
-   [Frequently asked
    questions](https://hongyuanjia.github.io/eplusr/articles/faq.html)

### Slides

-   [Slides: Interfacing EnergyPlus using
    R](https://hongyuanjia.github.io/eplusrIntro/)

## Additional resources

-   eplusr manual: <https://hongyuanjia.github.io/eplusr/>
-   eplusr Docker image: <https://github.com/hongyuanjia/eplusr-docker>
-   [epwshiftr](https://CRAN.R-project.org/package=epwshiftr) for
    creating future EnergyPlus weather files using CMIP6 data
-   [epluspar](https://github.com/hongyuanjia/epluspar) for conducting
    parametric analysis on EnergyPlus models, including sensitivity
    analysis, Bayesian calibration and optimization.

## Acknowledgement

I would like to thank many open source projects who have heavily
inspired the development of eplusr package, especially these below:

-   [EnergyPlus](https://www.energyplus.net): A whole building energy
    simulation program.
-   [OpenStudio](https://www.openstudio.net): A cross-platform
    collection of software tools to support whole building energy
    modeling using EnergyPlus and advanced daylight analysis using
    Radiance.
-   [eppy](https://github.com/santoshphilip/eppy): Scripting language
    for E+, EnergyPlus.
-   [JEplus](http://www.jeplus.org): An EnergyPlus simulation manager
    for parametrics.

## Author

Hongyuan Jia

## License

The project is released under the terms of MIT License.

Copyright © 2016-2020 Hongyuan Jia

------------------------------------------------------------------------

Please note that the ‘eplusr’ project is released with a [Contributor
Code of
Conduct](https://github.com/hongyuanjia/eplusr/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
