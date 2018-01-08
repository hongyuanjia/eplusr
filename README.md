
<!-- README.md is generated from README.Rmd. Please edit that file -->



# eplusr

[![Travis-CI Build Status](https://travis-ci.org/hongyuanjia/eplusr.svg?branch=master)](https://travis-ci.org/hongyuanjia/eplusr)[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/hongyuanjia/eplusr?branch=master&svg=true)](https://ci.appveyor.com/project/hongyuanjia/eplusr)<!--[![Coverage Status](https://img.shields.io/codecov/c/github/hongyuanjia/eplusr/master.svg)](https://codecov.io/github/hongyuanjia/eplusr?branch=master)-->[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/eplusr)](https://cran.r-project.org/package=eplusr)

> A Toolkit for Using EnergyPlus in R.

IDFEditor distributed along with [EnergyPlus](https://www.energyplus.net)
provides full support for preparing EnergyPus IDF and IMF files for
simulations. The parsing and writing process of IDF and IDD files in `eplusr`
is basically the same as that in IDFEditor. But `eplusr` takes advantage of the
powerful [`data.table`](http://r-datatable.com) package to speed up the whole
process and store the results. The IDD files for EnergyPlus 8.5 to 8.8 have
been pre-parsed and stored internally and will automatically be used when
parsing `IDF` and `IMF` files. The source codes of IDFEditor can be
found on [GitHub](https://github.com/NREL/EnergyPlus/tree/develop/src/IDF_Editor)
. There is still an option to give an additional IDD file path to parse if you
want. However, it will still take about 3-4 sec to parse an IDD file which is
much slower than IDFEditor written in Visual Basic.

Basically, all model data are stored as `data.table`s. And each object in the
model has an unique `ID`. Once you have the object `ID`, you can set fields
(using `$set`) in the object, duplicate (using `$dup`), delete (using `$del`)
the object. A full example of reading and editing an `IDF` file is given in
[Usage](#usage).

Also, `eplusr` has the functionalities of running EnergyPlus, and collecting
output.

---

-   [Warning](#warning)
-   [Installation](#installation)
-   [Features](#features)
-   [Usage](#usage)
    -   [Read and parse](#read-and-parse)
    -   [Query](#query)
    -   [Modify](#modify)
    -   [Notes](#notes)
    -   [Diff](#diff)
    -   [Check](#check)
    -   [Save](#save)
    -   [Reset](#reset)
    -   [Run and Collect Output](#run-and-collect-output)
-   [License](#license)

## Warning

This package is still in its infant stage of development and is subject to
change. It is not recommended to use it in working environment.

## Installation

`eplusr` is currently not on CRAN. You can install `eplusr` from GitHub with:


```r
# install.packages("devtools")
devtools::install_github("hongyuanjia/eplusr")
```

## Features

* Read and parse EnergyPlus `IDF`, `IMF` files
* Query on models, including classes, objects and fields
* Directly add, modify, duplicate, and delete objects of `IDF` and `IMF` files
  in R.
* Automatically change referred fields when modifying objects.
* Save the changed models into standard formats in the same way as IDFEditor
  distributed along with EnergyPlus.
* Run your models directly in R with customized run period which was directly
  set in R.
* Collect the simulation output of EnergyPlus in R.

## Usage


```r
library(eplusr)
```

For detailed reference, please see package documentation: `help(package
= "eplusr")`.

### Read and parse

All reading process starts with creating a `R6Class` called `eplus_model`. The
model will be printed in a similar style you see in IDFEditor, with an
additional heading lines show the `Path`, `Version` and `Type` of the model.
The classes of objects in the model are ordered by Group and the number of
objects in classes are shown in square bracket. All `Energy+.idd` files from
EnergyPlus v8.5 to v8.8 have been pre-parsed and stored. So you can just ignore
the `idd` argument if you are using those versions. If not, just pass the path
of the `Energy+.idd` file using `idd`.


```r
model <- eplus_model$new(path = system.file("extdata", "5Zone_Transformer.idf", package = "eplusr"), idd = NULL)

model
#> [ Path  ]: C:/Users/hongy/Documents/R/win-library/3.3/eplusr/extdata/5Zone_Transformer.idf
#> [Version]: 8.8
#> [ Type  ]: IDF
#> ===========================================================================
#>
#> Simulation Parameters
#> ---------------------------------------------------------------------------
#> [01] Version
#> [01] SimulationControl
#> [01] Building
#> [01] SurfaceConvectionAlgorithm:Inside
#> [01] SurfaceConvectionAlgorithm:Outside
#> [01] HeatBalanceAlgorithm
#> [01] Timestep
#>
#> Location and Climate
#> ---------------------------------------------------------------------------
#> [01] Site:Location
#> [02] SizingPeriod:DesignDay
#> [02] RunPeriod
#> [01] Site:GroundTemperature:BuildingSurface
#>
#> Schedules
#> ---------------------------------------------------------------------------
#> [06] ScheduleTypeLimits
#> [23] Schedule:Compact
#>
#> Surface Construction Elements
#> ---------------------------------------------------------------------------
#> [10] Material
....
```

### Query

#### `$all`

You can list all valid components you specified using `$all`. The `type`
argument will determine what kind of components you want to see.

All objects in the model will have an unique `ID` according to their sequences.
You can find all valid `ID`s using `$all(type = "id")`. The model will be
printed in a way that is much similar with what you see when you open your
model in a text editor, except that each object is bound an unique `ID` and
only first two lines of each object will be shown.


```r
model$all("id")
#> [ID:  1] Version,
#>     8.8;                     !- Version Identifier
#>
#> [ID:  2] SimulationControl,
#>     Yes,                     !- Do Zone Sizing Calculation
#>     Yes,                     !- Do System Sizing Calculation
#>     ........
#>
#> [ID:  3] Building,
#>     Building,                !- Name
#>     30.,                     !- North Axis {deg}
#>     ........
#>
#> [ID:  4] SurfaceConvectionAlgorithm:Inside,
#>     Simple;                  !- Algorithm
#>
#> [ID:  5] SurfaceConvectionAlgorithm:Outside,
#>     SimpleCombined;          !- Algorithm
#>
#> [ID:  6] HeatBalanceAlgorithm,
....
```

If you want to see all classes in your model, you can use `$all(type = "class")`.


```r
model$all(type = "class")
#>  [1] "Version"
#>  [2] "SimulationControl"
#>  [3] "Building"
#>  [4] "SurfaceConvectionAlgorithm:Inside"
#>  [5] "SurfaceConvectionAlgorithm:Outside"
#>  [6] "HeatBalanceAlgorithm"
#>  [7] "Timestep"
#>  [8] "Site:Location"
#>  [9] "SizingPeriod:DesignDay"
#> [10] "RunPeriod"
#> [11] "Site:GroundTemperature:BuildingSurface"
#> [12] "ScheduleTypeLimits"
#> [13] "Schedule:Compact"
#> [14] "Material"
....
```

You can find all available fields for all valid class in IDD using `$all(type =
"field", class = "any_valid_class_in_IDD")` which makes it handy to be used
along with `$add`, which you will see below. All required fields are marked
with `*`. For example, you can find all valid fields in class `Material`:


```r
model$all(type = "field", class = "Material")
#> *1: Name
#> *2: Roughness
#> *3: Thickness {m}
#> *4: Conductivity {W/m-K}
#> *5: Density {kg/m3}
#> *6: Specific Heat {J/kg-K}
#>  7: Thermal Absorptance
#>  8: Solar Absorptance
#>  9: Visible Absorptance
```

#### `$contains` & `$matches`

`$contains` and `$matches` will search and return objects that contain the
string or match the regular expression you give. The `scale` argument will
determine where you want to search. If `class`, only class names existing in
current model will be searched. If `field`, only fields in current model will
be searched. This is a handy option when you want to see if an object, e.g. one
`Material`, is referred by other objects, e.g. `Construction`s.


```r
model$contains(match = "Algorithm", scale = "class")
#>
#> == * 1 Objects Found in Class: SurfaceConvectionAlgorithm:Inside * ========
#>
#> [ID:4] SurfaceConvectionAlgorithm:Inside,
#>     Simple;                  !- Algorithm
#>
#>
#> == * 1 Objects Found in Class: SurfaceConvectionAlgorithm:Outside * =======
#>
#> [ID:5] SurfaceConvectionAlgorithm:Outside,
#>     SimpleCombined;          !- Algorithm
#>
#>
#> == * 1 Objects Found in Class: HeatBalanceAlgorithm * =====================
#>
#> [ID:6] HeatBalanceAlgorithm,
#>     ConductionTransferFunction;  !- Algorithm
```

If the `scale` is set to `field`, all matched fields will be marked with `(*)`.
What's more, `$matches` accepts extra arguments using `...`. All those
arguments will be directly passed to `grepl`.


```r
model$matches(match = "mat-clng-1", scale = "field", ignore.case = TRUE)
#> == * 2 Matched Fields Found * =============================================
#>
#> [ID:55] Material:NoMass,
#>     (*)MAT-CLNG-1,           !- Name
#>        Rough,                !- Roughness
#>        0.652259290,          !- Thermal Resistance {m2-K/W}
#>        0.65,                 !- Thermal Absorptance
#>        0.65,                 !- Solar Absorptance
#>        0.65;                 !- Visible Absorptance
#>
#> [ID:68] Construction,
#>        CLNG-1,               !- Name
#>     (*)MAT-CLNG-1;           !- Outside Layer
```

#### `$get`

`$get` will return you the objects with valid IDs or all objects in classes you
give.


```r
model$get(1, 2, 38)
#> [ID: 1] Version,
#> 1:       8.8;                     !- Version Identifier
#>
#> [ID: 2] SimulationControl,
#> 1:       Yes,                     !- Do Zone Sizing Calculation
#> 2:       Yes,                     !- Do System Sizing Calculation
#> 3:       Yes,                     !- Do Plant Sizing Calculation
#> 4:       No,                      !- Run Simulation for Sizing Periods
#> 5:       Yes;                     !- Run Simulation for Weather File Run Periods
#>
#> [ID:38] Schedule:Compact,
#> 1:       PlantOnSched,            !- Name
#> 2:       Fraction,                !- Schedule Type Limits Name
#> 3:       Through: 12/31,          !- Field 1
#> 4:       For: AllDays,            !- Field 2
#> 5:       Until: 24:00,            !- Field 3
#> 6:       1.0;                     !- Field 4
```


```r
model$get("Version", "Zone")
#> [ID: 1] Version,
#> 1:       8.8;                     !- Version Identifier
#>
#> [ID:74] Zone,
#> 1:       PLENUM-1,                !- Name
#> 2:       0,                       !- Direction of Relative North {deg}
#> 3:       0,                       !- X Origin {m}
#> 4:       0,                       !- Y Origin {m}
#> 5:       0,                       !- Z Origin {m}
#> 6:       1,                       !- Type
#> 7:       1,                       !- Multiplier
#> 8:       0.609600067,             !- Ceiling Height {m}
#> 9:       283.2;                   !- Volume {m3}
#>
#> [ID:75] Zone,
#> 1:       SPACE1-1,                !- Name
#> 2:       0,                       !- Direction of Relative North {deg}
#> 3:       0,                       !- X Origin {m}
#> 4:       0,                       !- Y Origin {m}
#> 5:       0,                       !- Z Origin {m}
....
```

### Modify

You can add, duplicate, modify and delete objects using `$add`, `$dup`, `$set`
and `$del` or `$hide` respectively.

All newly added, modified, deleted and hidden fields will be marked with `(+)`,
`(~)`, `(-)` and `(!)` respectively. The valid IDs will be appended after `$add`
and `$dup`, and the newly added (or duplicated) object will have the max ID.

For `$add` and `$set`, new field values should be given. Currently three styles
of value are acceptable:

* directly list all field values with no name. The values will be assigned to
  fields according to the order of values;
* give both field names and values in pair, e.g. `Name = "Test", "Specific
  Heat" = 150`. You can find all valid field names (with units) using
  `$all("field", class = "class_name_to_query")`;
* some kind of the same as above, but with all field names in lower cases and
  spaces replaced by `_`, e.g. `name = "Test", specific_heat = 150`.

> Note: All field names should be given **without** units. Error will occur when
the type (character or numeric), and the value (e.g. range) is not valid.

#### `$add`

`$add` will add an object in the class you give. All fields will be set to
their defaults if applicable.


```r
model$add(class = "Material", name = "test_add", roughness = "Rough",
          thickness = 0.8, conductivity = 55, density = 55,
          specific_heat = 100)
#> [ID:323] Material,
#> 1:(+)    test_add,                !- Name
#> 2:(+)    Rough,                   !- Roughness
#> 3:(+)    0.8,                     !- Thickness {m}
#> 4:(+)    55,                      !- Conductivity {W/m-K}
#> 5:(+)    55,                      !- Density {kg/m3}
#> 6:(+)    100;                     !- Specific Heat {J/kg-K}
```

Note that only minimum fields will be added by default. But you can change it by
setting `min` to FALSE.


```r
model$add("Material", name = "test_add", roughness = "Rough", thickness = 0.8,
          conductivity = 55, density = 55, specific_heat = 100,
          thermal_absorptance = 0.8, min = FALSE)
#> ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> Value for field 'Solar Absorptance' in class 'Material' is missing. Default value '0.7' is used.
#> Value for field 'Visible Absorptance' in class 'Material' is missing. Default value '0.7' is used.
#> ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> [ID:324] Material,
#> 1:(+)    test_add,                !- Name
#> 2:(+)    Rough,                   !- Roughness
#> 3:(+)    0.8,                     !- Thickness {m}
#> 4:(+)    55,                      !- Conductivity {W/m-K}
#> 5:(+)    55,                      !- Density {kg/m3}
#> 6:(+)    100,                     !- Specific Heat {J/kg-K}
#> 7:(+)    0.8,                     !- Thermal Absorptance
#> 8:(+)    .7,                      !- Solar Absorptance
#> 9:(+)    .7;                      !- Visible Absorptance
```

Existing unique objects cannot be added.


```r
model$add(class = "Version")
#> Error: 'Version' is an unique object and already exists
```

Errors will occur if required fields are missing.


```r
model$add("Material", roughness = "Rough", thickness = 0.8, conductivity = 55,
          density = 55, specific_heat = 100)
#> Error:
#> ===========================================================================
#> Errors found when checking 'Missing Value'
#> ---------------------------------------------------------------------------
#> Missing value for required field 'Name' in class 'Material'
#> ===========================================================================
```

#### `$set`

`$set` works pretty much the same way as `$add`, except it only accepts valid
`ID`, not class names.


```r
model$set(52, name = "test_set", roughness = "Rough", thickness = 0.8, conductivity = 55,
          density = 55, specific_heat = 100)
#> [ID:52] Material,
#> 1:(~)    test_set,                !- Name
#> 2:(~)    Rough,                   !- Roughness
#> 3:(~)    0.8,                     !- Thickness {m}
#> 4:(~)    55,                      !- Conductivity {W/m-K}
#> 5:(~)    55,                      !- Density {kg/m3}
#> 6:(~)    100,                     !- Specific Heat {J/kg-K}
#> 7:       0.9000000,               !- Thermal Absorptance
#> 8:       0.6500000,               !- Solar Absorptance
#> 9:       0.6500000;               !- Visible Absorptance
```

Also, if the modified fields were referred by fields in
other objects, the corresponding fields will also be updated.


```r
model$get(52, 69)
#> [ID:52] Material,
#> 1:(~)    test_set,                !- Name
#> 2:(~)    Rough,                   !- Roughness
#> 3:(~)    0.8,                     !- Thickness {m}
#> 4:(~)    55,                      !- Conductivity {W/m-K}
#> 5:(~)    55,                      !- Density {kg/m3}
#> 6:(~)    100,                     !- Specific Heat {J/kg-K}
#> 7:       0.9000000,               !- Thermal Absorptance
#> 8:       0.6500000,               !- Solar Absorptance
#> 9:       0.6500000;               !- Visible Absorptance
#>
#> [ID:69] Construction,
#> 1:       FLOOR-SLAB-1,            !- Name
#> 2:(~)    test_set;                !- Outside Layer
```

#### `$dup`

`$dup` will duplicate the object you specified using `id`. If there is a name
field in the class, you can assign a new name to the duplicated object using
`new_name`. If `NULL`, which is default, the duplicated object will have the same
name of the original object except with a suffix of "`_1`", "`_2`" and etc. Both
the original and the duplicated objects will be shown.


```r
model$dup(52)
#> [ID: 52] Material,
#> 1:(~)    test_set,                !- Name
#> 2:(~)    Rough,                   !- Roughness
#> 3:(~)    0.8,                     !- Thickness {m}
#> 4:(~)    55,                      !- Conductivity {W/m-K}
#> 5:(~)    55,                      !- Density {kg/m3}
#> 6:(~)    100,                     !- Specific Heat {J/kg-K}
#> 7:       0.9000000,               !- Thermal Absorptance
#> 8:       0.6500000,               !- Solar Absorptance
#> 9:       0.6500000;               !- Visible Absorptance
#>
#> [ID:325] Material,
#> 1:(+)    test_set_1,              !- Name
#> 2:(+)    Rough,                   !- Roughness
#> 3:(+)    0.8,                     !- Thickness {m}
#> 4:(+)    55,                      !- Conductivity {W/m-K}
#> 5:(+)    55,                      !- Density {kg/m3}
#> 6:(+)    100,                     !- Specific Heat {J/kg-K}
#> 7:(+)    0.9000000,               !- Thermal Absorptance
#> 8:(+)    0.6500000,               !- Solar Absorptance
#> 9:(+)    0.6500000;               !- Visible Absorptance
```

Same as `$add`, existing unique object cannot be duplicated.


```r
model$dup(2)
#> Error: 'SimulationControl' is an unique object and already exists
```

#### `$del`

`$del` will delete current object specified by `id`. If the object is referred
by other object(s), an error will given showing the fields that were referred.
You can still delete the object if you want by setting `force` to `TRUE`.


```r
model$del(55)
#> Error: Some field(s) in current object (ID:55) has been referred by other
#> object(s) (ID:68) below. Comfirm by setting 'force' to TRUE.
#> [ID:68] Construction,
#>        CLNG-1,                  !- Name
#> ($)    MAT-CLNG-1;              !- Outside Layer
```


```r
model$del(55, force = TRUE)
#> Warning: Force to delete object (ID:55) that has been referred by other
#> objects (ID:68). Errors may occur during simulations.
#> [ID:55] Material:NoMass,
#> 1:(-)    MAT-CLNG-1,              !- Name
#> 2:(-)    Rough,                   !- Roughness
#> 3:(-)    0.652259290,             !- Thermal Resistance {m2-K/W}
#> 4:(-)    0.65,                    !- Thermal Absorptance
#> 5:(-)    0.65,                    !- Solar Absorptance
#> 6:(-)    0.65;                    !- Visible Absorptance
```

#### `$hide`

`$hide` is the same as `$del`, except that `$hide` will comment out the object
instead of deleting it. This make if possible for you to get the hidden objects
back by uncomment it using any test editor.

> Note that the IDs of deleted or hidden objects are invalid after `$del` and
cannot be applied to methods `$set`, `$dup` and `$del`, of course. However,
unless you save the model, the deleted or hidden objects are still there
internally but with a special mark to prevent them accessible. This is done by
purpose, in order to provide a new method call `$undo` in the future, which will
enable you to un-delete the objects.


```r
model$hide(33, force = TRUE)
#> Warning: Force to hide object (ID:33) that has been referred by other
#> objects (ID:193, 194, 200). Errors may occur during simulations.
#> [ID:33] Schedule:Compact,
#>  1:(!)    CoolingCoilAvailSched,   !- Name
#>  2:(!)    Fraction,                !- Schedule Type Limits Name
#>  3:(!)    Through: 12/31,          !- Field 1
#>  4:(!)    For: WeekDays,           !- Field 2
#>  5:(!)    Until: 6:00,             !- Field 3
#>  6:(!)    0.0,                     !- Field 4
#>  7:(!)    Until: 20:00,            !- Field 5
#>  8:(!)    1.0,                     !- Field 6
#>  9:(!)    Until: 24:00,            !- Field 7
#> 10:(!)    0.0,                     !- Field 8
#> 11:(!)    For: SummerDesignDay WinterDesignDay,  !- Field 9
#> 12:(!)    Until: 24:00,            !- Field 10
#> 13:(!)    1.0,                     !- Field 11
#> 14:(!)    For: AllOtherDays,       !- Field 12
#> 15:(!)    Until: 24:00,            !- Field 13
#> 16:(!)    0.0;                     !- Field 14
```


#### Notes

`$notes` will show, add or delete notes (comments) for the object specified
using `id`.

If `...` is empty, then the object with notes will be shown directly.


```r
model$notes(1)
#> ! 5Zone_Transformer.idf
#> ! Basic file description:  1 story building divided into 4 exterior and one interior conditioned zones and return plenum.
#> !
#> ! Highlights:              Distribution transformer used to input power from electricity grid to the building
#> !
#> ! Simulation Location/Run: CHICAGO_IL_USA TMY2-94846, 2 design days, 2 run periods,
#> !                          Run Control executes the run periods using the weather file
#> !
#> ! Location:                Chicago, IL
#> !
#> ! Design Days:             CHICAGO_IL_USA Annual Heating 99% Design Conditions DB, MaxDB= -17.3癈
#> !                          CHICAGO_IL_USA Annual Cooling 1% Design Conditions, MaxDB=  31.5癈 MCWB=  23.0癈
#> !
#> ! Run Period (Weather File): Winter 1/14, Summer 7/7, CHICAGO_IL_USA TMY2-94846
#> !
#> ! Run Control:             Zone and System sizing with weather file run control (no design days run)
#> !
#> ! Building: Single floor rectangular building 100 ft x 50 ft. 5 zones - 4 exterior, 1 interior, zone height 8 feet.
#> !           Exterior zone depth is 12 feet. There is a 2 foot high return plenum: the overall building height is
#> !           10 feet. There are windows on all 4 facades; the south and north facades have glass doors.
....
```

You can wrap long notes at given length using `wrap`.


```r
model$notes(2, "Just some simple notes about this objects.", wrap = 10L)
#> ! Just some
#> ! simple
#> ! notes
#> ! about
#> ! this
#> ! objects.
#> [ID:2] SimulationControl,
#> 1:       Yes,                     !- Do Zone Sizing Calculation
#> 2:       Yes,                     !- Do System Sizing Calculation
#> 3:       Yes,                     !- Do Plant Sizing Calculation
#> 4:       No,                      !- Run Simulation for Sizing Periods
#> 5:       Yes;                     !- Run Simulation for Weather File Run Periods
```

If `append` is `TRUE`, new notes will be added to the end of existing ones,
otherwise the beginning of existing ones. If `NULL`, the already existing notes
will be deleted before adding new ones. You can delete all notes of one object
by given empty `...` and setting `append` to `NULL`.


```r
model$notes(2, "Add a new simple note after.", append = TRUE)
#> ! Just some
#> ! simple
#> ! notes
#> ! about
#> ! this
#> ! objects.
#> ! Add a new simple note after.
#> [ID:2] SimulationControl,
#> 1:       Yes,                     !- Do Zone Sizing Calculation
#> 2:       Yes,                     !- Do System Sizing Calculation
#> 3:       Yes,                     !- Do Plant Sizing Calculation
#> 4:       No,                      !- Run Simulation for Sizing Periods
#> 5:       Yes;                     !- Run Simulation for Weather File Run Periods
```


```r
model$notes(2, append = NULL)
#> [ID:2] SimulationControl,
#> 1:       Yes,                     !- Do Zone Sizing Calculation
#> 2:       Yes,                     !- Do System Sizing Calculation
#> 3:       Yes,                     !- Do Plant Sizing Calculation
#> 4:       No,                      !- Run Simulation for Sizing Periods
#> 5:       Yes;                     !- Run Simulation for Weather File Run Periods
```


### Diff

`$diff` will show all modifications you made, including added (or duplicated),
modified, deleted and hidden objects with markers `(+)`, `(~)`, `(-)` and `(!)`
respectively. You can also only show one kind of modifications using argument
`type`.


```r
model$diff()
#> [ID: 33] Schedule:Compact,
#>  1:(!)    CoolingCoilAvailSched,   !- Name
#>  2:(!)    Fraction,                !- Schedule Type Limits Name
#>  3:(!)    Through: 12/31,          !- Field 1
#>  4:(!)    For: WeekDays,           !- Field 2
#>  5:(!)    Until: 6:00,             !- Field 3
#>  6:(!)    0.0,                     !- Field 4
#>  7:(!)    Until: 20:00,            !- Field 5
#>  8:(!)    1.0,                     !- Field 6
#>  9:(!)    Until: 24:00,            !- Field 7
#> 10:(!)    0.0,                     !- Field 8
#> 11:(!)    For: SummerDesignDay WinterDesignDay,  !- Field 9
#> 12:(!)    Until: 24:00,            !- Field 10
#> 13:(!)    1.0,                     !- Field 11
#> 14:(!)    For: AllOtherDays,       !- Field 12
#> 15:(!)    Until: 24:00,            !- Field 13
#> 16:(!)    0.0;                     !- Field 14
#>
#> [ID: 52] Material,
#>  1:(~)    test_set,                !- Name
#>  2:(~)    Rough,                   !- Roughness
#>  3:(~)    0.8,                     !- Thickness {m}
#>  4:(~)    55,                      !- Conductivity {W/m-K}
#>  5:(~)    55,                      !- Density {kg/m3}
#>  6:(~)    100,                     !- Specific Heat {J/kg-K}
#>  7:       0.9000000,               !- Thermal Absorptance
#>  8:       0.6500000,               !- Solar Absorptance
#>  9:       0.6500000;               !- Visible Absorptance
#>
#> [ID:323] Material,
#>  1:(+)    test_add,                !- Name
#>  2:(+)    Rough,                   !- Roughness
#>  3:(+)    0.8,                     !- Thickness {m}
#>  4:(+)    55,                      !- Conductivity {W/m-K}
#>  5:(+)    55,                      !- Density {kg/m3}
#>  6:(+)    100;                     !- Specific Heat {J/kg-K}
#>
#> [ID:324] Material,
#>  1:(+)    test_add,                !- Name
#>  2:(+)    Rough,                   !- Roughness
#>  3:(+)    0.8,                     !- Thickness {m}
#>  4:(+)    55,                      !- Conductivity {W/m-K}
#>  5:(+)    55,                      !- Density {kg/m3}
#>  6:(+)    100,                     !- Specific Heat {J/kg-K}
#>  7:(+)    0.8,                     !- Thermal Absorptance
#>  8:(+)    .7,                      !- Solar Absorptance
#>  9:(+)    .7;                      !- Visible Absorptance
#>
#> [ID:325] Material,
#>  1:(+)    test_set_1,              !- Name
#>  2:(+)    Rough,                   !- Roughness
#>  3:(+)    0.8,                     !- Thickness {m}
#>  4:(+)    55,                      !- Conductivity {W/m-K}
#>  5:(+)    55,                      !- Density {kg/m3}
#>  6:(+)    100,                     !- Specific Heat {J/kg-K}
#>  7:(+)    0.9000000,               !- Thermal Absorptance
#>  8:(+)    0.6500000,               !- Solar Absorptance
#>  9:(+)    0.6500000;               !- Visible Absorptance
#>
#> [ID: 55] Material:NoMass,
#>  1:(-)    MAT-CLNG-1,              !- Name
#>  2:(-)    Rough,                   !- Roughness
#>  3:(-)    0.652259290,             !- Thermal Resistance {m2-K/W}
#>  4:(-)    0.65,                    !- Thermal Absorptance
#>  5:(-)    0.65,                    !- Solar Absorptance
#>  6:(-)    0.65;                    !- Visible Absorptance
#>
#> [ID: 69] Construction,
#>  1:       FLOOR-SLAB-1,            !- Name
#>  2:(~)    test_set;                !- Outside Layer
```

### Check

`$check` will check the validation of all fields in current model, including
missing required objected and fields, wrong value types, choices, references,
any value range exceeding, invalid autosizable and autocalculatable fields.


```r
model$check()
#>
#> ===========================================================================
#> Errors found when checking 'Reference'
#> ---------------------------------------------------------------------------
#> Invalid value 'MAT-CLNG-1' found for field 'Outside Layer' in class 'Construction' with ID 68 which should be one of references 'c("WD10", "RG01", "BR01", "IN46", "WD01", "PW03", "IN02", "GP01", "GP02", "test_set", "test_add", "test_add", "test_set_1", "CP01", "MAT-SB-U", "MAT-FLOOR-1", "AL21", "AL23", "CLEAR 3MM", "GREY 3MM", "CLEAR 6MM", "LoE CLEAR 6MM", "AIR 6MM", "AIR 13MM", "ARGON 13MM")'
#> Invalid value 'CoolingCoilAvailSched' found for field 'Availability Schedule Name' in class 'Coil:Cooling:Water' with ID 193 which should be one of references 'c("OCCUPY-1", "LIGHTS-1", "EQUIP-1", "INFIL-SCH", "ActSchd", "ShadeTransSch", "Htg-SetP-Sch", "PlenumHtg-SetP-Sch", "Clg-SetP-Sch", "PlenumClg-SetP-Sch", "Zone Control Type Sched", "Min OA Sched", "FanAvailSched", "CoolingPumpAvailSched", "ReheatCoilAvailSched", "CW Loop Temp Schedule", "HW Loop Temp Schedule", "PlantOnSched", "Seasonal Reset Supply Air Temp Sch", "OA Cooling Supply Air Temp Sch", "OA Heating Supply Air Temp Sch", "Always On")'
#> Invalid value 'CoolingCoilAvailSched' found for field 'Availability Schedule Name' in class 'Coil:Cooling:Water' with ID 194 which should be one of references 'c("OCCUPY-1", "LIGHTS-1", "EQUIP-1", "INFIL-SCH", "ActSchd", "ShadeTransSch", "Htg-SetP-Sch", "PlenumHtg-SetP-Sch", "Clg-SetP-Sch", "PlenumClg-SetP-Sch", "Zone Control Type Sched", "Min OA Sched", "FanAvailSched", "CoolingPumpAvailSched", "ReheatCoilAvailSched", "CW Loop Temp Schedule", "HW Loop Temp Schedule", "PlantOnSched", "Seasonal Reset Supply Air Temp Sch", "OA Cooling Supply Air Temp Sch", "OA Heating Supply Air Temp Sch", "Always On")'
#> Invalid value 'CoolingCoilAvailSched' found for field 'Availability Schedule Name' in class 'Coil:Heating:Water' with ID 200 which should be one of references 'c("OCCUPY-1", "LIGHTS-1", "EQUIP-1", "INFIL-SCH", "ActSchd", "ShadeTransSch", "Htg-SetP-Sch", "PlenumHtg-SetP-Sch", "Clg-SetP-Sch", "PlenumClg-SetP-Sch", "Zone Control Type Sched", "Min OA Sched", "FanAvailSched", "CoolingPumpAvailSched", "ReheatCoilAvailSched", "CW Loop Temp Schedule", "HW Loop Temp Schedule", "PlantOnSched", "Seasonal Reset Supply Air Temp Sch", "OA Cooling Supply Air Temp Sch", "OA Heating Supply Air Temp Sch", "Always On")'
#> ===========================================================================
#>
#>
#> ===========================================================================
#> Errors found when checking 'Autocalculatable'
#> ---------------------------------------------------------------------------
#> Value for field 'Maximum Flow per Zone Floor Area During Reheat {m3/s-m2}' in class 'AirTerminal:SingleDuct:VAV:Reheat' with ID 172 is not but was set to 'AUTOCALCULATE'
#> Value for field 'Maximum Flow Fraction During Reheat' in class 'AirTerminal:SingleDuct:VAV:Reheat' with ID 172 is not but was set to 'AUTOCALCULATE'
#> Value for field 'Maximum Flow per Zone Floor Area During Reheat {m3/s-m2}' in class 'AirTerminal:SingleDuct:VAV:Reheat' with ID 173 is not but was set to 'AUTOCALCULATE'
#> Value for field 'Maximum Flow Fraction During Reheat' in class 'AirTerminal:SingleDuct:VAV:Reheat' with ID 173 is not but was set to 'AUTOCALCULATE'
#> Value for field 'Maximum Flow per Zone Floor Area During Reheat {m3/s-m2}' in class 'AirTerminal:SingleDuct:VAV:Reheat' with ID 174 is not but was set to 'AUTOCALCULATE'
#> Value for field 'Maximum Flow Fraction During Reheat' in class 'AirTerminal:SingleDuct:VAV:Reheat' with ID 174 is not but was set to 'AUTOCALCULATE'
#> Value for field 'Maximum Flow per Zone Floor Area During Reheat {m3/s-m2}' in class 'AirTerminal:SingleDuct:VAV:Reheat' with ID 175 is not but was set to 'AUTOCALCULATE'
#> Value for field 'Maximum Flow Fraction During Reheat' in class 'AirTerminal:SingleDuct:VAV:Reheat' with ID 175 is not but was set to 'AUTOCALCULATE'
#> Value for field 'Maximum Flow per Zone Floor Area During Reheat {m3/s-m2}' in class 'AirTerminal:SingleDuct:VAV:Reheat' with ID 176 is not but was set to 'AUTOCALCULATE'
#> Value for field 'Maximum Flow Fraction During Reheat' in class 'AirTerminal:SingleDuct:VAV:Reheat' with ID 176 is not but was set to 'AUTOCALCULATE'
#> ===========================================================================
```

### Reset

`$reset` will reset the model to the status when it was last saved using
`$save` or `$saveas` (if never saved, first read and parsed using
`eplus_model$new`) All your modifications will be lost, so use with caution. It
is pretty useful if you messed things up during modifications.


```r
model$reset(confirm = TRUE)
#> The model has been reset to the status when it was first read at
#> '2018-01-08 16:40:36'.
```

### Save

You can save your model using `$save` and `$saveas`. `$save` is a shortcut of
`$saveas(path = "the_original_model_path")` and will overwrite the current file
which has a risk of losing your original file and data. So make sure you have
a safe copy of you original model.

`$saveas` will save the model as a new file.


```r
model$saveas("~/test_eplusr/test_model.idf", overwrite = TRUE)
```

### Run and Collect Output

#### `$run`

`$run` will run the current model within given period using corresponding
version of EnergyPlus.

`eplusr` will try to find corresponding version of EnergyPlus that was
installed in the standard location. If failed, an error will be given.

You can use `period` to override the `RunPeriod` objects. The original objects
in `RunPeriod` class will be commented out using `$hide`. Each side of a
`period` formula is specified as a character in format `'MM-DD'`, but powerful
shorthand is available:

* `~.`: Use existing `RunPeriod` objects. This is the default.
* `~"annual"`: Force to run annual simulation only.
* `~"design_day"`: Force to run design day only.
* `~4` or `~"4"` or `~"Apr"`: Force to run from April 1st to April 30th.
* `2~4` or `"2"~"4"` or `"Feb"~"Apr"`: Force to run from February 1st to
    April 30th.
* `"2-1"~"4-30"`: Same as above.


```r
model$run(period = ~"design_day", echo = FALSE)
#> Warning: Missing weather input, weather file located at
#> 'C:/EnergyPlusV8-8-0/WeatherData/USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.epw'
#> will been used.
#> Reset run period to 'Design Day Simulation'
#> Running command C:/EnergyPlusV8-8-0/energyplus.exe
#> Arguments:
#> --weather "C:/Users/hongy/Documents/R/win-library/3.3/eplusr/extdata/USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.epw" --output-directory "C:/Users/hongy/Documents/R/win-library/3.3/eplusr/extdata" --output-prefix "5Zone_Transformer" --output-suffix C   --readvars  --design-day  "C:/Users/hongy/Documents/R/win-library/3.3/eplusr/extdata/5Zone_Transformer.idf"
```




#### `$collect` and `$table`

`$collect` will collect the simulation variable (specified in `Output:Variable`
class) and meter (specified in `Output:Meter*` classes) output of current model.
The `"Date/Time"` column in the output will be renamed to `"datetime` and will
be converted to a `DateTimeClass` automatically.

> NOTE: You cannot collect the results until simulation ends successfully.


```r
model$collect("meter")
#>                 datetime Electricity:Facility [J](TimeStep)
#>   1: 2017-01-21 00:15:00                            1695473
#>   2: 2017-01-21 00:30:00                            1695473
#>   3: 2017-01-21 00:45:00                            1695473
#>   4: 2017-01-21 01:00:00                            1695473
#>   5: 2017-01-21 01:15:00                            1695473
#>  ---
#> 188: 2017-07-21 23:00:00                            2149990
#> 189: 2017-07-21 23:15:00                            2131850
#> 190: 2017-07-21 23:30:00                            2115123
#> 191: 2017-07-21 23:45:00                            2093699
#> 192: 2017-07-22 00:00:00                            2079832
#>      Electricity:Building [J](TimeStep)
#>   1:                            1237500
#>   2:                            1237500
#>   3:                            1237500
#>   4:                            1237500
#>   5:                            1237500
#>  ---
#> 188:                             427500
#> 189:                             427500
#> 190:                             427500
#> 191:                             427500
#> 192:                             427500
#>      InteriorLights:Electricity [J](TimeStep)
#>   1:                                   337500
#>   2:                                   337500
#>   3:                                   337500
#>   4:                                   337500
#>   5:                                   337500
#>  ---
#> 188:                                   337500
#> 189:                                   337500
#> 190:                                   337500
#> 191:                                   337500
#> 192:                                   337500
#>      Electricity:HVAC [J](TimeStep) Electricity:Plant [J](TimeStep)
#>   1:                       376759.6                        81213.75
#>   2:                       376759.6                        81213.77
#>   3:                       376759.6                        81213.77
#>   4:                       376759.6                        81213.77
#>   5:                       376759.6                        81213.60
#>  ---
#> 188:                       374825.8                      1347664.07
#> 189:                       374825.8                      1329524.25
#> 190:                       374825.8                      1312797.50
#> 191:                       374825.8                      1291373.50
#> 192:                       374825.8                      1277506.59
#>      Fans:Electricity [J](TimeStep) EnergyTransfer:Building [J](TimeStep)
#>   1:                       338588.4                              12927707
#>   2:                       338588.4                              12927767
#>   3:                       338588.4                              12927651
#>   4:                       338588.4                              12927600
#>   5:                       338588.4                              12932318
#>  ---
#> 188:                       338588.4                               3463968
#> 189:                       338588.4                               3404587
#> 190:                       338588.4                               3347742
#> 191:                       338588.4                               3293424
#> 192:                       338588.4                               3241605
```

`$table` will extract tables from simulation table (specified in
`Output:Table*` classes) output of current model.

> NOTE: The underlying functions in `$table` relies on the `HTML` format output.
If the `Column Separator` in `OutputControl:Table:Style` does not contain
`HTML` format, `eplusr` will automatically change it when running the model.
For example, `"Comma"` (which is the default value) will be changed into
`"CommaAndHTML"` and a warning message will be issued.


```r
model$table(table = c("Site and Source Energy", "Site to Source Energy Conversion Factors"))
#>                                         report             key
#> 1: Annual Building Utility Performance Summary Entire Facility
#> 2: Annual Building Utility Performance Summary Entire Facility
#>                                       table      content
#> 1:                   Site and Source Energy <data.frame>
#> 2: Site to Source Energy Conversion Factors <data.frame>
```

By default, a list column named `"content"` will returned with all extracted tables. You can see the contents of the table by setting `nest` to FALSE.


```r
model$table(table = c("Site and Source Energy", "Site to Source Energy Conversion Factors"), nest = FALSE)
#> [[1]]
#> [[1]]$report
#> [1] "Annual Building Utility Performance Summary"
#>
#> [[1]]$key
#> [1] "Entire Facility"
#>
#> [[1]]$table
#> [1] "Site and Source Energy"
#>
#> [[1]]$content
#>                       Total Energy [GJ]
#> 1   Total Site Energy                 0
#> 2     Net Site Energy                 0
#> 3 Total Source Energy                 0
#> 4   Net Source Energy                 0
#>   Energy Per Total Building Area [MJ/m2]
#> 1                                      0
#> 2                                      0
#> 3                                      0
#> 4                                      0
#>   Energy Per Conditioned Building Area [MJ/m2]
#> 1                                            0
#> 2                                            0
#> 3                                            0
#> 4                                            0
#>
#>
#> [[2]]
#> [[2]]$report
#> [1] "Annual Building Utility Performance Summary"
#>
#> [[2]]$key
#> [1] "Entire Facility"
#>
#> [[2]]$table
#> [1] "Site to Source Energy Conversion Factors"
#>
#> [[2]]$content
#>                     Site=>Source Conversion Factor
#> 1       Electricity                          3.167
#> 2       Natural Gas                          1.084
#> 3  District Cooling                          1.056
#> 4  District Heating                          3.613
#> 5             Steam                          0.300
#> 6          Gasoline                          1.050
#> 7            Diesel                          1.050
#> 8              Coal                          1.050
#> 9       Fuel Oil #1                          1.050
#> 10      Fuel Oil #2                          1.050
#> 11          Propane                          1.050
#> 12     Other Fuel 1                          1.000
#> 13     Other Fuel 2                          1.000
```


## License

MIT © Hongyuan Jia
