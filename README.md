
<!-- README.md is generated from README.Rmd. Please edit that file -->
eplusr
======

[![Travis-CI Build
Status](https://travis-ci.org/hongyuanjia/eplusr.svg?branch=master)](https://travis-ci.org/hongyuanjia/eplusr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/hongyuanjia/eplusr?branch=master&svg=true)](https://ci.appveyor.com/project/hongyuanjia/eplusr)
<!--[![Coverage Status](https://img.shields.io/codecov/c/github/hongyuanjia/eplusr/master.svg)](https://codecov.io/github/hongyuanjia/eplusr?branch=master)-->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/eplusr)](https://cran.r-project.org/package=eplusr)

> A Toolkit for Using EnergyPlus in R.

IDFEditor distributed along with
[EnergyPlus](https://www.energyplus.net) provides full support for
preparing EnergyPus IDF and IMF files for simulations. The parsing and
writing process of IDF and IDD files in `eplusr` is basically the same
as that in IDFEditor. But `eplusr` takes advantage of the powerful
[`data.table`](http://r-datatable.com) package to speed up the whole
process and store the results. The IDD files for EnergyPlus 8.3 to 8.8
have been pre-parsed and stored internally and will automatically be
used when parsing `IDF` and `IMF` files. The souce codes of IDFEditor
can be found on
[GitHub](https://github.com/NREL/EnergyPlus/tree/develop/src/IDF_Editor)
. There is still an option to give an additional IDD file path to parse
if you want. However, it will still take about 3-4 sec to parse an IDD
file which is much slower than IDFEditor written in Visual Basic.

Basically, all model data are stored as `data.table`s. And each object
in the model has an unique `ID`. Once you have the object `ID`, you can
set fields (using `$set`) in the object, duplicate (using `$dup`),
delete (using `$del`) the object. A full example of reading and editing
an `IDF` file is given in [Example](#example).

The functionality of running EnergyPlus, and collecting and analyze the
output is under development, and will release in the next version.

------------------------------------------------------------------------

-   [Warning](#warning)
-   [Installation](#installation)
-   [Features](#features)
-   [Usage](#usage)
    -   [Read and parse](#read-and-parse)
    -   [Query](#query)
    -   [Modify](#modify)
    -   [Diff](#diff)
    -   [Check](#check)
    -   [Save](#save)
    -   [Reset](#reset)
-   [License](#license)

Warning
-------

This package is still in its infant stage of development and is subject
to change. It is not recommended to use it in working environment.

Installation
------------

`eplusr` is currently not on CRAN. You can install eplusr from github
with:

``` r
# install.packages("devtools")
devtools::install_github("hongyuanjia/eplusr")
```

Features
--------

-   Read and parse EnergyPlus `IDF`, `IMF` files
-   Query on models, including classes, objects and fields
-   Directly add, modify, duplicate, and delete objects of parse `IDF`
    and `IMF` files in R.
-   Save the changed models into standard formats in the same way as
    IDFEditor distrubuted along with EnergyPlus.
-   Run your models directly in R (**under development**)
-   Collect and analyze the output of EnergyPlus in R (**under
    development**)

Usage
-----

``` r
library(eplusr)
```

For detailed reference, please see package documentation:
`help(package = "eplusr")`.

### Read and parse

All reading process starts with creating an `R6Class` called
`eplus_model`. The model will be printed in a similar style you see in
IDFEditor, with an additional heading lines show the `Path`, `Version`
and `Type` of the model. The classes of objects in the model are ordered
by Group and the number of objects in classes are shown in square
bracket. All `Energy+.idd` files from EnergyPlus v8.3 to v8.8 have been
pre-parsed and stored. So you can just ignore the `idd` argument if you
are using those verions. If not, just pass the path of the `Energy+.idd`
file using `idd`.

``` r
model <- eplus_model$new(path = system.file("extdata", "5Zone_Transformer.idf", package = "eplusr"), idd = NULL)

model
#> [ Path  ]: D:/Documents/R/win-library/3.4/eplusr/extdata/5Zone_Transformer.idf
#> [Version]: 8.8
#> [ Type  ]: IDF
#> ================================================================================
#> 
#> Simulation Parameters
#> --------------------------------------------------------------------------------
#> [01] Version
#> [01] SimulationControl
#> [01] Building
#> [01] SurfaceConvectionAlgorithm:Inside
#> [01] SurfaceConvectionAlgorithm:Outside
#> [01] HeatBalanceAlgorithm
#> [01] Timestep
#> 
#> Location and Climate
#> --------------------------------------------------------------------------------
#> [01] Site:Location
#> [02] SizingPeriod:DesignDay
#> [02] RunPeriod
#> [01] Site:GroundTemperature:BuildingSurface
#> 
#> Schedules
#> --------------------------------------------------------------------------------
#> [06] ScheduleTypeLimits
#> [23] Schedule:Compact
#> 
#> Surface Construction Elements
#> --------------------------------------------------------------------------------
#> [10] Material
....
```

### Query

#### `$all`

You can list all valid components you specified using `$all`. The `type`
argument will determine what kind of components you want to see.

All objects in the model will have an unique `ID` according to their
sequences. you can find all valid `ID`s using `$all(type = "id")`. The
model will be printed in a way that is much similar with what you see
when you open your model in an text editor, except that each object
binded an unique `ID` and only first two lines of each object will be
shown.

``` r
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

If you want to see all classes in your model, you can use
`$all(type = "class")`.

``` r
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

You can find all available fields for all valid class in IDD using
`$all(type = "field", class = "any_valid_class_in_IDD")` which makes it
handy to be used along with `$add` you will see below. All required
fields are marked with `*`. For example, you can find all valid fields
in class `Material`:

``` r
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

`$contains` and `$matches` will search and return objects that contain
the string or match the regular expression you give. The `scale`
argument will determine where you want to search. If `class`, only class
names existing in current model will be searched. If `field`, only
fields in current model will be searched. This is a handy option when
you want to see if an object, e.g. one `Material`, is referred by other
objects, e.g. `Construction`s.

``` r
model$contains(match = "Algorithm", scale = "class")
#> 
#> == * 1 Objects Found in Class: SurfaceConvectionAlgorithm:Inside * =============
#> 
#> [ID:4] SurfaceConvectionAlgorithm:Inside,
#>     Simple;                  !- Algorithm
#> 
#> 
#> == * 1 Objects Found in Class: SurfaceConvectionAlgorithm:Outside * ============
#> 
#> [ID:5] SurfaceConvectionAlgorithm:Outside,
#>     SimpleCombined;          !- Algorithm
#> 
#> 
#> == * 1 Objects Found in Class: HeatBalanceAlgorithm * ==========================
#> 
#> [ID:6] HeatBalanceAlgorithm,
#>     ConductionTransferFunction;  !- Algorithm
```

If the `scale` is set to `field`, all matched fields will be marked with
`(*)`. What’s more, `$matches` accepts extra arguments using `...`. All
those arguments will be directly passed to `grepl`.

``` r
model$matches(match = "mat-clng-1", scale = "field", ignore.case = TRUE)
#> == * 2 Matched Fields Found * ================================================== 
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

`$get` will return you the objects with valid IDs you give.

``` r
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

### Modify

You can add, duplicate, modify and delete objects using `$add`, `$dup`,
`$set` and `$del` respectively.

All newly added (duplicated), modified and deleted fields will be marked
with `(+)`, `(~)` and `(-)` respectively. The valid IDs will be appended
after `$add` and `$dup`, and the newly added (duplicated) object will
have the max ID.

For `$add` and `$set`, new field values should be given. Currently three
styles of value are acceptable:

-   directly list all field values with no name. The values will be
    assigned to fields according to the order of values;
-   give both field names and values in pair, e.g.
    `Name = "Test", "Sepcific Heat" = 150`. You can find all valid field
    names (with units) using
    `$all("field", class = "class_name_to_query")`;
-   some kind of the same as above, but with all field names in lower
    cases and spaces replaced by `_`, e.g.
    `name = "Test", specific_heat = 150`.

Note: All field names should be given **without** units. Error will
occur when the type (character or numeric), and the value (e.g. range)
is not valid.

#### `$add`

`$add` will add an object in the class you give. All fields will be set
to their defaults if applicable.

``` r
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

Note that only minimum fiels will be added by default. But you can
change it by setting `min` to FALSE.

``` r
model$add("Material", name = "test_add", roughness = "Rough", thickness = 0.8,
          conductivity = 55, density = 55, specific_heat = 100,
          thermal_absorptance = 0.8, min = FALSE)
#> ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#> Value for field 'Solar Absorptance' in class 'Material' is missing. Default value '0.7' is used.
#> Value for field 'Visible Absorptance' in class 'Material' is missing. Default value '0.7' is used.
#> ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

``` r
model$add(class = "Version")
#> Error: 'Version' is an unique object and already exists
```

Errors will occur if required fields are missing.

``` r
model$add("Material", roughness = "Rough", thickness = 0.8, conductivity = 55,
          density = 55, specific_heat = 100)
#> Error: 
#> ================================================================================
#> Errors found when checking 'Missing Value'
#> --------------------------------------------------------------------------------
#> Missing value for required field 'Name' in class 'Material'
#> ================================================================================
```

#### `$set`

`$set` works pretty much the same way as `$add`, except it only accepts
valid `ID` not class names.

``` r
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

Also, if the modified fields were referred by fields in other objects,
the corresponding fields will also be updated.

``` r
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

`$dup` will duplicate the object you specified using `id`. If there is a
name field in the class, you can assign a new name to the duplicated
object using `new_name`. If NULL, which is default, the duplicated
object will have the same name of the original object except with a
suffix of “\_1“,”\_2" and etc. Both the original and the duplicated
objects will be shown.

``` r
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

``` r
model$dup(2)
#> Error: 'SimulationControl' is an unique object and already exists
```

#### `$del`

`$del` will delete current object specified by `id`. If the object is
referred by other object(s), an error will given showing the fields that
were referred. You can still delete the object if you want by setting
`force` to `TRUE`.

``` r
model$del(55)
#> Error: Some field(s) in current object (ID:55) has been referred by other
#> object(s) (ID:68) below. Comfirm by setting 'force' to TRUE.
#> [ID:68] Construction,
#>        CLNG-1,                  !- Name
#> ($)    MAT-CLNG-1;              !- Outside Layer
```

``` r
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

Note that the IDs of deleted objects are invalid after `$del` and cannot
be applied to methods `$set`, `$dup` and `$del`, of course. However,
unless you save the model, the deleted objects are still there
internally but with a special mark to prevent them accessable. This is
done by purpose, in order to provide a new method call `$undo` in the
future, which will enable you to un-delete the objects.

### Diff

`$diff` will show all modifications you made, including added (or
duplicated), modified and deleted objects with markers `(+)`, `(~)`,
`(-)` respectively. You can also only show one kind of modifications
using argument `type`.

``` r
model$diff()
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
#> [ID:323] Material,
#> 1:(+)    test_add,                !- Name
#> 2:(+)    Rough,                   !- Roughness
#> 3:(+)    0.8,                     !- Thickness {m}
#> 4:(+)    55,                      !- Conductivity {W/m-K}
#> 5:(+)    55,                      !- Density {kg/m3}
#> 6:(+)    100;                     !- Specific Heat {J/kg-K}
#> 
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
#> 
#> [ID: 55] Material:NoMass,
#> 1:(-)    MAT-CLNG-1,              !- Name
#> 2:(-)    Rough,                   !- Roughness
#> 3:(-)    0.652259290,             !- Thermal Resistance {m2-K/W}
#> 4:(-)    0.65,                    !- Thermal Absorptance
#> 5:(-)    0.65,                    !- Solar Absorptance
#> 6:(-)    0.65;                    !- Visible Absorptance
#> 
#> [ID: 69] Construction,
#> 1:       FLOOR-SLAB-1,            !- Name
#> 2:(~)    test_set;                !- Outside Layer
```

### Check

`$check` will check the validation of all fields in current model,
including missing required objected and fields, wrong value types,
choices, references, any value range exceedings, invalid autosizable and
autocalculatable fields.

``` r
model$check()
#> 
#> ================================================================================
#> Errors found when checking 'Reference'
#> --------------------------------------------------------------------------------
#> Invalid value 'MAT-CLNG-1' found for field 'Outside Layer' in class 'Construction' with ID 68 which should be one of references 'c("WD10", "RG01", "BR01", "IN46", "WD01", "PW03", "IN02", "GP01", "GP02", "test_set", "test_add", "test_add", "test_set_1", "CP01", "MAT-SB-U", "MAT-FLOOR-1", "AL21", "AL23", "CLEAR 3MM", "GREY 3MM", "CLEAR 6MM", "LoE CLEAR 6MM", "AIR 6MM", "AIR 13MM", "ARGON 13MM")'
#> ================================================================================
#> 
#> 
#> ================================================================================
#> Errors found when checking 'Autocalculatable'
#> --------------------------------------------------------------------------------
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
#> ================================================================================
```

### Save

You can save your model using `$save` and `$saveas`. `$save` is a
shortcut of `$saveas(path = "the_original_model_path")` and will
overwrite the current file which has a risk of losing your original file
and data. So make sure you have a safe copy of you original model.

`$saveas` will save the model as a new file.

``` r
model$save(comfirm = TRUE)
```

### Reset

`$reset` will reset the model to the status when it was last saved using
`$save` or `$saveas` (if never saved, first read and parsed using
`eplus_model$new`) All your modifications will be lost, so use with
caution. It is pretty useful if you messed things up during
modifications.

``` r
model$reset(comfirm = TRUE)
#> The model has been reset to the status when it was first read at
#> '2018-01-02 00:08:13'.
```

License
-------

MIT © Hongyuan Jia
