
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

eplusr provides a rich toolkit of using
[EnergyPlus](https://energyplus.net/) directly in R, which enables
programmatic navigation, modification of EnergyPlus models and makes it
less painful to do parametric simulations and analysis.

<!-- vim-markdown-toc GFM -->

  - [Installation](#installation)
  - [Features](#features)
  - [Usage](#usage)
      - [Read and parse](#read-and-parse)
      - [Basic Info](#basic-info)
      - [Class definition](#class-definition)
      - [Get object](#get-object)
      - [Modify object](#modify-object)
      - [Validate](#validate)
      - [Save](#save)
      - [Run and Collect Output](#run-and-collect-output)
      - [Run Parametric Analysis](#run-parametric-analysis)
  - [Acknowledgements](#acknowledgements)
  - [Author](#author)
  - [License](#license)

<!-- vim-markdown-toc -->

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

## Usage

``` r
library(eplusr)
```

eplusr uses `Idf` class to present the whole IDF file and `IdfObject`
class to present a single object in an IDF. Both `Idf` and `IdfObject`
class contain member functions for helping modify the data in IDF so it
complies with the underlying EnergyPlus IDD (Input Data Dictionary).
Similarly, IDD file is wrapped into two classes, i.e. `Idd` and
`IddObject`.

It is highly recommended to read the documentation of `Idf`,
`IdfObject`, `Idd` and `IddObject` to get a thorough understanding on
each class. You can do that by running `?idf`, `?idf_object`, `?idd` and
`?idd_object`.

### Read and parse

All IDF reading process starts with function `read_idf()`, which returns
an `Idf` object. The model will be printed in a similar style you see in
IDFEditor, with additional heading lines showing the `Path`, `Version`
of the model. The classes of objects in the model are ordered by groups
and the number of objects in classes are shown in square bracket.

Parsing an IDF requires the IDD data of that version, which serves as
the schema. Usually, when you try to edit an IDF, the corresponding
EnergyPlus is likely to be installed already. If EnergyPlus is installed
in standard location (`C:\EnergyPlusVX-Y-0` on Windows,
`/usr/local/EnergyPlus-X-Y-0` on Linux and
`/Applications/EnergyPlus-X-Y-0` on MacOS), eplusr is able to find it
and use the `Energy+.idd` file distributed with that release to parse
the input IDF. The IDD file will be parsed first and an `Idd` object
will be created and cached. That `Idd` object will be reused whenever
parsing IDFs with that version. For more details, please see
`?use_idd()` and `?idd`.

Sometimes you may just want to edit the model without installing the
whole EnergyPlus software. You can just download the IDD file of that
version using `download_idd()` or set `download` to `TRUE` in
`use_idd()`.

``` r
path_idd <- download_idd(8.8)
use_idd(path_idd)

# OR
use_idd(8.8, download = TRUE)
```

Now let’s read an IDF file distributed with EnergyPlus 8.8.0. As
EnergyPlus v8.8.0 has been installed, we can just ignore the `idd`
argument.

``` r
model <- read_idf(path = "5Zone_Transformer.idf", idd = NULL)

model
#> # Path: `/home/hongyuanjia/5Zone_Transformer.idf`
#> # Version: `8.8`
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
#> [02] RunPeriod
#> [01] Site:GroundTemperature:BuildingSurface
#> 
#> Group: `Schedules`
#> ───────────────────────────────────────────────────────────────────────────
#> [06] ScheduleTypeLimits
#> [23] Schedule:Compact
#> 
#> Group: `Surface Construction Elements`
#> ───────────────────────────────────────────────────────────────────────────
#> [10] Material
#> [04] Material:NoMass
#> [02] Material:AirGap
....
```

`Idf` class contains several methods to help query, modify models. Here
lists all methods it has.

``` r
setdiff(ls(model), "initialize")
#>  [1] "add_object"      "class_name"      "clone"          
#>  [4] "definition"      "del_object"      "dup_object"     
#>  [7] "group_name"      "ins_object"      "is_unsaved"     
#> [10] "is_valid"        "is_valid_class"  "is_valid_group" 
#> [13] "is_valid_id"     "is_valid_name"   "object"         
#> [16] "object_id"       "object_in_class" "object_name"    
#> [19] "object_num"      "path"            "print"          
#> [22] "replace_value"   "run"             "save"           
#> [25] "search_object"   "search_value"    "set_object"     
#> [28] "string"          "validate"        "version"
```

Below will show same example usage of methods listed above.

### Basic Info

If you want to see all groups and classes in your model, use
`$group_name()` and `$class_name()` respectively.

``` r
model$group_name()
#>  [1] "Simulation Parameters"                        
#>  [2] "Location and Climate"                         
#>  [3] "Schedules"                                    
#>  [4] "Surface Construction Elements"                
#>  [5] "Thermal Zones and Surfaces"                   
#>  [6] "Internal Gains"                               
#>  [7] "Zone Airflow"                                 
#>  [8] "HVAC Design Objects"                          
#>  [9] "Zone HVAC Controls and Thermostats"           
#> [10] "Zone HVAC Air Loop Terminal Units"            
#> [11] "Zone HVAC Equipment Connections"              
#> [12] "Fans"                                         
#> [13] "Coils"                                        
#> [14] "Controllers"                                  
....
```

``` r
model$class_name()
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

Also `$is_valid_group()` and `$is_valid_class()` are provided to check
if given group and class exists in current model.

### Class definition

You can get class definitions using `$definition()`, which returns a
list of `IddObject`s. All required fields in each class are marked with
`*`. For example, you can get the `IddObject` of class `Material`:

``` r
def_mat <- model$definition(class = "Material")[[1]]
def_mat
#> << Class: `Material` >>
#> ───────────────────────────────── * MEMO * ────────────────────────────────
#>   "Regular materials described with full set of thermal properties"
#> ────────────────────────────── * PROPERTIES * ─────────────────────────────
#>   Group: `Surface Construction Elements`
#>   Unique: FALSE
#>   Required: FALSE
#>   Total fields: 9
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#>   1:* Name
#>   2:* Roughness
#>   3:* Thickness {m}
#>   4:* Conductivity {W/m-K}
#>   5:* Density {kg/m3}
#>   6:* Specific Heat {J/kg-K}
#>   7:  Thermal Absorptance
#>   8:  Solar Absorptance
#>   9:  Visible Absorptance
#> ───────────────────────────────────────────────────────────────────────────
```

You can also achieve this using methods in `Idd` class.

``` r
idd <- use_idd(8.8)

idd$Material
#> << Class: `Material` >>
#> ───────────────────────────────── * MEMO * ────────────────────────────────
#>   "Regular materials described with full set of thermal properties"
#> ────────────────────────────── * PROPERTIES * ─────────────────────────────
#>   Group: `Surface Construction Elements`
#>   Unique: FALSE
#>   Required: FALSE
#>   Total fields: 9
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#>   1:* Name
#>   2:* Roughness
#>   3:* Thickness {m}
#>   4:* Conductivity {W/m-K}
#>   5:* Density {kg/m3}
#>   6:* Specific Heat {J/kg-K}
#>   7:  Thermal Absorptance
#>   8:  Solar Absorptance
#>   9:  Visible Absorptance
#> ───────────────────────────────────────────────────────────────────────────

# OR
# idd$object("Material")[[1]]
```

With the `IddObject`, you can easily get class and field properties
using methods it has.

``` r
setdiff(ls(def_mat), "initialize")
#>  [1] "add_extensible_group"      "class_format"             
#>  [3] "class_index"               "class_name"               
#>  [5] "clone"                     "del_extensible_group"     
#>  [7] "extensible_group_num"      "field_choice"             
#>  [9] "field_default"             "field_index"              
#> [11] "field_name"                "field_note"               
#> [13] "field_range"               "field_type"               
#> [15] "field_unit"                "first_extensible_index"   
#> [17] "group_index"               "group_name"               
#> [19] "has_name"                  "is_autocalculatable_field"
#> [21] "is_autosizable_field"      "is_extensible"            
#> [23] "is_extensible_index"       "is_integer_field"         
#> [25] "is_numeric_field"          "is_required"              
#> [27] "is_required_field"         "is_unique"                
#> [29] "is_valid_field_index"      "is_valid_field_name"      
#> [31] "is_valid_field_num"        "memo"                     
#> [33] "min_fields"                "num_extensible"           
#> [35] "num_fields"                "print"
```

For example, you can get all default field values using
`$field_default()`.

``` r
def_val <- def_mat$field_default()
def_val
#> [[1]]
#> [1] NA
#> 
#> [[2]]
#> [1] NA
#> 
#> [[3]]
#> [1] NA
#> 
#> [[4]]
#> [1] NA
#> 
#> [[5]]
#> [1] NA
#> 
#> [[6]]
#> [1] NA
#> 
#> [[7]]
#> [1] 0.9
#> 
#> [[8]]
#> [1] 0.7
#> 
#> [[9]]
#> [1] 0.7
```

As we did not give any field index or name, a list will be returned
containing default values of all fields. The type of each value will be
consistent with the field definition.

``` r
vapply(def_val, class, character(1))
#> [1] "character" "character" "numeric"   "numeric"   "numeric"   "numeric"  
#> [7] "numeric"   "numeric"   "numeric"
```

> NOTE: For numeric fields with default values being `"autosize"` or
> `"autocalculate"`, the type of returned values will be “character”.

Please see `?idd_object` for detailed documentation on `IddObject`.

### Get object

In an `Idf`, all objects in the model are assigned with an unique `ID`
according to their appearance sequences in the IDF. You can find all
valid `ID`s using `$object_id()`.

``` r
model$object_id(class = c("Material", "Construction"), simplify = FALSE)
#> $Material
#>  [1] 43 44 45 46 47 48 49 50 51 52
#> 
#> $Construction
#> [1] 66 67 68 69 70 71 72
```

You can get all object names using `$object_name()`. If the class does
not have name attribute, `NA` will
returned.

``` r
model$object_name(class = c("Version", "Material", "Construction"), simplify = FALSE)
#> $Version
#> [1] NA
#> 
#> $Material
#>  [1] "WD10" "RG01" "BR01" "IN46" "WD01" "PW03" "IN02" "GP01" "GP02" "CC03"
#> 
#> $Construction
#> [1] "ROOF-1"               "WALL-1"               "CLNG-1"              
#> [4] "FLOOR-SLAB-1"         "INT-WALL-1"           "Dbl Clr 3mm/13mm Air"
#> [7] "Sgl Grey 3mm"
```

Object number in each class can be retrieved using
`$object_num()`.

``` r
model$object_num(c("BuildingSurface:Detailed", "Material", "Output:Variable"))
#> [1] 40 10 13
```

Having the object ID or name, you can easily get any object using
`$object()`.

> NOTE: The matching of object names is case-insensitive. For instance,
> `model$object("rOoF")` is equivalent to `model$object("roof")`.

`$object()` returns a list of `IdfObject`s. The names of returned list
are object names, except that all names are converted into valid R
names, i.e. all other characters except letters and numbers are replaced
by underscore. (Below this pattern will be referred as
“underscore-style”)

``` r
model$object(c("WD10", "ROOF-1"))
#> $WD10
#> <<[ID:43] `WD10`>> Material
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#> *1: WD10,              !- Name
#> *2: MediumSmooth,      !- Roughness
#> *3: 0.667,             !- Thickness {m}
#> *4: 0.115,             !- Conductivity {W/m-K}
#> *5: 513,               !- Density {kg/m3}
#> *6: 1381,              !- Specific Heat {J/kg-K}
#>  7: 0.9,               !- Thermal Absorptance
#>  8: 0.78,              !- Solar Absorptance
#>  9: 0.78;              !- Visible Absorptance
#> ───────────────────────────────────────────────────────────────────────────
#> 
#> $ROOF_1
#> <<[ID:66] `ROOF-1`>> Construction
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#> *1: ROOF-1,            !- Name
#> *2: RG01,              !- Outside Layer
#>  3: BR01,              !- Layer 2
#>  4: IN46,              !- Layer 3
#>  5: WD01;              !- Layer 4
#> ───────────────────────────────────────────────────────────────────────────
```

If you want to get all objects in a single class, use
`$object_in_class()`.

``` r
model$object_in_class("Material")
#> $WD10
#> <<[ID:43] `WD10`>> Material
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#> *1: WD10,              !- Name
#> *2: MediumSmooth,      !- Roughness
#> *3: 0.667,             !- Thickness {m}
#> *4: 0.115,             !- Conductivity {W/m-K}
#> *5: 513,               !- Density {kg/m3}
#> *6: 1381,              !- Specific Heat {J/kg-K}
#>  7: 0.9,               !- Thermal Absorptance
#>  8: 0.78,              !- Solar Absorptance
#>  9: 0.78;              !- Visible Absorptance
#> ───────────────────────────────────────────────────────────────────────────
#> 
#> $RG01
#> <<[ID:44] `RG01`>> Material
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#> *1: RG01,              !- Name
#> *2: Rough,             !- Roughness
#> *3: 0.0127,            !- Thickness {m}
#> *4: 1.442,             !- Conductivity {W/m-K}
#> *5: 881,               !- Density {kg/m3}
#> *6: 1674,              !- Specific Heat {J/kg-K}
#>  7: 0.9,               !- Thermal Absorptance
#>  8: 0.65,              !- Solar Absorptance
#>  9: 0.65;              !- Visible Absorptance
#> ───────────────────────────────────────────────────────────────────────────
#> 
#> $BR01
#> <<[ID:45] `BR01`>> Material
....
```

Also, you can get all objects in a single class using `"$"` or `"[["`.
Class names can be given in underscore-style, which is handy. For
example, you can just use `model$Material_NoMass` instead of
``model$`Material:Nomass` `` to save some typing.

``` r
model$Material_NoMass
#> $CP01
#> <<[ID:53] `CP01`>> Material:NoMass
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#> *1: CP01,              !- Name
#> *2: Rough,             !- Roughness
#> *3: 0.367,             !- Thermal Resistance {m2-K/W}
#>  4: 0.9,               !- Thermal Absorptance
#>  5: 0.75,              !- Solar Absorptance
#>  6: 0.75;              !- Visible Absorptance
#> ───────────────────────────────────────────────────────────────────────────
#> 
#> $MAT_SB_U
#> <<[ID:54] `MAT-SB-U`>> Material:NoMass
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#> *1: MAT-SB-U,          !- Name
#> *2: Rough,             !- Roughness
#> *3: 0.11740667,        !- Thermal Resistance {m2-K/W}
#>  4: 0.65,              !- Thermal Absorptance
#>  5: 0.65,              !- Solar Absorptance
#>  6: 0.65;              !- Visible Absorptance
#> ───────────────────────────────────────────────────────────────────────────
#> 
#> $MAT_CLNG_1
#> <<[ID:55] `MAT-CLNG-1`>> Material:NoMass
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#> *1: MAT-CLNG-1,        !- Name
#> *2: Rough,             !- Roughness
#> *3: 0.65225929,        !- Thermal Resistance {m2-K/W}
#>  4: 0.65,              !- Thermal Absorptance
#>  5: 0.65,              !- Solar Absorptance
....
# OR
# model[["Material_NoMass"]]
```

Based on the above, if you want to get the first object in class
`RunPeriod`, you can simply run:

``` r
rp <- model$RunPeriod[[1]]
rp
#> <<[ID:8] `WinterDay`>> RunPeriod
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#>   1: WinterDay,         !- Name
#> * 2: 1,                 !- Begin Month
#> * 3: 14,                !- Begin Day of Month
#> * 4: 1,                 !- End Month
#> * 5: 14,                !- End Day of Month
#>   6: Tuesday,           !- Day of Week for Start Day
#>   7: Yes,               !- Use Weather File Holidays and Special Days
#>   8: Yes,               !- Use Weather File Daylight Saving Period
#>   9: No,                !- Apply Weekend Holiday Rule
#>  10: Yes,               !- Use Weather File Rain Indicators
#>  11: Yes;               !- Use Weather File Snow Indicators
#> ───────────────────────────────────────────────────────────────────────────
```

`$search_object()` will search and return a list of objects whose names
meet the regular expression you give.

``` r
model$search_object("Demand", class = "Branch")
#> $`Heating Demand Inlet Branch`
#> <<[ID:222] `Heating Demand Inlet Branch`>> Branch
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#> *1: Heating Demand Inlet Branch,  !- Name
#>  2: <Blank>,           !- Pressure Drop Curve Name
#> *3: Pipe:Adiabatic,    !- Component 1 Object Type
#> *4: Heating Demand Inlet Pipe,  !- Component 1 Name
#> *5: HW Demand Inlet Node,  !- Component 1 Inlet Node Name
#> *6: HW Demand Entrance Pipe Outlet Node;  !- Component 1 Outlet Node Name
#> ───────────────────────────────────────────────────────────────────────────
#> 
#> $`Heating Demand Outlet Branch`
#> <<[ID:223] `Heating Demand Outlet Branch`>> Branch
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#> *1: Heating Demand Outlet Branch,  !- Name
#>  2: <Blank>,           !- Pressure Drop Curve Name
#> *3: Pipe:Adiabatic,    !- Component 1 Object Type
#> *4: Heating Demand Outlet Pipe,  !- Component 1 Name
#> *5: HW Demand Exit Pipe Inlet Node,  !- Component 1 Inlet Node Name
#> *6: HW Demand Outlet Node;  !- Component 1 Outlet Node Name
....
```

> NOTE: Under the hook, `stringr::str_detect` is used for matching,
> which is case-sensitive by default. If you want more controls on how
> the matching is performed, build your regular expression using
> `stringr::regex`.

After you get the objects, you can perform detailed modifications on
them using methods in `IdfObject`. Below lists all methods it has.

``` r
setdiff(ls(rp), "initialize")
#>  [1] "class_name"      "clone"           "definition"     
#>  [4] "get_comment"     "get_value"       "group_name"     
#>  [7] "has_ref"         "has_ref_by"      "has_ref_from"   
#> [10] "id"              "is_valid"        "name"           
#> [13] "possible_value"  "print"           "ref_by_object"  
#> [16] "ref_from_object" "set_comment"     "set_value"      
#> [19] "string"          "table"           "validate"
```

Similarly, you can use `"$"` and `"[["` to get a single value in an
`IdfObject` class or `"["` to get multiple values just like normal lists
in R.

``` r
rp$Begin_Day_of_Month
#> [1] 14

# OR
rp[["Begin_Day_of_Month"]]
#> [1] 14
rp[[3]]
#> [1] 14
```

You can also make a chain.

``` r
model$RunPeriod$WinterDay$Begin_Day_of_Month
#> [1] 14
```

### Modify object

There are two ways to modify objects in eplusr. One is using methods in
`Idf` which works on multiple objects, and the other way is using
methods in `IdfObject` which only works for a single object.

> NOTE: Validations are performed during object modifications under
> different strictness level (`none`, `draft`, `final`). For detailed
> explanations, please see `?eplusr_option()`.

You can duplicate, add, insert, modify and delete objects using
`$dup_object()`, `$add_object()`, `$ins_object()`, `$set_object()` and
`$del_object()` in `Idf`, respectively.

Object IDs will be appended after `$dup_object()`, `$add_object()` and
`$ins_object()`, and the newly added object will have the max ID. Object
IDs will never be reused, even though their binded objects have been
deleted using `$del_object()`.

For modifying object’s comments and values, you can also use
`$set_comment()` and `$set_value()` in `IdfObject` class.

#### Duplicate objects

`$dup_object()` will duplicate objects specified by object IDs or names.
If the target classes have a name attribute, you can assign new names to
the duplicated objects using argument `new_name`. If `new_name` is
`NULL`, which is default, the newly added object will have the same name
as the original object except a appended suffix of “`_1`”, “`_2`” and
etc.

``` r
model$dup_object(c("ROOF-1", "ROOF-1", "WALL-1"))
#> ── Info ───────────────────────────────────────────────────────────────────
#> New names of duplicated objects were not given. Automatically generated names were assigned:
#>   * Target Object [ID: 66] Name `ROOF-1`--> Auto Assigned Name: `ROOF-1_1`
#>   * Target Object [ID: 66] Name `ROOF-1`--> Auto Assigned Name: `ROOF-1_2`
#>   * Target Object [ID: 67] Name `WALL-1`--> Auto Assigned Name: `WALL-1_1`
#> $ROOF_1_1
#> <<[ID:323] `ROOF-1_1`>> Construction
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#> *1: ROOF-1_1,          !- Name
#> *2: RG01,              !- Outside Layer
#>  3: BR01,              !- Layer 2
#>  4: IN46,              !- Layer 3
#>  5: WD01;              !- Layer 4
#> ───────────────────────────────────────────────────────────────────────────
#> 
#> $ROOF_1_2
#> <<[ID:324] `ROOF-1_2`>> Construction
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#> *1: ROOF-1_2,          !- Name
#> *2: RG01,              !- Outside Layer
#>  3: BR01,              !- Layer 2
#>  4: IN46,              !- Layer 3
#>  5: WD01;              !- Layer 4
#> ───────────────────────────────────────────────────────────────────────────
#> 
#> $WALL_1_1
#> <<[ID:325] `WALL-1_1`>> Construction
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#> *1: WALL-1_1,          !- Name
#> *2: WD01,              !- Outside Layer
#>  3: PW03,              !- Layer 2
#>  4: IN02,              !- Layer 3
#>  5: GP01;              !- Layer 4
#> ───────────────────────────────────────────────────────────────────────────
```

#### Add new objects

You can add new objects using `$add_object()`. With `default` being
`TRUE`, all empty fields are filled with default values, if possible.
Only minimum fields will be added by default. But you can change it by
setting `all` to `TRUE`.

Field values should be given in a list following either pattern below:

  - directly list all field values with no name. The values will be
    assigned to fields according to the appearance order;
  - give both field names *without units* and values in pair, e.g.
    ``Name = "Test", `Begin Month` = 1``. You can find all valid field
    names using `$definition("class_name")[[1]]$field_name()`. Field
    names can also be given in underscore-style, e.g. `Name = "Test",
    begin_month = 1` (NOTE: matching is case-insensitive).

You can also add new comments alongside with new values.

For example, here we add two new objects in `RunPeriod`:

``` r
model$add_object(rep("RunPeriod", 2),
  value = list(
    list("rp_test_1", 1, 1, 2, 1),

    list(name = "rp_test_2",
         begin_month = 3,
         begin_day_of_month = 1,
         end_month = 4,
         end_day_of_month = 1)
    ),
  comment = list(
    list("Comment for new object 1", "Another comment"),
    list("Comment for new object 2")),
  default = TRUE
)
#> $rp_test_1
#> <<[ID:326] `rp_test_1`>> RunPeriod
#> ─────────────────────────────── * COMMENTS * ──────────────────────────────
#> !Comment for new object 1
#> !Another comment
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#>   1: rp_test_1,         !- Name
#> * 2: 1,                 !- Begin Month
#> * 3: 1,                 !- Begin Day of Month
#> * 4: 2,                 !- End Month
#> * 5: 1,                 !- End Day of Month
#>   6: UseWeatherFile,    !- Day of Week for Start Day
#>   7: Yes,               !- Use Weather File Holidays and Special Days
#>   8: Yes,               !- Use Weather File Daylight Saving Period
#>   9: No,                !- Apply Weekend Holiday Rule
#>  10: Yes,               !- Use Weather File Rain Indicators
#>  11: Yes;               !- Use Weather File Snow Indicators
#> ───────────────────────────────────────────────────────────────────────────
#> 
#> $rp_test_2
#> <<[ID:327] `rp_test_2`>> RunPeriod
#> ─────────────────────────────── * COMMENTS * ──────────────────────────────
#> !Comment for new object 2
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#>   1: rp_test_2,         !- Name
#> * 2: 3,                 !- Begin Month
#> * 3: 1,                 !- Begin Day of Month
#> * 4: 4,                 !- End Month
#> * 5: 1,                 !- End Day of Month
#>   6: UseWeatherFile,    !- Day of Week for Start Day
#>   7: Yes,               !- Use Weather File Holidays and Special Days
#>   8: Yes,               !- Use Weather File Daylight Saving Period
#>   9: No,                !- Apply Weekend Holiday Rule
#>  10: Yes,               !- Use Weather File Rain Indicators
#>  11: Yes;               !- Use Weather File Snow Indicators
#> ───────────────────────────────────────────────────────────────────────────
```

#### Set new values and comments

Changing values of existing objects can be conducted using
`$set_object()` in `Idf` or `$set_value()` in `IdfObject`. Basic rules
above of field values provided also apply to `$set_object()`, i.e. you
should give either named values or non-named values in lists. For
example:

``` r
model$set_object("rp_test_1", list(name = "rp_test_3", begin_day_of_month = 2),
  comment = list(format(Sys.Date()), "begin day has been changed."))
#> $rp_test_3
#> <<[ID:326] `rp_test_3`>> RunPeriod
#> ─────────────────────────────── * COMMENTS * ──────────────────────────────
#> !2018-08-06
#> !begin day has been changed.
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#>   1: rp_test_3,         !- Name
#> * 2: 1,                 !- Begin Month
#> * 3: 2,                 !- Begin Day of Month
#> * 4: 2,                 !- End Month
#> * 5: 1,                 !- End Day of Month
#>   6: UseWeatherFile,    !- Day of Week for Start Day
#>   7: Yes,               !- Use Weather File Holidays and Special Days
#>   8: Yes,               !- Use Weather File Daylight Saving Period
#>   9: No,                !- Apply Weekend Holiday Rule
#>  10: Yes,               !- Use Weather File Rain Indicators
#>  11: Yes;               !- Use Weather File Snow Indicators
#> ───────────────────────────────────────────────────────────────────────────
```

Also, if the modified fields are referenced by fields in other objects,
the corresponding fields will also be updated. You can check that by
comparing the values referencing the target object before and after.

``` r
mat <- model$Material$CC03

# get other objects referencing this object
mat$ref_by_object()
#> 1 object found that reference the target object [ID: 52].
#> $FLOOR_SLAB_1
#> <<[ID:69] `FLOOR-SLAB-1`>> Construction
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#> *1: FLOOR-SLAB-1,      !- Name
#> *2: CC03;              !- Outside Layer
#> ───────────────────────────────────────────────────────────────────────────

mat$set_value(name = "CC03_renamed")
#> <<[ID:52] `CC03_renamed`>> Material
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#> *1: CC03_renamed,      !- Name
#> *2: MediumRough,       !- Roughness
#> *3: 0.1016,            !- Thickness {m}
#> *4: 1.31,              !- Conductivity {W/m-K}
#> *5: 2243,              !- Density {kg/m3}
#> *6: 837,               !- Specific Heat {J/kg-K}
#>  7: 0.9,               !- Thermal Absorptance
#>  8: 0.65,              !- Solar Absorptance
#>  9: 0.65;              !- Visible Absorptance
#> ───────────────────────────────────────────────────────────────────────────

mat$ref_by_object()
#> 1 object found that reference the target object [ID: 52].
#> $FLOOR_SLAB_1
#> <<[ID:69] `FLOOR-SLAB-1`>> Construction
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#> *1: FLOOR-SLAB-1,      !- Name
#> *2: CC03_renamed;      !- Outside Layer
#> ───────────────────────────────────────────────────────────────────────────
```

Sometimes, you may want to get all possible values of fields before you
change them. You can achieve that by using `$possible_value()` method in
`IdfObject` class.

``` r
mat$possible_value()
#> ── 1: Field `Name` ────────────────────────────────────────────────────────
#> <Not Applicable>
#> 
#> ── 2: Field `Roughness` ───────────────────────────────────────────────────
#> * Choice:
#>   - `VeryRough`
#>   - `Rough`
#>   - `MediumRough`
#>   - `MediumSmooth`
#>   - `Smooth`
#>   - `VerySmooth`
#> 
#> ── 3: Field `Thickness` ───────────────────────────────────────────────────
#> * Range: (0, Inf)
#> 
#> ── 4: Field `Conductivity` ────────────────────────────────────────────────
#> * Range: (0, Inf)
#> 
#> ── 5: Field `Density` ─────────────────────────────────────────────────────
#> * Range: (0, Inf)
#> 
#> ── 6: Field `Specific Heat` ───────────────────────────────────────────────
#> * Range: [100, Inf)
#> 
#> ── 7: Field `Thermal Absorptance` ─────────────────────────────────────────
#> * Default: 0.9
#> * Range: (0, 0.99999]
#> 
#> ── 8: Field `Solar Absorptance` ───────────────────────────────────────────
#> * Default: 0.9
#> * Range: [0, 1]
#> 
#> ── 9: Field `Visible Absorptance` ─────────────────────────────────────────
#> * Default: 0.9
#> * Range: [0, 1]
```

#### Insert objects

Sometimes it may be useful to insert objects from other IDFs. For
example, you may want to import some design days from a “.ddy” file. You
can achieve that using `$ins_object()`.

``` r
# read ddy file as normal IDF
ddy <- read_idf("San_Francisco.ddy", idd = 8.8)
#> Warning: Missing version field in input Idf file. The given Idd version
#> 8.8.0 will be used. Parsing errors may occur.

model$ins_object(ddy$SizingPeriod_DesignDay)
#> $San_Francisco_Intl_Ap_Ann_Htg_99_6_Condns_DB
#> <<[ID:328] `San Francisco Intl Ap Ann Htg 99.6% Condns DB`>> SizingPeriod:DesignDay
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#> * 1: San Francisco Intl Ap Ann Htg 99.6% Condns DB,  !- Name
#> * 2: 1,                 !- Month
#> * 3: 21,                !- Day of Month
#> * 4: WinterDesignDay,   !- Day Type
#>   5: 3.8,               !- Maximum Dry-Bulb Temperature {C}
#>   6: 0,                 !- Daily Dry-Bulb Temperature Range {deltaC}
#>   7: DefaultMultipliers,!- Dry-Bulb Temperature Range Modifier Type
#>   8: <Blank>,           !- Dry-Bulb Temperature Range Modifier Day Schedule Name
#>   9: Wetbulb,           !- Humidity Condition Type
#>  10: 3.8,               !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}
#>  11: <Blank>,           !- Humidity Condition Day Schedule Name
#>  12: <Blank>,           !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}
#>  13: <Blank>,           !- Enthalpy at Maximum Dry-Bulb {J/kg}
#>  14: <Blank>,           !- Daily Wet-Bulb Temperature Range {deltaC}
#>  15: 101301,            !- Barometric Pressure {Pa}
#> *16: 2.2,               !- Wind Speed {m/s}
#> *17: 150,               !- Wind Direction {deg}
....
```

#### Delete object

`$del_object()` will delete current objects specified by object IDs or
names. For example, in current model, there is a material named
`"MAT-CLNG-1"` in class `Material:NoMass`. You can get objects
referencing `MAT-CLNG-1` by using `$ref_by_object()` in `IdfObject`.

``` r
clng <- model$Material_NoMass$MAT_CLNG_1
clng$ref_by_object()
#> 1 object found that reference the target object [ID: 55].
#> $CLNG_1
#> <<[ID:68] `CLNG-1`>> Construction
#> ──────────────────────────────── * FIELDS * ───────────────────────────────
#> *1: CLNG-1,            !- Name
#> *2: MAT-CLNG-1;        !- Outside Layer
#> ───────────────────────────────────────────────────────────────────────────
```

As we can see, `MAT-CLNG-1` has been referenced by a construction named
`"CLNG-1"`. In `final` validate level, if the object is referenced by
other object(s), it cannot be deleted.

``` r
eplusr_option("validate_level")
#> [1] "final"
model$del_object("mat-clng-1")
#> Error: Delete object that are referenced by others is prohibited in `final` validation level. Failed to delete target object [ID:`55`]:
#> 1: Object [ID:`55`] was referenced by other objects [ID:`68`].
```

In some cases, you may still want to delete that object. You can do that
by changing validate level to `draft` or `none`. For detail explanations
on each validate level, please see `?eplusr_option()`.

You can also delete objects referencing the target objects as well, by
setting `referenced` to `TRUE`.

``` r
eplusr_option(validate_level = "draft")
#> $validate_level
#> [1] "draft"
invisible(model$del_object("mat-clng-1", referenced = TRUE))
#> ── Info ───────────────────────────────────────────────────────────────────
#> Delete target object [ID:`55`] and also objects [ID: `68`] that are referencing target object.
```

### Validate

`$validate()` will check the validation of all fields in current model,
including missing required objects and fields, wrong value types,
choices, references, any value range exceeding, invalid autosizable and
autocalculatable fields. `$is_valid()` will return `TRUE` if no error is
found. Validate level can be changed using `eplusr_option()`. The
default level is `final`, which is the strictest level.

Material `MAT-CLNG-1` and construction `CLNG-1` have been all deleted in
above. After that, invalid references will be detected during model
validation, as construction `CLNG-1` was referenced by many other
objects in `BuildingSurface:Detailed`. And also, invalid autocalculate
fields in this model are detected.

``` r
eplusr_option(validate_level = "final")
#> $validate_level
#> [1] "final"
model$validate()
#>  ✖ [20] Errors found during validation.
#> ═══════════════════════════════════════════════════════════════════════════
#> 
#> ── [10] Invalid Autocalculate Field ───────────────────────────────────────
#> ⓧ Fields below cannot be `autocalculate`.
#> 
#>   Class `AirTerminal:SingleDuct:VAV:Reheat`
#>   +- Object [ID:172]
#>   |  +- 17: AUTOCALCULATE,     !- Maximum Flow per Zone Floor Area During Reheat {m3/s-m2}
#>   |  \- 18: AUTOCALCULATE;     !- Maximum Flow Fraction During Reheat
#>   +- Object [ID:173]
#>   |  +- 17: AUTOCALCULATE,     !- Maximum Flow per Zone Floor Area During Reheat {m3/s-m2}
#>   |  \- 18: AUTOCALCULATE;     !- Maximum Flow Fraction During Reheat
#>   +- Object [ID:174]
#>   |  +- 17: AUTOCALCULATE,     !- Maximum Flow per Zone Floor Area During Reheat {m3/s-m2}
#>   |  \- 18: AUTOCALCULATE;     !- Maximum Flow Fraction During Reheat
#>   +- Object [ID:175]
#>   |  +- 17: AUTOCALCULATE,     !- Maximum Flow per Zone Floor Area During Reheat {m3/s-m2}
#>   |  \- 18: AUTOCALCULATE;     !- Maximum Flow Fraction During Reheat
#>   \- Object [ID:176]
#>      +- 17: AUTOCALCULATE,     !- Maximum Flow per Zone Floor Area During Reheat {m3/s-m2}
#>      \- 18: AUTOCALCULATE;     !- Maximum Flow Fraction During Reheat
#> 
#> ── [10] Invalid Reference ─────────────────────────────────────────────────
#> ⓧ Fields below are not one of valid references.
#> 
#>   Class `BuildingSurface:Detailed`
#>   +- Object [ID:85]
#>   |  \- 3: CLNG-1;            !- Construction Name
#>   +- Object [ID:86]
#>   |  \- 3: CLNG-1;            !- Construction Name
#>   +- Object [ID:87]
#>   |  \- 3: CLNG-1;            !- Construction Name
#>   +- Object [ID:88]
#>   |  \- 3: CLNG-1;            !- Construction Name
#>   +- Object [ID:89]
#>   |  \- 3: CLNG-1;            !- Construction Name
#>   +- Object [ID:91]
#>   |  \- 3: CLNG-1;            !- Construction Name
#>   +- Object [ID:97]
#>   |  \- 3: CLNG-1;            !- Construction Name
#>   +- Object [ID:103]
#>   |  \- 3: CLNG-1;            !- Construction Name
#>   +- Object [ID:109]
#>   |  \- 3: CLNG-1;            !- Construction Name
#>   \- Object [ID:114]
#>      \- 3: CLNG-1;            !- Construction Name
```

### Save

You can save your model using `$save()`. If no path is given, the path
of model itself will be used. This may overwrite the current file which
has a risk of losing your original file and data. You have to set
`overwrite` to `TRUE` to confirm the process.

``` r
model$save(overwrite = TRUE)

model$save("test.idf")
```

### Run and Collect Output

eplusr uses the EnergyPlus command line interface which was introduced
since EnergyPlus v8.3.0, which means that `$run()` only supports models
with version higher than v8.3.0.

eplusr will auto-detect already installed EnergyPlus in the standard
installation locations. You can get all detected EnergyPlus versions
using `avail_eplus()`.

``` r
avail_eplus()
#> [1] "8.6.0" "8.8.0"
```

`$run()` will issue an error if corresponding version of EnergyPlus is
not found. If your EnergyPlus was not installed in standard location,
you can add that location into eplusr EnergyPlus location dictionary
using `use_eplus()`.

``` r
use_eplus("C:/EnergyPlusV8-8-0")
```

If the needed version of EnergyPlus was not installed, you can use
`install_eplus()` to install it.

``` r
install_eplus(ver = 8.9)
```

Sometimes, before simulation, it may be useful to retrieve weather data
from EnergyPlus Weather (EPW) file and conduct analysis on the weather.
eplusr provides `read_epw()` to let you read an EPW file and `Epw` class
to query and modify weather data.

``` r
epw_sf <- read_epw("San_Francisco.epw")
epw_sf
#> ── Location ───────────────────────────────────────────────────────────────
#> ● [ City    ]: San Francisco Intl Ap
#> ● [ State   ]: CA
#> ● [ Country ]: USA
#> ● [ Source  ]: TMY3
#> ● [ WMO Num ]: 724940
#> ● [Latitude ]: 37.62
#> ● [Longitude]: -122.4
#> ● [Time Zone]: -8
#> ● [Evevation]: 2
#> 
#> ── Data Period ────────────────────────────────────────────────────────────
#> ● [Period Num ]: 1
#> ● [Time Step  ]: 60 min
#> ● [Date Range ]: 1月 01 - 12月 31
#> ● [1st Weekday]: Sunday
#> ● [Real Year  ]: 1
#> 
#> ── Holidays and Daylight Savings ──────────────────────────────────────────
#> ● [ Leap Year ]: FALSE
#> ● [ DST Range ]: NA
#> ● [Holiday Num]: 0
```

`read_epw()` returns an `Epw` object. For details on `Epw` class, please
see `?epw`. Below are all methods of `Epw`:

``` r
setdiff(ls(epw_sf), "initialize")
#>  [1] "city"              "clone"             "country"          
#>  [4] "data_source"       "elevation"         "get_data"         
#>  [7] "latitude"          "longitude"         "path"             
#> [10] "print"             "save"              "set_data"         
#> [13] "start_day_of_week" "state_province"    "time_step"        
#> [16] "time_zone"         "wmo_number"
```

You can get all weather data using `$get_data()`.

``` r
epw_data <- epw_sf$get_data()
str(epw_data)
#> Classes 'data.table' and 'data.frame':   8760 obs. of  36 variables:
#>  $ datetime                                        : POSIXct, format: "1999-01-01 01:00:00" "1999-01-01 02:00:00" ...
#>  $ year                                            : int  1999 1999 1999 1999 1999 1999 1999 1999 1999 1999 ...
#>  $ month                                           : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ day                                             : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ hour                                            : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ minute                                          : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ datasource                                      : chr  "?9?9?9?9E0?9?9?9?9?9?9?9?9?9?9?9?9?9?9?9*9*9?9?9?9" "?9?9?9?9E0?9?9?9?9?9?9?9?9?9?9?9?9?9?9?9*9*9?9?9?9" "?9?9?9?9E0?9?9?9?9?9?9?9?9?9?9?9?9?9?9?9*9*9?9?9?9" "?9?9?9?9E0?9?9?9?9?9?9?9?9?9?9?9?9?9?9?9*9*9?9?9?9" ...
#>  $ dry_bulb_temperature                            : num  7.2 7.2 6.7 6.1 4.4 4.4 6.1 5 7.8 8.9 ...
#>  $ dew_point_temperature                           : num  5.6 5.6 5 5 3.9 3.9 4.4 2.8 5 6.7 ...
#>  $ relative_humidity                               : num  90 90 89 93 97 97 89 86 82 86 ...
#>  $ atmospheric_pressure                            : num  102200 102100 102200 102200 102200 ...
#>  $ extraterrestrial_horizontal_radiation           : num  0 0 0 0 0 0 0 36 243 443 ...
#>  $ extraterrestrial_direct_normal_radiation        : num  0 0 0 0 0 ...
#>  $ horizontal_infrared_radiation_intensity_from_sky: num  290 296 291 276 280 280 287 281 295 299 ...
#>  $ global_horizontal_radiation                     : num  0 0 0 0 0 0 0 2 121 276 ...
#>  $ direct_normal_radiation                         : num  0 0 0 0 0 0 0 69 477 651 ...
#>  $ diffuse_horizontal_radiation                    : num  0 0 0 0 0 0 0 1 39 71 ...
#>  $ global_horizontal_illuminance                   : num  0 0 0 0 0 ...
#>  $ direct_normal_illuminance                       : num  0 0 0 0 0 ...
#>  $ diffuse_horizontal_illuminance                  : num  0 0 0 0 0 ...
#>  $ zenith_luminance                                : num  0 0 0 0 0 ...
#>  $ wind_direction                                  : num  0 170 210 200 260 180 0 0 0 0 ...
#>  $ wind_speed                                      : num  0 2.1 2.1 1.5 3.1 2.1 0 0 0 0 ...
#>  $ total_sky_cover                                 : int  2 4 3 0 3 3 3 3 3 2 ...
#>  $ opaque_sky_cover                                : int  2 4 3 0 3 3 3 3 3 2 ...
#>  $ visibility                                      : num  16 16 16 16 16 16 16 16 16 16 ...
#>  $ ceiling_height                                  : num  77777 77777 77777 77777 77777 ...
#>  $ present_weather_observation                     : int  9 9 9 9 9 9 9 9 9 9 ...
#>  $ present_weather_codes                           : chr  NA NA NA NA ...
#>  $ precipitable_water                              : num  129 120 120 120 120 120 120 110 110 110 ...
#>  $ aerosol_optical_depth                           : num  0.108 0.108 0.108 0.108 0.108 0.108 0.108 0.108 0.108 0.108 ...
#>  $ snow_depth                                      : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ days_since_last_snow                            : int  88 88 88 88 88 88 88 88 88 88 ...
#>  $ albedo                                          : num  0.16 0.16 0.16 0.16 0.16 0.16 0.16 0.16 0.16 0.16 ...
#>  $ liquid_precip_depth                             : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ liquid_precip_rate                              : num  1 1 1 1 1 1 1 1 1 1 ...
#>  - attr(*, ".internal.selfref")=<externalptr>
```

`$run()` will run the current model with specified weather using
corresponding version of EnergyPlus. The model and the weather used will
be copied to the output directory. An `EplusJob` object will be returned
which provides detailed information of the simulation and methods to
collect simulation output. Please see `?job` for more detailed.

``` r
# read the model again
model <- read_idf("5Zone_Transformer.idf", idd = NULL)

job <- model$run(epw_sf, dir = ".", wait = TRUE)
#> ── Info ───────────────────────────────────────────────────────────────────
#> Adding object `Output:SQLite` and setting `Option Type` to `SimpleAndTabular`.
#> 
#> ── Info ───────────────────────────────────────────────────────────────────
#> Replace the existing file located  at /home/hongyuanjia/5Zone_Transformer.idf.
#> 
#> ── Info ───────────────────────────────────────────────────────────────────
#> Replace the existing file located  at /home/hongyuanjia/5Zone_Transformer.idf.
#> 
#> ExpandObjects Started.
#> No expanded file generated.
#> ExpandObjects Finished. Time:     0.027
#> EnergyPlus Starting
#> EnergyPlus, Version 8.8.0-7c3bbe4830, YMD=2018.08.06 23:31
#> Processing Data Dictionary
#> Processing Input File
#> Initializing Response Factors
#> Calculating CTFs for "ROOF-1", Construction # 1
#> Calculating CTFs for "WALL-1", Construction # 2
#> Calculating CTFs for "FLOOR-SLAB-1", Construction # 4
#> Calculating CTFs for "INT-WALL-1", Construction # 5
#> Initializing Window Optical Properties
#> Initializing Solar Calculations
#> Allocate Solar Module Arrays
#> Initializing Zone Report Variables
#> Initializing Surface (Shading) Report Variables
#> Computing Interior Solar Absorption Factors
#> Determining Shadowing Combinations
#> Computing Window Shade Absorption Factors
#> Proceeding with Initializing Solar Calculations
#> Initializing Surfaces
#> Initializing Outdoor environment for Surfaces
#> Setting up Surface Reporting Variables
#> Initializing Temperature and Flux Histories
#> Initializing Window Shading
#> Computing Interior Absorption Factors
#> Computing Interior Diffuse Solar Absorption Factors
#> Computing Interior Diffuse Solar Exchange through Interzone Windows
#> Initializing Solar Heat Gains
#> Initializing Internal Heat Gains
#> Initializing Interior Solar Distribution
#> Initializing Interior Convection Coefficients
#> Gathering Information for Predefined Reporting
#> Completed Initializing Surface Heat Balance
#> Calculate Outside Surface Heat Balance
#> Calculate Inside Surface Heat Balance
#> Calculate Air Heat Balance
#> Initializing HVAC
#> Warming up
#> Warming up
#> Warming up
#> Warming up
#> Warming up
#> Warming up
#> Performing Zone Sizing Simulation
#> ...for Sizing Period: #1 CHICAGO_IL_USA ANNUAL HEATING 99% DESIGN CONDITIONS DB
#> Warming up
#> Warming up
#> Warming up
#> Warming up
#> Warming up
#> Warming up
#> Performing Zone Sizing Simulation
#> ...for Sizing Period: #2 CHICAGO_IL_USA ANNUAL COOLING 1% DESIGN CONDITIONS DB/MCWB
#> Calculating System sizing
#> ...for Sizing Period: #1 CHICAGO_IL_USA ANNUAL HEATING 99% DESIGN CONDITIONS DB
#> Calculating System sizing
#> ...for Sizing Period: #2 CHICAGO_IL_USA ANNUAL COOLING 1% DESIGN CONDITIONS DB/MCWB
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
#> Starting Simulation at 01/14 for WINTERDAY
#> Initializing New Environment Parameters
#> Warming up {1}
#> Warming up {2}
#> Warming up {3}
#> Warming up {4}
#> Warming up {5}
#> Warming up {6}
#> Starting Simulation at 07/07 for SUMMERDAY
#> Writing tabular output file results using HTML format.
#> Writing final SQL reports
#>  ReadVarsESO program starting.
#>  ReadVars Run Time=00hr 00min  0.02sec
#>  ReadVarsESO program completed successfully.
#>  ReadVarsESO program starting.
#>  ReadVars Run Time=00hr 00min  0.01sec
#>  ReadVarsESO program completed successfully.
#> EnergyPlus Run Time=00hr 00min  1.35sec
job
#> ── EnergyPlus Simulation Job ──────────────────────────────────────────────
#> # Model: `/home/hongyuanjia/5Zone_Transformer.idf`
#> # Weather: `/home/hongyuanjia/San_Francisco.epw`
#> # EnergyPlus Version: `8.8.0`
#> # EnergyPlus Path: `/usr/local/EnergyPlus-8-8-0`
#>  Simulation started at `2018-08-06 23:31:59` and completed successfully after 1.42 secs.
```

#### Print simulation errors

You can get simulation errors using `$errors()`.

``` r
job$errors()
#> ══ During Zone Sizing Calculations ════════════════════════════════════════
#> ┌──────────────────────────────────────────────────────────────────────────────────────────────────────────┐
#> │Warning[1/1] Weather file location will be used rather than entered (IDF) Location object.                │
#> │  ..Location object=CHICAGO_IL_USA TMY2-94846                                                             │
#> │  ..Weather File Location=San Francisco Intl Ap CA USA TMY3 WMO#=724940                                   │
#> │  ..due to location differences, Latitude difference=[4.16] degrees, Longitude difference=[34.65] degrees.│
#> │  ..Time Zone difference=[2.0] hour(s), Elevation difference=[98.95] percent, [188.00] meters.            │
#> └──────────────────────────────────────────────────────────────────────────────────────────────────────────┘
#> 
#> EnergyPlus completed successfully with 1 Warning.
```

#### Retrieve simulation output

eplusr uses the EnergyPlus SQL output for extracting simulation output.
In order to do so, an object in `Output:SQLite` class with `Option Type`
value of `SimpleAndTabular` will be automatically created if it does not
exists. `EplusJob` has provided some wrappers that do SQL queries to get
report data results, i.e. results from `Output:Variable` and
`Output:Meter*`. But for `Output:Table` results, you have to be familiar
with the structure of the EnergyPlus SQL output, especially for table
*“TabularDataWithStrings”*. For details, please see *“2.20
eplusout.sql”*, especially *“2.20.4.4 TabularData Table”* in EnergyPlus
*“Output Details and Examples”* documentation.

`$report_data_dict()` returns a data.frame which contains all
information about report data. For details on the meaning of each
columns, please see *“2.20.2.1 ReportDataDictionary Table”* in
EnergyPlus *“Output Details and Examples”* documentation.

``` r
str(job$report_data_dict())
#> Classes 'data.table' and 'data.frame':   20 obs. of  10 variables:
#>  $ ReportDataDictionaryIndex: int  6 8 18 38 238 459 460 461 462 463 ...
#>  $ IsMeter                  : int  0 1 1 1 1 0 0 0 0 0 ...
#>  $ Type                     : chr  "Avg" "Sum" "Sum" "Sum" ...
#>  $ IndexGroup               : chr  "Zone" "Facility:Electricity" "Building:Electricity" "Facility:Electricity:InteriorLights" ...
#>  $ TimestepType             : chr  "HVAC System" "HVAC System" "HVAC System" "HVAC System" ...
#>  $ KeyValue                 : chr  "Environment" "" "" "" ...
#>  $ Name                     : chr  "Site Outdoor Air Drybulb Temperature" "Electricity:Facility" "Electricity:Building" "InteriorLights:Electricity" ...
#>  $ ReportingFrequency       : chr  "Zone Timestep" "Zone Timestep" "Zone Timestep" "Zone Timestep" ...
#>  $ ScheduleName             : chr  NA NA NA NA ...
#>  $ Units                    : chr  "C" "J" "J" "J" ...
#>  - attr(*, ".internal.selfref")=<externalptr>
```

`$report_data()` extracts the report data using key values and variable
names. Basically, `key_value` equals `KeyValue` and `name` equals `Name`
in the report data dictionary you get from `$report_data_dict()`.

``` r
str(job$report_data(name = "Site Outdoor Air Drybulb Temperature"))
#> Classes 'data.table' and 'data.frame':   192 obs. of  6 variables:
#>  $ Case    : chr  "5Zone_Transformer" "5Zone_Transformer" "5Zone_Transformer" "5Zone_Transformer" ...
#>  $ DateTime: POSIXct, format: "2018-01-14 00:15:00" "2018-01-14 00:30:00" ...
#>  $ KeyValue: chr  "Environment" "Environment" "Environment" "Environment" ...
#>  $ Name    : chr  "Site Outdoor Air Drybulb Temperature" "Site Outdoor Air Drybulb Temperature" "Site Outdoor Air Drybulb Temperature" "Site Outdoor Air Drybulb Temperature" ...
#>  $ Units   : chr  "C" "C" "C" "C" ...
#>  $ Value   : num  9.9 9.2 8.5 7.8 8.07 ...
#>  - attr(*, ".internal.selfref")=<externalptr>
```

`$tabular_data()` extracts all tabular data. For details on the meaning
of each columns, please see *“2.20.4.4 TabularData Table”* in EnergyPlus
*“Output Details and Examples”* documentation.

``` r
str(job$tabular_data())
#> Classes 'data.table' and 'data.frame':   6662 obs. of  8 variables:
#>  $ TabularDataIndex: int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ Value           : chr  "        1.34" "        1.34" "        3.65" "        3.65" ...
#>  $ ReportName      : chr  "AnnualBuildingUtilityPerformanceSummary" "AnnualBuildingUtilityPerformanceSummary" "AnnualBuildingUtilityPerformanceSummary" "AnnualBuildingUtilityPerformanceSummary" ...
#>  $ ReportForString : chr  "Entire Facility" "Entire Facility" "Entire Facility" "Entire Facility" ...
#>  $ TableName       : chr  "Site and Source Energy" "Site and Source Energy" "Site and Source Energy" "Site and Source Energy" ...
#>  $ RowName         : chr  "Total Site Energy" "Net Site Energy" "Total Source Energy" "Net Source Energy" ...
#>  $ ColumnName      : chr  "Total Energy" "Total Energy" "Total Energy" "Total Energy" ...
#>  $ Units           : chr  "GJ" "GJ" "GJ" "GJ" ...
#>  - attr(*, ".internal.selfref")=<externalptr>
```

### Run Parametric Analysis

eplusr provides tools to do parametric simulations which take full
advantages of eplusr’s model editing and result collecting
functionalities. You can create a parametric job using `param_job()`,
which takes an IDF file or an `Idf` object as the *seed* and an EPW file
or an `Epw` object as *weather*.

``` r
param <- param_job(idf = model, epw = epw_sf)

param
#> ── EnergPlus Parametric ───────────────────────────────────────────────────
#> ● Seed Model: `/home/hongyuanjia/5Zone_Transformer.idf`
#> ● Default Weather: `/home/hongyuanjia/San_Francisco.epw`
#> << No measure has been applied >>
```

`param_job()` returns a `ParametricJob` object which provides a
prototype of conducting parametric analysis of EnergyPlus simulations.
For more details, please see `?param`.

#### Apply measure

`$apply_measure()` allows to apply a measure to an `Idf` and create
parametric models for simulations. Here, the concept of measure in
eplusr is inspired by “measures” in
[OpenStudio](https://nrel.github.io/OpenStudio-user-documentation/getting_started/about_measures/).
Basically, a measure is just a function that takes an `Idf` object and
other arguments as input, and returns a modified `Idf` object as output.
Use `...` to supply different arguments to that measure.

Let’s create a function that modifies infiltration rate:

``` r
set_infil_rate <- function (idf, infil_rate) {

    # validate input value
    # this is optional, as validations will be performed when setting values
    stopifnot(is.numeric(infil_rate), infil_rate >= 0)

    if (!idf$is_valid_class("ZoneInfiltration:DesignFlowRate"))
      stop("Input model does not have any object in class `ZoneInfiltration:DesignFlowRate`")

    ids <- idf$object_id("ZoneInfiltration:DesignFlowRate", simplify = TRUE)

    idf$set_object(ids,
        value = rep(list(list(
            design_flow_rate_calculation_method = "AirChanges/Hour",
            air_changes_per_hour = infil_rate)),
            times = length(ids))
        )

    idf
}
```

The measure `set_infil_rate()` is pretty simple. First, it gets all
objects in class `ZoneInfiltration:DesignFlowRate`. Then it sets ACH in
all zones to the input value.

Now, let’s apply this measure to the seed model with different
infiltration rates from 0.0 to 4.0, respectively.

``` r
param$apply_measure(set_infil_rate, seq(0, 4, by = 1), .names = NULL)
#> Measure `set_infil_rate` has been applied with 5 new models created:
#> 1: set_infil_rate_1
#> 2: set_infil_rate_2
#> 3: set_infil_rate_3
#> 4: set_infil_rate_4
#> 5: set_infil_rate_5
```

As we can see, 5 models have been created. As we left `.names` as
`NULL`, each newly created models will be named as a combination of
measure name and model number.

#### Run in parallel and collect results

Now let’s run the parametric job. All simulations will be run in
parallel. The number of parallel EnergyPlus processes can be specified
using option `num_parallel`.

> Currently, unlike `EplusJob`, all simulations in `ParametricJob` will
> be run in waiting mode. This may be changed in the future.

``` r
param$run()
#> ── Info ───────────────────────────────────────────────────────────────────
#> Replace the existing file located  at /home/hongyuanjia/set_infil_rate_1/set_infil_rate_1.idf.
#> 
#> ── Info ───────────────────────────────────────────────────────────────────
#> Replace the existing file located  at /home/hongyuanjia/set_infil_rate_2/set_infil_rate_2.idf.
#> 
#> ── Info ───────────────────────────────────────────────────────────────────
#> Replace the existing file located  at /home/hongyuanjia/set_infil_rate_3/set_infil_rate_3.idf.
#> 
#> ── Info ───────────────────────────────────────────────────────────────────
#> Replace the existing file located  at /home/hongyuanjia/set_infil_rate_4/set_infil_rate_4.idf.
#> 
#> ── Info ───────────────────────────────────────────────────────────────────
#> Replace the existing file located  at /home/hongyuanjia/set_infil_rate_5/set_infil_rate_5.idf.
#> 
#> 
 Progress: ───────────────────────────────────                         100%
 Progress: ─────────────────────────────────────────────────────────── 100%
#> $set_infil_rate_1
#> ── EnergyPlus Simulation Job ──────────────────────────────────────────────
#> # Model: `/home/hongyuanjia/set_infil_rate_1/set_infil_rate_1.idf`
#> # Weather: `/home/hongyuanjia/San_Francisco.epw`
#> # EnergyPlus Version: `8.8.0`
#> # EnergyPlus Path: `/usr/local/EnergyPlus-8-8-0`
#>  Simulation started at `2018-08-06 23:32:03` and completed successfully after 2.64 secs.
#> 
#> $set_infil_rate_2
#> ── EnergyPlus Simulation Job ──────────────────────────────────────────────
#> # Model: `/home/hongyuanjia/set_infil_rate_2/set_infil_rate_2.idf`
#> # Weather: `/home/hongyuanjia/San_Francisco.epw`
#> # EnergyPlus Version: `8.8.0`
#> # EnergyPlus Path: `/usr/local/EnergyPlus-8-8-0`
#>  Simulation started at `2018-08-06 23:32:03` and completed successfully after 2.64 secs.
#> 
#> $set_infil_rate_3
#> ── EnergyPlus Simulation Job ──────────────────────────────────────────────
#> # Model: `/home/hongyuanjia/set_infil_rate_3/set_infil_rate_3.idf`
#> # Weather: `/home/hongyuanjia/San_Francisco.epw`
#> # EnergyPlus Version: `8.8.0`
#> # EnergyPlus Path: `/usr/local/EnergyPlus-8-8-0`
#>  Simulation started at `2018-08-06 23:32:03` and completed successfully after 2.64 secs.
#> 
#> $set_infil_rate_4
#> ── EnergyPlus Simulation Job ──────────────────────────────────────────────
#> # Model: `/home/hongyuanjia/set_infil_rate_4/set_infil_rate_4.idf`
#> # Weather: `/home/hongyuanjia/San_Francisco.epw`
#> # EnergyPlus Version: `8.8.0`
#> # EnergyPlus Path: `/usr/local/EnergyPlus-8-8-0`
#>  Simulation started at `2018-08-06 23:32:03` and completed successfully after 2.64 secs.
#> 
#> $set_infil_rate_5
#> ── EnergyPlus Simulation Job ──────────────────────────────────────────────
#> # Model: `/home/hongyuanjia/set_infil_rate_5/set_infil_rate_5.idf`
#> # Weather: `/home/hongyuanjia/San_Francisco.epw`
#> # EnergyPlus Version: `8.8.0`
#> # EnergyPlus Path: `/usr/local/EnergyPlus-8-8-0`
#>  Simulation started at `2018-08-06 23:32:03` and completed successfully after 2.64 secs.
```

After all simulations completed, let’s see the variations of total
energy.

``` r
tab <- param$tabular_data()

total_eng <- tab[TableName == "Site and Source Energy" &
    ColumnName == "Total Energy" &
    RowName == "Total Site Energy",
    list(Case, `Total Energy (GJ)` = as.numeric(Value))]
```

``` r
total_eng
```

| Case                | Total Energy (GJ) |
| :------------------ | ----------------: |
| set\_infil\_rate\_1 |              1.33 |
| set\_infil\_rate\_2 |              1.35 |
| set\_infil\_rate\_3 |              1.36 |
| set\_infil\_rate\_4 |              1.38 |
| set\_infil\_rate\_5 |              1.39 |

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
