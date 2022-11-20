# eplusr 0.15.3

## Bug fixes

* Fix EPW parsing error due to an update of lubridate (#532).

# eplusr 0.15.2

## New features

* A new `encoding` parameter has been added in `read_idf()`, `use_idd()` and
  `read_epw()`. The default value is `unknown` which indicates that the input
  file is native encoded (#515).
* Now `validate_level` option is respected when parsing EPW files. This makes it
  possible to parse EPW files with non-standard EPW header values, which is
  useful when only the core EPW data is needed (#520). E.g:
  ```
  with_option(list(validate_level = "none"), read_epw(YOUR_EPW_PATH))
  ```

## Bug fixes

* Now `IdfGeometry$coord_system()` can correctly work. The coordinate system
  type can also be `"world"`. `"absolute"` input now is automatically converted
  to `"world"` to be compatible with EnergyPlus. `IdfGeometry$coord_system()`
  now returns itself by default, instead of the parent `Idf` object. This
  enables to check the modified coordinate systems by printing the `IdfGeometry`
  (#506 #507).
* Fix `Idf$load()` when input is a single string without any new line (`\n`)
  (#510).
* Fix `IdfViewer$snapshot()` for PNG output. A new parameter `webshot` has been
  added in `IdfViewer$snapshot()` with default value being `FALSE` (#509).
* Now `IdfSchedule$... <- val` works (#512).
* Fix the error of invalid datetime in `Epw$data()` when data of Feb 28 is from
  a leap year while the EPW did not contain any leap year data (#552).

## Documentation

* Regenerate package documentation to fix CRAN NOTES on HTML manuals (#525).

# eplusr 0.15.1

## Bug fixes

* Fix `ParametricJob$cases()` when multiple objects are specified on the LHS in
  `ParametricJob$param()` (#492).
* Fix `install_eplus(9.1)` on Linux (#494, #495).
* Now `ParametricJob$run()` can correctly use weather file when only one
  parametric model exists (#497).

## Minor changes

* Compatibility changes for incoming {units} v0.8 (#499).

# eplusr v0.15.0

## New features

* `Idd$path()` is added to show the path of IDD parsed (#442).
* Add epw data sources from [climate.onebuilding.org](https://climate.onebuilding.org/)
  for `download_weather()`. `type` will always be `"all"` for those sources (#453).
* Add a new `"stat"` option in `type` in `download_weather()` (#453).
* `Idf$is_valid_id()` and `Idf$is_valid_name()` gain a new argument named
  `class` defaulting to `NULL` to check the validity of object IDs and names
  again a specific class.
* Add support for EnergyPlus v9.5 (#438).
* Add support for EnergyPlus v9.6 (#460).
* Internal function `locate_eplus()` has been exported to enable user to find
  all available EnergyPlus without restarting R (#477).
* `uninstall_eplus()` has been added to enable uninstall EnergyPlus from R on
  all platform (#477).
* Now force re-installation of EnergyPlus `install_eplus(force = TRUE)` has been
  updated to support all platforms, including macOS (#477).
* `install_eplus()` has been updated to behave smarter on macOS (#477).
* `run_idf()` and `run_multi()` have been refactored to mimick the `Epl-run.bat`
  procedure. It supports to call the `Basement` and `Slab` processors. Now the
  requirement of EnergyPlus >=v8.3 for running simulations has been droppped
  Now simulations with FMU, including obFMU are supported. (#467).
* `HVAC-Diagram` will be called when running simulations to make sure the `svg`
  output of HVAC diagram can be generated. Therefore, `hvac_diagram()`
  has been deprecated and will be removed in next major version (#467).
* Functions `path_eplus()` and `path_eplus_*()` have been added to help specify
  file paths under EnergyPlus installation directory (#467).
* A new `Idf` method `$external_deps()` is added to extra any external file
  resources specified in the IDF that are needed for simulation, e.g. schedule
  files (#467).
* Now `Idf$save(external = TRUE)` and `Idf$run(copy_external)` save external
  file dependencies based on the results of `Idf$external_deps()` (#467).
* A new `EplusJob` method `$list_files()` is added to list all inputs and output
  files for current simulation (#467).
* A new argument `readvars` can be specified in `EplusJob$run()` and
  `EplusGroupJob$run()`. Setting it to `FALSE` will disable to run ReadVarsESO
  post-processor and thus no CSVs will be generated for Report variables and
  meters. This can speed up simulations significantly if there are hundreds of
  outputs in the model. Setting it to `FALSE` will not affect any data
  extraction functionalities in eplusr, as it uses the SQLite output instead of
  the CSVs (#467).
* Now `.()` can also be used as an alias as `list()` in `Idf$add()` and
  `Idf$set()` (#445).

  ```r
  idf$add(Output_Variable = .("*", "zone mean air temperature"))
  # is equivalent to
  idf$add(Output_Variable = list("*", "zone mean air temperature"))
  ```
* A new argument `names` can be specified in `ParametricJob$models()` to rename
  the parametric models created (#487).
* A new interface for creating parametric models has been introduced using
  `ParametricJob$param()`. It takes parameter definitions in list format, which
  is similar to `Idf$set()` except that each field is not assigned with a single
  value, but a vector of any length, indicating the levels of each parameter.
  For example, the code block below defines 3 parameters (#487):

  * Field `Fan Total Efficiency` in object named `Supply Fan 1` in class
    `Fan:VariableVolume` class, with 10 levels being 0.1 to 1.0 with a
    0.1 step.
  * Field `Thickness` in all objects in class `Material`, with 10
    levels being 0.01 to 0.1 m with a 0.1 m step.
  * Field `Conductivity` in all objects in class `Material`, with 10
   levels being 0.1 to 1.0 W/m-K with a 0.1 W/m-K step.

  ```
  param$param(
      `Supply Fan 1` = list(Fan_Total_Efficiency = seq(0.1, 1.0, 0.1)),
      Material := list(Thickness = seq(0.01, 0.1, 0.1), Conductivity = seq(0.1, 1.0, 0.1))
  )
  ```
* `ParametricJob$cases()` is added to get a summary of parametric models and
  parameter values. It returns a `data.table` giving you the indices and names
  of the parametric models, and all parameter values used to create those
  models.For parametric models created using `ParametricJob$param()`, the column
  names will be the same as what you specified in `.names`. For the case of
  `ParametricJob$apply_measure()`, this will be the argument names of the
  measure functions (#487).
* Now `.names` in `ParametricJob$apply_measure()` can be a single character. In
  this case, it will be used as the prefix of all parametric models. The models
  will be named in the pattern `.names_X` where `X` is the model index (#487).

## Break changes

* `hvac_diagram()` has been deprecated as `HVAC-Diagram` will always be called
  after EnergyPlus simulation. If you still want to generate HVAC `svg` diagram
  manually, please use `HVAC_Diagram()` instead (#467).

## Minor changes

* When `type` is `"all"` in `download_weather()`, the ZIP file will be
  downloaded instead of downloading both `EPW` and `DDY` files (#453).
* `EplusJob$output_dir()` now use backslash in the returned path on Windows
  (#467).
* Better error message when no arguments are given to the measure function in
  `ParametricJob$apply_measure()` (#487).

## Bug fixes

* Fix the year value calculation when first day of a run period is holiday (#450).
* Fix `download_weather()` file downloading URL (#452).
* Fix `EplusSql$report_data(..., wide = TRUE)` when `Do HVAC Sizing Simulation
  for Sizing Periods` is set to `Yes` in `SimulationControl` (#461).
* Now `read_idf()` and other functions that read files from disk can use
  `stringi::stri_enc_detect()` to fix encoding issue (#467).
* Now `ParametricJob$run(dir = NULL)` will always use the seed model directory
  (#483).

# eplusr 0.14.2

## New features

* `IddObject$output()` is added to extract all possible outputs of current
  class. All outputs are extracted from the LaTeX source file of "Input
  Output Reference" for EnergyPlus v9.5.0 and later. So empty result will
  always be returned for `Idd` version lower than v9.5. It is possible that
  there are some mistakes introduced when extracting the output variables.
  Also, some outputs are only available if certain fields are set. Even they
  are listed in the results, it does not mean that the `Idf` can report all
  of them. It is strongly suggested to check the RDD and MDD file for
  correctness (#427). Example:

  ```r
  idd <- use_idd(8.8)
  idd$Lights$outputs()
  ```

## Break changes

* Autocompletion is enabled by registering a S3 `.DollarNames` method. Option
  `autocomplete` is deprecated. A warning is issued if you try to set it in
  `eplusr_option()`. Also, `with_speed()` is deprecated and falls back to
  `without_checking()` when called (#417).

## Bug fixes

* Fixed wrong transition of `FuelFactors` from v9.2 to v9.3 (#420).
* Fixed `Idf$del` error when both sources and referees are given (#433).
* Fixed the error that `EplusGroupJob$tabular_data(..., wide = TRUE)` did not
  return the `index` column (#449).

## Minor changes

* Better error and verbose messages (#422, #423).
* The default mouse mode for `wheel` has been changed to `"pull"`.

# eplusr 0.14.1

## Minor changes

* Compatible changes for [units](https://github.com/r-quantities/units) v0.7-0
  (#410).

# eplusr 0.14.0

## New features

* `Epw$save()` gets a new option `format_digit`. If `TRUE`, the trailing digits
  in EPW data will be formatted in the same way as Weather Converter (#323).
* Add EnergyPlus v9.3 and v9.4 support (#343, #347, #369).
* Now `ParametricJob$apply_measure()` will give a nice progress bar when
  parametric models are creating (#378).
* A new argument `separate` is added in `EplusGroupJob$run()` and
  `ParametricJob$run()` with default value being `TRUE`. If set to `FALSE`, all
  models are saved in `dir` when simulation, instead of creating a folder for
  each model and running simulation there (#381).
* When extracting grouped simulation results using `$report_data_dict()`,
  `$report_data()` and etc in `EplusGroupJob` and `ParametricJob` class, a new
  column `index` is added which contains the indices of simulated models (#388).
  This is because when the same model runs with different weather files, the
  original `case` column cannot be used as an identifier.

## Major changes

* Now eplusr will always use the SQLite output for data extraction instead of
  using the CSV output. In EnergyPlus v9.3 and above, ReadVarsESO is deprecated
  and EnergyPlus itself will generate CSV directly. CSVs for variables and
  meters are always separated in this way. Together with the newly introduced
  `Output:Control:Files` which can be set to disable CSV output, it becomes
  very cumbersome to continuously support the CSV for data extraction (#307).

## Minor changes

* Now rgl and decido package has been moved from *Imports* to *Suggests*, since
  they are only used in the `IdfViewer` class for 3D visualization which is not
  the main focus of this package (368).
* `run_multi()` now gets a new argument `expand_obj` to control whether
  `ExpandObjects` should be called before simulation or not. Because there is a
  known issue of `ExpandObjects` on Linux system
  (https://github.com/NREL/EnergyPlus/issues/8376), here we want to avoid
  unnecessary calls of `ExpandObjects` as possible. Also, `EplusJob$run()` and
  `EplusGroupJob$run()` now will detect if there are any `HVACTemplate:*`
  objects and set the `expand_obj` flag properly (#377).

## Bug fixes

* When `.empty` is `FALSE`, `Idf$set()` and `Idf$update()` will keep fields if
  there are not the last fields (#310).
* Year fields are correctly calculated for EnergyPlus v9.1 and above (#312).
* Fix coordinate system alignment in `IdfGeometry` class (#314).
* Fix time matching in `EplusSql$report_data()` (#315).
* EPW `COMMENT1` and `COMMENT2` are now parsed as a single string (#318).
* Preserve input year values in `Epw$set()` (#322).
* Additional columns in `Epw$set()` and `Epw$add()` input are now removed
  (#320).
* Now `Epw$abnormal_data()` can keep columns that contains abnormal data when
  `keep_all` is `FALSE` (#326).
* `Epw$save()` now formats the header in the same way as Weather Converter
  (#328).
* `Epw$comment1()` and `Epw$comment2()` now accepts `NULL` as input, which will
  remove the comments. And `NULL` is returned if no comments are found (#330).
* Fix the error when the first field is not in the input in `Idf$update()` (#332).
* Fix the error in `EplusJob$report_data()` caused by incomplete
  `EnvironmentPeriods` table in EnergyPlus SQL output (#336).
* Make sure `Day of Week for Start Day` is reset to empty if `UseWeatherFile`
  was used in transition from v8.9 to v9.0 (#338).
* Fix the error in `EplusJob$report_data()` when multiple reporting frequencies
  exist in the CSV output (#340).
* Fix simulation error when FMU files are given in relative paths (#344). Now
  all objects that reference to external files can be correctly copied into
  output directory when `copy_external` is set to `TRUE`.
* Fix the error when using an `Idf` as input in `Idf$insert()` (#348).
* Sub-hourly EPW files are supported (#351).
* Fix invalid references introduced by setting field values to empty (#355).
* Fix the error message in `EplusGroupJob` when no CSV output was found (#357).
* Fix false positive warnings when resolving IDF external file dependencies
  (#366).
* Empty string input, e.g. `"  "` in `Idf$add()` and `Idf$set()` are now
  correctly converted to `NA`s (#370).
* Now empty comments are kept in `IdfObject$comment()` (#372).
* Fix `IdfGeometry$print()` when no building object exists (#395).
* Now `IdfGeometry$round_digits()` also applies to `Zone` class (#397).
* `read_epw()` now accepts date rewind in `TYPICAL/EXTREME PERIODS` header
  (#401).
* Fix `c.EpwDate()` error (#403).
* Fix `IdfScheduleCompact$set()` evaluate issue when called in nested
  environment (#405).
* Now `IdfScheduleCompact$type_limits()` works properly when setting new type
  limits (#407).

# eplusr 0.13.0

## New features

* `Idf$add()` and `Idf$set()` have new features:

  ```r
  # refer to field using '..'
  idf$add(Material = list(..1 = "mat", ..7 = 0.95))
  idf$set(mat = list(..6 = 0.5))

  # using vector field values
  idf$add(Material := list(..1 = sprintf("mat%i", 1:10)))
  idf$set(c(sprintf("mat%i", 1:10)) := list(..6 = runif(10)))
  ```
* `Idf$to_table()` gains a new parameter `force`. The default value is `FALSE`. If
  `TRUE`, you can convert object data from any classes into a wide data.table.
  This may be useful when you know that target classes have the exact same
  fields, e.g.  `Ceiling:Adiabatic` and `Floor:Adiabatic` (#202).
* A new method `Idf$purge()` has been added (#223). It can be used to delete any
  resource objects that are not referenced by other objects. Here resource
  objects indicate all objects that can be referenced by other objects, e.g. all
  schedules. `$purge()` will ignore any inputs that are not resources. If inputs
  contain objects from multiple classes, references among them are also taken
  into account, which means purging is performed hierarchically. If both
  materials and constructions are specified, the latter will be purged first,
  because it is possible that input constructions reference input materials.
  `Idf$purge()` makes it quite straightforward to perform IDF cleaning. Actions
  like removing all materials, constructions and schedules can be easily
  achieved via
  ```r
  Idf$purge(class = c("Material", "Construction"), group = "Schedules")
  ```
* New methods `Idf$duplicatd()` and `Idf$unique()` have been added. They can be
  used to detect and remove duplicated objects, respectively. Here duplicated
  objects refer to objects whose field values are the same except the names.
  Object comments are ignored during comparison. These two methods can be
  useful when doing model cleaning (#227).
* Now if `class` is set to `NULL` in `Idf$definition()`, the underlying `Idd`
  object is returned (#237).
* Internal helper functions `with_option()`, `with_silent()`, `with_verbose()`,
  `with_speed()` and `without_checking()` have been exported. They can be used
  to evaluate an expression with temporary eplusr options (#240).
* Now `Idf$insert()` can directly take an `Idf` object or a list of `Idf`
  objects as input. And also `Version` objects in input will be directly
  skipped instead of giving an error (#245).
* A new option `all` has been added in `IdfObject$print()` with default being
  `FALSE`. If `TRUE`, all fields defined in [Idd] are printed even they do not
  exist in current object (#247).
* New S3 methods of `==` for all classes are added, i.e. `==.Idf`,
  `==.IdfObject`, `==.Idd`, `==.IddObject`, `==.Epw`, `==.EplusJob`,
  `==.EplusSql`, `==.EplusGroupJob`, `==.ParametricJob`. The negate methods are
  also added. This makes it quite easy to check the equality of R6 objects of
  these types (#250).
* A new Generic function `reload()` is added. eplusr relies heavily on the
  `data.table` package. The core data of all main
  classes in eplusr are saved as `data.table`s. This introduces
  a problem when loading saved `Idf` objects or other class objects via an
  `*.RDS` and `*.RData` file on disk: the stored `data.table`s lose
  their column over-allocation. `reload()` is a helper function that calls
  `data.table::setDT()` on all internal `data.table`s to make
  sure they are initialized properly. It is recommended to call `reload()` on
  each `Idd`, `Idf` and other class object in eplusr loaded with `readRDS()` or
  `load()`, to make sure all eplusr's functionaries works properly (#251).
* The implementation of `EplusSql$report_data()` has been refactored, resulting
  in a ~200% speed-up (#259).
* Now `day_type` in `EplusSql$report_data()` has a few new options (#259):
  - `"Weekday"`: All working days, i.e. from Monday to Friday
  - `"Weekend"`: Saturday and Sunday
  - `"DesignDay"`: Equivalent to `"SummerDesignDay"` plus `"WinterDesignDay"`
  - `"CustomDay"`: CustomDay1 and CustomDay2
  - `"SpecialDay"`: Equivalent to `"DesignDay"` plus `"CustomDay"`
  - `"NormalDay"`: Equivalent to `"Weekday"` and `"Weekend"` plus `"Holiday"`
* Some internal functions have been exported. They are mainly useful for
  developers to handle internal IDD and IDF data more efficiently (#260).
* A new `IdfScheduleCompact` class is introduced. A constructor function
  `schedule_compact()` is added. `IdfScheduleCompact` class provides more
  detailed methods to add, modify and extract schedule values. For more details,
  see `vignette("schedule")` (#256).
* New `IdfGeometry` and `IdfViewer` classes are introduced. `IdfGeometry` is
  designed to extract data for all geometry objects and perform geometric
  operations on them, while `IdfViewer` is to view IDF geoemtry in 3D using the
  rgl package in a similar way as OpenStudio SketchUp Plugin. `Idf$geometry()`
  and `Idf$view()` methods are added to directly create an `IdfGeometry` and
  `IdfViewer` object based on current `Idf` object, respectively (#296).
* A `plot.Idf` method is added which is basically a wrapper of `Idf$view()`
  (#296).
* Now eplusr can utilize the CSV output for report data extraction. Benefiting
  from the fantastic `data.table::fread`, this approach can be as 3~10X faster
  compared to the SQLite approach. eplusr will still use the SQLite if the CSV
  output is not available.

## Major changes

* The algorithm of object/field/value relation extraction has been completed
  refactored. Now it can correctly detect object recursive-reference and it's
  faster (#222, #223).

  All relation-related methods now have an unified interface:

  ```r
  X$method(which, direction, object = NULL, class = NULL, group = NULL, depth = NULL, keep = FALSE)
  ```

  Where `which` is a class index or object ID, `direction` is the target
  relation direction to extract. All results can be further constrained via
  3 extra arguments, i.e. `object`, `class` and `object`. `object` only
  applicable to `Idf` and `IdfObject`. The `depth` argument is used to control
  the depth for searching recursive relations. Default value is `0L`, which
  means no recursive relations will be detected, while `NULL` means to search
  all possible recursive relations.

  A new `keep` parameter with default value `FALSE` has been added. If `TRUE`,
  all input fields will be returned, even they may not have any relations. This
  is the default behavior of v0.12.0 and before. In this version, only fields
  that have relation with other objects will be returned.

  With this update, it is possible, for example, to directly know the structure
  of an air loop by using `idf$object_relation("AnAirLoop", depth = NULL)`

  Moreover, a new argument `class_ref` can be specified in methods of
  value-relation extraction. It can be used to specify how to handle
  class-name-references. Class name references refer to references in like
  field `Component 1 Object Type` in `Branch` objects. Their value refers to
  other many class names of objects, instaed of referring to specific field
  values. There are 3 options in total, i.e. `"none"`, `"both"` and `"all"`,
  with `"both"` being the default.

  * `"none"`: just ignore class-name-references. It is a reasonable
    option, as for most cases, class-name-references always come along with
    field value references. Ignoring class-name-references will not impact the
    most part of the relation structure.
  * `"both"`: only include class-name-references if this object
    also reference field values of the same one. For example, if the value of
    field `Component 1 Object Type` is `Coil:Heating:Water`, only the object
    that is referenced in the next field `Component 1 Name` is treated as
    referenced by `Component 1 Object Type`. This is the default option.
  * `"all"`: include all class-name-references. For example, if the
    value of field `Component 1 Object Type` is `Coil:Heating:Water`, all
    objects in `Coil:Heating:Water` will be treated as referenced by that
    field. This is the most aggressive option.
* `read_epw()` will proceed parsing for non-standard EPW header format (#236).
* Now `EplusSql$report_data()` will set the year values of day type
  `SummerDesignDay` and `WinterDesignDay` to current year and the `day_type`
  value will be left unchanged (#258).
* Now `read_idf()` will always make sure all necessary fields are added during
  parsing (#267).
* `[[.IdfObject` now only accept standard field names. No underscore-style
  names are allowed.
* The suffix of automatcially created names in `Idf$dup()` has been changed
  from `_X` to ` X`.
* The `warning` parameter in `read_epw()`, `Epw$add()` and `Epw$set()` has been
  deprecated (#298).

## Minor changes

* `EplusJob`, `EplusGroupJob` and `ParametricJob` will not parse input EPW
  files, but only validate their existences and store the paths (#215)
* `period` parameter in  `EplusSql$report_data()` now works as expected (#259).
* `run_idf()` and `run_multi()` now return additional element/column called
* `version` which contain the versions of EnergyPlus that are called during
  simulations
* `format.Idd()` now returns a single line string in format
  `<EnergyPlus IDD v[Version] (Build) with X classes`.
* The column `datasource` returned in `Epw$data()` has been renamed to
  `data_source`.

## Bug fixes

* Fix the bug caused by `ExpandObjects` executable that causes `run_idf` fails
  when running in parallel (#130)
* `Idf$insert()` now will remove all duplicated objects in input (#219).
* Fix the bug in `install_eplus()` on Windows (#230)
* Fix the error in `$<-.Idf` when input list of `IdfObject`s are all from the
  same `Idf` on the LHS (#238).
* Now `Idf$insert()` and `Idf$load()` can now successfully remove duplicated
  objects by comparing field values case-insensitively (#243)
* Now `Epw$save()` can work with empty `DESIGN CONDITIONS`, `TYPICAL/EXTREME
  PERIODS` and `GROUND TEMPERATURES` headers. Thanks @lukas-rokka for the
  bug report (#263).
* Fix output directory creation error in `EplusGroupJob`(#270).
* Fix IDF header option parsing (#278).
* Trailing spaces after class names in IDF can be handled correctly (#294).

# eplusr 0.12.0

## New features

* Now `group_job()` supports single IDF input with multiple EPW inputs (#185).
* A new method `Idf$last_job()` has been added to enable getting the last
  simulation job created using `Idf$run()` (#187).
* Provide a workaround to fix the issue of EnergyPlus v9.1 and above
  installation which fails to extract files into correct directory
  `/usr/local/EnergyPlus-X-Y-0`, but instead extracting all files directly into
  `/usr/local` ([NREL/EnergyPlus#7256](https://github.com/NREL/EnergyPlus/issues/7256))
  (#193).
* A new parameter `group_ext` has been added in `Idf$to_table()` and
  `IdfObject$to_table()`, with default value being `"none"`.
  If `"group"`, values from extensible fields will be grouped by the
  extensible group they belong to. For example, coordinate
  values of each vertex in class `BuildingSurface:Detailed` will
  be put into a list. If `"index"`, values from extensible fields
  will be grouped by the extensible field indices they belong to.
  For example, coordinate values of all x coordinates will be
  put into a list (#74).

## Bug fixes

* The algorithm of detecting numeric columns in `EplusSql$tabular_data()` has
  been improved (#190).
* Now `EplusSql$tabular_data()` keeps the original column order when `wide` is
  `TRUE` (#186).
* Fix EnergyPlus installation on macOS (#193).
* Fix parallel simulations on macOS (#194).
* Now `eplus_config()` will always return the expanded EnergyPlus path (#196).
* Now `group_job()` will return more informative error messages when input
  contains `Idf` objects that havn't been saved (#204).
* Fix error in `EplusGroupJob$run()` when custom `dir` is specified (#205).

# eplusr 0.11.0

## New features

* A new method `$models()` has been added in `ParametricJob` class. It returns a
  list of all parametric models generated after a measure has been applied. If
  no measure is applied, `NULL` is returned (#59). Thanks @yidan1214 for this
  feature request.
* A new method `$save()` has been added in `ParametricJob` class. It saves all
  generated parametric models and weather file into specified directory. You can
  use `separate` argument to determine whether each model is to be saved in a
  separate folder (#58). Thanks @yidan1214 for this feature request.
* New arguments `align` and `all` have been added to `$to_table()` method in
  `Idf` class. Setting `align` to `TRUE` will make sure that all returned object
  data per class have the same field number. The number of fields is the same as
  the object that have the most fields among those you specified. Setting `all`
  to `TRUE` will return all available fields in that class definition in IDD.
* Now the `weather` argument in `$run()` method in `Idf` class can be set to
  `NULL`. If so, design-day-only simulation is performed. Note that this needs
  at least one `Sizing:DesignDay` object exists in the `Idf` object (#80).
* Similar as above, the `epw` argument in `eplus_job()` and `param_job()` can
  also be `NULL` to force a design-day-only simulation.
* Now `$status()` in `ParametricJob` class includes a new member named
  `job_status` which is data.table containing detailed information on each
  simulation job (#70).
* Now `$print()` in `ParametricJob` will give you more informative details on
  each simulation job status, especially when `wait` is set to `FALSE` in
  `$run()`.
* A new column `index` is added in the returned `RddFile` and `MddFile`. It
  contains index of each variable.
* Two new methods `$read_rdd()` and `$read_mdd()` have been added in `EplusJob`
  class. `$read_rdd()` and `$read_mdd()` which parse the simulation RDD and MDD
  file (#84).
* Two new function `rdd_to_load()` and `mdd_to_load()` have been added, which
  format `RddFile` and `MddFile` into a data.table in acceptable format for
  `$load()` method in `Idf` class.
* Similar as `Output:SQLite`, when `$run()` in `Idf` object is called, an object
  in class `Output:VariableDictionary` is automatically created with `Key Field`
  being `IDF` (#85).
* A new argument `echo` has been added in `$run()` in `Idf`, `EplusJob` and
  `ParametricJob` class. It is only applicable when `wait` is `TRUE`. If `FALSE`,
  the simulation will be run silently without echoing any message from
  EnergyPlus.
* A new function `transition()` has been added. Basically it is an
  R implementation of IDFVersionUpdater. Currently the lowest version of IDF is
  v7.2. It should be much faster than IDFVersionUpdater.
* Now the way of find IDD file has been changed to take advantage of IDD files
  distributed along with IDFVersionUpdater. This update makes it possible to
  directly read IDF of most versions without downloading corresponding IDD.
* A new option `autocomplete` with default value being `interactive()` has been
  added. It is used to control whether to turn on autocompletion on class and
  field names. Underneath, `makeActiveBinding()` is used to add or move active
  bindings in `Idf` and `IdfObject`s to directly return objects in class or
  field values. This will make it possible to dynamically show current class
  and field names in both RStudio and in the terminal. However, this process
  does come in with a penalty on the performance. It can make adding or
  modifying large mounts of [Idf] and [IdfObject]s extremely slow. Default value
  make sure autocompletion works in interactive mode.
* A new syntax `class := list(field = value)` in `$set()` has been added. Note
  the use of `:=` instead of `=`. The main difference is that, unlike `=`, the
  left hand side of `:=` should be a valid class name in current `Idf` object.
  It will set the field of all objects in specified class to specified value.
* A new function `dt_to_load()` has been added, which formats a data.table from
  `Idf$to_table()` and `IdfObject$to_table()` with `wide` being to `TRUE` into a
  data.table in acceptable format for `Idf$load()` method and `Idf$update()`
  method.
* Now `read_epw()` support EPW files with non-integer timezones fail to load
  (#113). `$location()` in `Epw` class also support setting the timezone to
  non-integer one.
* A new method `$parent()` has been added in `IddObject` and `IdfObject` class
  to get parent `Idd` and `Idf` object, respectively (#76).
* Simulation status will be updated in the progress message in `run_multi()` and
  `$run()` method in `ParametricJob` class, instead of only showing `COMPLETED`
  (#124, #125).
* `EplusJob` class now will parse and store input IDF and EPW. It will also add
  `Output:SQLite` and `Output:VariableDictionary` object if necessary. This
  change makes sure using `EplusJob` can always have the ability to extract
  simulation results instead of totally relying on the input IDF (#118).
* As a result of previous change, the `$run()` method in `EplusJob` now takes
  the same parameters as `$run()` method in `Idf`, i.e. you can also change the
  EPW file to use and output directory using `epw` and `dir` argument (#118).
* A new class `EplusGroupJob` is added. It can be created using
  `group_job()`. `EplusGroupJob` provides a wrapper of `run_multi()` to group
  multiple EnergyPlus simulations together for running and collecting different
  EnergyPlus outputs (#117).
* A new parameter named `.empty` has been added in `$set()`, `$insert()`,
  `$load()`, `$update()`, `$paste()` methods in `Idf` class and `$set()` method
  in `IdfObject` class (#133).
* EnergyPlus v9.2 support has been added (#138).
* Now required objects and unique objects can be deleted in `$del()` when
  `.force` is set to `TRUE` (#149).
* Now for Windows and Linux, `install_eplus()` supports to install EnergyPlus to
  your home directory or your customized directory without requiring
  administrator privileges (#167).
  ```r
  # install to your home directory
  install_eplus(8.8, local = TRUE)
  # install to custom directory
  install_eplus(8.8, local = TRUE, dir = "~/MyPrograms")
  ```
  Please see `?install_eplus` for details.
* All documentation in R6 classes have been update thanks to roxygen2 R6 support (#156).
* Deprecated methods in each class have all been remove (#156).
* New parameter `case` has been added in
  `EplusSql$tabular_data()`. Similar like `case` parameter in
  `EplusSql$report_data()`, it lets you to add a `case` column to indicate the
  case of simulation. This brings some changes in the returned results of
  `EplusSql$tabular_data()`. Compared to previous version, there will always be
  a `case` column, unless `case` parameter is set to `NULL` (#182).
* New parameter `wide` and `string_value` have been added in
  `EplusSql$tabular_data()` and `EplusGroupJob$tabular_data()`. When `wide` is
  `TRUE`, each table will be converted into the similar format as it is shown in
  EnergyPlus HTML output file. And when `string_value` is `FALSE`, values in
  possible numeric columns are converted into numbers (#182).

## Bug fixes

* Fix the error in `$set()` when `$add_unit()` is performed in Epw class (#56).
* Now IDF file located in a folder whose name contains spaces can be
  successfully simulated (#60). Thanks @yidan1214 for reporting this bug.
* Now `$ref_to_object()` and `$ref_by_object()` can give correct results when
  `class` argument is specified.
* Now IDD version lower than 8.3 can successfully be downloaded and parsed.
* Now `ErrFile` objects returned from `$errors()` in `ParametricJob` can be
  successfully printed.
* Now one-line empty objects, e.g. `"Output:Surfaces:List,,;\n"`, can be
  successfully parsed (#88).
* Fix the error of `$to_table()` when no arguments were provided (#103).
* The references of newly added extensible fields are now correctly handled
  (#109).
* `$update()` now works for class `Schedule:Compact` with type checking on
  (#111).
* Fix the error in `$ref_to_node()` when no objects in specified class is found
  (#110).
* Now `leading` and `sep_at` argument work as expected in `Idf$to_string()` (#160).
* Now `Idf$to_table()` matches object names case-insensitively (#157).
* One-line-formatted `Version` object with trailing comments can be successfully
  parsed, e.g. `Version, 8.6; !- ABC` (#170).
* Better support for old IDD (#183).

## Minor changes

* Describe on how the arguments are matched in `$apply_measure()` (#57). Thanks
  @yidan1214 for pointing this out.
* Now the `echo` argument in `run_idf()` and `run_multi()` will only take effect
  when `wait` is `TRUE`. This makes it possible to remove the dependency on the
  later package.
* All messages can be suppressed by setting the `verbose_info` option to
  `FALSE`.
* `$delete()` method in `Epw` class has been deprecatd in flavor of `$del()` to
  provide a similar API as `Idf` class.
* When `all` is `TRUE` in `$report_data()` in `EplusSql`, `EplusJob` and
  `ParametricJob` class, an extra column `environment_period_index` is returned
  which contains the indices of each run period environment. This helps to
  distinguish different run period environment when no name is assigned.
* `param_job()` now gives more informative error message if input `Idf` object
  and `Epw` object is not created from a local file (#112).
* `param_job()` now preserve parametric model names from argument `.names` in
  `$apply_measure()` instead of calling `make.name()` to convert them into valid
  R names (#115).
* `$save()` works if weather was not given during initialization for
  `ParametricJob` (#120).
* Required fields in `IdfObject` are now marked with `*` when printing (#135).

# eplusr 0.10.4

This is a bug fix release which make sure eplusr is compatible with new version
of R6 [#164](https://github.com/hongyuanjia/eplusr/issues/164). No new feature
is included.

# eplusr 0.10.3

## New features

* Now you can get autocompletion of class names of `Idd` and `Idf` objects, and
  field names of `IdfObject` class in RStudio.
* `[[<-.Idf` and `$<-.Idf` now can work with unique-object classes as well.
* A new method `$update()` is added in `Idf` class which makes it possible to
  directly modify existing objects using character vectors and data.frames.
* A new argument `wide` is added in `$to_table()` method in `Idf` class,
  similar to `$to_table()` method in `IdfObject` class.
* New function `read_rdd()` and `read_mdd()` are added to help to directly parse
  `.rdd` and `.mdd` file.
* Now `$rename()` will not give an error if input name is the same as the old
  one.
* Now new method `$version()` is added in `EplusJob` and `ParametricJob` class,
  which returns the version of IDF it uses.
* A new argument `by_group` is added in `$class_index()`, `$class_name()` in
  `Idd` class and `$class_name()` in `Idf` class. If `TRUE`, a list is returned
  which separate class indexes/names by the group they belong to.
* Recursive relation support has been added to `$object_relation()` in `Idf`
  class, and `$value_relation()`, `$ref_to_object()`, `$ref_by_object()` in
  `IdfObject` class. All type of relations can be searched recursively by
  setting newly added argument `recursive` to `TRUE`. There is also a new
  argument called `depth` to control how many recursive searching steps should
  be performed.
* Add Node support into `$value_possible()` in `IdfObject` class, i.e. if
  specified fields are nodes, all nodes are returned in the `source` component.
* Now component node references support has been added. A new option `"node"`
  has been added to the `direction` argument in `$object_relation`,
  `$objects_in_relation()` in `Idf` class and `$value_relation()` in `IdfObject`
  class. New methods `$ref_to_node()` and `$has_ref_node()` have been added in
  `IdfObject` class. Now `$has_ref()` in `IdfObject` class will return `FALSE`
  only when all `$has_ref_to()`, `$has_ref_by()` and `$has_ref_node()` are all
  `FALSE`.

## Bug fixes

* Now all internal data, specifically all `data.table`s are correctly copied
  when calling `$clone()` in `Idf` and `Epw` class.
* `$add()` method and other methods that modify field values can correctly
  convert field values to numbers if necessary.
* Fix the error that holiday header got overwritten (#43).
* Fix EPW date parsing error (#42).
* Fix warnings in parsing `.err` file when there is no warning nor error.
* Fix `$errors()` error in `ParametricJob` class.
* The year of returned date time from `$data()`, `$abnormal_data()` and
  `$redundant_data()` now have correct values (#44).
* Reset year after checking datetime in `read_epw()` (#44).
* Add field name in input before validation in `paste()` (#45).
* Fix datetime calculation in `$report_data()` in `EplusSql` (#46).
* Update doc on EnergyPlus time notation (#47).
* Fix EPW design condition parsing error when `hours_8_to_4_12.8_20.6` is zero.
* Now recurring errors in `.err` file are parsed correctly in `ErrFile`.
* Handle large value references in `get_value_reference_map()` (#50).
* Fix the error when extract numeric default values using `$value_possible()`
  (#51).
* Fix the error that `.ref_to` argument in `$del()` in `Idf` class did not take
  effect.
* Fix field deletion in `$update()` in `Idf` class.
* Fix reference parsing error with mixed source types.
* External files used in `Schedule:File` class are now only copied once when
  `copy_external` is `TRUE` in `$save()` in `Idf` class.
* When `.ref_to` or `.ref_by` is `TRUE` in `$del()` in `Idf` class, objects
  whose class names are referenced by input will not be deleted, except all
  objects in referenced class are detected by field value reference or there is
  only one object in referenced class.
* Objects detected using `.ref_to` and `.ref_by` in `$del()` in `Idf` class can
  now be successfully deleted, even if they still have relations with other
  objects.
* Now invalid input object name is kept as it is, instead of converted to lower
  case.
* Now `$value_possible()` returns correct source values when fields refer to
  class names rather than field values.
* Now `$del()` method in `Idf` class works correctly with multiple inputs.
* Now trailing comments in IDF will be removed in `read_idf()`.
* Now `$to_table()` in `Idf` class will keep the input object order.
* Now `$set()` method in `IdfObject` can successfully delete fields when field
  values are set to `NULL`.
* Now `$run()` method in `Idf` will use the correct model path to run
  simulation.

## Minor changes

* Change message types in `$ref_to_object()` and `$ref_by_object()` in `IdfObject`.
* Now `ErrFile` is stored as a `data.table` with additional attributes instead
  of a list.
* Now when argument `.unqiue` is `TRUE` in `$insert()`, `$load()` and `$paste()`
  in `Idf` class, object comparison are performed case-insensitively.
* A new default value `"."` of `dir` in `download_idd()` is add.

# eplusr 0.10.2

This is a patch update mainly to fix CRAN check errors on a strict Latin locale,
with a bug fix.

## Bug fixes

* `$report_data()` method in `EplusSql` class now correctly returns data if
  even if `key_value` is not given (#37)

# eplusr 0.10.1

This is a patch update mainly to fix CRAN check errors, with few bug fixes.

## Bug fixes

* `$set()` method in `Idf` class now does not try to convert all input strings
  into numbers (#32)
* `$del()` method in `Idf` class now also respects validation settings.
  You should be able to delete any object if current validation level does not
  include reference checking
* `$del()` method in `Idf` class now will only print object relation only if
  `.ref_to` or `.ref_by` is `TRUE`
* Now `$set()` method in `Epw` class will correctly reset leap year flag in the
  header (#32)
* Now `$save()` method in `Epw` class returns the path of saved file invisibly

# eplusr 0.10.0

eplusr v0.10.0 introduces dozens of improvements, unfortunately also has some
break changes. Detailed updates per class are as follow:

## `Idd` Class

* `$object()` now takes a single class name and returns an `IddObject`.

* New method `$objects()` is added as the successor of `$object()` in previous
  versions.

* New method `$object_relation()` and `$objects_in_relation()` are added to get
  all objects that have relation (one refer to others or one referred by others).

* `$object_in_group()` has been deprecated in flavor of `$objects_in_group()`.

* New method `$to_table()` and `$to_string()` are added to help easily extract
  internal IDD data in `data.table` format and string format respectively.

* `$clone()` method is now deprecated.

* S3 method `str.Idd()`, `format.Idd()` are added.

## `IddObject` Class

* A constructor `idd_object()` has been added to help directly create
  `IddObject`. It takes a parent IDD version or an `Idd` object and a valid
  class name or class index, e.g. `idd_object(8.8, "Material")`,
  `idd_object(use_idd(8.8), "Construction")`

* New method `$version()` is added to extract the version of parent IDD.

* New method `$field_relation()` is added to extract the field relation in
  current class with other class fields.

* New method `$has_ref()`, `$has_ref_to()`, and `$has_ref_by()` are added to
  help to detect field relations

* New method `$is_real_field()` is added to detect if specified fields should
  have values of real numbers but not integers.

* New method `$has_ref()`, `$has_ref_by()`, `$has_ref_to()` are added to detect
  if specified fields refer to other fields or can be referred by other field in
  other classes.

* New method `$to_table()` and `$to_string()` are added to help easily extract
  internal IDD data in `data.table` format and string format respectively.

* `field_possible()` now does not need `IdfObject` data to run.

* Argument `lower` in `$field_name()` has been deprecated.

* `$field_reference()` in `IddObject` class has been deprecated in flavor of
  `$field_relation()`.

* A `brief` argument is added in `$print()` with default value of `FALSE`. If
  `TRUE`, only the class name of the `IddObject` is printed.

* `$clone()` method is now deprecated.

* S3 method `format.IddObject()`, `as.character.IddObject()` and
  `str.IddObject()` are added which calls `$to_string()` and `$print()`.

## `Idf` Class

* An argument `sorted` is added in `$class_name()` and `$group_name()`to control
  whether to sort the returned value according to the occurrence in IDD or not.

* `$definition()` now only accepts one class name and returns a single
  `IddObject`, not a list of `IddObject`.

* `$object()` now takes a single object ID or name and returns an `IdfObject`.

* New method `$objects()` is added as the successor of `$object()` in previous
  versions.

* New method `$object_unique()` is added which returns an `IdfObject` in
  unique-object class, e.g. `SimulaSimulationContrl`. This makes it easy to
  directly extract and modify those objects, e.g.
  `model$object_unique("SimulationContrl")$set(...)`.

* `$object_in_class()` now is deprecated in flavor of `$objects_in_class()`.

* New method `$objects_in_group()` is added to get all objects in specified group.

* `$search_object()`, `$search_value()` and `$replace_value()` now can take same
  arguments as `base::grepl()` to further control the way it searches objects.

* `$replace_value()` now will perform validations when replacing value.

* Now all methods in `Idf` class that return `IdfObject`s will not convert
  object names into valid R names. The former conversion behavior is unnecessary
  and a little bit confusing.

* `$add_object()`, `$set_object()`, `$del_object()`, `$ins_object()` and
  `$del_object()` are now all deprecated. Please use `$add()`, `$set()`,
  `$del()` and `insert()` instead, which provide much cleaner syntax.

* New method `$rename()` is added which helps to modify only object names.

* New method `$paste()` is added which will parse contents that copied from IDF
  Editor using the `Copy Obj` button and directly insert them into current `Idf`.

* `$validate()` and `$is_valid()` now accepts an argument `level`. Also, a
  helper `custom_validate()` is added to customize what kind of validations to
  check.

* `$string()` is deprecated in flavor of `$to_string()` in `Idf` class.

* New method `$to_table()` is added which can extract object data into a
  `data.table`.

* The default value of `deep` argument in `$clone()` method is set to `TRUE`. As
  in almost all cases, a total cloned object is desired when calling `$clone()`.

* `plain` argument is deprecated in `$print()` in IDF class. Because the same
  thing can be achieved using `$to_string()` and `base::cat()`. New argument
  `zoom` and `order` are added, which give more control on how detailed should
  the model be printed.

* New method `$object_relation()` and `$objects_in_relation()` are added to
  extract all objects in relation.

## `IdfObject` Class

* New constructor `idf_object()` is added.

* New method `$version()` is added to get the underlying version of IDD.

* `$get_comment()`, `$get_value()` now have been deprecated in flavor of
  `$comment()` and `$value()`

* `$comment()` (former `$get_comment()`) now returns `NULL` if the object does
  not have any comment.

* `$set_value()` now has been deprecated in flavor of `$set()`

* `$possible_value()` now has been deprecated in flavor of `$value_possible()`.

* `$ref_from_object()` and `$has_ref_from()` now have been deprecated in flavor
  of `$ref_to_object()` and `$has_ref_to()`.

* `$has_ref_by()`, `$has_ref_to()` and `$has_ref()` now return a logical vector
  of the same length as field numbers.

* `$validate()` and `$is_valid()` now accepts an argument `level`. Also, a
  helper `custom_validate()` is added to customize what kind of validations to
  check.

* `$string()` and `$table()` are now deprecated in flavor of `$to_string()` and
  `$to_table()`.

* `$clone()` method is now deprecated.

* S3 methods `format.IdfObject()`, `as.character.IdfObject()` and
  `str.IdfObject()` are added, which calls `$to_string()` and `$value()`.

## `Epw` Class

* `Epw` class has been totally rewritten which provides much more
  functionalities. Please refer to package documentation for details.
  All headers are parsed and can be retrieve in `Epw` class.

* The default value of `deep` in `$clone()` has been change to `TRUE`.

## `EplusSql` Class

* New method `$path_idf()` is added to return the parent IDF file path.

* New arguments `period`, `month`, `day`, `hour`, `minute`, `interval`,
  `simulation_days`, `day_type` and `environment_name` are added in
  `$report_data()` which provide extra subsetting on the SQL file.

* New arguments `report_name`, `report_for`, `table_name`, `column_name` and
  `row_name` are added in `$tabular_data()` which provide extra subsetting on
  the SQL file.

* `$clone()` method is now deprecated.

## `EplusJob` Class

* New method `$path()` is added to return the path of IDF and/or EPW file of
  the job.

* New method `$list_table()`, `$read_table()` are added which provide the same
  functionalities as they are in `EplusSql` class.

* `$clone()` is now deprecated.

* New arguments `period`, `month`, `day`, `hour`, `minute`, `interval`,
  `simulation_days`, `day_type` and `environment_name` are added in
  `$report_data()` which provide extra subsetting on the SQL file.

* New arguments `report_name`, `report_for`, `table_name`, `column_name` and
  `row_name` are added in `$tabular_data()` which provide extra subsetting on
  the SQL file.

* `$clone()` method is now deprecated.

## `ParametricJob` Class

* New arguments `force` and `copy_external` are added in `$run()` which have the
  same effect as in `$run()` method in `Idf` class.

* New arguments `period`, `month`, `day`, `hour`, `minute`, `interval`,
  `simulation_days`, `day_type` and `environment_name` are added in
  `$report_data()` which provide extra subsetting on the SQL file.

* New arguments `report_name`, `report_for`, `table_name`, `column_name` and
  `row_name` are added in `$tabular_data()` which provide extra subsetting on
  the SQL file.

* `$clone()` method is now deprecated.

## Miscellaneous

* Function `read_err()` is added which takes a path of an `.err` file, parses it
  and returns an `ErrFile` object.

* Functions `is_eplus_ver()`, `is_idd_ver()`, `is_eplus_path()`, `is_idd()`,
  `is_idf()`, `is_iddobject()`, `is_idfobject()` and `is_epw()` are now exported.

* Function `custom_validate()` and `level_checks()` are added to customize
  validation.

* Right now, all returned object and field names will remain what they are as in
  the IDF, instead of converting them into `underscore` style names in the
  returned lists.

* The `num_digits` option has been deprecated as formatting of numeric fields
  are now handled by R itself.

* Error will be given instead of warning if there is no build tag or multiple
  build tags found in input IDD.

* `eplusr_option()` accepts custom validate level using newly-added function
  `custom_validate()`.

# eplusr 0.9.4

## New features

* Now you can directly download EnergyPlus Weather File (.epw) and Design Day
  File (.ddy) using new function `download_weather()`. It takes a regular
  expression as input, searches through EnergyPlus weather data base (stored
  in this package), download corresponding files and return the saved paths.
  Below are some examples:
  - Search locations that contains string `"los angeles"` and `"tmy"`, return a
    menu to select which one(s) to download. Once selected, download both
    corresponding weather file(s) and design day file(s):
    ```r
    download_weather("los angeles.*tmy", type = "all", ask = TRUE)
    ```
  - Same as above, expect that all files will be renamed to `la.epw(ddy)`,
    `la_1.epw(ddy)`, `la_2.epw(ddy)`:
    ```r
    download_weather("los angeles.*tmy", filename = "la", type = "all", ask = TRUE)
    ```
  - Search locations that contains string `"beijing"` and `"cswd"`. If no more
    than 3 results found, directly download all weather files and save them to
    temporary directory.
    ```r
    download_weather("beijing.*cswd", dir = tempdir(), type = "epw", ask = FALSE, max_match = 3)
    ```

## Minor changes

* Now `clean_wd()` is run before every call to EnergyPlus.

## Bug fixes

* `$clone()` method has been refactored for `Idf` and `Idd` class. This fix the
  issue that internal shared environments were not cloned in version 0.9.3.

* Fix the error that `$save()` and `$string()` in `Idf` class did not respect
  `format` argument.

* Fix the error that `$apply_measure()` in `ParametricJob` class did not
  respect `.names` argument.

# eplusr 0.9.3

## New features

* Add support for EnergyPlus v9.0.0.

## Bug fixes

* Remove duplicates when update value references (#20).

# eplusr 0.9.2

## Break changes

* Clean up the dirty code that manually modifies `$clone()` method in R6 in
  order to be compatible with (#19). After this, `deep` has to be set to `TRUE`
  if a completed cloned copy is desired. Also, documentations on `$clone()`
  method in `Epw`, `EplusJob`, `ParametricJob` have been added.

## Minor changes

* `clean_wd()` is called internally when running EnergyPlus models. This
  guarantees that the old output file from last simulation is cleaned up before
  the new simulation starts.

## New features

* A new class `EplusSql` has been added. This makes it possible to directly
  retrieve simulation results without creating an `EplusJob` object which can
  only get simulation outputs after the job was successfully run before. It can
  be easily created using `eplus_sql()` function.  However, it should be noted
  that, unlike `EplusJob`, there is no checking on whether the simulation is
  terminated or completed unsuccessfully, or the parent Idf has been changed
  since last simulation. This means that you may encounter some problems when
  retrieve data from an unsuccessful simulation. It is suggested to carefully go
  through the `.err` file to make sure the output data in the SQLite is correct
  and reliable. Currently, there are only few methods in `EplusSql` class which
  have some overlaps with theses in `EplusJob`, but more methods may be added in
  the future. For more details, please see the documentation of `EplusSql`.

* A new method `$is_double_field()` is added to `IddObject` class to check if
  specified fields should be double numbers

## Bug fixes

* Fix the error of missing Idf file when `dir` is not `NULL` in `$run()` in
  `Idf` class.

# eplusr 0.9.1

## Minor changes

* Long lines in err files now will be wrapped when printed.

## Bug fixes

* Fix the error when checking invalid extensible fields in value validation.

* Fix the error that value references did not get updated when setting values to
  `NA` in `$set_value()` in `IdfObject` and `$set_object()` in `Idf`.

# eplusr 0.9.0

## Break Changes

* `parallel_backend` argument in `run_multi()` has been removed, due to the
   reason that supporting remote parallel computing is out of the scope of this
   package. This makes it possible to remove both `future` and `furrr` package
   dependencies. The default behavior of `run_multi()` does not change if
   running on local machine, as it still runs multiple EnergyPlus instances in
   parallel.

* In `run_idf()` and `run_multi()`, `eplus` argument has been moved to be the
  last argument with default value setting to `NULL`. This will make them a
  little bit more convenient to run EnergyPlus without explicitly specify the
  version. If `NULL`, the version of EnergyPlus is automatically detected using
  the version field of input model. For example:
    ```r
    # before
    run_idf(8.8, model.idf, weather.epw)

    # after
    run_idf(model.idf, weather.epw)
    ```

* Both argument `echo` and `wait` have been added to `run_idf()` and
  `run_multi()`. Unlike the behavior in eplusr 0.8.3 when `echo` is `TRUE` in
  `run_idf()`, right now `echo` only control whether to show the output from
  EnergyPlus command line interface. Please use `wait` to control whether to wait
  until the simulation is complete or not.

## New features

* Package documentation has been heavily updated. Examples of every exported
  class and most functions have been added. Also, an example IDF file
  `"1ZoneUncontrolled.idf"` from EnergyPlus v8.8.0 is included in the package,
  which makes it possible to run most examples without installing EnergyPlus. Of
  cause, for examples in `EplusJob` and `ParametricJob` class, EnergyPlus
  installation is needed to run them successfully.

* The brilliant package [crayon](https://CRAN.R-project.org/package=crayon) is
  used to support colorful printing of `Idd`, `Idf`, `IddObject`, `IdfObject`,
  `Epw`, `EplusJob` and `ParametricJob` classes.

* A new type of `"character"` validation has been added, which will check if
  field values should be characters but are not.

* A new option `copy_external` has been added in `$run()` in `Idf` class. If
   `TRUE`, the external files will also be copied into the output directory.
   Currently, only `Schedule:File` class is supported. This ensures that the
   output directory will have all files needed for the model to run.

* `$validate()` in `Idf` and `IdfObject` class will also check incomplete
   extensible groups. Extensible groups that only contain any empty field are
   treated as invalid.

* New methods `$field_reference()` and `$field_possible()` have been added into
  `IddObject` class. `$field_possible()` is basically the same as the
  `$possible_value()` in `IdfObject` class. This makes it possible to show all
  available references for a class that does not have any object yet.
  `$field_reference()` only returns all available reference values for specified
  fields.  **NOTE**: `$field_reference()` and `$field_possible()` can only be
  used in `IddObject`s that are created using `$definition()` in `Idf` class and
  `IdfObject` class, and cannot be used in `IddObject`s that are created using
  `$object()` or equivalent in `Idd` class. This is because both methods need
  shared Idf value data to collect all reference values.

## Minor changes

* `use_eplus()` now returns an invisible list of EnergyPlus configure data
  instead of `NULL`.

* The default value of `dir` argument in `download_eplus()` has been removed,
  which enforce the user to explicitly specify the directory to save
  EnergyPlus installer file, as per CRAN reviewer comment.

* A new option `"auto"` for `download` argument in `use_idd()` has been added,
  which will automatically download corresponding EnergyPlus IDD file if the
  file or Idd object is currently not available.

* The `dir` argument in `install_eplus()` has been removed, as per CRAN reviewer
  comments. The EnergyPlus installer file will be saved into `tempdir()`.

* The default value of `dir` argument in `$run()` in `Idf` class has been
  removed, as per CRAN reviewer comments. Users can explicitly set `dir` to
  `NULL` if they want to use the directory of input IDF file.

* The default value of `echo` in `run_idf()` has been changed to `TRUE`, which
  will always show EnergyPlus simulation process to the console.

* Fix the error of `output_dir` argument checking in `rum_multi()`.

* Now an informative error message is given when there is no SQL output found
  when trying to read simulation output using `$report_data()`,
  `$report_data_dict` and `tabular_data()` in `EplusJob` class.

* `run_idf()` and `run_multi()` now does not call `clean_wd()` as this is
   automatically handled by EnergyPlus itself.

## Bug fixes

* Fix errors when try to get units `$field_name()`, `$field_unit()` and
  `$field_default()` in `IddObject` class.

* Fix the error that `$get_value()` and `$table()` in `IdfObject` class did not
  return all field values even `all` was set to `TRUE`.

* Fix the error that `$table()` in `IdfObject` class did not return field units
  even `unit` was set to `TRUE`.

* Fix the error that `$replace_value()` in `Idf` class did not update object
  names.

* Fix the error when `unit` is set to `TRUE` in `$get_data()` in `Epw` class.

* Fix the error that `$state_province` in `Epw` class always returns `NULL`.

* Fix the error of missing expanded IDF files which occurred randomly in
  `run_multi()`.

# eplusr 0.8.3

## New features

* A new method `$possible_value()` has been added into `IdfObject` class, which
  will return all possible values for selected fields, including auto-value
  (autosize or autocalculate), default, range, choices and references.

* New parameter `dir` has been added to `install_eplus()`, which makes it
  possible for keeping the downloaded EnergyPlus installation file.

* `$dup_object()` in `Idf` class now can duplicate one object multiple times.

## Minor changes

* The names of returned list of `$get_value()` in `IdfObject` is "underscore"
  name, not lower name. This makes its behavior being consistent with
  `$object()` in `Idf` class.

* When `wait` is `FALSE`, `$run()` in `EplusJob` and `ParametricJob` will
  return itself instead of the time when simulation started.

* A clear message will be given when trying to run `$kill()` in `ParametricJob`,
  which inform the user that currently `$kill()` does not for parametric
  simulations.

* A warning will be given if no configuration data found in `eplus_config()`.

* Now the names of returned list from `$search_object()` in `Idf` class will
  also be underscore-style object names, which makes its behavior being
  consistent with `$object()` and `$object_in_class()`. Also, if no results
  found, `$search_object()` now will return invisible `NULL` and give a message.

* `$dup_object()` will give an error if given `new_name` is the an existing
  object name.

## Bug fixes

* Fix errors in `$status()`, `$output_dir()` and `$locate_output()` in
  `ParametricJob` class when `which` arg is not given (#12).

* Fix idf input version parsing in `param_job` (#13).

* Fix EnergyPlus downloading and installing errors on Linux and macOS (#14, #17).

* Fix `run_idf()` and `run_multi()` errors on Linux and macOS (#14).

* Fix missing name attribute in class `NodeList` (#16).

* Fix errors in `use_eplus()` when input is an EnergyPlus installation path (#18).

# eplusr 0.8.2

## New feature

* `$get_value()` in `IdfObject` class new has a new argument `simplify`. If
  `TRUE`, a character vector will be returned instead of a named list. Default
  is `FALSE`.

## Minor changes

* The names of returned list of `$get_value()` in `IdfObject` is "underscore"
  name, not lower name. This makes its behavior being consistent with
  `$object()` in `Idf` class.

## Bug fixes

* Fix warning messages of column type coercion from data.table in `Idf` and
  `IdfObject`.

* `$set_value()` in `IdfObject` and `$set_object()` in `Idf` now will delete empty
   fields with empty value. This fix the error when trying to reassign only some
   empty fields which have been deleted before.

# eplusr 0.8.1

## Major changes

* Add package logo.
* Add Epw, EplusJob and ParametricJob class and methods.
* Add IdfObject and IddObject class and methods.
* Refactor Idf and Idd class methods.
