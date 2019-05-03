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
