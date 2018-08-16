# eplusr 0.8.4

## New features

* Package documentation has been heavily updated. Examples of every exported
  class and most functions have been added. Also, an example IDF file
  `"1ZoneUncontrolled.idf"` from EnergyPus v8.8.0 is included in the package,
  which makes it possible to run most examples without installing EnergyPlus. Of
  cause, for examples in `EplusJob` and `ParametricJob` class, EnergyPlus
  installation is needed to run them successfully.

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
  when tring to read simulation output using `$report_data()`,
  `$report_data_dict` and `tabular_data()` in `EplusJob` class.

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
  `TRUE`, a character vecotr will be returned instead of a named list. Default
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
