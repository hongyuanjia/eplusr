# eplusr 0.8.3

## New features

* A new method `$possible_value()` has been added into `IdfObject` class, which
  will return all possible values for selected fields, including auto-value
  (autosize or autocalculate), default, range, choices and references.

* New parameter `dir` has been added to `install_eplus()`, which makes it
  possible for keeping the downloaded EnergyPlus installation file.

## Minor changes

* The names of returned list of `$get_value()` in `IdfObject` is "underscore"
  name, not lower name. This makes its behavior being consistent with
  `$object()` in `Idf` class.

* When `wait` is `FALSE`, `$run()` in `Idf`, `EplusJob` and `ParametricJob` will
  return a `process` object instead of the time when simulation started.

* A clear message will be given when trying to run `$kill()` in `ParametricJob`,
  which inform the user that currently `$kill()` does not for parametric
  simulations.

* A warning will be given if no configuration data found in `eplus_config()`.

## Bug fixes

* Fix errors in `$status()`, `$output_dir()` and `$locate_output()` in
  `ParametricJob` class when `which` arg is not given (#12).

* Fix idf input version parsing in `param_job` (#13).

* Fix EnergyPlus downloading and installing errors on Linux and MacOS (#14, #17).

* Fix `run_idf()` and `run_multi()` errors on Linux and MacOS (#14).

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
