* Object reference:
  - Classes like `Branch` and `ZoneHVAC:EquipmentList` reference other objects
    in fields "Component X Object Type" and "Zone Equipment 1 Object Type", but
    they do not have `\object-list` attributes. This makes it impossible to
    detect invalid field references.
  - One possible way is to use `$search_value()` to detect if the values in
    those fields could be targeted or not.

* Model version conversion

* Remove unnecessary `as.integer()`

* Fix extensible field names when they should be "A123" or "N123" but not
  self-explainary

* Using `units` package to add units in the returned data.table

* Apply multiple weather file to a seed in `ParametricJob` class

* Add more tests on value reference updating during value modifications.

* Only use minimal columns in data modifications to speed up the whole process

* Change the column names of returned data.tables to lower case style in
  `$report_data_dict()` and `$report_data()`.

* Optimize table joins using `data.table::setindex()`, `mult` argument in
  `data.table::data.table()`

* Change `clean_wd()` to make it work properly in `run_multi()`.

* Use `data.table::fread(sep = NULL)` instead of `readr::read_lines()`

* Add functionality to parse `.rdd` and `.mdd` file and add possible
  output variables into the Idf, possibly also a gui interface providing
  autocompletion using `DT::datatable()` and `parse_rdd_file()`.

* Add functionality that can set objects using input data.frame instead of
  nested list.

* Add external file dependency (e.g. file existance in `Schedule:File` class)
  checking in `$validate()`.
