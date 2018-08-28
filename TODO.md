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
