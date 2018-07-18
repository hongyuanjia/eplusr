# Object reference

Classes like `Branch` and `ZoneHVAC:EquipmentList` reference other objects in
fields "Component X Object Type" and "Zone Equipment 1 Object Type", but they do
not have `\object-list` attributes. This makes it impossible to detect invalid
field references.

One possible way is to use `$search_value()` to detect if the values in those
fields could be targeted or not.
