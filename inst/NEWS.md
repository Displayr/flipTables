Changes in Version 2.3.0 (2017-12-11)
--------------------------------------------------------

NEW FEATURES

* New functions `HideEmptyRowsAndColumns`,
  `GetNonEmptyRowsAndColumns`, and `GetNonEmptyElements` support
  deletion/detection of row and columns of a matrix or elements of a
  vector that contain all NA or 0-percentages (DS-1492)

Changes in Version 2.1.0 (2017-10-20)
--------------------------------------------------------

NEW FEATURES

* New functions `RemoveByName`, `RemoveRowsAndOrColumns`,
and `RetainedRowsAndOrColumns` for removing entries from a vector or 
rows or columns of a matrix/data.frame/table by name (DS-1471)

Changes in Version 2.0.0 (2017-10-18)
--------------------------------------------------------

NEW FEATURES

* `BasicTable` has been renamed to `TidyTabularData` (DS-1471)
* `AsBasicTable` has been renamed to `AsTidyTabularData` (DS-1471)

Changes in Version 1.0.1 (2017-10-11)
--------------------------------------------------------

NEW FEATURES

* Begin using semantic versioning
* The output of `AsBasicTable` and `BasicTable` is now always
either of class "matrix" or "numeric" and not additionally of class
`Basictable` (DS-1471)

BUG FIXES

* `BasicTable` will now correctly remove entries by name in the case
when the supplied data has been coerced to a vector and one or both
of the arguments "row.names.to.remove" or "col.names.to.remove"
are a comma or semi-colon separated string of names to remove (DS-1530)
