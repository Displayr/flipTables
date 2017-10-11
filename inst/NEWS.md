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
