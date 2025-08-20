# SOILmilaR 0.1.0

* Deprecate `thresh` argument, replaced with `thresh_single` (maximum difference in one property) and `thresh_all` (sum of differences across all properties)

* Threshold logic now uses `<` instead of `<=` to be more consistent with many similar soils guides

* Add `design_mapunit()` function for iterative calls to `similar_soils()` to exhaustively group all observations (for example, within a map unit or other conceptual unit of interest)
