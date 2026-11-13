# clim4health 0.1.1

* `c4h_space()` now handles latitudes stored in either direction correctly
  (north-south and south-north), fixing a bug in the aggregation of gridded
  data over areas. 
* `c4h_downscale()` now reverses latitudes stored north-south so that they
  conform to the south-north ordering required by the `"Analogs"` method, 
  no longer assumes that observations are daily when several monthly lead times
  are loaded, and correctly validates the `bbox` argument.
* `c4h_collapse()` now returns `NA` (instead of `NaN`, `Inf` or `-Inf` with a
  warning) when all values are missing.
* `c4h_convert()` and `c4h_collapse()` have been improved with internal
  refactoring and more strict internal checks.
* `c4h_plotskill()`: legend labels for categorical skill scores are now
  shown as intervals (e.g. "0 - 0.2"), and boxes no longer appear around the
  significance markers when saving to PDF.
* The documentation of many of the functions has been improved.
* The example station, reanalysis and hindcast data now include 2013,
  examples and vignettes have been updated accordingly. 
* Vignettes have been revised and the figures are now correctly displayed.


# clim4health 0.1.0

* Initial CRAN submission.
