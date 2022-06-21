# ggshakeR 0.2.0

* Standardize function argument and object names
  * snake_case: `binSize` to `bin_size`, `dataType` to `data_type`, etc.
  * data arguments all conform to `data` 
  * US spelling: `color` rather than `colour`, etc.
  * Argument names made longer for clarity: `subt_size` to `subtitle_size`, `roll_avg` to `rolling_average`, etc.
  * See [issue #27](https://github.com/abhiamishra/ggshakeR/issues/27;)

# ggshakeR 0.1.2

* Remove extraneous dependencies from package imports
  * See [issue #20](https://github.com/abhiamishra/ggshakeR/issues/20)

# ggshakeR 0.1.1

* Implement Github Actions CI tools:
  * codecov test coverage checks
  * R Package Build checks
  * lintr checks
  * Build pkgdown website

# ggshakeR 0.1.0 

* Added `plot_timeline()` function
* Added vignettes: `Expected Treat`, `Pitch Plots`, `Pizza Plots`
* Created pkgdown website
* Added `plot_sonar()` function
* Updated tests
* Fixed `plot_pass()`, `plot_passflow()`, and `plot_heatmap()` functions to get plot positions on the proper side of the field
* Package review (Ryo N.). See [issue #9](https://github.com/abhiamishra/ggshakeR/issues/9)
* Various solutions to pass package checks. See [issue #12](https://github.com/abhiamishra/ggshakeR/issues/12)


# ggshakeR 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
