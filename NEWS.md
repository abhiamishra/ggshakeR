# ggshakeR 0.2.0.9002

- Bug Fixes
  - Fixes bugs in the `plot_pizza` in the custom option of the comparison plot [issue #123](https://github.com/abhiamishra/ggshakeR/issues/123). 

# ggshakeR 0.2.0.9001

- Bug Fixes
	- Fixed `plot_pizza` bugs caused by FBRef updates [issue #121](https://github.com/abhiamishra/ggshakeR/issues/121)
		- Changed stat selection to named stats instead of selecting row numbers, which change because of FBRef data changes.
		- Added method to de-duplicate some stats that were returned for multiple positions, causing errors.
		- Changed season default argument to "Last 365 Days Men's Big 5 Leagues, UCL, UEL" to match FBRef data for men's big 5 league players. (this makes the caption wider than before, so some have been split over two lines)
		- Replaced long `|`-statements with `%in%`.
		- Deleted some old unused code.

# ggshakeR 0.2.0.9000

-   Bug Fixes
    - Fixed `plot_heatmap` verification assessment [issue #101](https://github.com/abhiamishra/ggshakeR/issues/101)
    - Fixed pkgdown build issues [issue #104](https://github.com/abhiamishra/ggshakeR/issues/104)
    - Fixed `plot_passnet` direction orientation [issue #108](https://github.com/abhiamishra/ggshakeR/issues/108)
    - Fixed `plot_heatmap` and `plot_passflow`direction orientation + `plot_pass`opta functionality was fixed.[issue #111](https://github.com/abhiamishra/ggshakeR/issues/111)
    - Added "Created using ggshakeR" to `plot_heatmap`, `plot_passflow`, and `plot_pass`[issue #110](https://github.com/abhiamishra/ggshakeR/issues/110)
    - Added "Created using ggshakeR" to the rest of the functions [issue #110](https://github.com/abhiamishra/ggshakeR/issues/110)
    - Converted `plot_pizza` to work with new FBRef data from Opta. [issue #118](https://github.com/abhiamishra/ggshakeR/issues/118)

# ggshakeR 0.2.0

-   Standardized function argument and object names
    -   Use snake_case: `binSize` to `bin_size`, `dataType` to `data_type`, etc.
    -   Data arguments all conform to `data`
    -   Use US spelling: `color` rather than `colour`, etc.
    -   Argument names made longer for clarity: `subt_size` to `subtitle_size`, `roll_avg` to `rolling_average`, etc.
    -   See [issue #27](https://github.com/abhiamishra/ggshakeR/issues/27)
-   Created new functions
    -   `plot_convexhull()` & `hull_fun()` utility function: See [issue #28](https://github.com/abhiamishra/ggshakeR/issues/28)
    -   `plot_voronoi()` function: See [issue #28](https://github.com/abhiamishra/ggshakeR/issues/28)
    -   `plot_passnet()` function: See [issue #31](https://github.com/abhiamishra/ggshakeR/issues/31), [issue #53](https://github.com/abhiamishra/ggshakeR/issues/53)
    -   `calculate_epv()` function: See [issue #25](https://github.com/abhiamishra/ggshakeR/issues/25)
    -   Created vignettes for new functions: See [issue #45](https://github.com/abhiamishra/ggshakeR/issues/45)
-   Improved functions
    -   Improved `plot_pizza()`: See [issue #37](https://github.com/abhiamishra/ggshakeR/issues/37)
    -   Added `jdp` pitch type option for `plot_heatmap()`: See [issue #37](https://github.com/abhiamishra/ggshakeR/issues/37)
    -   Simplified `plot_pass()`: See [issue #64](https://github.com/abhiamishra/ggshakeR/issues/64)
-   Improved test coverage
    -   See [issue #44](https://github.com/abhiamishra/ggshakeR/issues/44)
-   Other minor fixes
    -   See [issue #21](https://github.com/abhiamishra/ggshakeR/issues/21), [issue #29](https://github.com/abhiamishra/ggshakeR/issues/29), [issue #32](https://github.com/abhiamishra/ggshakeR/issues/32), [issue #33](https://github.com/abhiamishra/ggshakeR/issues/33), [issue #47](https://github.com/abhiamishra/ggshakeR/issues/47), [issue #56](https://github.com/abhiamishra/ggshakeR/issues/56), [issue #58](https://github.com/abhiamishra/ggshakeR/issues/58), [issue #59](https://github.com/abhiamishra/ggshakeR/issues/59), [issue #62](https://github.com/abhiamishra/ggshakeR/issues/62), [issue #66](https://github.com/abhiamishra/ggshakeR/issues/66), [issue #76](https://github.com/abhiamishra/ggshakeR/issues/76), [issue #78](https://github.com/abhiamishra/ggshakeR/issues/78), [issue #81](https://github.com/abhiamishra/ggshakeR/issues/81), [issue #82](https://github.com/abhiamishra/ggshakeR/issues/82), [issue #88](https://github.com/abhiamishra/ggshakeR/issues/88), [issue #101](https://github.com/abhiamishra/ggshakeR/issues/101)

# ggshakeR 0.1.2

-   Removed extraneous dependencies from package imports
    -   See [issue #20](https://github.com/abhiamishra/ggshakeR/issues/20)

# ggshakeR 0.1.1

-   Implemented Github Actions CI tools:
    -   codecov test coverage checks
    -   R Package Build checks
    -   lintr checks
    -   Build pkgdown website

# ggshakeR 0.1.0

-   Added `plot_timeline()` function
-   Added vignettes: `Expected Treat`, `Pitch Plots`, `Pizza Plots`
-   Created pkgdown website
-   Added `plot_sonar()` function
-   Updated tests
-   Fixed `plot_pass()`, `plot_passflow()`, and `plot_heatmap()` functions to get plot positions on the proper side of the field
-   Package review (Ryo N.). See [issue #9](https://github.com/abhiamishra/ggshakeR/issues/9)
-   Various solutions to pass package checks. See [issue #12](https://github.com/abhiamishra/ggshakeR/issues/12)

# ggshakeR 0.0.0.9000

-   Added a `NEWS.md` file to track changes to the package.
