**Checklist**

Before creating the Pull-Request, I...

-   [ ] Ran `devtools::document()` and `Install and Restart`.
-   [ ] Ran `devtools::check()` (GHA will do this but doesn't hurt to check yourself).
-   [ ] (If applicable) Checked changes made to website with `pkgdown::build_site()`.
-   [ ] Wrote commit message(s) throughout the time spent working on this branch with clear details of changes made.
-   [ ] (If applicable) Created new tests for new functions or changes made to existing functions (Check `codecov`).

After all of the Github Actions Checks have finished, I...

-   [ ] Checked `lintr` issues by going to the latest commit or the `Checks` tab.
-   [ ] Used `Squash & Merge` option.
-   [ ] Wrote in **detail** of all the changes made in the various commits on this branch.
-   [ ] Added a `keyword` like `closes #56`, `fixes #123` at the end of the message.
-   [ ] Merged the Pull-Request.
-   [ ] Deleted the branch.
