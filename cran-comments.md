## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* Some example and test code paths depend on packages in Suggests (FielDHub,
  rMVP, GAPIT, rrBLUP, qtl, BGLR, AlphaSimR, randomForest, xgboost, e1071,
  glmnet). These are guarded with requireNamespace() and wrapped in
  \dontrun{} / skip_if_not_installed(), so they do not run when the suggested
  package is unavailable.

## Test environments

* local: <your OS>, R <version>
* win-builder: devel and release (https://win-builder.r-project.org/)
* R-hub: windows-x86_64-devel, ubuntu-gcc-release, fedora-clang-devel
* macOS builder (https://mac.r-project.org/macbuilder/submit.html)

## Downstream dependencies

There are currently no downstream dependencies for this package.
