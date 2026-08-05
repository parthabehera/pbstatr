# Publishing PbStatR to CRAN

A step-by-step guide to preparing, checking, and submitting **PbStatR** to the
Comprehensive R Archive Network (CRAN). Work through the sections in order.

---

## 0. What you need first

* **R** (latest release) and **RStudio** (recommended).
* The developer toolchain packages:

  ```r
  install.packages(c("devtools", "roxygen2", "testthat", "rmarkdown",
                     "knitr", "spelling", "rhub", "pkgdown", "urlchecker"))
  ```

* On **Windows**: install **Rtools** (matching your R version) from
  <https://cran.r-project.org/bin/windows/Rtools/>.
* On **macOS**: install the Xcode command-line tools (`xcode-select --install`).
* A working **LaTeX** (for PDF manual) — the lightweight **tinytex** is easiest:

  ```r
  install.packages("tinytex"); tinytex::install_tinytex()
  ```

---

## 1. Fill in the package metadata

Open `DESCRIPTION` and replace the placeholders:

* **`Authors@R`** — put your real name, email, and (optionally) ORCID. The
  `cre` (maintainer) email must be one you actively monitor; CRAN sends the
  confirmation and all correspondence there.
* **`URL` / `BugReports`** — already set to your GitHub account (`parthabehera`); update if you move the repo
  (or remove these lines if you have no repository).
* **`Version`** — CRAN's first-submission convention is a `0.x.y` or `1.0.0`
  number. `0.9.0` is fine.
* **`Description`** — must be one paragraph, start with a capital letter, end
  with a full stop, and wrap software/package names in single quotes (already
  done). Do **not** start it with "This package" or the package name.

Also update these files with your name/repo:

* `README.md` — links already point to `parthabehera`; update the author line with your name.
* `_pkgdown.yml` — the `url:` field.
* `LICENSE`-related: the package uses `License: GPL-3`, a standard license, so
  **no `LICENSE` file is needed**. (If you switch to `MIT + file LICENSE` you
  must add a two-line `LICENSE` file with `YEAR` and `COPYRIGHT HOLDER`.)

---

## 2. Regenerate documentation and assets

From the package root:

```r
# 2a. Build all .Rd help files and the NAMESPACE from the roxygen comments
devtools::document()

# 2b. (optional) regenerate the example figures and logo from live output
#     — needs the plotting suggests installed
# Rscript data-raw/make_figures.R
# python3 data-raw/make_logo.py
```

`devtools::document()` is essential: it turns the `#'` comments in `R/*.R` into
the `man/*.Rd` files CRAN checks. Re-run it after **every** change to the
roxygen blocks.

---

## 3. Run the checks locally

This is the single most important step. Fix everything until it is clean.

```r
# Full check as CRAN runs it
devtools::check(remote = TRUE, manual = TRUE)

# Or the stricter, CRAN-flavoured check
devtools::check(args = c("--as-cran"))
```

Aim for **0 errors, 0 warnings, 0 notes**. A first submission may legitimately
carry **one** NOTE that reads "New submission" — that is expected. Any other
NOTE should be resolved or explained in `cran-comments.md`.

Common issues and fixes (most are already handled in this package):

| Message | Fix |
|---|---|
| `no visible binding for global variable 'x'` | Use `.data$x` inside `ggplot2::aes()` and `dplyr` verbs (done throughout). |
| `Undocumented arguments` / `missing \value` | Every exported function needs `@param` for each argument and an `@return`. |
| Example takes too long / needs a Suggests pkg | Wrap in `\donttest{}` or `\dontrun{}`. |
| `checking CRAN incoming feasibility ... Note` about examples/tests using Suggests | Guard with `requireNamespace()`; skip tests with `testthat::skip_if_not_installed()`. |
| Non-ASCII characters | Use `\uXXXX` escapes (the code uses `\u00d7` for the ×). |

Also run the spell-check and URL check:

```r
spelling::spell_check_package()   # add false positives to inst/WORDLIST
urlchecker::url_check()           # fixes/report broken URLs
```

---

## 4. Check on other platforms

CRAN builds on Windows, macOS, and several Linux flavours, so test there too:

```r
# Windows (devel + release), emails you the results
devtools::check_win_devel()
devtools::check_win_release()

# R-hub v2 (GitHub-Actions-based); run once to set up, then:
rhub::rhub_setup()      # one-time, configures a workflow in your repo
rhub::rhub_check()      # choose platforms interactively

# macOS
# submit the .tar.gz at https://mac.r-project.org/macbuilder/submit.html
```

Wait for the emailed/online results and clear any platform-specific problems
(the DNA/plot code is pure R + ggplot2, so cross-platform issues are unlikely).

---

## 5. Build the source tarball

CRAN wants a **source** package (`.tar.gz`), not a binary:

```r
devtools::build()          # writes ../PbStatR_0.9.0.tar.gz
# equivalently on the shell:
#   R CMD build .
#   R CMD check --as-cran PbStatR_0.9.0.tar.gz
```

---

## 6. Update `cran-comments.md`

Edit `cran-comments.md` (already scaffolded) so it states your actual test
environments and results, e.g.:

```
## R CMD check results
0 errors | 0 warnings | 1 note
* This is a new release.

## Test environments
* local: macOS 14.5, R 4.4.1
* win-builder: devel and release
* R-hub: ubuntu-gcc-release, windows-x86_64-devel, fedora-clang-devel
```

CRAN reviewers read this first, so be honest and concise. If you have any
remaining NOTE, explain why it is acceptable here.

---

## 7. Submit

Two equivalent routes:

**A. From R (recommended):**

```r
devtools::release()
```

This re-runs checks, asks you a series of confirmation questions, builds the
tarball, and uploads it to CRAN — then emails you a confirmation link.

**B. Manual web form:**

1. Go to <https://cran.r-project.org/submit.html>.
2. Upload `PbStatR_0.9.0.tar.gz`.
3. Enter your name and the maintainer email (must match `DESCRIPTION`).
4. Paste the contents of `cran-comments.md` into the comments box.
5. Submit, then click the confirmation link in the email CRAN sends you.

---

## 8. After submitting

* CRAN's incoming checks run automatically; you'll get an email within minutes
  to a few hours if the automated checks fail.
* A human reviewer may follow up with change requests. Respond promptly and
  politely, make the fixes, bump the **Version** (e.g. `0.9.0` → `0.9.1`),
  update `cran-comments.md` with a short "Resubmission" note describing what
  changed, and resubmit.
* Once accepted, the package appears at
  `https://CRAN.R-project.org/package=PbStatR` (usually within a day, after all
  the platform binaries build).

---

## 9. Housekeeping for future releases

* Keep a running `NEWS.md` (already present) — CRAN and users read it.
* For each new version: bump `Version`, update `NEWS.md`, re-run
  `devtools::check()` and the win/mac/R-hub checks, then `devtools::release()`.
* CRAN policy forbids frequent releases (roughly no more than one every 1–2
  months without good reason).
* Never email large numbers of CRAN maintainers or resubmit rapidly after a
  rejection without addressing the feedback.

---

## Quick command summary

```r
# one-time setup
install.packages(c("devtools","roxygen2","spelling","rhub","urlchecker","tinytex"))

# every release
devtools::document()                       # 1. build docs + NAMESPACE
devtools::check(args = c("--as-cran"))     # 2. local check (want 0/0/0-or-1)
spelling::spell_check_package()            #    spelling
devtools::check_win_devel()                # 3. Windows
rhub::rhub_check()                         #    other platforms
devtools::build()                          # 4. source tarball
# edit cran-comments.md                    # 5. record results
devtools::release()                        # 6. submit
```

---

### PbStatR-specific notes

* **Suggested packages.** Many features rely on Suggests
  (`FielDHub`, `rMVP`, `GAPIT`, `rrBLUP`, `qtl`, `BGLR`, `AlphaSimR`,
  `randomForest`, `xgboost`, `e1071`, `glmnet`). All are guarded with
  `requireNamespace()`, and their examples use `\dontrun{}`. **`GAPIT` is not
  on CRAN** (it installs from GitHub); keeping it in Suggests is acceptable
  because nothing in the package *requires* it at check time, but double-check
  the current CRAN policy on non-mainstream Suggests — if flagged, move the
  GAPIT wrapper's documentation example to `\dontrun{}` (already done) and note
  it in `cran-comments.md`.
* **Data.** Example datasets ship as CSVs in `inst/extdata/` and load via
  `pb_data()`; there is no `data/` directory, so `LazyData` has been removed
  from `DESCRIPTION` (having `LazyData: true` with no `data/` triggers a NOTE).
* **Reports.** `pb_report()` needs `rmarkdown` + Pandoc; its example is under
  `\dontrun{}` so it never runs during checks.
* **Figures.** `man/figures/` holds the README/pkgdown thumbnails and logo.
  Regenerate them with `data-raw/make_figures.R` (plots) and
  `data-raw/make_logo.py` (logo) before release if the styling changed.

---

## Appendix: Continuous integration (GitHub Actions)

The package ships four workflows in `.github/workflows/`, which run
automatically on every push and pull request once the repo is on GitHub:

| Workflow | What it does |
|---|---|
| `R-CMD-check.yaml` | Runs `R CMD check` on **Windows, macOS and Linux** (R release, devel, and oldrel-1) — the same matrix CRAN cares about. Errors on any warning. |
| `test-coverage.yaml` | Runs the testthat suite under `covr` and uploads coverage to Codecov. |
| `lint.yaml` | Static style/lint check with `lintr` (config in `.lintr`). |
| `pkgdown.yaml` | Builds the documentation site and deploys it to the `gh-pages` branch. |

Setup notes:

* These activate automatically — just push the repo to GitHub. No secrets are
  needed for public repositories (the built-in `GITHUB_TOKEN` is enough).
* For **private** repos, add a `CODECOV_TOKEN` secret for coverage uploads.
* `_R_CHECK_FORCE_SUGGESTS_: false` is set so the GitHub-only **GAPIT** package
  (and any other absent Suggests) doesn't fail CI; guarded code paths simply
  don't run there.
* The green **R-CMD-check** badge on a passing repo is exactly the signal CRAN
  reviewers like to see before a submission.
* To enable the pkgdown site: in the repo's **Settings → Pages**, set the source
  to the `gh-pages` branch after the first successful `pkgdown` run.
