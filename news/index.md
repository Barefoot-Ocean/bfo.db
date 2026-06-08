# Changelog

## bfo.db 0.3.0

### New features

- **Informative console messages.** Each step now prints a short `cli`
  explanation of what is happening and what to do next (connecting, lazy
  vs. collected views, rows fetched, pool closed). Pass `quiet = TRUE`
  to `DatabaseConnector$new()` to silence them.
- **Safer connection cleanup.** Added
  [`close()`](https://rdrr.io/r/base/connections.html) (and its alias
  `disconnect()`) to close the connection pool. Closing is now
  **idempotent** — calling it more than once is a no-op instead of an
  error — and the object correctly reports itself as disconnected
  afterwards. The pool is also closed automatically on garbage
  collection.

### Breaking changes

- `finalize()` is no longer called directly; it is now a private
  garbage-collection finalizer (as recommended by R6 \>= 2.4.0). **Use
  `db$close()` instead of `db$finalize()`.**

### Certificates

- The AWS RDS CA bundles now ship **inside the package** (`inst/cert/`),
  so `sslrootcert` (and `ssl_key` in `config.yml`) is optional. When
  omitted, the connection defaults to the **global** bundle (covers all
  regions) and falls back to the `us-east-1` bundle. You can still pass
  your own certificate path to override. These bundles are public AWS
  certificates, not secrets.

### Documentation

- Reworked the README into a step-by-step user guide, with a dedicated
  **SSL certificate** section and a security note clarifying that
  database host/user/password are secrets (keep them in
  `config.yml`/`.Renviron`, never in git) while the CA bundle is public.
- Added a **“Getting started”** vignette
  ([`vignette("getting-started")`](https://barefoot-ocean.github.io/bfo.db/articles/getting-started.md)).
- Added a **pkgdown** website (`_pkgdown.yml`) built and deployed to
  GitHub Pages by a new `pkgdown` GitHub Actions workflow.

### Infrastructure

- Updated the `renv.lock` to R 4.4.1 and current package versions.
- The `renv.lock` now records the development toolchain (devtools,
  roxygen2, pkgdown, testthat, knitr, rmarkdown), so `renv::restore()`
  installs everything needed to develop, document, and build the
  package. These are declared in `dev/dependencies.R`.
- Filled in the package `DESCRIPTION` metadata (title, description,
  author, MIT license, URLs, `Suggests`, `VignetteBuilder`) and
  regenerated the documentation with roxygen2 7.3.3.

## bfo.db 0.2.0

- Initial documented release: `DatabaseConnector` R6 class with
  `config.yml` and direct-parameter connections, `get_view()`,
  `collect_view()`, `get_user_information()`, and `finalize()`.
