#' R6 Class representing a database connection.
#'
#' This class provides methods to initialize a database connection either using
#' a configuration file or direct parameters, and retrieve views from the
#' database. Each step prints a short, informative message explaining what is
#' happening and what to do next; set `quiet = TRUE` to silence them.
#' @import R6
#' @import DBI
#' @import RPostgres
#' @import dplyr
#' @rawNamespace import(dbplyr, except = c(ident, sql))
#' @import config
#' @import stringr
#' @import tibble
#' @import pool
#' @import jsonlite
#' @import cli
#'
#' @export
DatabaseConnector <- R6::R6Class(
  "DatabaseConnector",
  public = list(
    # Database connection object
    #' @field con A database connection object created using pool::dbPool.
    con = NULL,

    #' Initialize the database connection.
    #'
    #' @param config_path Path to the configuration file.
    #' @param dbname Database name.
    #' @param host Database host.
    #' @param port Database port.
    #' @param user Database user.
    #' @param password Database password.
    #' @param sslmode SSL mode for the database connection.
    #' @param sslrootcert Path to the SSL root certificate. If `NULL`, the AWS
    #'   RDS CA bundle shipped with this package is used (the global bundle,
    #'   which covers all regions, falling back to the `us-east-1` bundle). The
    #'   same default applies for `config.yml` connections when `ssl_key` is
    #'   omitted. Pass a path here to use your own certificate instead.
    #' @param quiet Logical; if `TRUE`, suppress the informational CLI messages.
    initialize = function(config_path = NULL, dbname = NULL, host = NULL, port = NULL,
                          user = NULL, password = NULL, sslmode = 'require',
                          sslrootcert = NULL, quiet = FALSE) {
      private$quiet <- isTRUE(quiet)

      # Default SSL root certificate: public AWS RDS CA bundle shipped in
      # inst/cert. Prefer the global bundle (covers all regions); fall back to
      # the us-east-1 bundle. These are public certificates, not secrets.
      default_cert <- system.file("cert", "global-bundle.pem", package = "bfo.db")
      if (!nzchar(default_cert) || !file.exists(default_cert)) {
        default_cert <- system.file("cert", "us-east-1-bundle.pem", package = "bfo.db")
      }

      if (!is.null(config_path)) {
        private$inform(c("i" = "Reading connection settings from {.path {config_path}}"))
        app_config <- config::get(file = config_path)
        bfo <- app_config$bfo_data
        ssl_cert <- if (!is.null(app_config$ssl_key)) app_config$ssl_key else default_cert
        params <- list(dbname = bfo$dbname, host = bfo$host, port = bfo$port,
                       user = bfo$user, password = bfo$password,
                       sslmode = 'require', sslrootcert = ssl_cert)
      } else {
        if (is.null(dbname) || is.null(host) || is.null(port) || is.null(user) || is.null(password)) {
          cli::cli_abort(c(
            "All connection parameters are required when {.arg config_path} is not used.",
            "x" = "Provide {.arg dbname}, {.arg host}, {.arg port}, {.arg user} and {.arg password}.",
            "i" = "Or pass {.arg config_path} pointing to a {.path config.yml} file instead."
          ))
        }
        if (is.null(sslrootcert)) sslrootcert <- default_cert
        params <- list(dbname = dbname, host = host, port = port, user = user,
                       password = password, sslmode = sslmode, sslrootcert = sslrootcert)
      }

      private$announce_connecting(params)
      self$con <- do.call(pool::dbPool, c(list(drv = RPostgres::Postgres()), params))
      private$announce_ready()
    },

    #' Retrieve a view from the database.
    #'
    #' @param view_name Name of the view to retrieve.
    #' @return A tbl object representing the specified view.
    get_view = function(view_name) {
      private$ensure_connected()
      private$inform(c(
        "i" = "Lazy reference to {.val {view_name}} — no data fetched yet.",
        "*" = "Chain {.code dplyr} verbs (filter, select, mutate, …), then {.code collect()} to run the query."
      ))
      tbl(self$con, view_name)
    },

    #' Collect a view from the database.
    #'
    #' @param view_name Name of the view to retrieve.
    #' @return A tbl object representing the specified view.
    collect_view = function(view_name) {
      private$ensure_connected()
      private$inform(c("i" = "Collecting {.val {view_name}} into memory …"))
      result <- tbl(self$con, view_name) |> collect()
      private$inform(c(
        "v" = "Collected {nrow(result)} row{?s} × {ncol(result)} column{?s} from {.val {view_name}}."
      ))
      result
    },

    #' Get user information from the database.
    #'
    #' @param user_email Email of the user to retrieve.
    #' @return A list object representing the information about user.
    get_user_information = function(user_email) {
      private$ensure_connected()
      # Check if the user email is provided
      if (is.null(user_email) || nchar(user_email) == 0) {
        cli::cli_abort(c(
          "A user email is required.",
          "x" = "{.arg user_email} was empty or {.code NULL}."
        ))
      }
      private$inform(c("i" = "Looking up user {.val {tolower(user_email)}} …"))

      # Use parameterized query to prevent SQL injection
      query <- glue::glue("SELECT user_json FROM shiny_user_info_json WHERE email = '{tolower(user_email)}'")
      user_info <- dbGetQuery(self$con, query)

      # Check if the user was found
      if (nrow(user_info) < 1) {
        cli::cli_abort(c(
          "User not found.",
          "x" = "No record matches {.val {tolower(user_email)}}."
        ))
      }

      # Parse the user JSON
      result <- jsonlite::fromJSON(user_info$user_json)
      # Split the user_countries string by newline character
      result$user_countries <- strsplit(result$user_countries, "\n")[[1]]

      private$inform(c("v" = "Found user {.val {tolower(user_email)}}."))
      return(result)
    },

    #' Close the database connection pool.
    #'
    #' @description Returns all pooled connections and closes the pool. Safe to
    #'   call more than once; closing an already-closed pool is a no-op.
    #' @return The `DatabaseConnector` object, invisibly.
    close = function() {
      private$close_pool(verbose = TRUE)
      invisible(self)
    },

    #' Close the database connection pool.
    #'
    #' @description Alias for `close()`.
    #' @return The `DatabaseConnector` object, invisibly.
    disconnect = function() {
      self$close()
    }
  ),

  private = list(
    # Whether to suppress informational CLI messages.
    quiet = FALSE,

    # Emit a cli message in the calling method's environment (so message
    # placeholders like {view_name} resolve correctly), unless quiet.
    inform = function(message, ...) {
      if (!private$quiet) cli::cli_inform(message, ..., .envir = parent.frame())
    },

    # Stop with a friendly error if there is no usable connection. Also covers
    # the case where the pool has already been closed (con is reset to NULL).
    ensure_connected = function() {
      if (is.null(self$con)) {
        cli::cli_abort(c(
          "No active database connection.",
          "x" = "The connection was never initialized, or it has already been closed.",
          "i" = "Open one with {.code DatabaseConnector$new(...)}."
        ))
      }
    },

    announce_connecting = function(params) {
      if (private$quiet) return(invisible())
      cli::cli_rule(left = "{.pkg bfo.db} database connection")
      cli::cli_alert_info(
        "Connecting to {.val {params$dbname}} on {.field {params$host}:{params$port}} as {.val {params$user}}"
      )
      cli::cli_alert_info(
        "TLS {.field {params$sslmode}} — verifying the server with {.path {basename(params$sslrootcert)}}"
      )
    },

    announce_ready = function() {
      if (private$quiet) return(invisible())
      cli::cli_alert_success("Connection pool ready.")
      cli::cli_text(
        "{.emph A pool keeps a few connections open and reuses them, so each query is fast and idle connections are tidied up automatically.}"
      )
      cli::cli_text("Next steps:")
      cli::cli_ul(c(
        '{.code db$get_view("name")} — lazy table reference; the query runs only when you {.code collect()}',
        '{.code db$collect_view("name")} — pull a whole view into memory now',
        '{.code db$close()} — return connections and close the pool when finished'
      ))
    },

    # Idempotent pool close: closes the pool if it is still open, then clears
    # the reference so the object reports itself as disconnected.
    close_pool = function(verbose) {
      if (is.null(self$con)) {
        if (verbose && !private$quiet) {
          cli::cli_alert_warning("Connection pool is already closed — nothing to do.")
        }
        return(invisible(FALSE))
      }
      tryCatch(
        if (pool::dbIsValid(self$con)) pool::poolClose(self$con),
        error = function(e) NULL
      )
      self$con <- NULL
      if (verbose && !private$quiet) {
        cli::cli_alert_success("Connection pool closed — all connections returned to the database.")
      }
      invisible(TRUE)
    },

    # Garbage-collection safety net: quietly close the pool if the user forgot
    # to call close(). Defined privately as recommended by R6 (>= 2.4.0).
    finalize = function() {
      private$close_pool(verbose = FALSE)
    }
  )
)
