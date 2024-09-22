#' R6 Class representing a database connection.
#'
#' This class provides methods to initialize a database connection either using
#' a configuration file or direct parameters, and retrieve views from the database.
#' @import R6
#' @import DBI
#' @import RPostgres
#' @import dplyr
#' @import dbplyr
#' @import config
#' @import stringr
#' @import tibble
#' @import pool
#' @import jsonlite
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
    #' @param sslrootcert Path to the SSL root certificate.
    initialize = function(config_path = NULL, dbname = NULL, host = NULL, port = NULL, user = NULL, password = NULL, sslmode = 'require', sslrootcert = NULL) {
      if (!is.null(config_path)) {
        app_config <- config::get(file = config_path)
        bfo <- app_config$bfo_data
        self$con <- pool::dbPool(
          drv = RPostgres::Postgres(),
          dbname = bfo$dbname,
          host = bfo$host,
          port = bfo$port,
          user = bfo$user,
          password = bfo$password,
          sslmode = 'require',
          sslrootcert = app_config$ssl_key
        )
      } else {
        if (is.null(dbname) || is.null(host) || is.null(port) || is.null(user) || is.null(password) || is.null(sslrootcert)) {
          stop("All database connection parameters must be provided if config_path is not used.")
        }
        self$con <- pool::dbPool(
          drv = RPostgres::Postgres(),
          dbname = dbname,
          host = host,
          port = port,
          user = user,
          password = password,
          sslmode = sslmode,
          sslrootcert = sslrootcert
        )
      }
    },

    #' Retrieve a view from the database.
    #'
    #' @param view_name Name of the view to retrieve.
    #' @return A tbl object representing the specified view.
    get_view = function(view_name) {
      if (is.null(self$con)) {
        stop("Connection has not been initialized.")
      }
      tbl(self$con, view_name)
    },

    #' Collect a view from the database.
    #'
    #' @param view_name Name of the view to retrieve.
    #' @return A tbl object representing the specified view.
    collect_view = function(view_name) {
      if (is.null(self$con)) {
        stop("Connection has not been initialized.")
      }
      tbl(self$con, view_name) |> collect()
    },

    #' Get user information from the database.
    #'
    #' @param user_email Email of the user to retrieve.
    #' @return A list object representing the information about user.
    get_user_information = function(user_email) {
      # Check if the database connection is initialized
      if (is.null(self$con)) {
        stop("Connection has not been initialized.")
      }
      # Check if the user email is provided
      if (is.null(user_email) || nchar(user_email) == 0) {
        stop("User email must be provided.")
      }

      # Use parameterized query to prevent SQL injection
      query <- glue::glue("SELECT user_json FROM shiny_user_info_json WHERE email = '{user_email}'")
      user_info <- dbGetQuery(self$con, query)

      # Check if the user was found
      if (length(user_info) == 0) {
        stop("User not found.")
      }

      # Parse the user JSON
      result <- jsonlite::fromJSON(user_info$user_json)
      # Split the user_countries string by newline character
      result$user_countries <- strsplit(result$user_countries, "\n")[[1]]

      return(result)
    },

    #' Finalize the database connection.
    #'
    #' @description Closes the database connection.
    finalize = function() {
      if (!is.null(self$con)) {
        pool::poolClose(self$con)
      }
    }
  )
)
