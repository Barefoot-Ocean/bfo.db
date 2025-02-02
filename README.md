
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bfo.db

<!-- badges: start -->
<!-- badges: end -->

`{bfo.db}` is an R package that facilitates database connections and
view retrieval using `RPostgres`, `pool`, and other related packages.

## Installation

You can install the development version of `bfo.db` from GitHub:

``` r
devtools::install_github("Barefoot-Ocean/bfo.db")
remotes::install_github("Barefoot-Ocean/bfo.db")
```

## Usage

This is a basic example:

``` r
# Load the package
library(bfo.db)

# Example of initializing a database connection with a configuration file
db <- bfo.db::DatabaseConnector$new(config_path = "path/to/config.yml")

# Example of initializing a database connection with a credentials
db <- bfo.db::DatabaseConnector$new(dbname = XXXXXXX, 
                                    host = XXXXXXX, 
                                    user = XXXXXXX, 
                                    password = "XXXXXXX", 
                                    port = XXXXXXX, 
                                    sslmode = "require", 
                                    sslrootcert = "path/to/XXXXXXX.pem"
                                    )

# Retrieve a view from the database
view_connection <- db$get_view("view_name")

# Close the database connection
db$finalize()
```

### Get user information by email

``` r
library(bfo.db)
db <- bfo.db::DatabaseConnector$new(config_path = "path/to/config.yml")

# Get user information by email
db$get_user_information(user_email = 'anastasiia@barefootocean.org')
```

## Features

- Easy initialization of database connections from a configuration file
  or direct parameters.
- Retrieval of database views using get_view() method.
- Automatic closing of database connections with finalize() method.

## Dependencies

- R (\>= 4.2.2)
- DBI (== 1.2.3)
- RPostgres (== 1.4.7)
- dplyr (== 1.1.4)
- dbplyr (== 2.5.0)
- config (== 0.3.2)
- stringr (== 1.5.1)
- tibble (== 3.2.1)
- pool (== 1.0.3)
- R6 (== 2.5.1)
- later (== 1.3.2)
- rlang (== 1.1.4)
- assertthat (== 0.2.1)
- glue (== 1.7.0),
- jsonlite (== 1.8.8),

## Contact

- Maintainer: Anastasiia Kostiv \[<anastasiia.kostiv@bluventures.org>\]
