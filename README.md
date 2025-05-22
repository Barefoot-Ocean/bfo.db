
<!-- README.md is generated from README.Rmd. Please edit that file -->

# 📦 How to Install and Use the `{bfo.db}` Package in R

This guide will walk you through installing and using the `{bfo.db}` R
package to interact with the BFO database.

------------------------------------------------------------------------

## ✅ Step 1: Check Your R Version

Make sure you are using **R version 4.1.0 or newer**.

You can check your version in RStudio:

- Look at the **Console tab** (top-left by default)
- Or run one of the following commands:

<details>
<summary>
Check version via terminal:
</summary>

**Windows** (Command Prompt or PowerShell):

``` cmd
R --version
```

**macOS / Ubuntu / Linux** (Terminal):

``` bash
R --version
```

</details>

------------------------------------------------------------------------

## 📦 Step 2: Install the `pak` Package (If Not Installed)

`pak` is a modern R package manager. Install it by running:

``` r
install.packages("pak")
```

> 🔄 You might be prompted to confirm installation of dependencies—click
> **“Yes”** or type `y` in the console.

Wait until you see a message like:

    ✔ DONE (pak)

------------------------------------------------------------------------

## 🔌 Step 3: Install the `{bfo.db}` Package

To install the `{bfo.db}` package (recommended version `v0.2.0`), run:

``` r
pak::pak("Barefoot-Ocean/bfo.db@v0.2.0", upgrade = TRUE)
```

This version is recommended for stable database interaction.

------------------------------------------------------------------------

## 📚 Step 4: Load the Package

Once installed, load the package:

``` r
library(bfo.db)
```

------------------------------------------------------------------------

## 🔐 Step 5: Set Up Database Credentials

You can connect to the database in one of two ways:

### Option 1: Use a `config.yml` File

``` r
db <- bfo.db::DatabaseConnector$new(config_path = "path/to/config.yml")
```

**Example `config.yml` structure:**

``` yaml
default:
  ssl_key: XXXXXXXXXX
  bfo_data:
    dbname: XXXXXX
    host: XXXXXX
    user: XXXXXX
    password: XXXXXX
    port: XXXXXX
```

------------------------------------------------------------------------

### Option 2: Provide Credentials Directly in the Script

``` r
db <- bfo.db::DatabaseConnector$new(
  dbname = "XXXXXXX",
  host = "XXXXXXX",
  user = "XXXXXXX",
  password = "XXXXXXX",
  port = XXXXX,
  sslmode = "require",
  sslrootcert = "path/to/your-cert.pem"
)
```

------------------------------------------------------------------------

## 📅 Step 6: Fetch Data from the Database

You can fetch data in two ways depending on your workflow:

### Option A: Load All Data Immediately

``` r
global_ref_data <- db$collect_view("global_ref")
```

### Option B: Work with the Database Object First (More Memory-Efficient)

``` r
global_ref_data <- db$get_view("global_ref")
# You can apply filters, joins, etc. before collecting:
global_ref_data <- global_ref_data |>
      dplyr::filter(country == "IDN") |>
      dplyr::collect()
```

------------------------------------------------------------------------

## 🔒 Step 7: Close the Database Connection

Once you’re done, don’t forget to clean up:

``` r
db$finalize()
```

------------------------------------------------------------------------

## ⚠️ Notes: Possible Issues and Solutions

1.  **GitHub Access**: Make sure you are a **member of the
    `Barefoot-Ocean` organization** on GitHub. If not, you won’t be able
    to access the private repository.

2.  **Authentication Token (Optional but Recommended)**: Adding a GitHub
    token to your `.Renviron` file helps avoid GitHub rate limits during
    package installation.

### Add a GitHub Token to `.Renviron`

**Step-by-step instructions:**

#### 💻 On Windows:

1.  Open RStudio.

2.  Run `usethis::edit_r_environ()` to open your `.Renviron` file.

3.  Add the following line (replace `your_token`):

        GITHUB_PAT=your_token

4.  Save and close the file.

5.  Restart RStudio.

#### 💻 On macOS/Linux:

1.  Open Terminal.

2.  Edit the `.Renviron` file in your home directory (or use RStudio as
    above):

    ``` bash
    open -a TextEdit ~/.Renviron  # for macOS
    nano ~/.Renviron              # for CLI edit
    ```

3.  Add the line:

        GITHUB_PAT=your_token

4.  Save the file and restart RStudio.

You can create a new token at: <https://github.com/settings/tokens>

------------------------------------------------------------------------

## Contact

- Maintainer: Anastasiia Kostiv \[<anastasiia.kostiv@bluventures.org>\]

------------------------------------------------------------------------

## Development notes (only for developers)

`{bfo.db}` is an R package that facilitates database connections and
view retrieval using `RPostgres`, `pool`, and other related packages.

### Installation

You can install the development version of `bfo.db` from GitHub:

``` r
devtools::install_github("Barefoot-Ocean/bfo.db")
remotes::install_github("Barefoot-Ocean/bfo.db")
```

### Usage

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

#### Get user information by email

``` r
library(bfo.db)
db <- bfo.db::DatabaseConnector$new(config_path = "path/to/config.yml")

# Get user information by email
db$get_user_information(user_email = 'anastasiia@barefootocean.org')
```

### Features

- Easy initialization of database connections from a configuration file
  or direct parameters.
- Retrieval of database views using get_view() method.
- Automatic closing of database connections with finalize() method.

### Dependencies

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
