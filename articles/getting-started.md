# Getting started with bfo.db

[bfo.db](https://github.com/Barefoot-Ocean/bfo.db) provides a small
[R6](https://r6.r-lib.org/) interface, `DatabaseConnector`, for
connecting to the Barefoot Ocean PostgreSQL database over a secure (TLS)
connection and pulling database views into R.

``` r

library(bfo.db)
```

## 1. Open a connection

There are two ways to connect. Either way, the package prints short
messages explaining what is happening and what to do next; pass
`quiet = TRUE` to silence them.

### Option A — a `config.yml` file (recommended)

Keep your secrets in a `config.yml` file that is **never committed to
git**:

``` yaml
default:
  bfo_data:
    dbname: XXXXXX
    host: XXXXXX
    user: XXXXXX
    password: XXXXXX
    port: XXXXXX
  # Optional: omit to use the CA bundle shipped with the package.
  # ssl_key: path/to/your-cert.pem
```

``` r

db <- DatabaseConnector$new(config_path = "path/to/config.yml")
```

### Option B — direct parameters

``` r

db <- DatabaseConnector$new(
  dbname   = "XXXXXX",
  host     = "XXXXXX",
  user     = "XXXXXX",
  password = "XXXXXX",
  port     = 5432,
  sslmode  = "require"
  # sslrootcert is optional — defaults to the bundled AWS RDS CA bundle.
)
```

## 2. Credentials vs. the SSL certificate

Your database **host, user, and password are secrets** — keep them in
`config.yml` or `.Renviron` and out of version control.

The **SSL certificate** (`sslrootcert` / `ssl_key`) is *not* a secret:
it is AWS’s public Certificate Authority bundle, used only to verify the
server’s identity. For convenience this package ships the bundles
itself, so by default you don’t need to download or manage any `.pem`
file. The connection uses the **global** bundle (which covers every AWS
region), falling back to the `us-east-1` bundle:

``` r

system.file("cert", "global-bundle.pem", package = "bfo.db")   # preferred
system.file("cert", "us-east-1-bundle.pem", package = "bfo.db") # fallback
```

Only set `sslrootcert`/`ssl_key` yourself to pin a specific certificate.

## 3. Fetch data

You can work with a view lazily (recommended) or pull it straight into
memory.

### Lazily, then collect

`get_view()` returns a lazy reference — no data is fetched until you
`collect()`, so you can push filters and joins down to the database:

``` r

global_ref <- db$get_view("global_ref")

global_ref |>
  dplyr::filter(country == "IDN") |>
  dplyr::collect()
```

### All at once

``` r

global_ref_data <- db$collect_view("global_ref")
```

### Look up a user

``` r

db$get_user_information(user_email = "anastasiia@barefootocean.org")
```

## 4. Close the connection

When you’re done, return the pooled connections and close the pool:

``` r

db$close()        # db$disconnect() is an alias
```

[`close()`](https://rdrr.io/r/base/connections.html) is safe to call
more than once, and the pool is also closed automatically if the object
is garbage-collected — but closing it explicitly is good practice.
