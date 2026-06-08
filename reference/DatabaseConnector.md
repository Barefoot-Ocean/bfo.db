# R6 Class representing a database connection.

R6 Class representing a database connection.

R6 Class representing a database connection.

## Details

This class provides methods to initialize a database connection either
using a configuration file or direct parameters, and retrieve views from
the database. Each step prints a short, informative message explaining
what is happening and what to do next; set \`quiet = TRUE\` to silence
them.

## Public fields

- `con`:

  A database connection object created using pool::dbPool. Initialize
  the database connection.

## Methods

### Public methods

- [`DatabaseConnector$new()`](#method-DatabaseConnector-new)

- [`DatabaseConnector$get_view()`](#method-DatabaseConnector-get_view)

- [`DatabaseConnector$collect_view()`](#method-DatabaseConnector-collect_view)

- [`DatabaseConnector$get_user_information()`](#method-DatabaseConnector-get_user_information)

- [`DatabaseConnector$close()`](#method-DatabaseConnector-close)

- [`DatabaseConnector$disconnect()`](#method-DatabaseConnector-disconnect)

- [`DatabaseConnector$clone()`](#method-DatabaseConnector-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    DatabaseConnector$new(
      config_path = NULL,
      dbname = NULL,
      host = NULL,
      port = NULL,
      user = NULL,
      password = NULL,
      sslmode = "require",
      sslrootcert = NULL,
      quiet = FALSE
    )

#### Arguments

- `config_path`:

  Path to the configuration file.

- `dbname`:

  Database name.

- `host`:

  Database host.

- `port`:

  Database port.

- `user`:

  Database user.

- `password`:

  Database password.

- `sslmode`:

  SSL mode for the database connection.

- `sslrootcert`:

  Path to the SSL root certificate. If \`NULL\`, the AWS RDS CA bundle
  shipped with this package is used (the global bundle, which covers all
  regions, falling back to the \`us-east-1\` bundle). The same default
  applies for \`config.yml\` connections when \`ssl_key\` is omitted.
  Pass a path here to use your own certificate instead.

- `quiet`:

  Logical; if \`TRUE\`, suppress the informational CLI messages.
  Retrieve a view from the database.

------------------------------------------------------------------------

### Method `get_view()`

#### Usage

    DatabaseConnector$get_view(view_name)

#### Arguments

- `view_name`:

  Name of the view to retrieve.

#### Returns

A tbl object representing the specified view. Collect a view from the
database.

------------------------------------------------------------------------

### Method `collect_view()`

#### Usage

    DatabaseConnector$collect_view(view_name)

#### Arguments

- `view_name`:

  Name of the view to retrieve.

#### Returns

A tbl object representing the specified view. Get user information from
the database.

------------------------------------------------------------------------

### Method `get_user_information()`

#### Usage

    DatabaseConnector$get_user_information(user_email)

#### Arguments

- `user_email`:

  Email of the user to retrieve.

#### Returns

A list object representing the information about user. Close the
database connection pool.

------------------------------------------------------------------------

### Method [`close()`](https://rdrr.io/r/base/connections.html)

Returns all pooled connections and closes the pool. Safe to call more
than once; closing an already-closed pool is a no-op.

#### Usage

    DatabaseConnector$close()

#### Returns

The \`DatabaseConnector\` object, invisibly. Close the database
connection pool.

------------------------------------------------------------------------

### Method `disconnect()`

Alias for \`close()\`.

#### Usage

    DatabaseConnector$disconnect()

#### Returns

The \`DatabaseConnector\` object, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DatabaseConnector$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
