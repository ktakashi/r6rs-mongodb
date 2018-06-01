# R6RS MongoDB library Users' refernece

This document describes high level APIs.

## `(mongodb)` library
This is the library exports high level APIs

## MongoDB connection

- `(mongodb-connection? obj)`
Returns `#t` if the given `obj` is MongoDB connection, otherwise `#f`.

- `(make-mongodb-connection host)`
- `(make-mongodb-connection host port)`
- `(make-mongodb-connection host port option)`
Creates a MongoDB connection object which connects to given `host` with
port number `27017.

If the second form is used, then the `port` must be a positive fixnum which
represents the port number of the connecting `host`.

If the third form is used, then the `option` must be a MongoDB connection
option described below section.

- `(open-mongodb-connection! connection)`
Opens the given MogoDB connection `connection` and returns the `connection`

- `(close-mongodb-connection! connection)`
Closes the given MogoDB connection `connection` and returns the `connection`

- `(mongodb-connection-open? connection)`
Returns `#t` if the given `connection` is open. Otherwise `#f`.

- `(mongodb-connection-list-databases connection)`
Returns a string list of database names.

## MongoDB connection option

- `(mongodb-connection-option? obj)`
Returns `#t` if the given `obj` is MongoDB connection option, otherwise `#f`.

- `(make-mongodb-connection-option request-id-strategy socket-converter use-iso-date?)`
Creates a MongoDB connection option.

`request-id-strategy` must be a procedure which takes 2 arguments,
MongoDB connection and MongoDB database, and must return a exact integer
no bigger than 32 bit.

The latter argument might be `#f` if the CRUD request is made by the
connection.

`socket-converter` must be a procedure which takes 1 argument,
socket wrapper, and must return a socket wrapper.

This is a sort of hook to convert normal TCP connection to TLS since
this library doesn't support TLS connection.

`use-iso-date?` specifies the BSON reader to convert UTC time to ISO date 
string or not.

## MongoDB database

- `(mongodb-database?? obj)`
Returns `#t` if the given `obj` is MongoDB database, otherwise `#f`.

- `(make-mongodb-database connection name)`
Creates a MongoDB database object.

`connection` must be a MongoDB connection object.

`name` must be a string of MongoDB database name.

TBD
