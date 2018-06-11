# MongoDB authenticate

## `(mongodb authenticate)`

This library provide authentication procedure for MogoDB. Currently, 
it only supports SCRAM method.

- `(mongodb-database-authenticate! database username password)`

Authenticates the given credentials `username` and `password`.
The arguments must be strings.

If authentication failed, then it raises `&mongodb-authenticate` condition.

- `(mongodb-authenticate-error? obj)`

Return `#t` if the given `obj` is a condition of `&mongodb-authenticate`,
otherwise `#f`.
