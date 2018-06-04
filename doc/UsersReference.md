# R6RS MongoDB library Users' refernece

This document describes high level APIs.

For the detail of the CRUD operation and command specification, please
refer to the official MongoDB document. [MongoDB document](https://docs.mongodb.com/)

For the SBSON, please refer [SBSON](SBSON.md)

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

### Query operation

- `(mongodb-database-query database collection query)`
- `(mongodb-database-query database collection query number-to-skip)`
- `(mongodb-database-query database collection query number-to-skip number-to-return)`
Sends SBSON `query` to the given collection `collection` and returns
query result object described below section.

The `collection` must be either a name of the collection or a list of
the collection names (e.g. `(test collection)` will be `test.colection`).

If the second form is used, then it specifies the starting position of
the collection.

If the third form is used, then it also limits the number of returning
documents.

For example, the following query returns 5 results:
```scheme
(mongodb-database-query database collection '() 0 5)
```

- `(mongodb-database-query/selector database collection query selector)`
- `(mongodb-database-query/selector database collection query selector number-to-skip)`
- `(mongodb-database-query/selector database collection query selector number-to-skip number-to-return)`
Similar with the `mongodb-database-query` but specifying the returning
elements by the given `selector`.

The `selector` must be a list of string which specifying the name of the
returning document element.

For example, the following query returns only `lang` element and `_id` element
which is MongoDB internal element.
```scheme
(mongodb-database-query/selector database collection '() '("lang"))
```

### Cursor and query result

- `(mongodb-cursor? obj)`
Returns `#t` if the given `obj` is MongoDB cursor object, otherwise `#f`.

- `(make-mongodb-cursor cursor-id database ns)`
Creates a MongoDB cursor object.

The `id` must be an positive integer no bigger than 64 bit.

The `database` must be a MongoDB database object.

The `ns` must be a string of full collection name (namespace).

- `(mongodb-cursor-id cursor)`
Returns cursor id of the `cursor`. 

- `(mongodb-cursor-ns cursor)`
Returns namespace of the `cursor`. 


NOTE: This is useful only for custom query writers.

- `(mongodb-query-result? obj)`
Returns `#t` if the given `obj` is MongoDB query result object, otherwise `#f`.

- `(mongodb-query-result-id query-result)`
Returns request id of the `query-result`. 

- `(mongodb-query-result-to query-result)`
Returns response to of the `query-result`. 

- `(mongodb-query-result-starting-from query-result)`
Returns starting from of the `query-result`. 

- `(mongodb-query-result-documents query-result)`
Returns SBSON documents of the `query-result`. The returning documents is
a vector.

## Insert operation

- `(mongodb-database-insert database collection documents)`
- `(mongodb-database-insert database collection documents ignore-error)`
Inserts the given vector of SBSON `documents` into the specified `collection`
of the MongoDB database `database`.

If the second form is used and true value is passed, then it won't check error.

The following 3 example show normal flow, error flow and ignoring error flow,
respectively.
```scheme
(mongodb-database-insert db collection
  '#(
     (("name" "R6RS mongodb") 
	  ("lang" "Scheme") 
	  ("comment" "It's awesome!")
      ("date" (utc-datetime 0)))
	 (("name" "R7RS postgres") 
	  ("lang" "Scheme") 
	  ("comment" "Wow it's portable!")
      ("date" (utc-datetime 0)))
	))
```
```scheme
(mongodb-database-insert db collection
  '#(
     (("_id" 1)
	  ("name" "R6RS mongodb") 
	  ("lang" "Scheme") 
	  ("comment" "It's awesome!")
      ("date" (utc-datetime 0)))
	 (("_id" 1)
	  ("name" "R7RS postgres") 
	  ("lang" "Scheme") 
	  ("comment" "Wow it's portable!")
      ("date" (utc-datetime 0)))
	))
;; -> &mongodb
```
```scheme
(mongodb-database-insert db collection
  '#(
     (("_id" 1)
	  ("name" "R6RS mongodb") 
	  ("lang" "Scheme") 
	  ("comment" "It's awesome!")
      ("date" (utc-datetime 0)))
	 (("_id" 1)
	  ("name" "R7RS postgres") 
	  ("lang" "Scheme") 
	  ("comment" "Wow it's portable!")
      ("date" (utc-datetime 0)))
	)
	#t)
```

- `(mongodb-database-update database collection selector update)`
- `(mongodb-database-upsert database collection selector update)`
- `(mongodb-database-update-all database collection selector update)`
Updates the document(s) in the `collection` of the `database`.

The `selector` must be a SBSON of the query selector specified by the
official document.

The `update` must be a SBSON of the update operation specified by the
official document.

The first form updates one record which is selected by the given
`selector`.

The second form performs upsert; if the document is *not* found, then
it insert the document, otherwise no operation.

The third form updates all the selected documents.

- `(mongodb-database-delete database collection selector)`
- `(mongodb-database-delete-all database collection selector)`
Deletes the document(s) in the `collection` of the `database`.

The first form deletes only one document selected by the `selector`.

The second form deletes all documents selected by the `selector`.

## Misc operations

- `(mongodb-database-get-last-error database)`
Retrieves the last error of the given `database` in SBSON format.

- `(mongodb-database-drop-collection database collection)`
Drops the given `collection` of the `database`.

# Query result generators

A generator is a thunk. This library provides generator conversion for
better integration with [SRFI-121](https://srfi.schemers.org/srfi-121/)
or [SRFI-158](https://srfi.schemers.org/srfi-158/)

- `(mongodb-query-result->generator query-result)`
- `(mongodb-query-result->get-more-generator query-result)`
Returns a generator. The generator returns a SBSON document or EOF object
when it reaches to the end.

The second form also proceed the cursor.

NOTE: if the second form is used, then the cursor of the given `query-result`
is consumed, thus low cursor operation may throw an error if the cursor
reached to the end.

# Query result fold, map and for-each

- `(mongodb-query-fold proc seed query-result)`
- `(mongodb-query-fold proc seed query-result all?)`
Folds the given `query-result` with given `seed` as its initial value
using the `proc`. The `proc` receives 2 arguments, document and seed,
respectively.

This is an anologue of `fold`.

- `(mongodb-query-for-each proc query-result)`
- `(mongodb-query-for-each proc query-result all?)`
Iterates all documents of `query-result` with the given `proc`. The `proc`
receives a document as its argument.

This is an anologue of `for-each`.

- `(mongodb-query-map proc query-result)`
- `(mongodb-query-map proc query-result all?)`
Converts all documents of `query-result` with the return value of 
the given `proc`. The `proc` receives a document as its argument.

This is an anologue of `map`.

For all procedures, if the second form is used and true value is 
passed to `all?`, then the `query-result` would retrieve all the 
result using the cursor.
