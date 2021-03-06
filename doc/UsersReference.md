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


- `(make-mongodb-connection-option-default-request-id-strategy)`

Returns a procedure which takes 2 arguments, connection and database.

The returning procedure can be used for the *request-id-strategy* connection
options described above.

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

The MongoDB query result type is a subtype of MongoDB cursor type.


- `(mongodb-query-result-id query-result)`

Returns request id of the `query-result`. 


- `(mongodb-query-result-to query-result)`

Returns response to of the `query-result`. 


- `(mongodb-query-result-starting-from query-result)`

Returns starting from of the `query-result`. 


- `(mongodb-query-result-documents query-result)`

Returns SBSON documents of the `query-result`. The returning documents is
a vector.


- `(mongodb-database-cursor-get-more cursor)`
- `(mongodb-database-cursor-get-more cursor number-to-return)`

Returns query result if the given `cursor` still has documents to read.
Otherwise `#f`.

If the second form is used, then the number of documents returning
by this procedure will be limited to the `number-to-return`.


- `(mongodb-database-kill-cursors database cursor ...)`

Terminates the given `cursors`.


### Insert operation

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


### Misc operations

- `(mongodb-database-get-last-error database)`

Retrieves the last error of the given `database` in SBSON format.


- `(mongodb-database-drop-collection database collection)`

Drops the given `collection` of the `database`.


- `(call-with-mongodb-connection host port proc)`
- `(call-with-mongodb-connection host port proc option)`

Connects to the given `host` of the given `port` and calling the given
`proc` passing freshly made MongoDB connection object. Returning
value(s) of the `call-with-mongodb-connection` is the returning
value(s) of the `proc`.

The procedure always closes the connection so capturing continuation
and reentering it would cause an error.

The second form specifies the connection option.

```scheme
(call-with-mongodb-connection "localhost" 27017
  (lambda (connection)
    (let ((db1 (make-mongodb-database connection "db1"))
	      (db2 (make-mongodb-database connection "db2")))
      ;; CRUD operations agaist db1 and db2
	  #t)))
;; -> #t
```


- `(call-with-mongodb-database host port database-name proc)`
- `(call-with-mongodb-database host port database-name proc option)`

Connects to the given `host` of the given `port` and creating a
MongoDB database object, then calling the given `proc` passing the
database object. Returning value(s) of the
`call-with-mongodb-database` is the returning value(s) of the `proc`.

The procedure always closes the connection so capturing continuation
and reentering it would cause an error.

The second form specifies the connection option.

This is useful when the connection only used once and connects to a
single database.

```scheme
(call-with-mongodb-database "localhost" 27017 "db1"
  (lambda (database)
    (mongodb-query-map values
	  (mongodb-database-query database "collection" '()))))
;; -> list of SBSON documents
```


#### Query result generators

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


#### Query result fold, map and for-each

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


### Command execution

- `(mongodb-database-insert-command database collection documents . options)`

Executes *insert* command on the `collection` of the `database`.

The `documents` must be a vector of the *documents* field described 
in the official munual [insert](https://docs.mongodb.com/manual/reference/command/insert/)

The `options` is the optional fields of the *insert* command.


- `(mongodb-database-update-command database collection updates . options)`

Executes *update* command on the `collection` of the `database`.

The `updates` must be a vector of the *updates* field described 
in the official munual [update](https://docs.mongodb.com/manual/reference/command/update/)

The `options` is the optional fields of the *update* command.


- `(mongodb-database-delete-command database collection deletes . options)`

Executes *delete* command on the `collection` of the `database`.

The `deletes` must be a vector of the *deletes* field described 
in the official munual [delete](https://docs.mongodb.com/manual/reference/command/delete/)

The `options` is the optional fields of the *delete* command.


- `(mongodb-database-run-command database command)`

Executes the given `command` on the given `database`.


- `(mongodb-connection-run-command connection command)`
- `(mongodb-database-admin-command database command)`

Executes the given `command` on the *admin* database.

The first form uses the given MongoDB connection object `connection`.
And the second form uses the given MongoDB database object `database`.

The `command` must be complied with the specification written in the
official document [Database Commands](https://docs.mongodb.com/manual/reference/command/).

## Conditions

The library only exports predicate of the conditions. If you need to
extend one of the conditions, then you need to refer the 
[Developers' Reference](DevelopersReference.md)

- `(bson-error? obj)`

Return `#t` if the given `obj` is a condition of `&bson-error`

`&bson-error` is the condition related to BSON read or write.


- `(mongodb-error? obj)`

Return `#t` if the given `obj` is a condition of `&mongodb`.

`&mongodb` is the base condition of the MongoDB operation related
condition.


- `(mongodb-connection-error? obj)`

Return `#t` if the given `obj` is a condition of `&mongodb-connection`.

`&mongodb-connection` is the base condition of the MongoDB connection 
operation related condition.


- `(mongodb-connection-closed? obj)`

Return `#t` if the given `obj` is a condition of `&mongodb-connection-closed`.

`&mongodb-connection-closed` represents a connection is closed.

- `(mongodb-invalid-cursor? obj)`

Return `#t` if the given `obj` is a condition of `&mongodb-invalid-cursor`.

`&mongodb-invalid-cursor` represents that specified cursor was invalid.


- `(mongodb-query-failure? obj)`

Return `#t` if the given `obj` is a condition of `&mongodb-query-failure`.

`&mongodb-query-failure` represents that a query request failed.
