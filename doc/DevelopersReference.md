# R6RS MongoDB library Developers' refernece

This document describes insight of this library and possible extensions.

## Library structure

The following shows the dependency structure of the libraries (duplicates are
the same libraries). The deeper gets the lower the layers.

- `(mongodb)`
  - `(mongodb conditions)`
  - `(mongodb connection)`
    - `(mongodb conditions)`
    - `(mongodb net socket)`
	- `(mongodb net tcp)`
	- `(mongodb protocol)`
  - `(mongodb database)`
    - `(mongodb bson)`
    - `(mongodb conditions)`
    - `(mongodb connection)`
    - `(mongodb protocol)`
    - `(mongodb util parameters)`
  - `(mongodb bson)`
    - `(mongodb conditions)`
    - `(mongodb bson parser)`
   	  - `(mongodb bson conditions)`
   	  - `(mongodb bson validators)`
   	  - `(mongodb util bytevectors)`
   	  - `(mongodb util iso-date)`
   	  - `(mongodb util parameters)`
   	  - `(mongodb util ports)`
   	  - `(mongodb util uuid)`
    - `(mongodb bson writer)`
   	  - `(mongodb bson conditions)`
   	  - `(mongodb bson validators)`
   	  - `(mongodb util bytevectors)`
   	  - `(mongodb util iso-date)`
   	  - `(mongodb util ports)`
   	  - `(mongodb util uuid)`

The below sections describe the APIs defined in the libraries.

NOTE: I don't describe `(mongodb util *)` and `(mongodb bson *)` libraries.
These are purely utilities.

## `(mongodb conditions)`

This library provides the base condition.

- `&mongodb`

The base condition of the MongoDB related operations. Users can extend
this condition to implement own condition for library extensions.


- `(make-mongodb-error)`

Creates an instance of `&mongodb`.


The following procedures are the same as `(mongodb)` (re-exported)

- `mongodb-error?`

## `(mongodb connection)`

This library provides the connection object and connection option operations.

- `(mongodb-connection-input-port connection)`

Returns binary input port associated to the given `connection`.

Reading data from this port requires deep knowledge of the wire protocol
of MongoDB. And closing this port may cause unexpected behaviour. 

This is only relevant for library developers.

- `(mongodb-connection-output-port connection)`

Returns binary output port associated to the given `connection`.

Writing data to this port requires deep knowledge of the wire protocol
of MongoDB. And closing this port may cause unexpected behaviour. 

This is only relevant for library developers.


- `(mongodb-connection-option connection)`

Returns MongoDB connection option associated to the given `connection`.

- `(mongodb-connection-option-use-iso-date? option)`

Returns value of the *use-iso-date?* field of the given `option`.

- `&mongodb-connection`

A condition represents connection error. This inherits the `&mongodb`.


- `(make-mongodb-connection-error)`

Creates an instance of `&mongodb-connection`.


- `&mongodb-connection-closed`

A condition represents that a connection is closed. 
This inherits the `&mongodb-connection`.


- `(make-mongodb-connection-closed)`

Creates an instance of `&mongodb-connection-closed`.

- `(mongodb-connection-request-id-strategy connection)`

Returns the request id strategy procedure associated to the given `connection`.


The following procedures are the same as `(mongodb)` (re-exported)

- `mongodb-connection?`
- `make-mongodb-connection`
- `open-mongodb-connection!`
- `close-mongodb-connection!`
- `mongodb-connection-open?`
- `mongodb-connection-list-databases`
- `mongodb-connection-option?`
- `make-mongodb-connection-option`
- `make-mongodb-connection-option-default-request-id-strategy`
- `mongodb-connection-error?`
- `mongodb-connection-closed?`

## `(mongodb database)`

This library provides the database object operations.

- `(mongodb-database-name database)`

Returns the *name* field of the given `database`.


- `(mongodb-database->namespace database collection)`

Returns a string represents fully qualified collection name (namespace)
constructed from the given `database` and `collection`.

The `collection` can be a string, a list of string, empty list or `#f`.

```scheme
(define database (make-mongodb-database connection "test"))

(mongodb-database->namespace database "collection")
;; -> "test.collection"

(mongodb-database->namespace database '("collection" "user"))
;; -> "test.collection.user"

(mongodb-database->namespace database '())
;; -> "test"

(mongodb-database->namespace database #f)
;; -> "test"
```


- `(mongodb-database-query-request database collection number-to-skip number-to-return query return-fields-selector)`

Sends *OP_QUERY* message to the given `database` and returns query result.

The `collection` must be a name of the querying collection.

The `number-to-skip` and `number-to-return` must be a 32 bit integer.

The `query` must be a SBSON object represents MongoDB query.

The `return-fields-selector` must be either a vector of strings represents
field names of the returning documents' field or #f.

For more detail, see [OP_QUERY](https://docs.mongodb.com/manual/reference/mongodb-wire-protocol/#wire-op-query)

- `(mongodb-database-insert-request database collection flags documents)`

Sends *OP_INSERT* message to the given `database`.

The `collection` must be a name of the inserting collection.

The `flags` must a 32 bit integer.

The `documents` must be a vector of SBSON.

For more detail, see [OP_INSERT](https://docs.mongodb.com/manual/reference/mongodb-wire-protocol/#wire-op-insert)


- `(mongodb-database-update-request database collection flags selector update)`

Sends *OP_UPDATE* message to the given `database`.

The `collection` must be a name of the updating collection.

The `flags` must a 32 bit integer.

The `selector` must be an SBSON of query selector.

The `update` must be an SBSON of updates.

For more detail, see [OP_UPDATE](https://docs.mongodb.com/manual/reference/mongodb-wire-protocol/#wire-op-update)


- `(mongodb-database-delete-request database collection flags selector)`

Sends *OP_DELETE* message to the given `database`.

The `collection` must be a name of the deleting collection.

The `flags` must a 32 bit integer.

The `selector` must be an SBSON of query selector.

For more detail, see [OP_DELETE](https://docs.mongodb.com/manual/reference/mongodb-wire-protocol/#wire-op-delete)


- `(mongodb-database-send-query database collection number-to-skip number-to-return query return-fields-selector)`

Sends *OP_QUERY* message to the given `database` and returns request id.

The arguments are the same as `mongodb-database-query-request`.


- `(mongodb-database-receive-reply database)`

Receives reply from the given `database` and returns query result.

It expects to be a *OP_REPLY* message.

- `(mongodb-database-send-get-more query number-to-return)`

Sends *OP_GET_MORE* to the database associated to the given `query`.

The `number-to-return` specifies to the number of returning documents.


### Cursors

- `(make-mongodb-cursor id ns database)`
- `(mongodb-cursor-database cursor)`


The following procedures are the same as `(mongodb)` (re-exported)

- `mongodb-database?`
- `make-mongodb-database`
- `mongodb-database-query`
- `mongodb-database-query/selector`
- `mongodb-database-insert`
- `mongodb-database-update`
- `mongodb-database-upsert`
- `mongodb-database-update-all`
- `mongodb-database-delete`
- `mongodb-database-delete-all`
- `mongodb-database-get-last-error`
- `mongodb-database-drop-collection`
- `mongodb-cursor? make-mongodb-cursor`
- `mongodb-cursor-id mongodb-cursor-ns`
- `mongodb-query-result?`
- `mongodb-query-result-id`
- `mongodb-query-result-to`
- `mongodb-query-result-starting-from`
- `mongodb-query-result-documents`

## `(mongodb bson)`

This library provides the BSON read and write operations.

- `(bson-read)`
- `(bson-read input-port)`

Reads a BSON document from the given `input-port` and returns an SBSON object.

The `input-port` must be binary input port.

If the first form is used, then it uses `(current-input-port)` as its
input.


- `(bson-write sbson)`
- `(bson-write sbson output-port)`

Writes the given SBSON object `sbson` to the `output-port`.

The `output-port` must be a binary output port.

If the first form is used, then it uses `(current-output-port)` as its
output

- `*bson:use-iso-date?*`

Parameter to specify whether or not the reader should convert UTC datetime to 
ISO date string.

The default value is `#f` means don't convert.

The following procedures are the same as `(mongodb)` (re-exported)

- `bson-error?`


## `(mongodb protocol)`

- `(read-mongodb-message input-port)`

Reads a MongoDB message from the given `input-port`.

The `input-port` must be a binary input port.

Even though MongoDB servers return only *OP_REPLY*, the procedure reads
the supported messages as well.


- `(write-mongodb-message output-port message)`

Writes the given `message` to the given `output-port`

The `message` must be a MongoDB message.

The `output-port` must be a binary output port.

### MongoDB messages

MongoDB messages represents the wire protocol messages. It's really rare for
users to care about this. So this document won't describe them.

If you need to know what it is, please see the source code directly.

All messages are located under the `src/mongodb/protocol` directory.


## `(mongodb net socket)`

This library provides socket wrapper.

- `(socket? obj)`

Returns `#t` if the given `obj` is a socket wrapper, otherwise `#f`.


- `(socket-close! socket)`

Closes the given `socket` in implementation dependent maner.


- `(socket-input-port socket)`

Returns a binary input port associated to the given `socket`.


- `(socket-output-port socket)`

Returns a binary output port associated to the given `socket`.


- `(socket-raw-socket socket)`

Returns a raw socket, implementation dependent socket,  associated 
to the given `socket`.


- `(make-socket raw-socket closer input-port output-port)`

Creates a socket wrapper.

The `closer` must be a thunk which closes the `raw-socket` with the
implementation dependent manner.

The following example shows how to create a socket wrapper using SRFI-106:

```scheme
(import (rnrs)
        (prefix (srfi :106) srfi:)
		(mongodb net socket))

(let ((s (srfi:make-client-socket host service)))
  (make-socket s
               (lambda ()
                 (srfi:socket-shutdown s srfi:*shut-rdwr*)
                 (srfi:socket-close s))
               (srfi:socket-input-port s)
               (srfi:socket-output-port s)))
```

## `(mongodb net tcp)`

This library provides implementation dependent manner of socket createions.

If you need to support a new implementation, you may need to write this
library with the name of `tcp.${implemenation}.sls` under the
`src/mongodb/net/` directory. 

For example, Chez Scheme's file is like this:
`src/mongodb/net/tcp.chezscheme.sls`.

The library must implement the following procedure:

- `(tcp-connect host service)`

The `host` is a string represent the host name to connect.

The `service` is a string represent the service name (or port number)
to connect.

If there's no implementation dependent file, then it uses vanilla 
implementation which uses SRFI-106.
