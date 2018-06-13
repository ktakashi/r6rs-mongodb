# MongoDB binding for R6RS

[![Build Status](https://travis-ci.org/ktakashi/r6rs-mongodb.svg?branch=master)](https://travis-ci.org/ktakashi/r6rs-mongodb)

This is an R6RS Scheme implementation of MongoDB wire protocol.

# How to use

The most of users should use only `(mongodb)` library which contains
MongoDB connection, database and CRUD operations. The following piece of
code shows the simple CRUD operation with this library:

```scheme
(import (rnrs)
        (mongodb))

(define connection 
  (open-mongodb-connection! (make-mongodb-connection "localhost")))
(define database (make-mongodb-database connection "test"))

(define collection "testCollection")

(mongodb-database-insert database collection
  '#(
     (("id" 1) ("name" "R6RS MongoDB library") ("lang" "Scheme")))
     )
(mongodb-database-upsert database collection
  '(("name" "R7RS PostgreSQL library"))
  '(("$set"
     (("id" 2) ("name" "R7RS PostgreSQL library") ("lang" "Scheme")))))

(mongodb-database-update database collection
  '(("id" 1))
  '(("$set" (("lang" "R6RS Scheme")))))
(mongodb-database-update-all database collection
  '(("id" (("$gt" 0))))
  '(("$set" (("comment" "Portable Scheme library")))))

(mongodb-query-for-each (lambda (doc) (write doc) (newline))
  (mongodb-database-query database collection '()))

(mongodb-database-delete-all database collection '())

(display
 (mongodb-query-result-documents
  (mongodb-database-query database collection '()))) (newline)

(close-mongodb-connection! connection)

```
The above code prints more or less like this output (formatted for readability):
```
(("_id" (object-id "5b1110aa9614bc720de985b6"))
 ("id" 1)
 ("name" "R6RS MongoDB library")
 ("lang" "R6RS Scheme")
 ("comment" "Portable Scheme library"))
(("_id" (object-id "5b1110aa9614bc720de985b8"))
 ("id" 2)
 ("name" "R7RS PostgreSQL library") 
 ("lang" "Scheme")
 ("comment" "Portable Scheme library"))
#()
```

# Documents

- [Users' reference](doc/UsersReference.md)
- [SBSON](doc/SBSON.md)
- [Developers' reference](doc/DevelopersReference.md)

# Supporting implementations

The following implementations are tested:

- Sagittarius 0.9.2
- Larceny 1.3
- Chez Scheme v9.5

## For Chez Scheme

Chez Scheme requires the shared object made of
`src/mongodb/net/tcp/chez.c` file which provides simple socket
interface for Chez Scheme. The file is written C however, it can only
be compiled on POSIX environment (only tested on Ubuntu). The build
command is the following:

```shell
$ gcc -fPIC -shared -O3 src/mongodb/net/tcp/chez.c \
  -o src/mongodb/net/tcp/chez.so
```

The shared file name must also be the one specified above (even on OS X).

If you have better solution, please send a PR.

# Extension libraries

There're implementation dependent extension libraries as well. These are
located under the [`contrib`](contrib) directory.


# Copyright and lincence

Copyright 2018 Takashi Kato. Code released under the BSD-style license.
See [COPYING](COPYING).
