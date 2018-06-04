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

TBD

## `(mongodb connection)`

TBD

## `(mongodb database)`

TBD

## `(mongodb bson)`

TBD

## `(mongodb protocol)`

TBD

## `(mongodb net socket)`

TBD

## `(mongodb net tcp)`

TBD
