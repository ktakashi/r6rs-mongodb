# BSON in S-expression

NOTE: SBSON (S-expression BSON) is the term I made. So it's not used anywhere
in the world but here.

BSON is the binary format used by MongoDB. It's similar to JSON. Since it's
a binary format and human can't read binary easily, I'v created a new format
of BSON for Scheme.

## Specification

The following is the ABNF of SBSON.

```abnf
document ::= "(" *element ")"
element  ::= "(" e-name e-value ")"
e-name   ::= cstring
e-value  ::= 'min-key'                 ; BSON Min Key
           / 'max-key'                 ; BSON Max Key
           / <number>                  ; BSON 64-bit binary floating point
           / <string>                  ; BSON UTF-8 string
           / document
           / array
           / binary                    ; BSON Binary
           / "(" 'uuid' uuid ")"       ; BSON UUID subtype binary
           / 'undefine'                ; BSON undefine value
           / #t                        ; BSON Boolean true
           / #f                        ; BSON Boolean false
           / "(" 'utc-datetime' <integer> ")" ; BSON UTC datetime in millis
           / "(" 'iso-date' ISODate ")" ; BSON UTC datatime in ISO format
           / 'null'                    ; BSON Null value
           / "(" 'regex' regex flags ") | BSON regular experssion
           / "(" 'db-pointer' <string> <bytevector> ")" ; BSON DBPointer
           / "(" 'javascript' <string> ")" ; BSON JavaScript code
           / "(" 'symbol' <string> ")" ; BSON Symbol
           / "(" 'javascript/scope' <string> document ")" ; BSON JavaScript code w/ scope
           / "(" 's32' <32-bit-exact-integer> ")" ; BSON 32-bit integer
           / "(" 'u64' <64-bit-exact-integer> ")" ; BSON Timestamp (64-bit unsigned integer)
           / "(" 's64' <64-bit-exact-integer> ")" ; BSON 64-bit integer

array   ::= "#(" *e-value ")"          ; Vector of e-value
binary  ::= "(" 'binary' subtype <bytevector> ")"
subtype ::= %x00 / %x01 / %x02 / %x03 / %x04 / %x05 / %x80
cstring ::= <string>                   ; Must only contain ASCII
uuid    ::= String representation of UUID
ISODate ::= yyyy "-" MM "-" dd
          / yyyy "-" MM "-" dd "T" hh ":" mm ":" ss
          / yyyy "-" MM "-" dd "T" hh ":" mm ":" ss "Z"
          / yyyy "-" MM "-" dd "T" hh ":" mm ":" ss "." mi
          / yyyy "-" MM "-" dd "T" hh ":" mm ":" ss "." mi "Z"
regex   ::= <string>
flags   ::= ["i"] ["l"] ["m"] ["s"] ["u"] ["x"]
yyyy    ::= 4DIGIT
MM      ::= 2DIGIT
dd      ::= 2DIGIT
hh      ::= 2DIGIT
mm      ::= 2DIGIT
ss      ::= 2DIGIT
mi      ::= 1*DIGIT

; <type> notations are Scheme types.
; Double quotation means string. Single quotation means symbol
; <bytevector> in 'db-pointer' must be of length 12
; Decimal 128 is not supported yet.

```

The number `e-value` is always treated as double. Thus the following SBSON 
document's value would be stored as double. If you need this to be exact
integer, use `s32`, `u64` or `s64` notation.

```scheme
'(("number" 1))
```

## Reference

- [BSON](http://bsonspec.org/spec.html)
