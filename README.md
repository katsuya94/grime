# grime

## TODO

* [ ] Add syntax lexemes to the lexer/reader
* [ ] Implement syntax hygiene
* [ ] Implement rnrs (6) library
* [ ] Handle hex literals in ids
* [ ] Improve error messaging
* [ ] Track error line/col
* [ ] Replace references to common with .
* [ ] 100% test coverage
* [ ] Documentation
* [ ] Implement Clojure-style keywords
* [ ] Implement call/cc
* [ ] Implement lambda
* [ ] Implement REPL with continuations
* [ ] Use a better data structure for bindings
* [ ] Make command line utility for generating bindings for arbitrary go packages

## Notes

* https://github.com/racket/r6rs is the only explicit implementation of r6rs found so far
* Implementation plan
  * Implement syntax-case style pattern matching
  * Use pattern matching directly to implement core functionality
  * Refactor into library implemenation
  * Define syntax-rules in terms of syntax-case