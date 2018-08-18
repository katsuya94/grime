# grime

## TODO

* [x] Refactor reader to use ok, err semantics for backtracking
* [ ] Implement macro expansion
* [ ] Add syntax lexemes to the lexer/reader
* [ ] Use shared structs for read/eval
* [ ] Write documentation with references to the r6rs specification
* [ ] Implement rnrs (6) library
* [ ] Implement print methods for data
* [ ] Handle hex literals in ids
* [ ] Improve error messaging
* [ ] Track error line/col

## Notes

* https://github.com/racket/r6rs is the only explicit implementation of r6rs found so far
* Implementation plan
  * Implement syntax-case style pattern matching
  * Use pattern matching directly to implement core functionality
  * Refactor into library implemenation
  * Define syntax-rules in terms of syntax-case