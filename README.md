# grime

## TODO

* [ ] Implement syntax hygiene
* [ ] Implement rnrs (6) library
* [ ] Handle hex literals in ids
* [ ] Improve error messaging
* [ ] Track error line/col
* [ ] Replace references to common with .
* [ ] 100% test coverage
* [ ] Documentation
* [ ] Implement Clojure-style keywords
* [ ] Implement REPL with continuations
* [ ] Make command line utility for generating bindings for arbitrary go packages
* [ ] Macro transformers shouldn't be responsible for expanding their own subforms, instead the expander should know how to expand the resulting core forms. This will allow us to pass an isolated environment into native callables
* [ ] Use consistent coding style with success cases in tail-position
* [ ] Implement variadic lambdas
* [ ] resolve references at expand time
* [ ] Consider removing error handling in match in favor of a panic
* [ ] Consider adding match result type for easy result retrieval

## Notes

* https://github.com/racket/r6rs is the only explicit implementation of r6rs found so far
* Implementation plan
  * Implement syntax-case style pattern matching
  * Use pattern matching directly to implement core functionality
  * Refactor into library implemenation
  * Define syntax-rules in terms of syntax-case