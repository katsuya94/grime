package common

type Compiler func(env Environment, forms []Datum) (Expression, BindingSet, error)
