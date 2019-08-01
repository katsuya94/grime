package common

type Location struct {
	value *Datum
}

func NewLocation() Location {
	value := Datum(nil)
	return Location{&value}
}

func (l Location) Get() Datum {
	return *l.value
}

func (l Location) Set(value Datum) {
	*l.value = value
}
