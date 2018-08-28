package common

type Library struct {
	name []Symbol
	version []int
	importSpecs []importSpec
	exportSpecs []exportSpec
	body []Datum
}

type importSpec{}
type exportSpec{}

type LibraryInstance struct {

}