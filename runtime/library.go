package runtime

import "github.com/katsuya94/grime/common"

type Library struct {
	name        []common.Symbol
	version     []int
	importSpecs []importSpec
	exportSpecs []exportSpec
	body        []common.Datum
}

func NewLibrary(source common.Datum) (*Library, error) {
	return &Library{}, nil
}

type importSpec struct{}
type exportSpec struct{}

type LibraryInstance struct {
}
