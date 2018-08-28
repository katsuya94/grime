package common

type Implementation struct {
	libraryBindings []libraryBinding
}

func NewImplementation() *Implementation {
	return &Implementation{}
}

func (i *Implementation) Bind(*Library) {

}

type libraryBinding struct {
	library *Library
	implementation *LibraryInstance
}
