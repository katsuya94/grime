package runtime

type Runtime struct {
	libraryBindings []libraryBinding
}

func NewRuntime() *Runtime {
	return &Runtime{}
}

func (i *Runtime) Bind(*Library) {

}

type libraryBinding struct {
	library        *Library
	implementation *LibraryInstance
}
