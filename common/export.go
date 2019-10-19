package common

type Export struct {
	id   Identifier
	role Role
}

func NewExport(id Identifier, role Role) Export {
	return Export{id, role}
}
