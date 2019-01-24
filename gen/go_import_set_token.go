package gen

type goImportSetSubtoken struct {
	rest  []string
	token goImportSetToken
}

type goImportSetToken map[string]*goImportSetSubtoken

func newGoImportSetToken() goImportSetToken {
	return goImportSetToken{}
}

func (token goImportSetToken) add(parts []string) {
	last := parts[len(parts)-1]
	rest := parts[0 : len(parts)-1]
	existing, ok := token[last]
	if ok {
		if existing.token == nil {
			existing.token = newGoImportSetToken()
			existing.token.add(existing.rest)
		}
		existing.token.add(rest)
	} else {
		token[last] = &goImportSetSubtoken{rest, nil}
	}
}

func (token goImportSetToken) get(parts []string) []string {
	last := parts[len(parts)-1]
	rest := parts[0 : len(parts)-1]
	subtoken := token[last]
	if subtoken.token == nil {
		return []string{last}
	}
	return append(subtoken.token.get(rest), last)
}

func (token goImportSetToken) paths() [][]string {
	paths := [][]string{}
	for part, subtoken := range token {
		if subtoken.token == nil {
			paths = append(paths, append(subtoken.rest, part))
		} else {
			for _, path := range subtoken.token.paths() {
				paths = append(paths, append(path, part))
			}
		}
	}
	return paths
}
