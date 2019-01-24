package gen

type goImportSetSubtoken struct {
	rest  []string
	token *goImportSetToken
}

type goImportSetToken struct {
	subtokens map[string]*goImportSetSubtoken
}

func (token *goImportSetToken) add(parts []string) {
	if token.subtokens == nil {
		token.subtokens = map[string]*goImportSetSubtoken{}
	}
	last := parts[len(parts)-1]
	rest := parts[0 : len(parts)-1]
	other, ok := token.subtokens[last]
	if ok {
		if other.token.subtokens == nil {

		}
	} else {
		token.subtokens[last] = &goImportSetSubtoken{rest, &goImportSetToken{nil}}
	}
}

func (token *goImportSetToken) get(parts []string) []string {
	last := parts[len(parts)-1]
	rest := parts[0 : len(parts)-1]
	if token.subtokens == nil {
		return []string{}
	}
	return append(token.subtokens[last].get(rest), last)
}

func (token *goImportSetToken) paths() [][]string {
	if token.subtokens == nil {
		return [][]string{token.rest}
	}
	paths := [][]string{}
	for part, subtoken := range token.subtokens {
		subpaths := subtoken.paths()
		for _, subpath := range subpaths {
			paths = append(paths, append(subpath, part))
		}
	}
	return paths
}
