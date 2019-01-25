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
	if len(parts) == 0 {
		token[""] = nil
		return
	}
	last := parts[len(parts)-1]
	rest := parts[0 : len(parts)-1]
	existing, ok := token[last]
	if !ok {
		token[last] = &goImportSetSubtoken{rest, nil}
		return
	}
	if existing.token == nil {
		if partsEqual(rest, existing.rest) {
			return
		}
		existing.token = newGoImportSetToken()
		existing.token.add(existing.rest)
		existing.rest = nil
	}
	existing.token.add(rest)
}

func (token goImportSetToken) get(parts []string) []string {
	if len(parts) == 0 {
		_ = token[""]
		return []string{}
	}
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
		if part == "" {
			paths = append(paths, []string{})
		} else if subtoken.token == nil {
			paths = append(paths, append(subtoken.rest, part))
		} else {
			for _, path := range subtoken.token.paths() {
				paths = append(paths, append(path, part))
			}
		}
	}
	return paths
}

func partsEqual(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
