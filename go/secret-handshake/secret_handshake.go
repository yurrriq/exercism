package secret

const reverse = 16

func normalActions() []string {
	return []string{
		"wink",
		"double blink",
		"close your eyes",
		"jump",
	}
}

func Handshake(code uint) []string {
	actions := make([]string, 0, 5)

	var addToSlice func(action string, actions *[]string)

	if hasInstruction(reverse, code) {
		addToSlice = cons
	} else {
		addToSlice = snoc
	}

	for i, action := range normalActions() {
		if hasInstruction(1<<i, code) {
			addToSlice(action, &actions)
		}
	}

	return actions
}

func hasInstruction(instruction uint, code uint) bool {
	return instruction&code == instruction
}

func cons(str string, slice *[]string) {
	*slice = append([]string{str}, *slice...)
}

func snoc(str string, slice *[]string) {
	*slice = append(*slice, str)
}
