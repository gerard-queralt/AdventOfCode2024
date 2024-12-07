package filereader

import (
	"fmt"
	"os"
)

func ReadInputOfDay(day int) []byte {
	data, err := os.ReadFile(fmt.Sprintf("../day%d/input.txt", day))
	if err != nil {
		panic(err)
	}
	return data
}
