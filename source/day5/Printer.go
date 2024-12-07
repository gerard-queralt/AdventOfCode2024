package main

import (
	"filereader"
	"fmt"
	"regexp"
	"slices"
	"strconv"
	"strings"
)

type OrderingMap = map[int][]int
type Update = []int

func sumMiddlePageOfUpdates(i_updates []Update) int {
	result := 0
	for _, update := range i_updates {
		result += update[len(update)/2]
	}
	return result
}

func findValidUpdates(i_orderingMap OrderingMap, i_updates []Update) []Update {
	validUpdates := make([]Update, 0)

	for _, update := range i_updates {
		isValid := true
		for index, page := range update {
			previousPages := update[:index]
			for _, previousPage := range previousPages {
				if slices.Contains(i_orderingMap[page], previousPage) {
					// one of the pages that must be after was found before!
					isValid = false
					break
				}
			}
			if !isValid {
				break
			}
		}

		if isValid {
			validUpdates = append(validUpdates, update)
		}
	}

	return validUpdates
}

func createUpdateSlice(i_updatesAsString []string) []Update {
	updatesSlice := make([][]int, len(i_updatesAsString))
	for i, update := range i_updatesAsString {
		pages := make([]int, 0)

		for _, page := range strings.Split(update, ",") {
			pageAsInt, err := strconv.Atoi(page)
			if err != nil {
				panic(err)
			}
			pages = append(pages, pageAsInt)
		}

		updatesSlice[i] = pages
	}
	return updatesSlice
}

func createOrderingMap(i_orderingSlice []string) OrderingMap {
	orderingMap := make(OrderingMap)

	for _, orderingPair := range i_orderingSlice {
		splitPair := strings.Split(orderingPair, "|")
		predecessor, err := strconv.Atoi(splitPair[0])
		if err != nil {
			panic(err)
		}
		successor, err := strconv.Atoi(splitPair[1])
		if err != nil {
			panic(err)
		}
		orderingMap[predecessor] = append(orderingMap[predecessor], successor)
	}

	return orderingMap
}

func splitInputIntoSlices(i_input string) (orderingSlice, updateSlice []string) {
	inputSanitized := regexp.MustCompile(`\r\n`).ReplaceAllString(i_input, "\n")

	emptyNewLineRegex := regexp.MustCompile(`\n\s*\n`)
	splitSlices := emptyNewLineRegex.Split(inputSanitized, -1)

	orderingSlice = strings.Split(splitSlices[0], "\n")
	updateSlice = strings.Split(splitSlices[1], "\n")
	return orderingSlice, updateSlice
}

func processInput(i_input []byte) (OrderingMap, []Update) {
	orderingStrings, updateStrings := splitInputIntoSlices(string(i_input))
	return createOrderingMap(orderingStrings), createUpdateSlice(updateStrings)
}

func main() {
	orderingMap, updateSlice := processInput(filereader.ReadInputOfDay(5))
	validUpdates := findValidUpdates(orderingMap, updateSlice)
	fmt.Printf("Part 1: %d\n", sumMiddlePageOfUpdates(validUpdates))
}
