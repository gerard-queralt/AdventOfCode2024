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

func fixUpdate(i_orderingMap OrderingMap, i_updateToFix Update) Update {
	for !isUpdateValid(i_orderingMap, i_updateToFix) {
		for index, page := range i_updateToFix {
			previousPages := i_updateToFix[:index]
			for previousPageIndex, previousPage := range previousPages {
				if slices.Contains(i_orderingMap[page], previousPage) {
					// one of the pages that must be after was found before!
					// first, remove it from the slice
					i_updateToFix = append(i_updateToFix[:previousPageIndex], i_updateToFix[previousPageIndex+1:]...)
					// then add it after the "page"
					i_updateToFix = append(i_updateToFix[:index], append([]int{previousPage}, i_updateToFix[index:]...)...)
				}
			}
		}
	}
	return i_updateToFix
}

func sumMiddlePageOfUpdates(i_updates []Update) int {
	result := 0
	for _, update := range i_updates {
		result += update[len(update)/2]
	}
	return result
}

func isUpdateValid(i_orderingMap OrderingMap, i_update Update) bool {
	for index, page := range i_update {
		previousPages := i_update[:index]
		for _, previousPage := range previousPages {
			if slices.Contains(i_orderingMap[page], previousPage) {
				// one of the pages that must be after was found before!
				return false
			}
		}
	}
	return true
}

func findValidAndInvalidUpdates(i_orderingMap OrderingMap, i_updates []Update) (validUpdates, invalidUpdates []Update) {
	validUpdates = make([]Update, 0)
	invalidUpdates = make([]Update, 0)

	for _, update := range i_updates {
		if isUpdateValid(i_orderingMap, update) {
			validUpdates = append(validUpdates, update)
		} else {
			invalidUpdates = append(invalidUpdates, update)
		}
	}

	return validUpdates, invalidUpdates
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

	validUpdates, invalidUpdates := findValidAndInvalidUpdates(orderingMap, updateSlice)
	fmt.Printf("Part 1: %d\n", sumMiddlePageOfUpdates(validUpdates))

	fixedUpdates := make([]Update, len(invalidUpdates))
	for i, update := range invalidUpdates {
		fixedUpdates[i] = fixUpdate(orderingMap, update)
	}
	fmt.Printf("Part 2: %d\n", sumMiddlePageOfUpdates(fixedUpdates))
}
