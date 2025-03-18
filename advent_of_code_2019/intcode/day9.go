package main

import (
	"fmt"
	"os"
)

func PartOneDay9(program []int) int {
	a := SimpleRun(program, []int{1}, true)

	return <-a.Outputs
}

func PartTwoDay9(program []int, verbose bool) int {
	a := SimpleRun(program, []int{2}, true)

	return <-a.Outputs
}

func RunDay9() {
	fmt.Println("Day 9")

	b, _ := os.ReadFile("input9.txt")
	input := Parse(string(b))
	fmt.Printf("Part one: %d\n", PartOneDay9(input))
	fmt.Printf("Part two: %d\n", PartTwoDay9(input, false))
}
