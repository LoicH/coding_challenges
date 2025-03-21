package main

import (
	"fmt"
	"os"
)

func PartOneDay7(program []int) int {
	max_signal := 0
	for _, phase_sequence := range Permutations([]int{0, 1, 2, 3, 4}) {
		// Simply chain 5 amplifiers with the given phase sequence
		a := SimpleRun(program, []int{phase_sequence[0], 0}, true)
		outA := <-a.Outputs
		b := SimpleRun(program, []int{phase_sequence[1], outA}, true)
		outB := <-b.Outputs
		c := SimpleRun(program, []int{phase_sequence[2], outB}, true)
		outC := <-c.Outputs
		d := SimpleRun(program, []int{phase_sequence[3], outC}, true)
		outD := <-d.Outputs
		e := SimpleRun(program, []int{phase_sequence[4], outD}, true)
		if outE := <-e.Outputs; outE >= max_signal {
			fmt.Printf("New signal found with settings %d: %d\n", phase_sequence, outE)
			max_signal = outE
		}

	}
	return max_signal
}

func PartTwoDay7(program []int, verbose bool) int {
	maxSignal := 0
	phaseSettings := []int{5, 6, 7, 8, 9}

	for _, phaseSeq := range Permutations(phaseSettings) {
		if output := RunCircular(program, phaseSeq, verbose); output > maxSignal {
			if verbose {
				fmt.Printf("New maximum signal found: %d with phase sequence %v\n", output, phaseSeq)
			}
			maxSignal = output
		}
	}

	if verbose {
		fmt.Printf("Final maximum signal: %d\n", maxSignal)
	}
	return maxSignal
}

func RunDay7() {
	fmt.Println("Day 7")

	b, _ := os.ReadFile("input7.txt")
	input := Parse(string(b))
	fmt.Printf("Part one: %d\n", PartOneDay7(input))
	fmt.Printf("Part two: %d\n", PartTwoDay7(input, false))
}
