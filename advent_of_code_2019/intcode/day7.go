package main

import (
	"fmt"
	"os"
)

func PartOne(program []int) int {
	max_signal := 0
	// for _, phase_sequence := range Permutations([]int{0, 1, 2, 3, 4}) {
	// 	_, outA := Run(program, []int{phase_sequence[0], 0})
	// 	_, outB := Run(program, []int{phase_sequence[1], outA[0]})
	// 	_, outC := Run(program, []int{phase_sequence[2], outB[0]})
	// 	_, outD := Run(program, []int{phase_sequence[3], outC[0]})
	// 	_, outE := Run(program, []int{phase_sequence[4], outD[0]})
	// 	if outE[0] >= max_signal {
	// 		fmt.Printf("New signal found with settings %d: %d\n", phase_sequence, outE[0])
	// 		max_signal = outE[0]
	// 	}

	// }
	return max_signal
}

func PartTwo(program []int) int {
	max_signal := 0
	for _, phase_sequence := range Permutations([]int{5, 6, 7, 8, 9}) {
		output_e := RunCircular(program, phase_sequence)
		if output_e >= max_signal {
			fmt.Printf("New signal found with settings %d: %d\n", phase_sequence, output_e)
			max_signal = output_e
		}

	}
	return max_signal
}

func main() {
	fmt.Println("Day 7")

	b, _ := os.ReadFile("input7.txt")
	input := Parse(string(b))
	fmt.Printf("Part two: %d", PartTwo(input))
}
