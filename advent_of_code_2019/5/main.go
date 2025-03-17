package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func Parse(line string) []int {
	words := strings.Split(line, ",")
	var nums []int
	for _, w := range words {
		n, _ := strconv.Atoi(w)
		nums = append(nums, n)
	}
	return nums
}

func Step(ops []int, pos, input int) (newOps []int, nextPos, output int, didOutput bool) {
	newOps = ops
	didOutput = false
	op := ops[pos]
	switch {
	case op%100 == 99:
		nextPos = -1
	case op%100 <= 2: // 0001, 0002, ..., 1101, 1102
		paramA := ops[pos+1]
		if (op/100)%2 == 0 { // op is x0xx => position mode for 1st parameter
			paramA = ops[paramA]
		}
		paramB := ops[pos+2]
		if op < 1000 { // op is 0xxx => position mode for 2nd parameter
			paramB = ops[paramB]
		}
		var res int
		if op%10 == 1 { // op is xxx1 => addition
			res = paramA + paramB
		} else if op%10 == 2 { // op is xxx2 => multiplication
			res = paramA * paramB
		} else {
			fmt.Printf("Unknown operation at position %d: %d", pos, op)
			panic("Unknown operation")
		}
		dest := ops[pos+3]
		ops[dest] = res
		nextPos = pos + 4

	case op%100 == 3:
		dest := ops[pos+1]
		newOps[dest] = input
		nextPos = pos + 2
	case op%100 == 4:
		param := ops[pos+1]
		if op < 10 { // opcode was (0)4 => position mode
			param = ops[param]
		}
		didOutput = true
		output = param
		nextPos = pos + 2
	case op%100 <= 6: // jump-if-true or jump-if-false, 2 parameters
		paramA := ops[pos+1]
		if (op/100)%2 == 0 { // op is x0xx => position mode for 1st parameter
			paramA = ops[paramA]
		}
		paramB := ops[pos+2]
		if op < 1000 { // op is 0xxx => position mode for 2nd parameter
			paramB = ops[paramB]
		}
		if (op%100 == 5 && paramA != 0) || (op%100 == 6 && paramA == 0) { // Jump
			nextPos = paramB
		} else {
			nextPos = pos + 3
		}
	case op%100 <= 8: // less than (opcode 7), or equals (opcode 8), takes 2 parameters and stores a result
		paramA := ops[pos+1]
		if (op/100)%2 == 0 { // op is x0xx => position mode for 1st parameter
			paramA = ops[paramA]
		}
		paramB := ops[pos+2]
		if op < 1000 { // op is 0xxx => position mode for 2nd parameter
			paramB = ops[paramB]
		}
		res := 0
		if (op%100 == 7 && paramA < paramB) || (op%100 == 8 && paramA == paramB) {
			res = 1
		}
		dest := ops[pos+3]
		ops[dest] = res
		nextPos = pos + 4

	}
	return
}

func Run(ops []int, input int) ([]int, []int) {
	pos := 0
	newOps := make([]int, len(ops))
	var output int
	var didOutput bool
	copy(newOps, ops)
	outputs := []int{}
	for {
		newOps, pos, output, didOutput = Step(newOps, pos, input)
		// fmt.Println(newOps, pos)
		if didOutput {
			// fmt.Println(output)
			outputs = append(outputs, output)
		}
		if pos < 0 {
			break
		}
	}
	return newOps, outputs
}

func PartOne(ops []int) {
	_, outputs := Run(ops, 1)
	fmt.Println("Done, outputs =", outputs)
}

func PartTwo(ops []int) {
	_, outputs := Run(ops, 5)
	fmt.Println("Done, outputs =", outputs)
}

func main() {
	fmt.Println("Day 5")

	b, _ := os.ReadFile("input.txt")
	ops := Parse(string(b))
	PartOne(ops)
	PartTwo(ops)
}
