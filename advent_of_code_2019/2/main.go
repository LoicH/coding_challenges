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

func Step(ops []int, pos int) (newOps []int, nextPos int) {
	newOps = ops
	switch ops[pos] {
	case 99:
		nextPos = -1
	case 1:
		ia, ib := ops[pos+1], ops[pos+2]
		res := ops[ia] + ops[ib]
		idest := newOps[pos+3]
		newOps[idest] = res
		nextPos = pos + 4
	case 2:
		ia, ib := ops[pos+1], ops[pos+2]
		res := ops[ia] * ops[ib]
		idest := newOps[pos+3]
		newOps[idest] = res
		nextPos = pos + 4
	}
	return
}

func Run(ops []int) []int {
	pos := 0
	newOps := make([]int, len(ops))
	copy(newOps, ops)
	for {
		newOps, pos = Step(newOps, pos)
		// fmt.Println(newOps, pos)
		if pos < 0 {
			break
		}
	}
	return newOps
}

func PartOne(ops []int, noun int, verb int) int {

	newOps := make([]int, len(ops))
	copy(newOps, ops)
	newOps[1] = noun // 12
	newOps[2] = verb // 2
	res := Run(newOps)
	return res[0]
}

func PartTwo(ops []int) int {
	goal := 19690720
	for noun := 0; noun < 100; noun++ {
		for verb := 0; verb < 100; verb++ {
			fmt.Printf("noun=%d, verb=%d", noun, verb)
			res := PartOne(ops, noun, verb)
			fmt.Printf(", res=%d\n", res)
			if res == goal {
				return 100*noun + verb
			}
		}
	}
	return -1
}

func main() {
	// 1st example 1,0,0,0,99
	// example1 := Parse("1,0,0,0,99")
	// fmt.Println(example1)
	// fmt.Println(Run(example1))

	// // 2nd example 2,3,0,3,99
	// ex2 := Parse("2,3,0,3,99")
	// fmt.Println(ex2)
	// fmt.Println(Run(ex2))

	// // 3: 2,4,4,5,99,0
	// ex3 := Parse("2,4,4,5,99,0")
	// fmt.Println(ex3)
	// fmt.Println(Run(ex3))

	// // 4: 1,1,1,4,99,5,6,0,99   =>   30,1,1,4,2,5,6,0,99
	// ex4 := Parse("1,1,1,4,99,5,6,0,99")
	// fmt.Println(ex4)
	// fmt.Println(Run(ex4))

	b, _ := os.ReadFile("input.txt")
	ops := Parse(string(b))
	// fmt.Println(PartOne(ops, 12, 2))
	fmt.Println(PartTwo(ops))
}
