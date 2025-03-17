package main

import (
	"fmt"
	"sync"
)

type Software struct {
	Operations []int
	Position   int
	Inputs     chan int
	Outputs    chan int
	Name       string
}

func NewSoftware(ops []int, inputs chan int) Software {
	opsCopy := make([]int, len(ops))
	copy(opsCopy, ops)
	return Software{
		Operations: opsCopy,
		Inputs:     inputs,
		Position:   0,
		Outputs:    make(chan int, 2), // Increase buffer size to reduce deadlocks
		Name:       "default",
	}
}
func Step(soft *Software) {
	ops := soft.Operations
	// fmt.Printf("[%s] Adresse de Operations: %p\n", soft.Name, &soft.Operations)
	pos := soft.Position
	op := ops[pos]
	switch {
	case op%100 == 99:
		fmt.Printf("[%s] Program finished\n", soft.Name)
		soft.Position = -1
	case op%100 <= 2: // 0001, 0002, ..., 1101, 1102
		paramA := ops[pos+1]
		dest := ops[pos+3]
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
			fmt.Printf("[%s, %d, %d] Adding %d to %d = %d at position %d\n", soft.Name, pos, op, paramA, paramB, res, dest)
		} else if op%10 == 2 { // op is xxx2 => multiplication
			res = paramA * paramB
			fmt.Printf("[%s, %d, %d] Multiplying %d by %d = %d at position %d\n", soft.Name, pos, op, paramA, paramB, res, dest)
		} else {
			fmt.Printf("[%s, %d, %d] Unknown operation at position %d: %d", soft.Name, pos, op, pos, op)
			panic("Unknown operation")
		}
		soft.Operations[dest] = res
		soft.Position = pos + 4

	case op%100 == 3: // input
		dest := ops[pos+1]
		fmt.Printf("[%s, %d, %d] Waiting for input...\n", soft.Name, pos, op)
		in := <-soft.Inputs
		soft.Operations[dest] = in
		fmt.Printf("[%s, %d, %d] Input %d at position %d\n", soft.Name, pos, op, in, dest)
		soft.Position = pos + 2
	case op%100 == 4: // output
		param := ops[pos+1]
		if op < 10 { // opcode was (0)4 => position mode
			param = ops[param]
		}
		soft.Outputs <- param
		fmt.Printf("[%s, %d, %d] Output %d\n", soft.Name, pos, op, param)
		soft.Position = pos + 2
	case op%100 <= 6: // jump-if-true or jump-if-false, 2 parameters
		paramA := ops[pos+1]
		if (op/100)%2 == 0 { // op is x0xx => position mode for 1st parameter
			paramA = ops[paramA]
		}
		paramB := ops[pos+2]
		if op < 1000 { // op is 0xxx => position mode for 2nd parameter
			paramB = ops[paramB]
		}
		fmt.Printf("[%s, %d, %d] op=%d, paramA=%d, paramB=%d\n", soft.Name, pos, op, op, paramA, paramB)
		if (op%100 == 5 && paramA != 0) || (op%100 == 6 && paramA == 0) { // '5' => jump if paramA is not 0, '6' => jump if paramA is 0
			soft.Position = paramB
			fmt.Printf("[%s, %d, %d] Jumping to %d\n", soft.Name, pos, op, paramB)
		} else {
			soft.Position = pos + 3
			fmt.Printf("[%s, %d, %d] Not jumping to %d\n", soft.Name, pos, op, paramB)
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
		soft.Operations[dest] = res
		fmt.Printf("[%s, %d, %d] Setting %d to %d at position %d\n", soft.Name, pos, op, res, ops[dest], dest)
		soft.Position = pos + 4

	}
}

func RunParallel(s Software, wg *sync.WaitGroup) {
	defer wg.Done()
	Run(&s)

}

func Run(s *Software) {
	for {
		Step(s)
		if s.Position < 0 {
			break
		}
	}
}

func SimpleRun(ops, inputs []int) Software {
	in := make(chan int, len(inputs))
	for _, n := range inputs {
		in <- n
	}
	start := NewSoftware(ops, in)
	Run(&start)
	return start
}

func RunCircular(program, phase_sequence []int) int { // Used for Day 7 part two
	input_a := make(chan int, 10)
	a := NewSoftware(program, input_a)
	b := NewSoftware(program, a.Outputs)
	c := NewSoftware(program, b.Outputs)
	d := NewSoftware(program, c.Outputs)
	e := NewSoftware(program, d.Outputs)
	a.Name = "a"
	b.Name = "b"
	c.Name = "c"
	d.Name = "d"
	e.Name = "e"
	a.Inputs = e.Outputs
	a.Inputs <- phase_sequence[0]
	a.Inputs <- 0
	b.Inputs <- phase_sequence[1]
	c.Inputs <- phase_sequence[2]
	d.Inputs <- phase_sequence[3]
	e.Inputs <- phase_sequence[4]
	var wg sync.WaitGroup
	wg.Add(5)
	go RunParallel(a, &wg)
	go RunParallel(b, &wg)
	go RunParallel(c, &wg)
	go RunParallel(d, &wg)
	go RunParallel(e, &wg)

	wg.Wait()

	output_e := <-e.Outputs

	return output_e

}
