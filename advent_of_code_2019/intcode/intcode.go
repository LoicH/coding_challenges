package main

import (
	"fmt"
	"sync"
)

// Software represents an intcode program with its memory and state
type Software struct {
	Operations   DefaultMap
	Position     int
	Inputs       chan int
	Outputs      chan int
	Name         string
	RelativeBase int
}

type DefaultMap struct {
	m            map[int]int
	defaultValue int
}

func NewDefaultMap(defaultValue int) *DefaultMap {
	return &DefaultMap{
		m:            make(map[int]int),
		defaultValue: defaultValue,
	}
}

func (d *DefaultMap) Get(key int) int {
	if val, exists := d.m[key]; exists {
		return val
	}
	return d.defaultValue
}

func (d *DefaultMap) Set(key int, value int) {
	d.m[key] = value
}

func NewSoftware(ops []int, inputs chan int) Software {
	// Create map and copy operations into it
	opsCopy := NewDefaultMap(0)
	for i, op := range ops {
		opsCopy.Set(i, op)
	}
	return Software{
		Operations:   *opsCopy,
		Inputs:       inputs,
		Position:     0,
		Outputs:      make(chan int, 20), // Increase buffer size to reduce deadlocks
		Name:         "default",
		RelativeBase: 0,
	}
}
func GetParamValue(isOutput bool, ops DefaultMap, pos int, mode int, relativeBase int) int {
	// TODO ne pas gérer pareil les paramètres quand on veut les lire ou les  écrire
	// CF bug de 21102 : le paramètre C devrait être juste le param + base
	param := ops.Get(pos)
	if isOutput {
		switch mode {
		case 0: // Position mode
			return param
		case 1: // Immediate mode
			panic("Immediate mode impossible when saving input.")
		case 2: // Relative mode
			return param + relativeBase
		default:
			panic(fmt.Sprintf("Unknown parameter mode: %d", mode))
		}

	}
	switch mode {
	case 0: // Position mode
		return ops.Get(param)
	case 1: // Immediate mode
		return param
	case 2: // Relative mode
		return ops.Get(param + relativeBase)
	default:
		panic(fmt.Sprintf("Unknown parameter mode: %d", mode))
	}
}

func Step(soft *Software, verbose bool) {
	ops := soft.Operations
	// fmt.Printf("[%s] Adresse de Operations: %p\n", soft.Name, &soft.Operations)
	pos := soft.Position
	op := ops.Get(pos)
	switch {
	case op%100 == 99:
		if verbose {
			fmt.Printf("[%s] Program finished\n", soft.Name)
		}
		soft.Position = -1
	case op%100 <= 2: // 0001, 0002, ..., 1101, 1102
		paramA := GetParamValue(false, ops, pos+1, (op/100)%10, soft.RelativeBase)
		paramB := GetParamValue(false, ops, pos+2, (op/1000)%10, soft.RelativeBase)
		paramC := GetParamValue(true, ops, pos+3, (op/10000)%10, soft.RelativeBase)
		var res int
		if op%10 == 1 { // op is xxx1 => addition
			res = paramA + paramB
			if verbose {
				fmt.Printf("[%s, %d, %d] Adding %d to %d = %d at position %d\n", soft.Name, pos, op, paramA, paramB, res, paramC)
			}
		} else if op%10 == 2 { // op is xxx2 => multiplication
			res = paramA * paramB
			if verbose {
				fmt.Printf("[%s, %d, %d] Multiplying %d by %d = %d at position %d\n", soft.Name, pos, op, paramA, paramB, res, paramC)
			}
		} else {
			if verbose {
				fmt.Printf("[%s, %d, %d] Unknown operation at position %d: %d", soft.Name, pos, op, pos, op)
			}
			panic("Unknown operation")
		}
		soft.Operations.Set(paramC, res)
		soft.Position = pos + 4
	case op%100 == 3: // input
		dest := GetParamValue(true, ops, pos+1, (op/100)%10, soft.RelativeBase)
		if verbose {
			fmt.Printf("[%s, %d, %d] Waiting for input...\n", soft.Name, pos, op)
		}
		in := <-soft.Inputs
		soft.Operations.Set(dest, in)
		if verbose {
			fmt.Printf("[%s, %d, %d] Input %d at position %d\n", soft.Name, pos, op, in, dest)
		}
		soft.Position = pos + 2
	case op%100 == 4: // output
		param := GetParamValue(false, ops, pos+1, (op/100)%10, soft.RelativeBase)
		soft.Outputs <- param
		if verbose {
			fmt.Printf("[%s, %d, %d] Output %d\n", soft.Name, pos, op, param)
		}
		soft.Position = pos + 2
	case op%100 <= 6: // jump-if-true or jump-if-false, 2 parameters
		paramA := GetParamValue(false, ops, pos+1, (op/100)%10, soft.RelativeBase)
		paramB := GetParamValue(false, ops, pos+2, (op/1000)%10, soft.RelativeBase)
		if verbose {
			fmt.Printf("[%s, %d, %d] op=%d, paramA=%d, paramB=%d\n", soft.Name, pos, op, op, paramA, paramB)
		}
		if (op%100 == 5 && paramA != 0) || (op%100 == 6 && paramA == 0) { // '5' => jump if paramA is not 0, '6' => jump if paramA is 0
			soft.Position = paramB
			if verbose {
				fmt.Printf("[%s, %d, %d] Jumping to %d\n", soft.Name, pos, op, paramB)
			}
		} else {
			soft.Position = pos + 3
			if verbose {
				fmt.Printf("[%s, %d, %d] Not jumping to %d\n", soft.Name, pos, op, paramB)
			}
		}
	case op%100 <= 8: // less than (opcode 7), or equals (opcode 8), takes 2 parameters and stores a result
		paramA := GetParamValue(false, ops, pos+1, (op/100)%10, soft.RelativeBase)
		paramB := GetParamValue(false, ops, pos+2, (op/1000)%10, soft.RelativeBase)
		paramC := GetParamValue(true, ops, pos+3, (op/10000)%10, soft.RelativeBase)
		res := 0
		if (op%100 == 7 && paramA < paramB) || (op%100 == 8 && paramA == paramB) {
			res = 1
		}
		soft.Operations.Set(paramC, res)
		if verbose {
			fmt.Printf("[%s, %d, %d] Writing %d at position %d\n", soft.Name, pos, op, res, paramC)
		}
		soft.Position = pos + 4
	case op%100 == 9: // adjust relative base
		param := GetParamValue(false, ops, pos+1, (op/100)%10, soft.RelativeBase)
		soft.RelativeBase += param
		if verbose {
			fmt.Printf("[%s, %d, %d] Adjusting relative base by %d to %d\n", soft.Name, pos, op, param, soft.RelativeBase)
		}
		soft.Position = pos + 2
	}
}

func RunParallel(s Software, wg *sync.WaitGroup, verbose bool) {
	defer wg.Done()
	Run(&s, verbose)

}

func Run(s *Software, verbose bool) {
	for {
		Step(s, verbose)
		if s.Position < 0 {
			break
		}
	}
}

func SimpleRun(ops, inputs []int, verbose bool) Software {
	in := make(chan int, len(inputs))
	for _, n := range inputs {
		in <- n
	}
	start := NewSoftware(ops, in)
	Run(&start, verbose)
	return start
}

func RunCircular(program, phase_sequence []int, verbose bool) int { // Used for Day 7 part two
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
	go RunParallel(a, &wg, verbose)
	go RunParallel(b, &wg, verbose)
	go RunParallel(c, &wg, verbose)
	go RunParallel(d, &wg, verbose)
	go RunParallel(e, &wg, verbose)

	wg.Wait()

	output_e := <-e.Outputs

	return output_e

}
