package main

import (
	"reflect"
	"testing"
)

// Test described in https://adventofcode.com/2019/day/5 part 1
func TestP1AddNegative(t *testing.T) {
	newOps := SimpleRun([]int{1101, 100, -1, 4, 0}, []int{1}).Operations
	want := []int{1101, 100, -1, 4, 99}
	if !reflect.DeepEqual(newOps, want) {
		t.Errorf("got %d, wanted %d", newOps, want)
	}
}

func TestP1Mult(t *testing.T) {
	newOps := SimpleRun([]int{1002, 4, 3, 4, 33}, []int{1}).Operations
	want := []int{1002, 4, 3, 4, 99}
	if !reflect.DeepEqual(newOps, want) {
		t.Errorf("got %d, wanted %d", newOps, want)
	}
}

func TestP2PosModeEqual8(t *testing.T) {
	outputs := SimpleRun([]int{3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8}, []int{8}).Outputs
	output := []int{<-outputs}
	want := []int{1}
	if !reflect.DeepEqual(output, want) {
		t.Errorf("got %d, wanted %d", output, want)
	}

}

// func TestP2PosModeNotEqual8(t *testing.T) {
// 	_, output := Run([]int{3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8}, []int{7})
// 	want := []int{0}
// 	if !reflect.DeepEqual(output, want) {
// 		t.Errorf("got %d, wanted %d", output, want)
// 	}

// }

// func TestP2PosModeLessThan8(t *testing.T) {
// 	_, output := Run([]int{3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8}, []int{7})
// 	want := []int{1}
// 	if !reflect.DeepEqual(output, want) {
// 		t.Errorf("got %d, wanted %d", output, want)
// 	}

// }

// func TestP2PosModeNotLessThan8(t *testing.T) {
// 	_, output := Run([]int{3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8}, []int{8})
// 	want := []int{0}
// 	if !reflect.DeepEqual(output, want) {
// 		t.Errorf("got %d, wanted %d", output, want)
// 	}

// }

// func TestP2ImmediateModeEqual8(t *testing.T) {
// 	_, output := Run([]int{3, 3, 1108, -1, 8, 3, 4, 3, 99}, []int{8})
// 	want := []int{1}
// 	if !reflect.DeepEqual(output, want) {
// 		t.Errorf("got %d, wanted %d", output, want)
// 	}

// }

// func TestP2ImmediateModeNotEqual8(t *testing.T) {
// 	_, output := Run([]int{3, 3, 1108, -1, 8, 3, 4, 3, 99}, []int{7})
// 	want := []int{0}
// 	if !reflect.DeepEqual(output, want) {
// 		t.Errorf("got %d, wanted %d", output, want)
// 	}

// }

// func TestP2ImmediateModeLessThan8(t *testing.T) {
// 	_, output := Run([]int{3, 3, 1107, -1, 8, 3, 4, 3, 99}, []int{7})
// 	want := []int{1}
// 	if !reflect.DeepEqual(output, want) {
// 		t.Errorf("got %d, wanted %d", output, want)
// 	}

// }

// func TestP2ImmediateModeNotLessThan8(t *testing.T) {
// 	_, output := Run([]int{3, 3, 1107, -1, 8, 3, 4, 3, 99}, []int{8})
// 	want := []int{0}
// 	if !reflect.DeepEqual(output, want) {
// 		t.Errorf("got %d, wanted %d", output, want)
// 	}

// }

// func TestP2PosModeJumpZero(t *testing.T) {
// 	_, output := Run([]int{3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9}, []int{0})
// 	want := []int{0}
// 	if !reflect.DeepEqual(output, want) {
// 		t.Errorf("got %d, wanted %d", output, want)
// 	}

// }

// func TestP2PosModeJumpNonZero(t *testing.T) {
// 	_, output := Run([]int{3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9}, []int{1})
// 	want := []int{1}
// 	if !reflect.DeepEqual(output, want) {
// 		t.Errorf("got %d, wanted %d", output, want)
// 	}

// }

// func TestP2ImmediateModeJumpZero(t *testing.T) {
// 	_, output := Run([]int{3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1}, []int{0})
// 	want := []int{0}
// 	if !reflect.DeepEqual(output, want) {
// 		t.Errorf("got %d, wanted %d", output, want)
// 	}

// }

// func TestP2ImmediateModeJumpNonZero(t *testing.T) {
// 	_, output := Run([]int{3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1}, []int{1})
// 	want := []int{1}
// 	if !reflect.DeepEqual(output, want) {
// 		t.Errorf("got %d, wanted %d", output, want)
// 	}

// }

// func TestP2LargerExampleLessThan8(t *testing.T) {
// 	_, output := Run([]int{3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
// 		1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
// 		999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99}, []int{7})
// 	want := []int{999}
// 	if !reflect.DeepEqual(output, want) {
// 		t.Errorf("got %d, wanted %d", output, want)
// 	}

// }
// func TestP2LargerExampleEqual8(t *testing.T) {
// 	_, output := Run([]int{3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
// 		1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
// 		999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99}, []int{8})
// 	want := []int{1000}
// 	if !reflect.DeepEqual(output, want) {
// 		t.Errorf("got %d, wanted %d", output, want)
// 	}

// }
// Found in https://adventofcode.com/2019/day/5 part two
func TestP2LargerExampleGreaterThan8(t *testing.T) {
	outputs := SimpleRun([]int{3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
		1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
		999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99}, []int{9}).Outputs
	output := []int{<-outputs}
	want := []int{1001}
	if !reflect.DeepEqual(output, want) {
		t.Errorf("got %d, wanted %d", output, want)
	}

}

func TestD7P2Example1(t *testing.T) {
	// Max thruster signal 139629729 (from phase setting sequence 9,8,7,6,5):
	program := []int{3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26,
		27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5}

	phase_sequence := []int{9, 8, 7, 6, 5}
	got := RunCircular(program, phase_sequence, true)
	want := 139629729
	if got != want {
		t.Errorf("got %d, wanted %d", got, want)
	}

}
func TestD7P2Example2(t *testing.T) {
	program := []int{3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54,
		-5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4,
		53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10}
	phase_sequence := []int{9, 7, 8, 5, 6}
	got := RunCircular(program, phase_sequence, true)
	want := 18216
	if got != want {
		t.Errorf("got %d, wanted %d", got, want)
	}

}
func TestD7P2Example1WithPartTwo(t *testing.T) {
	program := []int{3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26,
		27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5}

	got := PartTwo(program, true)
	want := 139629729
	if got != want {
		t.Errorf("got %d, wanted %d", got, want)
	}
}

func TestD7P2Example2WithPartTwo(t *testing.T) {
	program := []int{3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54,
		-5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4,
		53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10}

	got := PartTwo(program, true)
	want := 18216
	if got != want {
		t.Errorf("got %d, wanted %d", got, want)
	}
}
