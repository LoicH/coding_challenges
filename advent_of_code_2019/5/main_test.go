package main

import (
	"reflect"
	"testing"
)

func TestP1AddNegative(t *testing.T) {
	newOps, _ := Run([]int{1101, 100, -1, 4, 0}, 1)
	want := []int{1101, 100, -1, 4, 99}
	if !reflect.DeepEqual(newOps, want) {
		t.Errorf("got %d, wanted %d", newOps, want)
	}
}
func TestP1Mult(t *testing.T) {
	newOps, _ := Run([]int{1002, 4, 3, 4, 33}, 1)
	want := []int{1002, 4, 3, 4, 99}
	if !reflect.DeepEqual(newOps, want) {
		t.Errorf("got %d, wanted %d", newOps, want)
	}
}

func TestP2PosModeEqual8(t *testing.T) {
	_, output := Run([]int{3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8}, 8)
	want := []int{1}
	if !reflect.DeepEqual(output, want) {
		t.Errorf("got %d, wanted %d", output, want)
	}

}

func TestP2PosModeNotEqual8(t *testing.T) {
	_, output := Run([]int{3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8}, 7)
	want := []int{0}
	if !reflect.DeepEqual(output, want) {
		t.Errorf("got %d, wanted %d", output, want)
	}

}

func TestP2PosModeLessThan8(t *testing.T) {
	_, output := Run([]int{3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8}, 7)
	want := []int{1}
	if !reflect.DeepEqual(output, want) {
		t.Errorf("got %d, wanted %d", output, want)
	}

}

func TestP2PosModeNotLessThan8(t *testing.T) {
	_, output := Run([]int{3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8}, 8)
	want := []int{0}
	if !reflect.DeepEqual(output, want) {
		t.Errorf("got %d, wanted %d", output, want)
	}

}

func TestP2ImmediateModeEqual8(t *testing.T) {
	_, output := Run([]int{3, 3, 1108, -1, 8, 3, 4, 3, 99}, 8)
	want := []int{1}
	if !reflect.DeepEqual(output, want) {
		t.Errorf("got %d, wanted %d", output, want)
	}

}

func TestP2ImmediateModeNotEqual8(t *testing.T) {
	_, output := Run([]int{3, 3, 1108, -1, 8, 3, 4, 3, 99}, 7)
	want := []int{0}
	if !reflect.DeepEqual(output, want) {
		t.Errorf("got %d, wanted %d", output, want)
	}

}

func TestP2ImmediateModeLessThan8(t *testing.T) {
	_, output := Run([]int{3, 3, 1107, -1, 8, 3, 4, 3, 99}, 7)
	want := []int{1}
	if !reflect.DeepEqual(output, want) {
		t.Errorf("got %d, wanted %d", output, want)
	}

}

func TestP2ImmediateModeNotLessThan8(t *testing.T) {
	_, output := Run([]int{3, 3, 1107, -1, 8, 3, 4, 3, 99}, 8)
	want := []int{0}
	if !reflect.DeepEqual(output, want) {
		t.Errorf("got %d, wanted %d", output, want)
	}

}

func TestP2PosModeJumpZero(t *testing.T) {
	_, output := Run([]int{3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9}, 0)
	want := []int{0}
	if !reflect.DeepEqual(output, want) {
		t.Errorf("got %d, wanted %d", output, want)
	}

}

func TestP2PosModeJumpNonZero(t *testing.T) {
	_, output := Run([]int{3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9}, 1)
	want := []int{1}
	if !reflect.DeepEqual(output, want) {
		t.Errorf("got %d, wanted %d", output, want)
	}

}

func TestP2ImmediateModeJumpZero(t *testing.T) {
	_, output := Run([]int{3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1}, 0)
	want := []int{0}
	if !reflect.DeepEqual(output, want) {
		t.Errorf("got %d, wanted %d", output, want)
	}

}

func TestP2ImmediateModeJumpNonZero(t *testing.T) {
	_, output := Run([]int{3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1}, 1)
	want := []int{1}
	if !reflect.DeepEqual(output, want) {
		t.Errorf("got %d, wanted %d", output, want)
	}

}

func TestP2LargerExampleLessThan8(t *testing.T) {
	_, output := Run([]int{3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
		1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
		999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99}, 7)
	want := []int{999}
	if !reflect.DeepEqual(output, want) {
		t.Errorf("got %d, wanted %d", output, want)
	}

}
func TestP2LargerExampleEqual8(t *testing.T) {
	_, output := Run([]int{3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
		1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
		999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99}, 8)
	want := []int{1000}
	if !reflect.DeepEqual(output, want) {
		t.Errorf("got %d, wanted %d", output, want)
	}

}
func TestP2LargerExampleGreaterThan8(t *testing.T) {
	_, output := Run([]int{3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
		1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
		999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99}, 9)
	want := []int{1001}
	if !reflect.DeepEqual(output, want) {
		t.Errorf("got %d, wanted %d", output, want)
	}

}
