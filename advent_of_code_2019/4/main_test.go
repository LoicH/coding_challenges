package main

import (
	"reflect"
	"testing"
)

func TestCheckDoubleOK(t *testing.T) {
	got := CheckDouble([]int{1, 2, 2})
	want := true
	if got != want {
		t.Errorf("got %t, wanted %t", got, want)
	}
}

func TestCheckDoubleNotOK(t *testing.T) {
	got := CheckDouble([]int{1, 2, 1})
	want := false
	if got != want {
		t.Errorf("got %t, wanted %t", got, want)
	}
}

func TestCheckIncreasingOK(t *testing.T) {
	got := CheckIncreasing([]int{1, 2, 2})
	want := true
	if got != want {
		t.Errorf("got %t, wanted %t", got, want)
	}
}

func TestCheckIncreasingNotOK(t *testing.T) {
	got := CheckIncreasing([]int{1, 2, 1})
	want := true
	if got != want {
		t.Errorf("got %t, wanted %t", got, want)
	}
}

func TestCheckGetDigits(t *testing.T) {
	got := GetDigits(12345)
	want := []int{1, 2, 3, 4, 5}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("got %d, wanted %d", got, want)
	}
}

func TestCheckValidAllOnes(t *testing.T) {
	got := CheckValid(111111)
	want := true
	if got != want {
		t.Errorf("got %t, wanted %t", got, want)
	}
}

func TestCheckValidDecreasing(t *testing.T) {
	got := CheckValid(223450)
	want := false
	if got != want {
		t.Errorf("got %t, wanted %t", got, want)
	}
}

func TestCheckValidNoDouble(t *testing.T) {
	got := CheckValid(123789)
	want := false
	if got != want {
		t.Errorf("got %t, wanted %t", got, want)
	}
}

func TestCheckTripleTrue(t *testing.T) {
	got := CheckDoubleNotTriple(GetDigits(11122))
	want := true
	if got != want {
		t.Errorf("got %t, wanted %t", got, want)
	}
}

func TestCheckTripleFalse(t *testing.T) {
	got := CheckDoubleNotTriple(GetDigits(123444))
	want := false
	if got != want {
		t.Errorf("got %t, wanted %t", got, want)
	}
}
