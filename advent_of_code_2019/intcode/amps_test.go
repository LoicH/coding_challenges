package main

import (
	"testing"
)

func Test1stExample(t *testing.T) {
	got := PartOneDay7([]int{3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0})
	want := 43210
	if got != want {
		t.Errorf("got %d, wanted %d", got, want)
	}
}

func Test2ndExample(t *testing.T) {
	got := PartOneDay7([]int{3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23,
		101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0})
	want := 54321
	if got != want {
		t.Errorf("got %d, wanted %d", got, want)
	}
}

func Test3rdExample(t *testing.T) {
	got := PartOneDay7([]int{3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33,
		1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0})
	want := 65210
	if got != want {
		t.Errorf("got %d, wanted %d", got, want)
	}
}
