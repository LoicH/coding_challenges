package main

import (
	"reflect"
	"testing"
)

func TestSingletonPermutation(t *testing.T) {
	got := Permutations([]int{1})
	want := [][]int{{1}}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("got=%d, want=%d", got, want)
	}
}

func TestTwoElementsPermutation(t *testing.T) {
	got := Permutations([]int{1, 2})
	want := [][]int{{1, 2}, {2, 1}}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("got=%d, want=%d", got, want)
	}
}

func TestThreeElementsPermutation(t *testing.T) {
	perms := Permutations([]int{1, 2, 3})
	got := len(perms)
	want := 6
	if !reflect.DeepEqual(got, want) {
		t.Errorf("got=%d, want=%d", got, want)
	}
}
