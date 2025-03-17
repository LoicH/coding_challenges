package main

import (
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

func Permutations(s []int) [][]int {
	if len(s) <= 1 {
		return [][]int{s}
	}
	perms := [][]int{}
	for i, n := range s {
		remainder := make([]int, len(s))
		copy(remainder, s)
		if i == len(s)-1 {
			remainder = remainder[:i]
		} else {
			remainder = append(remainder[:i], remainder[i+1:]...)
		}
		// fmt.Printf("source slice: %[1]v, address: %[1]p\n", s)
		// fmt.Printf("remainder slice: %[1]v, address: %[1]p\n", remainder)
		for _, other_perm := range Permutations(remainder) {
			new_perm := append([]int{n}, other_perm...)
			perms = append(perms, new_perm)
		}
	}
	return perms
}
