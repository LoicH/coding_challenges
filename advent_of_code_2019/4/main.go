package main

import (
	"fmt"
)

func CheckDouble(digits []int) bool {
	for i := range digits[:len(digits)-1] {
		if digits[i] == digits[i+1] {
			return true
		}
	}
	return false
}

func CheckIncreasing(digits []int) bool {
	for i := range digits[:len(digits)-1] {
		if digits[i] > digits[i+1] {
			return false
		}
	}
	return true

}

func GetDigits(n int) []int {
	var digits []int
	for tmp := n; tmp > 0; tmp /= 10 {
		lastDigit := tmp % 10
		digits = append([]int{lastDigit}, digits...)
	}
	return digits
}

func CheckValid(n int) bool {
	digits := GetDigits(n)
	return CheckDouble(digits) && CheckIncreasing(digits)
}

func PartOne(start, end int) int {
	count := 0
	for i := start; i <= end; i++ {
		if CheckValid(i) {
			count++
		}
	}
	return count
}

func CheckDoubleNotTriple(digits []int) bool {
	i := 0
	for {
		if i >= len(digits)-1 {
			return false
		}
		j := 1 // count the number of same digits
		for {
			if i+j < len(digits) && digits[i] == digits[i+j] {
				j++
			} else {
				break
			}
		}
		if j == 2 {
			return true
		}
		i += j
	}

}

func CheckValidTwo(n int) bool {
	digits := GetDigits(n)
	return CheckDoubleNotTriple(digits) && CheckIncreasing(digits)
}

func PartTwo(start, end int) int {
	count := 0
	for i := start; i <= end; i++ {
		if CheckValidTwo(i) {
			count++
		}
	}
	return count
}

func main() {
	// Your puzzle input is 145852-616942.
	fmt.Println(PartTwo(145852, 616942))
}
