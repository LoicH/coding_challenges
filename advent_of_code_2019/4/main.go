package main

import "fmt"

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

	fmt.Println(CheckDouble([]int{1, 2, 2}))
	fmt.Println(CheckDouble([]int{1, 2, 3}))
	fmt.Println(CheckDouble([]int{1, 2, 1}))
	fmt.Println(CheckIncreasing([]int{1, 2, 2}))
	fmt.Println(CheckIncreasing([]int{1, 2, 3}))
	fmt.Println(CheckIncreasing([]int{1, 2, 1}))
	fmt.Println(GetDigits(12345))
	fmt.Println(CheckValid(111111))
	fmt.Println(CheckValid(223450))
	fmt.Println(CheckValid(123789))
	// 111111 meets these criteria (double 11, never decreases).
	// 223450 does not meet these criteria (decreasing pair of digits 50).
	// 123789 does not meet these criteria (no double).

	// How many different passwords within the range given in your puzzle input meet these criteria?

	fmt.Println(CheckDoubleNotTriple(GetDigits(11122)))
	fmt.Println(CheckDoubleNotTriple(GetDigits(123444)))
	// Your puzzle input is 145852-616942.
	fmt.Println(PartTwo(145852, 616942))
}
