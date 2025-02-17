package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func FindFuel(mass int) int {
	return mass/3 - 2
}

func Parse(filename string) []int {
	file, _ := os.Open(filename)
	var numbers []int
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		num, _ := strconv.Atoi(scanner.Text())
		numbers = append(numbers, num)
	}

	return numbers

}

func PartOne(nums []int) int {
	total := 0
	for _, n := range nums {
		total += FindFuel(n)
	}
	return total
}

func FindTotalFuel(mass int) int {
	return aux(FindFuel(mass))
}

func aux(mass int) int {
	if mass <= 0 {
		return 0
	}
	fuel := FindFuel(mass)
	return mass + aux(fuel)
}

func PartTwo(nums []int) int {
	total := 0
	for _, n := range nums {
		total += FindTotalFuel(n)
	}
	return total
}

func main() {
	ints := Parse("input.txt")
	fmt.Println(PartOne(ints))
	fmt.Println(PartTwo(ints))
}
