package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type Coord struct{ i, j int }

type Path map[Coord]int

func ParseLine(line string) []string {
	words := strings.Split(line, ",")
	return words
}
func Delta(dir byte) Coord {
	switch dir {
	case 'R':
		return Coord{0, 1}
	case 'L':
		return Coord{0, -1}
	case 'U':
		return Coord{-1, 0}
	case 'D':
		return Coord{1, 0}
	default:
		panic(dir)
	}
}

func Add(a, b Coord) Coord {
	return Coord{a.i + b.i, a.j + b.j}
}

func StraightLine(line Path, dir byte, pos Coord, steps int, curSteps int) (newPath Path, newPos Coord) {
	newPath = line
	newPos = Add(pos, Coord{0, 0})
	for range steps {
		newPos = Add(newPos, Delta(dir))
		curSteps++
		_, exists := line[newPos]
		if !exists {
			newPath[newPos] = curSteps
		}
	}
	return
}

func Walk(words []string) Path {
	p := make(Path)
	pos := Coord{0, 0}
	var curSteps int
	for _, w := range words {
		dir := w[0]
		steps, _ := strconv.Atoi(w[1:])
		newPath, newPos := StraightLine(p, dir, pos, steps, curSteps)
		p = newPath
		curSteps += steps
		// fmt.Println(p)
		pos = newPos
	}
	return p
}

func Intersect(a, b Path) map[Coord]int {
	common := make(map[Coord]int)
	for coordA, stepsA := range a {
		stepsB, existsB := b[coordA]
		if existsB {
			common[coordA] = stepsA + stepsB
		}
	}
	return common
}

func ManhattanDist(c Coord) int {
	return int(math.Abs(float64(c.i)) + math.Abs(float64(c.j)))
}

func Closest(p map[Coord]int) Coord {
	init := true
	var closest Coord
	for c := range p {
		// fmt.Println(c, ManhattanDist(c))
		if init || ManhattanDist(c) < ManhattanDist(closest) {
			// fmt.Println("New closest")
			closest = c
			init = false
		}
	}
	return closest
}

func Quickest(p map[Coord]int) int {
	minSteps := -1
	for _, steps := range p {
		// fmt.Println(c, ManhattanDist(c))
		if minSteps < 0 || steps < minSteps {
			fmt.Println("New closest:", steps)
			minSteps = steps
		}
	}
	return minSteps
}

func PartOne(lines []string) int {
	wordsA := ParseLine(lines[0])
	wordsB := ParseLine(lines[1])
	walkA := Walk(wordsA)
	walkB := Walk(wordsB)
	// fmt.Println(walkA)
	// fmt.Println(walkB)
	common := Intersect(walkA, walkB)
	// fmt.Println(common)
	c := Closest(common)
	return ManhattanDist(c)
}

func PartTwo(lines []string) int {
	wordsA := ParseLine(lines[0])
	wordsB := ParseLine(lines[1])
	walkA := Walk(wordsA)
	walkB := Walk(wordsB)
	// fmt.Println(walkA)
	// fmt.Println(walkB)
	common := Intersect(walkA, walkB)
	// fmt.Println(common)
	c := Quickest(common)
	return c
}

func SplitLines(s string) []string {
	lines := strings.Split(s, "\n")
	return lines
}

func main() {
	fmt.Println("Hello")
	// fmt.Println(StraightLine('R', Coord{0, 0}, 3))
	// fmt.Println(Walk([]string{"R8", "U5"}))
	fmt.Println(PartOne([]string{"R8,U5,L5,D3", "U7,R6,D4,L4"}))                                                          // 6
	fmt.Println(PartOne([]string{"R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"}))               // 159
	fmt.Println(PartOne([]string{"R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"})) // 135

	fmt.Println(PartTwo([]string{"R8,U5,L5,D3", "U7,R6,D4,L4"}))                                                          // 30
	fmt.Println(PartTwo([]string{"R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"}))               // 610
	fmt.Println(PartTwo([]string{"R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"})) // 410

	b, _ := os.ReadFile("input.txt")
	lines := SplitLines(string(b))
	fmt.Println(PartTwo(lines))
}
