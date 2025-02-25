package main

import (
	"fmt"
	"os"
	"strings"
)

func Parse(s string) (map[string][]string, map[string]string) {
	orbits := map[string][]string{}
	orbiting := map[string]string{}
	for _, line := range strings.Split(s, "\r\n") {
		if line == "" {
			break
		}
		// fmt.Printf("line = %s\n", line)
		objects := strings.Split(line, ")")
		// fmt.Printf("objects = %s\n", objects)
		left, right := objects[0], objects[1]
		// fmt.Printf("left=%s, right=%s\n", left, right)
		// Adding the right object to the list of objets orbiting the left object
		moons, exist := orbits[left]
		if exist {
			orbits[left] = append(moons, right)
		} else {
			orbits[left] = []string{right}
		}
		// Checking if we mark the left object as "orbiting nothing"
		_, orbitsAnything := orbiting[left]
		if !orbitsAnything {
			orbiting[left] = ""
		}
		orbiting[right] = left
	}
	return orbits, orbiting
}

func CountOrbits(orbits map[string][]string, orbiting map[string]string) int {
	count := 0
	stack := []string{}
	numOrbits := map[string]int{}
	// We start with objects that orbit nothing
	for moon, center := range orbiting {
		if center == "" {
			stack = append(stack, moon)
			numOrbits[moon] = 0
		}
	}
	// Counting the orbits
	for {
		if len(stack) == 0 {
			break
		}
		// Popping the next element of the stack
		object := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		fmt.Println(object)
		center := orbiting[object]
		if center != "" {
			n := numOrbits[center] + 1
			numOrbits[object] = n
			count += n
		}
		// Populating the stack for the next iteration
		moons, exist := orbits[object]
		if exist {
			stack = append(stack, moons...)
		}
	}
	return count
}

func PartOne(b []byte) int {
	orbits, orbiting := Parse(string(b))
	return CountOrbits(orbits, orbiting)

}

type Pair struct {
	node  string
	steps int
}

func BFS(orbits map[string][]string, orbiting map[string]string) int {
	stack := []Pair{{node: "YOU", steps: -1}}
	goal := orbiting["SAN"]
	visited := map[string]int{}
	for {
		n := len(stack)
		if n == 0 {
			panic("n=0")
		}
		// Popping the stack
		pair := stack[n-1]
		stack = stack[:n-1]
		node := pair.node
		steps := pair.steps
		if node == goal {
			return steps
		}
		visited[node] = steps
		// Adding neighbours to the stack
		center := orbiting[node]
		_, centerAlreadyVisited := visited[center]
		if !centerAlreadyVisited {
			stack = append(stack, Pair{node: center, steps: steps + 1})
		}
		moons := orbits[node]
		for _, m := range moons {
			_, alreadyVisited := visited[m]
			if !alreadyVisited {
				stack = append(stack, Pair{node: m, steps: steps + 1})
			}
		}
	}
}

func PartTwo(b []byte) int {
	orbits, orbiting := Parse(string(b))
	return BFS(orbits, orbiting)

}
func main() {
	fmt.Println("Hello")

	example_b, _ := os.ReadFile("example_input.txt")
	fmt.Println(PartTwo(example_b))

	puzzle_b, _ := os.ReadFile("puzzle_input.txt")
	fmt.Println(PartTwo(puzzle_b))
}
