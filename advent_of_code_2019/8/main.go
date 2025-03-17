package main

import (
	"fmt"
	"os"
	"strconv"
)

func PartOne(input string) int {
	// The image is 25 pixels wide and 6 pixels tall
	// Transform the input into a list of int
	pixels := make([]int, len(input))
	for i, c := range input {
		if digit, err := strconv.Atoi(string(c)); err == nil {
			pixels[i] = digit
		}
	}
	// Get the index of the group that has the least number of zeros
	width, height := 25, 6
	layerSize := width * height
	numLayers := len(pixels) / layerSize

	minZeros := layerSize + 1 // More than possible in one layer
	result := 0

	// For each layer
	for layer := 0; layer < numLayers; layer++ {
		zeros, ones, twos := 0, 0, 0
		// Count digits in this layer
		for i := layer * layerSize; i < (layer+1)*layerSize; i++ {
			switch pixels[i] {
			case 0:
				zeros++
			case 1:
				ones++
			case 2:
				twos++
			}
		}
		// If this layer has fewer zeros, update result
		if zeros < minZeros {
			minZeros = zeros
			result = ones * twos
		}
	}
	return result
}

func PartTwo(input string) int {
	// Transform the input into a list of int
	pixels := make([]int, len(input))
	for i, c := range input {
		if digit, err := strconv.Atoi(string(c)); err == nil {
			pixels[i] = digit
		}
	}

	// Split into layers
	width, height := 25, 6
	layerSize := width * height
	numLayers := len(pixels) / layerSize

	// Create 2D slice to hold layers
	layers := make([][]int, numLayers)
	for i := range layers {
		layers[i] = make([]int, layerSize)
		copy(layers[i], pixels[i*layerSize:(i+1)*layerSize])
	}

	// Create final image
	finalImage := make([]int, layerSize)

	// For each pixel position
	for pos := 0; pos < layerSize; pos++ {
		// Start with top layer
		pixel := layers[0][pos]
		layer := 1
		// Keep going through layers until we find non-transparent pixel
		for pixel == 2 && layer < numLayers {
			pixel = layers[layer][pos]
			layer++
		}
		finalImage[pos] = pixel
	}

	// Print the image
	for i := 0; i < height; i++ {
		for j := 0; j < width; j++ {
			if finalImage[i*width+j] == 1 {
				fmt.Print("#")
			} else {
				fmt.Print(" ")
			}
		}
		fmt.Println()
	}
	return 0
}

func main() {
	fmt.Println("Hello, World!")
	puzzle_b, _ := os.ReadFile("input.txt")
	fmt.Println(PartOne(string(puzzle_b)))
	fmt.Println(PartTwo(string(puzzle_b)))
}
