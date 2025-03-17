package main

import (
	"fmt"
	"sync"
)

func my_function(in, out chan int, wg *sync.WaitGroup) {
	defer wg.Done()
	for i := range 3 {
		a := <-in
		result := a + i
		fmt.Printf("a=%d, i=%d, result=%d\n", a, i, result)
		out <- result
	}
}

func main() {

	input_a := make(chan int, 2)
	input_b := make(chan int, 2)
	input_c := make(chan int, 2)

	input_a <- 1
	var wg sync.WaitGroup
	wg.Add(3)

	go my_function(input_a, input_b, &wg)
	go my_function(input_b, input_c, &wg)
	go my_function(input_c, input_a, &wg)

	wg.Wait()
	close(input_a)

	for x := range input_a {
		fmt.Println("Result:", x)
	}
}

// phase_sequence := []int{9, 8}
// input_a := make(chan int)
// input_a <- 0
// input_a <- phase_sequence[0]
// output_a := make(chan int)

// go func() {
// 	fmt.Println("In add, waiting for first input...")
// 	a := <-input_a
// 	fmt.Printf("In add, got %d, waiting for 2nd input...\n", a)
// 	b := <-input_a
// 	fmt.Printf("In add, got %d, computing result...\n", b)
// 	time.Sleep(2 * time.Second)
// 	result := a + b
// 	fmt.Printf("In add, result = %d\n", result)
// 	output_a <- result
// }()

// // func mult(input chan int, output chan int) {
// // 	fmt.Println("In mult, waiting for first input...")
// // 	a := <-input
// // 	fmt.Printf("In mult, got %d, waiting for 2nd input...", a)
// // 	b := <-input
// // 	fmt.Printf("In mult, got %d, computing result...\n", b)
// // 	time.Sleep(2 * time.Second)
// // 	result := a * b
// // 	fmt.Printf("In mult, result = %d\n", result)
// // 	output <- result
// // }
// fmt.Println("Calling goroutine add...")
// fmt.Println(<-output_a)
// // output_a <- phase_sequence[1]
// // fmt.Println("Calling goroutine mult...")
// // output_b := make(chan int)
// // go mult(output_a, output_b)
// // fmt.Println("Waiting ")
// // res := <-output_b
// // fmt.Println(res)
