package main

import "fmt"

type Node struct {
	prev, next *Node
	val        uint
}

func (n *Node) Insert(v uint) *Node {
	nn := &Node{
		prev: n,
		next: n.next,
		val:  v,
	}
	n.next = nn
	return nn
}

func (n *Node) Move(s int) *Node {
	t := n
	for i := 0; i < s; i++ {
		t = t.next
	}
	return t
}

func (n *Node) Print() {
	cur := n
	fmt.Print(cur.val)
	for cur = cur.next; cur != n; cur = cur.next {
		fmt.Print("-")
		fmt.Print(cur.val)
	}
	fmt.Println("")
}

func main() {
	// real	11m50.661s
	// user	12m14.510s
	// sys	0m2.439s

	step := 366
	total := uint(50000000)

	start := &Node{val: 0}
	start.next = start
	start.prev = start

	cur := start
	for i := uint(1); i <= total; i++ {
		cur = cur.Move(step)
		cur = cur.Insert(i)
	}

	fmt.Println("val after 0:", start.next.val)
}
