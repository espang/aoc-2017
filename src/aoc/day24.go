package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

const input = `32/31
2/2
0/43
45/15
33/24
20/20
14/42
2/35
50/27
2/17
5/45
3/14
26/1
33/38
29/6
50/32
9/48
36/34
33/50
37/35
12/12
26/13
19/4
5/5
14/46
17/29
45/43
5/0
18/18
41/22
50/3
4/4
17/1
40/7
19/0
33/7
22/48
9/14
50/43
26/29
19/33
46/31
3/16
29/46
16/0
34/17
31/7
5/27
7/4
49/49
14/21
50/9
14/44
29/29
13/38
31/11`

type Component struct {
	v1, v2 int
}

func ParseLine(l string) (Component, error) {
	parts := strings.Split(l, "/")
	if len(parts) != 2 {
		return Component{}, fmt.Errorf("parts %q", l)
	}
	v1, err := strconv.Atoi(parts[0])
	if err != nil {
		return Component{}, fmt.Errorf("parts %q: %w", l, err)
	}
	v2, err := strconv.Atoi(parts[1])
	if err != nil {
		return Component{}, fmt.Errorf("parts %q: %w", l, err)
	}
	return Component{v1: v1, v2: v2}, nil
}

func strength(b []Component) int {
	if len(b) == 0 {
		return 0
	}
	return b[0].v1 + b[0].v2 + strength(b[1:])
}

func solveR(b []Component, next int, used map[Component]struct{}, cs []Component) int {
	max := strength(b)

	for _, c := range cs {
		_, ok := used[c]
		if ok {
			continue
		}

		if c.v1 == next {
			used[c] = struct{}{}
			b = append(b, c)
			s := solveR(
				b,
				c.v2,
				used,
				cs,
			)
			if s > max {
				max = s
			}
			b = b[:len(b)-1]
			delete(used, c)
		}

		if c.v2 == next {
			used[c] = struct{}{}
			b = append(b, c)
			s := solveR(
				b,
				c.v2,
				used,
				cs,
			)
			if s > max {
				max = s
			}
			b = b[:len(b)-1]
			delete(used, c)
		}
	}
	return max
}

func solve(cs []Component) int {
	max := int(math.MinInt64)
	for _, c := range cs {

		if c.v1 == 0 {
			fmt.Println("solve from ", c)
			s := solveR(
				[]Component{c},
				c.v2,
				map[Component]struct{}{c: struct{}{}},
				cs,
			)
			if s > max {
				max = s
			}
		}
		if c.v2 == 0 {
			fmt.Println("solve from ", c)
			s := solveR(
				[]Component{c},
				c.v1,
				map[Component]struct{}{c: struct{}{}},
				cs,
			)
			if s > max {
				max = s
			}
		}
	}
	return max
}

func main() {
	lines := strings.Split(input, "\n")
	var cs []Component

	unique := map[string]bool{}
	for i, l := range lines {
		c, err := ParseLine(l)
		if err != nil {
			fmt.Println("error in line ", i)
			fmt.Println(err)
			os.Exit(1)
		}
		var key string
		if c.v1 < c.v2 {
			key = strconv.Itoa(c.v1) + strconv.Itoa(c.v2)
		} else {
			key = strconv.Itoa(c.v2) + strconv.Itoa(c.v1)
		}

		_, ok := unique[key]
		if ok {
			fmt.Println("non unique component:", key)
		}
		unique[key] = true
		cs = append(cs, c)
	}

	fmt.Println(solve(cs))
}
