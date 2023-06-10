package linkedlist

import (
	"errors"
)

type Node struct {
	value int
	next  *Node
}

type List struct {
	head *Node
	size int
}

func New(elements []int) *List {
	list := &List{}

	for _, element := range elements {
		list.Push(element)
	}

	return list
}

func (l *List) Size() (count int) {
	return l.size
}

func (l *List) Push(element int) {
	l.head = &Node{value: element, next: l.head}
	l.size++
}

func (l *List) Pop() (value int, err error) {
	if l.size == 0 {
		return -1, errors.New("empty list")
	}

	value, l.head, l.head.next = l.head.value, l.head.next, nil
	l.size--

	return value, nil
}

func (l *List) Array() []int {
	arr := make([]int, l.size)

	for i, head := l.size-1, l.head; head != nil; i, head = i-1, head.next {
		arr[i] = head.value
	}

	return arr
}

func (l *List) Reverse() *List {
	list := New(nil)

	for value, err := l.Pop(); err == nil; value, err = l.Pop() {
		list.Push(value)
	}

	return list
}
