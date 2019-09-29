package src.main.scala.com.marc.algorithm

import scala.util.control.Breaks._

object SingleLinkedListCase {
	def main(args: Array[String]): Unit = {
		val node1 = new Node(1, "marc2")
		val node2 = new Node(3, "marc1")
		val node3 = new Node(5, "marc5")
		val node4 = new Node(8, "marc3")
		val node5 = new Node(10, "marc4")

		val list1 = new SingleLinkedListClass()
		list1.add(node1)
		list1.add(node2)
		list1.add(node3)
		list1.add(node4)
		list1.add(node5)

		//		list1.traverse()
		//		println()

		val node11 = new Node(2, "marc2")
		val node21 = new Node(4, "marc1")
		val node31 = new Node(6, "marc5")
		val node41 = new Node(7, "marc3")
		val node51 = new Node(9, "marc4")

		val list2 = new SingleLinkedListClass()
		list2.add(node11)
		list2.add(node21)
		list2.add(node31)
		list2.add(node41)
		list2.add(node51)

		val list = list2.merge(list1)
		list.traverse()

		println()

		list.reverse(list.head)
		list.traverse()


//		val node = list.reverseGet(11)
//		if (node != null) {
//			println(node.no + "-" + node.value)
//		} else {
//			println("node is null")
//		}

		//		println(list.size())

		//		list.reversePrint(list.head)

	}
}

class SingleLinkedListClass {

	val head = new Node(-1, "")

	//	isEmpty()
	def isEmpty(): Boolean = {
		head.next == null
	}

	//	add(node)
	def add(node: Node): Unit = {
		var tmp: Node = head
		while (tmp.next != null) {
			tmp = tmp.next
		}
		tmp.next = node
	}

	//	traverse()
	def traverse(): Unit = {
		if (isEmpty()) {
			println("list is empty...")
		}
		var tmp = head.next
		while (tmp != null) {
			println(tmp.no + "-" + tmp.value)
			tmp = tmp.next
		}
	}

	//	update(node)
	def update(node: Node): Unit = {
		if (isEmpty()) {
			println("list is empty...")
		}
		var tmp = head
		var flag = false
		breakable {
			while (tmp.next != null) {
				if (tmp.next.no == node.no) {
					flag = true
					break();
				}
				tmp = tmp.next
			}
		}
		if (flag) {
			node.next = tmp.next.next
			tmp.next = node
		}
	}

	//	delete(node.no)
	def delete(no: Int): Unit = {
		if (isEmpty()) {
			println("list is empty...")
		}

		var tmp = head
		var flag = false
		breakable {
			while (tmp.next != null) {
				if (tmp.next.no == no) {
					flag = true
					break()
				}
				tmp = tmp.next
			}
		}

		if (flag) {
			tmp.next = tmp.next.next
		}
	}

	//	addByOrder(node)
	def addByOrder(node: Node): Unit = {
		var tmp = head
		var flag = 1 // 1. 最后 2.覆盖 3.找到
		breakable {
			while (tmp.next != null) {
				if (node.no < tmp.next.no) {
					flag = 3
					break()
				} else if (node.no == tmp.next.no) {
					flag = 2
					break()
				}
				tmp = tmp.next
			}
		}
		if (flag == 1) {
			add(node)
		} else if (flag == 2) {
			node.next = tmp.next.next
			tmp.next = node
		} else {
			node.next = tmp.next
			tmp.next = node
		}
	}

	//	reversePrint(head)
	def reversePrint(node: Node): Unit = {
		if (node.next != null) {
			reversePrint(node.next)
		}
		if (node.no != -1) {
			println(node.no + "-" + node.value)
		}
	}

	//	size()
	def size(): Int = {
		var count = 0
		var tmp = head.next
		while (tmp != null) {
			count += 1
			tmp = tmp.next
		}
		count
	}

	//	merge(l: SingleLinkedList)
	def merge(l: SingleLinkedListClass): SingleLinkedListClass = {
		val _this = this
		var tmp = l.head.next
		while (tmp != null) {
			val mid = new Node(tmp.no, tmp.value)
			_this.addByOrder(mid)
			tmp = tmp.next
		}
		_this
	}

	//	reverseGet(k: Int)
	def reverseGet(k: Int): Node = {
		if (isEmpty() || k > size()) {
			return null
		}
		var tmp = head.next
		var count = size() - k
		while (count != 0) {
			count -= 1
			tmp = tmp.next
		}
		tmp
	}

	//	reverse(h: HeroNode)
	def reverse(head: Node): Unit = {
		if (head.next == null || head.next.next == null) {
			return
		}
		val newHead: Node = new Node(-1, "")
		var cur = head.next
		var after: Node = null
		while (cur != null) {
			after = cur.next
			cur.next = newHead.next
			newHead.next = cur
			cur = after
		}
		head.next = newHead.next
	}
}

class Node(hNo: Int, vl: String) {
	val no = hNo
	val value = vl
	var next: Node = _
}
