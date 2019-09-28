package com.atguigu.algorithm

import scala.util.control.Breaks._

object SingleLinkedListTest {
	def main(args: Array[String]): Unit = {
		val list1 = new SingleLinkedList
		val list2 = new SingleLinkedList

//		list.traverse()
		list1.add(new HeroNode(1, "marc", ""))
		list1.add(new HeroNode(3, "juli", ""))
		list1.add(new HeroNode(5, "echo", ""))
		list1.add(new HeroNode(6, "philip", ""))
//		list1.traverse()

		list2.add(new HeroNode(2, "marc1", ""))
		list2.add(new HeroNode(4, "juli1", ""))
		list2.add(new HeroNode(7, "echo1", ""))
		list2.add(new HeroNode(8, "philip1", ""))
//		list2.traverse()

		val list = list1.merge(list2)
		list.traverse()

		println()

		list.reverse(list.head)
		list.traverse()

//		println(list.reverseGet(1).no + "-" + list.reverseGet(1).name)

//		val list3 = new SingleLinkedList
//		val result = list3.merge(list1, list2)
//		result.traverse()

//		list.traverse()
//		println()

//		println(list.size())

//		list.reversePrint(list.head)

//		list.traverse()
//		println()
//		list.addByOrder(new HeroNode(5, "gugugu", ""))
//		list.traverse()
//		println()

//		list.delete(2)
//		list.update(new HeroNode(4, "zakk", ""))
	}
}

class SingleLinkedList {
	val head: HeroNode = new HeroNode(-1, "", "")

	def reverse(h: HeroNode){
		if (h.next == null || h.next.next == null) {
			return
		}
		var cur: HeroNode = h.next
		var after: HeroNode = null
		var newHead: HeroNode = new HeroNode(-1, "", "")
		while (cur != null) {
			after = cur.next
			cur.next = newHead.next
			newHead.next = cur
			cur = after
		}
		h.next = newHead.next
	}

	def reverseGet(k: Int): HeroNode = {
		if (isEmpty()) {
			return null
		}
		if (k > size()) {
			return null
		}
		var count = size() - k
		var tmp = head.next
		while (count != 0) {
			count -= 1
			tmp = tmp.next
		}
		tmp
	}

	def merge(l: SingleLinkedList): SingleLinkedList = {
		val _this: SingleLinkedList = this
		var tmp: HeroNode = l.head.next
		while (tmp != null) {
			val node = new HeroNode(tmp.no, tmp.name, tmp.nickName)
			_this.addByOrder(node)
			tmp = tmp.next
		}
		_this
	}

	def size(): Int = {
		var tmp = head.next
		var count = 0
		while (tmp != null) {
			count += 1
			tmp = tmp.next
		}
		count
	}

	def reversePrint(node: HeroNode): Unit = {
		if (node.next != null) {
			reversePrint(node.next)
		}
		if (node.no != -1) {
			println(node.no + "-" + node.name)
		}
	}

	def addByOrder(node: HeroNode): Unit = {
//		if (isEmpty()) {
//			add(node)
//			return
//		}

		var tmp: HeroNode = head
		var flag = 1 // 1.添加后端 2.插入 3.幂等覆盖
		breakable {
			while (tmp.next != null) {
				if (tmp.next.no > node.no) {
					flag = 2
					break()
				} else if (tmp.next.no == node.no) {
					flag = 3
					break()
				}
				tmp = tmp.next
			}
		}

		if (flag == 1) {
			add(node)
		} else if (flag == 2) {
			node.next = tmp.next
			tmp.next = node
		} else {
			node.next = tmp.next.next
			tmp.next = node
		}
	}

	def delete(no: Int): Unit = {
		if (isEmpty()) {
			println("this list is empty...")
			return
		}
		var tmp = head
		var flag = false
		breakable{
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
		} else {
			println("node not exists......")
		}
	}

	def update(node: HeroNode): Unit = {
		if (isEmpty()) {
			println("this list is empty...")
			return
		}

		var tmp = head.next
		var flag = false
		breakable {
			while (tmp != null) {
				if (tmp.no == node.no) {
					flag = true
					break()
				}
				tmp = tmp.next
			}
		}
		if (flag) {
			tmp.name = node.name
			tmp.nickName = node.nickName
		} else {
			println("node not exists...")
		}
	}

	def traverse(): Unit = {
		if (isEmpty()) {
			println("this list is empty...")
			return
		}
		var tmp = head.next
		while (tmp != null) {
			println(tmp.no + "-" + tmp.name)
			tmp = tmp.next
		}
	}

	def isEmpty(): Boolean = {
		head.next == null
	}

	def add(node: HeroNode): Unit = {
		var tmp: HeroNode = head
		while (tmp.next != null) {
			tmp = tmp.next
		}
		tmp.next = node
	}
}

class HeroNode(hNo: Int, hName: String, hNickName: String) {
	val no = hNo
	var name = hName
	var nickName = hNickName

	var next: HeroNode = _
}
