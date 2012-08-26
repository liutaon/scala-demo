package interview
/*
2.设计包含min函数的栈（栈）
定义栈的数据结构，要求添加一个min函数，能够得到栈的最小元素。
要求函数min、push以及pop的时间复杂度都是O(1)。
*/

object I002 extends Demo {
	sealed case class Node(value: Int, var prev: Node) {
		override def toString(): String =  value.toString
	}
	sealed case class MNode(var next: MNode, node: Node)

	class Stack {
		val head = Node(Int.MaxValue, null)
		var minhead: MNode = _

		def push(i: Int) {
			val n = Node(i, head.prev)
			head.prev = n
			if (minhead == null) {
				minhead = MNode(null, n)
			} else if (i < minhead.node.value) {
				minhead = MNode(minhead, n)
			}
		}

		def pop() = {
			val p = head.prev
			if (p != null) {
				head.prev = p.prev
				if (minhead.node == p) {
					val t = minhead
					minhead = t.next
					// help gc
					t.next = null
				}
				// help gc
				p.prev = null
				p.value
			} else {
				throw new Exception("empty")
			}
		}

		def min() = if (minhead != null) minhead.node.value else throw new Exception("empty")
	}

	def test() = {
		val s = new Stack
		val input = List(1,3,9,5,4,8,12,4,2)
		input.foreach(s.push(_))
		input.foreach(_ => println(s.pop))
		println
		s.push(3)
		s.push(4)
		s.push(2)
		s.push(1)
		println(s.min)
		s.pop
		println(s.min)
		s.pop
		println(s.min)
		s.pop
		println(s.min)
	}
}