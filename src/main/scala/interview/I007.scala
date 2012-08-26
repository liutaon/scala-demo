package interview
/*
第7题
------------------------------------
微软亚院之编程判断俩个链表是否相交
给出俩个单向链表的头指针，比如h1，h2，判断这俩个链表是否相交。
为了简化问题，我们假设俩个链表均不带环。

问题扩展：
1.如果链表可能有环列?
2.如果需要求出俩个链表相交的第一个节点列?
*/
object I007 extends Demo {
	case class ListNode(value: Int, next: ListNode) {
		def last() = {
			var count = 1
			var node = this
			while (node.next != null && node.next != this) {
				node = node.next
				count += 1
			}
			(count, node)
		}

		def cross(node: ListNode): ListNode = {
			val (c1, l1) = last
			val (c2, l2) = node.last
			if (l1 != l2) {
				return null
			}
			var n1 = if (c1 > c2) this else node
			var n2 = if (c1 > c2) node else this
			var index = math.abs(c1 - c2)
			while (index != 0) {
				n1 = n1.next
				index -= 1
			}
			while(n1 != n2) {
				n1 = n1.next
				n2 = n2.next
			}
			return n1
		}
	}

	def test() = {}
}