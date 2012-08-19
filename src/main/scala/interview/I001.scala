package interview
/*
1.把二元查找树转变成排序的双向链表（树）
 题目：
输入一棵二元查找树，将该二元查找树转换成一个排序的双向链表。
要求不能创建任何新的结点，只调整指针的指向。
   10
  / \
  6  14
 / \ / \
4  8 12 16
 转换成双向链表
4=6=8=10=12=14=16。
*/

object I001 extends App {
	case class Node(value: Int, var left: Node, var right: Node) {
		def add(t: Int): Node = t.compare(value) match {
			case i if i < 0 && left != null => left.add(t)
			case i if i < 0 && left == null => left = Node(t);this
			case _ if right != null => right.add(t)
			case _ if right == null => right = Node(t);this
		}

		def add(tt: List[Int]): Node = { tt.foreach(add(_)); this }

		private var last: Node = null

		def toList(node: Node): Node = {
			if (node != null) {
				toList(node.left)
				process(node)
				toList(node.right)
			}
			last
		}

		private def process(node: Node) {
			node.left = last
			if (last != null) {
				last.right = node
			}
			last = node
		}

		def leftString: String = if (left != null) left.leftString + "," + value else value.toString
	}

	object Node {
		def apply(i: Int) = new Node(i, null, null)

		def from(ii: List[Int]) = ii match {
			case Nil => null
			case head :: xs => new Node(head, null, null).add(xs)
		}
	}

	val input = List(10,6,14,4,8,12,16)
	val root = Node.from(input)
	println(root.toList(root).leftString)
}