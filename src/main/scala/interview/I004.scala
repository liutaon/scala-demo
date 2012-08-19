package interview
/*
4.在二元树中找出和为某一值的所有路径（树）
题目：输入一个整数和一棵二元树。
从树的根结点开始往下访问一直到叶结点所经过的所有结点形成一条路径。
打印出和与输入整数相等的所有路径。
例如 输入整数22和如下二元树
  10  
  / \   
 5  12   
 / \   
4   7
则打印出两条路径：10, 12和10, 5, 7。
*/

object I004 extends App {
	import scala.collection.mutable.ArrayStack
	case class Node(value: Int, var left: Node, var right: Node) {
		def add(t: Int): Node = t.compare(value) match {
			case i if i < 0 && left != null => left.add(t)
			case i if i < 0 && left == null => left = Node(t);this
			case _ if right != null => right.add(t)
			case _ if right == null => right = Node(t);this
		}

		def add(tt: List[Int]): Node = { tt.foreach(add(_)); this }

		def isleaf = left == null && right == null

		def findSum(sum: Int, stack: ArrayStack[Int]): Unit = {
			stack.push(value)
			try {
				if (value == sum && isleaf) {
					println(stack.toList.reverse.mkString(","))
				} else {
					if (left != null) left.findSum(sum - value, stack)
					if (right != null) right.findSum(sum - value, stack)
				}
			} finally {
				stack.pop
			}
		}
	}

	object Node {
		def apply(i: Int) = new Node(i, null, null)

		def from(ii: List[Int]) = ii match {
			case Nil => null
			case head :: xs => new Node(head, null, null).add(xs)
		}
	}

	val node = Node.from(List(10,5,12,4,7))
	node.findSum(22, ArrayStack[Int]())
}