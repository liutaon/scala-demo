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

object I001 extends Demo {
    implicit def wrapper(node: Node) = new MyNode(node)
    case class MyNode(node: Node) {
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
    }

    def leftString(node: Node): String = {
        import scala.collection.mutable
        val list = mutable.ListBuffer[Int]()
        var n = node
        while (n != null) {
            list += n.value
            n = n.left
        }
        list.reverse.mkString("=")
    }

    def test() = {
        val input = List(10,6,14,4,8,12,16)
        val root = Node.from(input)
        println(leftString(root.toList(root)))
    }
}