package interview
/*
第24 题：
链表操作，
（1）单链表就地逆置，
（2）合并链表
*/
object I024 extends Demo {
    case class ListNode(value: Int, var next: ListNode) {
        def swap(pre: ListNode = null): ListNode = {
            val temp = next
            next = pre
            if (temp != null) temp.swap(this) else this
        }

        def join(node: ListNode): ListNode = {
            if (next != null) next.join(node) else next = node
            this
        }

        override def toString(): String =  value + (if (next != null) "," + next.toString else "")
    }

    object ListNode {
        def apply(ints: List[Int]): ListNode = ints match {
            case Nil => null
            case head::xs => new ListNode(head, ListNode(xs))
        }

        def apply(ints: Int*): ListNode = ListNode(ints.toList)
    }

    def test() = {
        var root = ListNode(1,2,3,4,5,6)
        println(root)
        root = root.swap()
        println(root)
        root = root.join(ListNode(7,8,9,10))
        println(root)
    }
}