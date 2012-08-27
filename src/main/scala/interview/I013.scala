package interview
/*
第13 题：
题目：输入一个单向链表，输出该链表中倒数第k 个结点。链表的倒数第0 个结点为链表的尾指针。
链表结点定义如下：
struct ListNode
{
int m_nKey;
ListNode* m_pNext;
};
*/
object I013 extends Demo {
    case class ListNode(value: Int, var next: ListNode) {
        def size:Int = {
            if (next != null) 1 + next.size else 1
        }

        def lastNode(k: Int): ListNode = {
            var c = size - k
            var node = this
            while (c != 0 && node != null) {
                node = node.next
                c -= 1
            }
            node
        }
    }

    object ListNode {
        def apply(ints: List[Int]): ListNode = ints match {
            case Nil => null
            case head::xs => new ListNode(head, ListNode(xs))
        }
    }

    def test() = {
        val head = ListNode(List(1,2,3,4,5,6))
        println(head.lastNode(0))
        println(head.lastNode(1))
        println(head.lastNode(2))
        println(head.lastNode(3))
    }
}