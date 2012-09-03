package interview
/*
29.栈的push、pop 序列
题目：输入两个整数序列。其中一个序列表示栈的push 顺序，
判断另一个序列有没有可能是对应的pop 顺序。
为了简单起见，我们假设push 序列的任意两个整数都是不相等的。
比如输入的push 序列是1、2、3、4、5，那么4、5、3、2、1 就有可能是一个pop 系列。
因为可以有如下的push 和pop 序列：
push 1，push 2，push 3，push 4，pop，push 5，pop，pop，pop，pop，
这样得到的pop 序列就是4、5、3、2、1。
但序列4、3、5、1、2 就不可能是push 序列1、2、3、4、5 的pop 序列。
*/
object I029 extends Demo {
    import scala.collection.mutable
    def check(source: Seq[Int], dest: Seq[Int]): Boolean = {
        val stack = mutable.Stack[Int]()
        val queue = mutable.Queue[Int]()
        source.foreach(queue.enqueue(_))
        stack.push(queue.dequeue)
        dest.forall { d =>
            var ok = true
            while (stack.top != d && ok) {
                if (queue.isEmpty) ok = false else stack.push(queue.dequeue)
            }
            if (ok) stack.pop
            ok
        }
    }

    def test() = {
        println(check(List(1,2,3,4,5), List(4,5,3,2,1)))
        println(check(List(1,2,3,4,5), List(4,3,5,1,2)))
        println(check(List(1,2,3,4,5), List(5,4,3,2,1)))
    }
}