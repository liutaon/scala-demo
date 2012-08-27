package interview
/*
第21 题
2010 年中兴面试题
编程求解：
输入两个整数n 和m，从数列1，2，3.......n 中随意取几个数,
使其和等于m ,要求将其中所有的可能组合列出来.
*/
object I021 extends Demo {

    case class Find(n: Int, m: Int)(f: Seq[Int] => Unit) {
        import scala.collection.mutable
        private val stack = mutable.Stack[Int]()

        def find() = find0(0, 1)

        private def find0(total: Int, start: Int): Unit = {
            (start to n).foreach { i =>
                stack.push(i)
                total + i match {
                    case t if (t == m) => f(stack)
                    case t if (t < m) => find0(t, i + 1)
                    case _ =>
                }
                stack.pop()
            }
        }
    }

    def test() = {
        Find(18, 20)(s => println(s.reverse)).find    
    }
}