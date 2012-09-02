package interview
/*
27.跳台阶问题
题目：一个台阶总共有n 级，如果一次可以跳1 级，也可以跳2 级。
求总共有多少总跳法，并分析算法的时间复杂度。
这道题最近经常出现，包括MicroStrategy 等比较重视算法的公司
都曾先后选用过个这道题作为面试题或者笔试题。
*/
object I027 extends Demo {
    case class Step(n: Int) {
        private var count = 0

        def step(total: Int = 0): Int = {
            (1 to 2).foreach { i => total + i match {
                case t if t == n => count += 1
                case t if t > n => 
                case t => step(t)
            }}
           count
        }
    }

    case class Step2(n: Int) {
        private var count = 0

        def step(total: Int = n): Int = {
            (1 to 2).foreach { i => total - i match {
                case t if t == 0 => count += 1;
                case t if t < 0 => 
                case t => step(t)
            }}
            count
        }
    }

    def test() = {
        println(Step(6).step())
        println(Step(26).step())
    }
}