package interview
/*
第19 题：
题目：定义Fibonacci 数列如下：
/ 0 n=0
f(n)= 1 n=1
\ f(n-1)+f(n-2) n=2
输入n，用最快的方法求该数列的第n 项。
分析：在很多C 语言教科书中讲到递归函数的时候，都会用Fibonacci 作为例子。
*/
object I019 extends Demo {

    def f(n: Int): Long = f1(1, 0, n)

    private def f1(a: Long, b: Long, count: Int): Long = count match {
        case 0 => b
        case i => f1(a + b, a, count - 1)
    }

    def test() = {
        println(f(5))
        println(f(25))
        println(f(40))
        println(f(8888888))
    }
}