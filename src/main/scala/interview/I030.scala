package interview
/*
30.在从1 到n 的正数中1 出现的次数
题目：输入一个整数n，求从1 到n 这n 个整数的十进制表示中1 出现的次数。
例如输入12，从1 到12 这些整数中包含1 的数字有1，10，11 和12，1 一共出现了5 次。
分析：这是一道广为流传的google 面试题。
*/
object I030 extends Demo {
    /***
    我觉得可以一位一位考虑，对于10^b位：
    1)此位大于1，这一位上1的个数有 ([n / 10^(b+1) ] + 1) * 10^b
    2)此位等于0，为 ([n / 10^(b+1) ] ) * 10^b
    3)此位等于1，在0的基础上加上n mod 10^b + 1
    ***/
    def count(n: Int): Int = {
        val pows = Array(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)
        var b = 0
        var count = 0
        while (n >= pows(b)) {
            (n % pows(b + 1)) / pows(b) match {
                case 0 => count += (n / pows(b + 1)) * pows(b)
                case 1 => {
                    count += (n / pows(b + 1)) * pows(b)
                    count += n % pows(b) + 1
                }
                case _ => count += (n / pows(b + 1) + 1) * pows(b)
            }
            b += 1
        }
        count
    }

    def test() = {
        println(count(12))
        println(count(100000000))
    }
}