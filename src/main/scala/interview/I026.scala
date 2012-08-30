package interview
/*
26.左旋转字符串
题目：
定义字符串的左旋转操作：把字符串前面的若干个字符移动到字符串的尾部。
如把字符串abcdef 左旋转2 位得到字符串cdefab。请实现字符串左旋转的函数。
要求时间对长度为n 的字符串操作的复杂度为O(n)，辅助内存为O(1)。
*/
object I026 extends Demo {
    case class LeftShift(input: String, n: Int) {
        private val buffer = input.toBuffer

        def shift(): String = {
            right(0, input.size - 1, n % input.size)
            buffer.mkString
        }

        private def right(start: Int, end: Int, size: Int): Unit = {
            if (start == end || size == 0) return
            if (end - start + 1 >= 2 * size) {
                (0 until size).foreach(i => swap(start + i, start + size + i))
                right(start + size, end, size)
            } else {
                left(start +  size, start, end - start - size + 1)
            }
        }

        private def left(start: Int, end: Int, size: Int): Unit = {
            if (start == end || size == 0) return
            if (start - end >= size) {
                (0 until size).foreach( i => swap(start + i, start - size + i))
                left(start - size, end, size)
            } else {
                right(end, start + size - 1, start - end)
            }
        }

        private def swap(source: Int, target: Int) {
            val t = buffer(source)
            buffer(source) = buffer(target)
            buffer(target) = t
        }
    }

    def test() = {
        println(LeftShift("abcdefg", 2).shift)
        println(LeftShift("abcdefg", 5).shift)
        println(LeftShift(('a' to 'z').mkString, 88).shift)
    }
}