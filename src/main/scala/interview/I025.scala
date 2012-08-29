package interview
/*
第25 题：
写一个函数,它的原形是int continumax(char *outputstr,char *intputstr)
功能：
在字符串中找出连续最长的数字串，并把这个串的长度返回，
并把这个最长数字串付给其中一个函数参数outputstr 所指内存。
例如："abcd12345ed125ss123456789"的首地址传给intputstr 后，函数将返回9，
outputstr 所指的值为123456789
*/
object I025 extends Demo {
    case class ContinuMax(input: String) {
        private var start = -1
        private var max: Option[(Int, Int)] = None

        def continumax(): String = { 
            input.zipWithIndex.foreach { case (char, index) =>
                if (char.isDigit) {
                    if (start == -1) {
                        start = index
                    }
                } else {
                    if (start != -1) {
                        handle(index)
                        start = -1
                    }
                }
            }
            if (start != -1) handle(input.size)
            max match {
                case Some((i, s)) => input.substring(i, i + s)
                case _ => ""
            }
        }

        private def handle(index: Int) = max match {
            case Some((i, s)) if (s < (index - start)) => max = Some(start, index - start)
            case Some((i, s)) =>
            case None => max = Some(start, index - start)
        }
    }

    def test() = {
        println(ContinuMax("abcd12345ed125ss123456789").continumax)
    }
}