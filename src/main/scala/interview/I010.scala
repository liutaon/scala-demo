package interview
/*
第10题，单词翻转。
单词翻转
例子: I am a student. => .student a am I
*/
object I010 extends App {
	case class Swap(input: String) {
		val buffer = new StringBuilder(input.size)
		val word = new StringBuilder(20)

		def swap(): String = {
			input.reverse.foreach { c =>
				if (!c.isLetterOrDigit) {
					buffer.append(word.toString.reverse)
					buffer.append(c)
					word.clear
				} else {
					word.append(c)
				}
			}
			// 需要添加多余的字符
			buffer.append(word.toString.reverse)
			buffer.toString
		}
	}

	println(Swap("I am a student.").swap)
}