package interview
/*
第20 题：
题目：输入一个表示整数的字符串，把该字符串转换成整数并输出。
例如输入字符串"345"，则输出整数345。
*/
object I020 extends Demo {
	def atoi(input: String): Int = {
		if (input.isEmpty) return 0
		var ret = 0
		var neg: Option[Boolean] = None
		input.foreach { c => c match {
			case '-' if !neg.isDefined => neg = Some(true)
			case '+' if !neg.isDefined => neg = Some(false)
			case c if c <= '9' && c >= '0' => ret = ret * 10 + (c - '0')
			case _ => throw new Exception("Invalid input")
		}}
		neg match {
			case Some(true) => -ret
			case _ => ret
		}
	}

	def test() = {
		println(atoi("12345"))
		println(atoi("-874"))
		println(atoi("+874"))
	}
}