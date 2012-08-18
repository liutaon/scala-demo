package api.bstree
import org.specs2.mutable._
import org.specs2.specification._

class BSTreeSpec extends Specification with AllExpectations {
	"Test encode and decode" should {
		"test from long value" in {
			val n = End.add(1)
			println(n)
			println(n.add(2))
			println(BSTreeNode.fromList(List(1,2,3,4,5)))
			println(BSTreeNode.fromList(List(10,6,4,8,14,12,16)))
			println(BSTreeNode.fromList(List(10,6,14,4,8,12,16)))
			1 === 1
		}
	}
}