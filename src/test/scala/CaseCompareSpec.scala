import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.ArrayBuffer

class CaseCompareSpec extends AnyFunSuite with Matchers {
  test("compare uses value ordering when indices match") {
    val c1 = Case(ArrayBuffer((0,1),(1,1)), 0, 1)
    val c2 = Case(ArrayBuffer((0,1),(1,2)), 0, 1)
    c1.compare(c2,1) should be < 0
    c2.compare(c1,1) should be > 0
  }

  test("lexicographical ordering considers both index and value") {
    val c1 = Case(ArrayBuffer((0,1),(1,2)), 0, 1)
    val c2 = Case(ArrayBuffer((0,2),(1,1)), 0, 1)
    val c3 = Case(ArrayBuffer((0,2),(1,2)), 0, 1)
    val arr = Array(c3, c2, c1)
    val sorted = arr.sortWith((x,y) => x.compare(y,1) < 0)
    sorted.toSeq shouldEqual Seq(c1, c2, c3)
  }
}
