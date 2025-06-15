import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ArrayBuffer

class CaseCompareSpec extends AnyFunSuite {
  test("Case.compare uses feature values when indexes match") {
    val c1 = Case(ArrayBuffer((0,1), (2,1)), 0, 1)
    val c2 = Case(ArrayBuffer((0,1), (2,2)), 0, 1)
    assert(c1.compare(c2, 2) == 1)
    assert(c2.compare(c1, 2) == -1)
  }
}
