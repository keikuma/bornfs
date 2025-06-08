import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

class HopZeroSpec extends AnyFunSuite {
  test("select works when hop is zero") {
    val path = "data/test.arff"
    val data = ARFFReader(path)
    val mapdata: Seq[(ArrayBuffer[(Int, Int)], Int)] =
      data.sparse_instances.to(mutable.ArrayBuffer).map { x =>
        (x._1.map(y => (data.attr2index(y._1), y._2)), x._2)
      }.toSeq
    val ds = Dataset(mapdata, sort = 0, tutorial = false, verbose = false)
    val result = ds.select(1.0, hop = 0)
    assert(result.isInstanceOf[Seq[Int]])
  }
}
