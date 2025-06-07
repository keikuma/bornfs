import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.ArrayBuffer

class DataSetupTest extends AnyFunSuite with Matchers with BeforeAndAfterAll {

  var result: Seq[scwc.Attr] = _
  var selected_attrs: List[Symbol] = _

  override def beforeAll(): Unit = {
    val path: String = "data/test.arff"
    val data: ARFFReader = ARFFReader(path)
    println(data)
    var sort: Int = 0
    var tutorial: Boolean = false
    var verbose: Boolean = true
    var hop: Int= 1
    var threshold: Double = 1.0
    val mapdata: ArrayBuffer[Tuple2[ArrayBuffer[Tuple2[Int, Int]], Int]] = data.sparse_instances.to(ArrayBuffer).map{x =>
      (x._1.map{y => (data.attr2index(y._1), y._2)}, x._2)}
    println(mapdata)
    val ds: Dataset = Dataset(mapdata.toSeq, sort, tutorial, verbose)
    println("=== ready test data ===")
    result = ds.select(threshold, hop)
    selected_attrs = result.map{i => data.index2attr(i)}.toList
  }

  test("dummy test - Confirmation of test data input") {
    println
    println(result.size + " features have been selected.")
    println
    println("Selected features are: " + selected_attrs.mkString(" "))
    assert(true)
  }
}
