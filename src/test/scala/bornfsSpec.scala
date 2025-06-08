import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

class DataSetupTest extends AnyFunSuite with Matchers with BeforeAndAfterAll {

  var result: Seq[scwc.Attr] = _
  var selected_attrs: List[Symbol] = _
  val expected_attrs: List[Symbol] = List(Symbol("a"), Symbol("c"), Symbol("f"))

  override def beforeAll(): Unit = {
    val path: String = "data/test.arff"
    val data: ARFFReader = ARFFReader(path)
    println(data)
    var sort: Int = 0
    var tutorial: Boolean = false
    var verbose: Boolean = true
    var hop: Int= 1
    var threshold: Double = 1.0
    val mapdata: Seq[(ArrayBuffer[(Int, Int)], Int)] =
      data.sparse_instances.to(mutable.ArrayBuffer).map { x =>
        (x._1.map(y => (data.attr2index(y._1), y._2)), x._2)
      }.toSeq
    println(mapdata)
    val ds: Dataset = Dataset(mapdata, sort, tutorial, verbose)
    println("=== ready test data ===")
    result = ds.select(threshold, hop)
    selected_attrs = result.map{i => data.index2attr(i)}.toList
  }

  test("selected features for test.arff are stable") {
    println
    println(result.size.toString + " features have been selected.")
    println
    println("Selected features are: " + selected_attrs.map(_.name).mkString(" "))
    selected_attrs shouldBe expected_attrs
  }
}

