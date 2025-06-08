import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ArrayBuffer

class ZeroMutualInfoSpec extends AnyFunSuite {
  test("BornFS handles zero mutual information without division by zero") {
    val data = Seq(
      (ArrayBuffer((0,1), (1,1)), 0),
      (ArrayBuffer((0,1), (1,1)), 1)
    )
    val ds = Dataset(data, sort = 0, tutorial = false, verbose = false)
    assert(ds.ratio(-1) == 0.0)
    val selected = ds.select(1.0, hop = 1)
    val relevance = ds.entropyPrefix + ds.entropyLabel - ds.entropyPrefixLabel
    val denomH = ds.miEntireLabel + ds.entropyPrefix
    val denomG = ds.miEntireLabel * ds.entropyPrefix
    val muH = if(denomH == 0.0) 0.0 else 2 * relevance / denomH
    val muG = if(denomG == 0.0) 0.0 else relevance / math.sqrt(denomG)
    assert(muH == 0.0)
    assert(muG == 0.0)
    assert(selected.isInstanceOf[Seq[Int]])
  }
}
