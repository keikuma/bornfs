import scwc._
import collection.mutable.{ArrayBuffer, HashMap}
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._
import java.text.DecimalFormat


case class Case(var row: ArrayBuffer[(Attr,Value)], val classLabel: Value, val frq: Int) {
  //  rowの中は特徴番号の順に昇順にソートされていると仮定する。
  //  以下のコードをいれて、ソートを仮定しなくてもよい。
  //
  //  row = row.sortWith {_._1 < _._1}
  //
  /*
   rowは、0以外の値を持つ特徴の(id, value)の並び。
   windowは、特徴全体の部分集合を指定する。
   window内では、一時的なindexが、0から順に一時的に振られる。
   windowの実態は、0以外の値を持つ特徴の(index, value)の並び
   */

  var window = row.clone()

  val size = row.size

  def apply(i: Index): Value = {
    /*
     window中でインデックスiの特徴の値
     */
    val j = this.indexOf(i)
    return if(j < 0) 0 else window(j)._2
  }

  def indexOf(i: Index): Int = {
    /*
     window中でインデックスiの特徴のwindow内での位置
     */
    var l = 0
    var u = window.size - 1
    if(u < 0 || window(l)._1 > i || window(u)._1 < i) return -1
    if(window(u)._1 == i) return u
    /*
     以下では常に、window(l)._1 <= i、かつ、window(u) > iが成り立つ。
     */
    while(true) {
      if(l + 1 == u) return if(window(l)._1 == i) l else -1
      val j = (l + u)/2
      window(j)._1 match {
        case y if y <= i => l = j
        case y if y > i => u = j
      }
    }
    0 // Syntax Errorを回避するためのダミー
  }

  def value(a: Attr): Value = {
    /*
     row中で識別子iの特徴の値
     */
    val j = this.locationOf(a)
    return if(j < 0) 0 else row(j)._2
  }

  def locationOf(a: Attr): Int = {
    /*
     row中で識別子aの特徴のrow内での位置
     */
    var l = 0
    var u = row.size - 1
    if(u < 0 || row(l)._1 > a || row(u)._1 < a) return -1
    if(row(u)._1 == a) return u
    /*
     以下では常に、row(l)._1 <= a、かつ、row(u) > aが成り立つ。
     */
    while(true) {
      if(l + 1 == u) return if(row(l)._1 == a) l else -1
      val j = (l + u)/2
      row(j)._1 match {
        case y if y <= a => l = j
        case y if y > a => u = j
      }
    }
    0 // Syntax Errorを回避するためのダミー
  }


  def renumber(order: Array[Index]): Unit = {
    /*
     order(i)は、現在のwindow中のインデックスiの特徴の、新たなwindowにおけるインデックス
     orderは、0, ..., rs-1の順列
     */
    val temp = ArrayBuffer[(Index, Value)]()
    val lim = order.size
    val ws = window.size
    var i = 0
    while(i < ws && window(i)._1 < lim) {
      val p = window(i)
      temp.append((order(p._1), p._2))
      i += 1
    }
    // Ensure the window is sorted by the new index order
    // so that subsequent binary searches behave correctly.
    window = temp.sortBy(_._1)
  }

  def compare(that: Case, index: Index): Int = {
    /*
     window内でインデックスが[0, index]内の特徴に関する辞書式順序でthisとthatを比較
     this < that => -1
     this == that => 0
     this > that => +1
     */
    var i = 0
    val lim = math.min(this.window.size, that.window.size)
    while(i < lim) {
      val x = this.window(i)
      val y = that.window(i)
      if(x._1 <= index) {
        x._1 - y._1 match {
          case diff if diff < 0 => return 1
          case diff if diff > 0 => return -1
          case _ =>
            x._2 - y._2 match {
              case diff if diff < 0 => return 1
              case diff if diff > 0 => return -1
              case _ => i += 1
            }
        }
      } else {
        return if(y._1 <= index) -1 else 0
      }
    }
    if(i == this.window.size) {
      if(i == that.window.size) return 0
      return if(that.window(i)._1 <= index) -1 else 0
    } else {
      return if(this.window(i)._1 <= index) 1 else 0
    }
  }

  def serialize: String = row.map { x => s"${x._1}>${x._2}" }.mkString(":")

}

case class Dataset(raw_data: Seq[(ArrayBuffer[(Attr, Value)], Value)], sort: Int, tutorial: Boolean, verbose: Boolean) {

  val f = new DecimalFormat("0.0000")
  val fsn = new DecimalFormat("#,### nsec")
  val very_large = 1000.0


  // To record function calls executed for profiling.
  // The first string indicates a function name.
  // The first float number indicates the running time in milli-seconds.
  // The subsequent float numbers are function specific, but maybe describes the arguments.

  val events = ArrayBuffer[(Symbol, Array[Long])]()

  if(tutorial) {
    println
    println(">> Tutorial starts now.")
    println
    println("Let's learn about how BornFS works.")
  }

  // Aggregate identical cases while ensuring each case's feature list is
  // consistently ordered by attribute index.
  val temp_dict = collection.mutable.Map[(ArrayBuffer[(Attr, Value)], Value), Int]()
  for (x <- raw_data) {
    val orderedCase = (x._1.sortBy(_._1), x._2)
    if (temp_dict.isDefinedAt(orderedCase)) {
      temp_dict(orderedCase) += 1
    } else {
      temp_dict(orderedCase) = 1
    }
  }

  val data = temp_dict.par.map{x => Case(x._1._1, x._1._2, x._2)}.to(mutable.ArrayBuffer)
  val maxAttr = data.par.map(_.row).par.flatMap{x => x.map(_._1)}.max
  val maxVal = (0 to maxAttr).par.map{a => data.map(_(a)).max}
  val maxLabel = data.par.map(_.classLabel).max
  val nCases = data.size
  val nSamples = data.par.foldLeft(0)(_ + _.frq)
  var prefix = ArrayBuffer[Attr]()
  var partitions = Array[Seq[Int]](0 until nCases)
  var entropyPrefix = 0.0
  var entropyPrefixLabel = 0.0
  var entity = (0 to maxAttr).toArray // Index to Attr
  var lim = maxAttr

  /*
   Runtime to be measured
   */
  var timeSortFeatures: Long = 0L
  var timeSortInstances: Long = 0L
  var timeSelectFeatures: Long = 0L

  if(tutorial) {
    println
    println("The current dataset includes " + nSamples + " samples described by " +
      (maxAttr + 1) + " features identified by 0 through 5.")
    println("Each sample is labeled by an integer in [0, " + maxLabel + "].")
    println("Samples with the same feature values and labels are aggregated into a single case object, ")
    println("and the frequency of occurrence of each case object is counted.")
    println("The following are the case objects of the current dataset:")
    println
    data.foreach { c =>
      print((0 to maxAttr).map(a => s"$a>${c.value(a)}").mkString(" "))
      println(" label>" + c.classLabel + " frq>" + c.frq)
    }
    println
    println("A single row represents a single case object. In each row, ")
    println("- [a]>[v] indicates a value v of a feature a;")
    println("- label>[l] indicates a class label l;")
    println("- frq>[n] indicates an occurrence frequency n.")
  }

  def log2(x: Double): Double = math.log(x)/math.log(2)
  def xlog2(x: Double): Double = if(x == 0) 0 else x*log2(x)


  def entropyPrefixPlusUntil(index: Index): (Double, Double) = {
    /*
     (H(prefix, [0, i]), H(prefix, [0, i], C))
     */
    val (hf, hfc) = partitions.par.map{p =>
      val blocks = ArrayBuffer[ArrayBuffer[Int]]()
      var last = data(p(0))
      var block = ArrayBuffer.fill(maxLabel+1)(0)
      block(last.classLabel) = last.frq
      for(i <- p.tail) {
        val current = data(i)
        if(current.compare(last, index) != 0) {
          blocks += block
          block = ArrayBuffer.fill(maxLabel+1)(0)
          block(current.classLabel) = current.frq
          last = current
        } else {
          block(current.classLabel) += current.frq
        }
      }
      blocks += block
      var _hf = 0.0
      var _hfc = 0.0
      for(b <- blocks) {
        _hf -= xlog2(b.sum)
        _hfc -= b.map(x => xlog2(x)).sum
      }
      (_hf, _hfc)
    }.foldLeft((0.0, 0.0))((x, y) => (x._1 + y._1, x._2 + y._2))
    return (hf / nSamples + log2(nSamples), hfc / nSamples + log2(nSamples))
  }

  def entropyPrefixPlus(index: Index): (Double, Double) = {
    /*
     (H(index, prefix), H(index, prefix C)
     prefixの値ごとにpartitionに分割されていることを仮定する
     サンプルがソートされていることを仮定しない。
     */
    val tempVal = partitions.par.map{p =>
      val temp = Array.fill(maxVal(entity(index))+1)(Array.fill(maxLabel+1)(0))
      for(i <- p) {
        val c = data(i)
        temp(c(index))(c.classLabel) += c.frq
      }
      (temp.map(x => -xlog2(x.sum)).sum, temp.map(x => x.map(-xlog2(_)).sum).sum)
    }
    (tempVal.map(_._1).sum/nSamples + log2(nSamples),
      tempVal.map(_._2).sum/nSamples + log2(nSamples))
  }

  //  Prefixが空なので、H(F)とH(F,C)を各属性に対して計算する

  val entropyPairs = (0 to maxAttr).toArray.par.map(a => entropyPrefixPlus(a))
  val entropyAttr = entropyPairs.map(_._1)
  val entropyAttrLabel = entropyPairs.map(_._2)

  var count = HashMap[String, Int]()
  for(c <- data) {
    val pttn = c.serialize
    if(count.isDefinedAt(pttn)) {
      count(pttn) += c.frq
    } else {
      count(pttn) = c.frq
    }
  }

  val entropyEntire = count.values.filter(_ > 0).par.map{x => -x*log2(x)}.sum/nSamples + log2(nSamples)


  count = HashMap[String, Int]()
  for(c <- data) {
    val pttn = c.serialize + "=" + c.classLabel
    if(count.isDefinedAt(pttn)) {
      count(pttn) += c.frq
    } else {
      count(pttn) = c.frq
    }
  }

  val entropyEntireLabel = count.values.filter(_ > 0).par.map{x => -x*log2(x)}.sum/nSamples + log2(nSamples)


  val countLabel = ArrayBuffer.fill(maxLabel+1)(0)
  for(c <- data) {
    countLabel(c.classLabel) += c.frq
  }

  val entropyLabel = countLabel.filter(_ > 0).par.map{x => -x*log2(x)}.sum/nSamples + log2(nSamples)


  val miEntireLabel = entropyEntire + entropyLabel - entropyEntireLabel

  if(tutorial) {
    println
    println("We see some important statistics of the current dataset.\n")
    println("- The entropy of the class variable H(Class) is " + f.format(entropyLabel) + ".")
    println("- The scores of entropy of individual features F are:")
    println
    println("H(F)= " + (0 to maxAttr).map { i => s"$i:${f.format(entropyAttr(i))}" }.mkString(" "))
    println
    println("- The scores of mutual information of individual features to labels are:\n")
    println("I(F;C)= " + (0 to maxAttr).map {
      i => s"$i:${f.format(entropyAttr(i) + entropyLabel - entropyAttrLabel(i))}" }.mkString(" "))
    println
    println("- The entropy of the entire features H(Entire) is " + f.format(entropyEntire) + ".")
    println("- The mutual information of the entire features to labels I(Entire;Class) is " +
      f.format(miEntireLabel) + ".")
  }

  def initializePrefix(): Unit = {
    prefix = ArrayBuffer[Attr]()
    partitions = Array[Seq[Int]](0 until nCases)
    entropyPrefix = 0.0
    entropyPrefixLabel = entropyLabel
  }


  def addPrefix(index: Index): Unit = {
    prefix += index
    lim = index - 1

    /*
     各partitionをindexをインデックスに持つ特徴の値で再分割し、partitionsを更新
     */

      partitions = partitions.par.map { p =>
        val temp = Vector.fill(maxVal(entity(index)) + 1)(ArrayBuffer[Int]())
        for (i <- p) temp(data(i)(index)) += i
        temp.filter(_.nonEmpty).map(_.toSeq)
      }.flatMap(x => x).toArray
    val tmp = entropyPrefixPlusUntil(-1)
    entropyPrefix = tmp._1
    entropyPrefixLabel = tmp._2
  }

  def nSamples(indices: Seq[Int]): Int = indices.foldLeft(0)(_+data(_).frq)

  def sortCases(): Unit = {

    /*
     partitionごとに、[0,lim]の属性のの辞書式順序にcaseをソート
     ソートした結果でpartitionsを更新
     */

    partitions = partitions.par.map{p =>
      p.sortWith((x, y) => data(x).compare(data(y), lim) < 0)
    }.toArray
  }

  def ratio(i: Index): Double = {

    /*
     H(prefix + [0,i]; C) / H(entire; C)
     */

    val (h, hc) = entropyPrefixPlusUntil(i)

    if(isZero(miEntireLabel)) 0.0 else (entropyLabel + h - hc)/miEntireLabel
  }

  def findBorder(delta: Double, lim: Index): Int = {

    /*
     I(attrs[i, attrs.size-1], prefix; C) >= delta I(entire, C) が成り立つ
     最大のiをbinary searchで探索して、出力する。

     以下では、
     I(attrs[u, attrs.size-1], prefix; C) < delta I(entire; C)
     I(attrs[l, attrs.size-1], prefix; C) >= delta I(entire; C)
     が成り立つように、l(ower)とu(pper)を制御

     I(attrs[attrs.size, attrs.size-1], prefix; C) >= delta I(entire; C)
     をチェックし、成り立てば、u = attrs.size と初期化する。
     */

    if(ratio(-1) >= delta) {
      return -1
    }

    /*
     I(prefix; C) < delta I(entire; C) を確認したので、l = 0と初期化する。
     */

    var l = -1

    /*
     I([0, lim], prefix; C) >= delta I(entire; C)
     は成り立つものと仮定できるので、u = lim と初期化する。
     */

    var u = lim

    while(true) {
      if(l + 1 == u) return u
      val c = (l + u)/2

      if(!tutorial && verbose) print(".")

      if(ratio(c) < delta) {
        l = c
      } else {
        u = c
      }
    }
    // エラーを回避するためのダミー出力

    return 0
  }

  def isZero(x: Double): Boolean = {
    return if(math.abs(x) <= 1e-8) true else false
  }

  def sortVal(index: Index): Double = {
    val (hfp, hfpc) = entropyPrefixPlus(index) // (H(F, Prefix), H(F, Prefix, C))
    // val very_large = 1000.0

    val noise_gain = hfpc - entropyPrefixLabel
    val relevance_gain = entropyPrefixLabel + hfp - hfpc - entropyPrefix

    return sort match{
      case 0 =>
        if(isZero(relevance_gain))
          0
        else if(isZero(noise_gain))
          very_large
        else relevance_gain/noise_gain
      // Ratio of noise gain and relevance gain
      case 1 => - noise_gain // Negative noise gain
      case 2 => relevance_gain // Relevance gain
      case 3 => relevance_gain - noise_gain
      case 4 => 2 * (hfp + entropyLabel - hfpc) / (miEntireLabel + hfp)
      case _ => 0.0
    }
  }

  def sortAttrs: (Array[Int], Array[Int]) = {
    /*
     現在のインデックスが[0, lim]である特徴を、sortValの値の降順にソート
     sorted(i)は、順位がi番目の特徴のインデックス
     order(i)は、インデックスiの特徴の順位
     data中の各case objectのwindow内のインデックスも更新する
     entity（インデックスから特徴の識別番号をひく配列）も更新する
     (sorted, order)を返す
     */

    if(tutorial) {
      print("The search range consists of the features of: ")
      println((0 to lim).map(entity(_)).mkString(" "))
    }

    val tmp = (0 to lim).par.map(i => (i, sortVal(i))).toArray.sortWith(_._2 > _._2)
    val sorted = tmp.map(_._1)
    var order = Array.fill(lim+1)(0)
    (0 to lim).par.foreach(i => order(sorted(i)) = i)
    // val order = sorted.zipWithIndex.sortWith(_._1 < _._1).map(_._2)
    val clip = (0 to lim).par.map(i => entity(sorted(i)))
    (0 to lim).par.foreach(i => entity(i) = clip(i))
    data.par.foreach(_.renumber(order))

    if(tutorial) {
      print("The features F in the search range are sorted in a decreasing order of ")
      sort match {
        case 0 =>
          print("the estimated ratio of relevance gain to noise gain: ")
          println("[I(F;Selected,Class)-I(F;Selected)] / [H(F)-I(F;Selected,Class].")
        case 1 =>
          println("the estimated negative noise gain: I(F;Selected,Class)-H(F).")
        case 2 =>
          println("the estimated relevance gain: I(F;Selected,Class)-I(F;Selected).")
        case _ =>
      }
      println("The features of the current search range are sorted to:\n")
      println((0 to lim).map { i => s"${entity(i)}(${f.format(tmp(i)._2)})" }.mkString("", " ", "\n"))
      print("A figure in parentheses indicates ")
      sort match {
        case 0 =>
          println("the estimated ratio of relevance gain to noise gain. ")
        case 1 =>
          println("the estimated negative noise gain: I(F;Selected,Class)-H(F).")
        case 2 =>
          println("the estimated relevance gain: I(F;Selected,Class)-I(F;Selected).")
        case _ =>
      }
    }
    return (sorted, order)
  }

  def select(delta: Double, hop: Int): Seq[Attr] = {

    // lim = maxAttr
    // var range = (0 to maxAttr).toArray
    initializePrefix()

    if(tutorial) {
      println("\nThe following is a high-level description of BornFS.\n")
      println("BornFS is a greedy but fast algorithm to find an optimal feature set FS that yields:")
      println("* high relevance I(Selected;Class); and ")
      println("* low noise H(Selected|Class).")
      print("To be specific, given threshold parameter t, ")
      println("BornFS approximately solves the optimization problem:")
      println("\n* Minimize H(Selected|Class);")
      println("* Subject to I(Selected;Class) > t * I(Entire;Class).")
      println("\nAs an algorithm, BornFS deploys the forward selection approach, and therefore, ")
      println("it iterates selection of a single feature until it reaches a final answer.")
      println("Each iteration is associated with a search range, an ordered set of features.")
      println("The high efficiency of BornFS when selecting a feature from the search range is due to leverage of binary search.")
      println("The search range shrinks as feature selection proceeds.")
      println("BornFS stops when the search range becomes empty and returns the features selected at the moment as a final answer.")
      println("Features in search ranges are sorted with the frequency specified by an option (-h).")
      println("The frequency affects the quality of outputs by and the time efficiency of BornFS.")
      println("To be more specific, we can expect that the quality will improve and the time efficiency decreases with higher frequency.")
      print("In this run of BornFS, ")
      if(hop == maxAttr + 1) {
        println("sorting features is executed only once at the first iteration.")
      } else {
        println("sorting features is executed every " + hop + " iterations.")
      }
      println("\nNow, we let BornFS start feature selection.")
      println("In the following, we describe the operations performed in each iteration.")
    }

    var start: Long = 0L
    var ellapsed_t: Long = 0L
    var it_count = 0

    while(true) {
      if(tutorial) {
        println
        println("******")
        println("Iteration #" + (it_count+1) + " starts.")
        if(prefix.isEmpty) {
          println("No features have been selected so far.")
        } else {
          print("The features selected so far are features of: ")
          println(prefix.map(entity(_)).mkString(" "))
        }
      }

      start = System.nanoTime()

      var sorted, order = (0 to lim).toArray

      if(hop > 0 && it_count % hop == 0) {
        val tempResult = sortAttrs
        sorted = tempResult._1
        order = tempResult._2

        ellapsed_t = System.nanoTime() - start
        events += ((Symbol("sortAttrs"), Array(ellapsed_t, lim)))
        timeSortFeatures += ellapsed_t

        start = System.nanoTime()

        sortCases()

        ellapsed_t = System.nanoTime() - start
        events += ((Symbol("sortCases"), Array(ellapsed_t, lim)))
        timeSortInstances += ellapsed_t
      } else if (tutorial) {
        println("This iteration does not perform sorting features.")
      }

      it_count += 1

      if(tutorial) {
        println("The iteration is also associated with one or more partitions.")
        println("Each case object of the dataset belongs to exactly one partition.")
        println("Case objects in the same partition have the same values for the features selected at the moment.")
        if(prefix.isEmpty) {
          println("Since no features have been selected when the first iteration starts, the iteration has only a single partition, which consists of all the case objects.")
        }
        println("In each partition, the case objects belonging to it are sorted in a lexicographical order of values of the features of the sorted search range: ")
        println
        for(p <- partitions) {
          println(">> Partition")
          p.foreach{i =>
            val c = data(i)
            print((prefix ++ (0 to lim)).map(a =>
              s"${entity(a)}>${c.value(entity(a))}").mkString(" "))
            println(" label>" + c.classLabel + " frq>" + c.frq)
          }
        }
        println
        println("Selection of a feature is conducted with respect to importance indices of the features in the search range.")
        print("The importance of a feature F at a position i ")
        print("in the sorted search range (F = Range[i]) ")
        println("is estimated by the ratio of:")
        print("* Mutual information to labels of the features prior to F ")
        print("plus the features selected so far = ")
        println("I(Range[0:i-1],Selected;Class); and")
        println("* Mutual information of the entire features to labels = I(Entire;Class).")

        println("The importance indices to select a feature are computed as: ")
        println
        println((0 to lim).map { i =>
          s"${entity(i)}:${f.format(ratio(i-1))}" }.mkString("", " ", "\n"))
        println("BornFS selects the right-most feature subject to the condition that the ratio is smaller than the given threshold, in this case, " + delta + ".")
      }

      start = System.nanoTime()

      val i = findBorder(delta, lim)

      ellapsed_t = System.nanoTime() - start
        events += ((Symbol("findBorder"), Array(ellapsed_t, lim)))
      timeSelectFeatures += ellapsed_t

      if(i < 0) {

        if(tutorial) {
          println
          print("Finally BornFS has selected the features of: ")
          println(prefix.map(entity(_)).mkString(" "))
          println
          println("<< Tutorial finishes.  Thank you!")
        }

          return prefix.toSeq
      } else {
        addPrefix(i)

        if(tutorial) {
          println("In fact, BornFS selects the feature of " + entity(i) + ".")
          println
          println("The relevance and noise of the currently selected features are:")
          val relevance = entropyPrefix + entropyLabel - entropyPrefixLabel
          val noise = entropyPrefix - relevance
          println("* Relevance I(Selected;Class) = " + f.format(relevance) + ";")
          println("* Noise H(Selected|Class) = " + f.format(noise) + ".")
          println("The harmonic and geometric means of I(S;C)/I(E;C) and I(S;C)/H(FS) are computed as:")
          val denomH = miEntireLabel + entropyPrefix
          val denomG = miEntireLabel * entropyPrefix
          val mu_H = if(isZero(denomH)) 0.0 else 2 * relevance/denomH
          val mu_G = if(isZero(denomG)) 0.0 else relevance/math.sqrt(denomG)
          println("* mu_H = " + f.format(mu_H) + ";")
          println("* mu_G = " + f.format(mu_G) + ".")
        } else if(verbose) {
          print("." + entity(i) + ".")
        }

        if(lim < 0) {
          if(tutorial) {
            println
            print("Finally, BornFS has selected the features of: ")
            println(prefix.map(entity(_)).mkString(" "))
            println
            println("<< Tutorial finishes.  Thank you!")
          }

          return prefix.map(entity(_)).toSeq
        }
      }
    }
    Seq[Attr]()
  }
}
