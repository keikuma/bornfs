package object scwc {
  def time[A](a: => A) = {
    System.gc
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    (result, micros)
  }

  type Attr = Int
  type Index = Int
  type Value = Int
}
