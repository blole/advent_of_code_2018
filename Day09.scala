import scala.collection.mutable

object Day09 extends App {
  def day09(playerCount: Int, marbleCount: Int): Int = {
    val scores = new Array[Int](playerCount)
    val circle = new mutable.ListBuffer[Int]
    circle += 0
    var curr = 0

    for (marble <- 1 to marbleCount) {
      if (marble%23 == 0) {
        curr = (curr - 7 + circle.length) % circle.length
        scores(marble % playerCount) += marble + circle.remove(curr)
      }
      else {
        curr = (curr + 2) % circle.length
        circle.insert(curr, marble)
      }
    }
    scores.max
  }

  val part1 = day09(410, 72059)
  println(part1)
}
