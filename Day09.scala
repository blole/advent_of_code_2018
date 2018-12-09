import scala.collection.mutable

object Day09 extends App {
  def day09(playerCount: Int, marbleCount: Int): Long = {
    val scores = new Array[Long](playerCount)

    var circle = mutable.DoubleLinkedList(0,1)
    circle.next.next = circle

    for (marble <- 2 to marbleCount) {
      if (marble%23 == 0) {
        circle = circle.prev.prev.prev.prev.prev.prev.prev
        scores(marble % playerCount) += marble + circle.head
        circle.remove
        circle = circle.next
      }
      else {
        circle = circle.next
        val tmp = mutable.DoubleLinkedList(marble)
        tmp.next = circle.next
        circle.next.prev = tmp
        circle.next = tmp
        tmp.prev = circle
        circle = circle.next
      }
    }
    scores.max
  }

  val part1 = day09(410, 72059)
  println(part1)

  val part2 = day09(410, 7205900)
  println(part2)
}
