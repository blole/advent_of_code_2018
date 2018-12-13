import scala.collection.mutable.ListBuffer
import Function.tupled

object Day12 extends App {
  val input =
    """
initial state: #...#..###.#.###.####.####.#..#.##..#..##..#.....#.#.#.##.#...###.#..##..#.##..###..#..##.#..##...

...#. => #
#..## => #
..... => .
##.## => .
.##.. => #
.##.# => .
####. => #
.#.#. => .
..#.# => .
.#.## => .
.#..# => .
##... => #
#...# => #
##### => .
#.### => #
..### => #
###.. => .
#.#.# => #
##..# => #
..#.. => #
.#### => .
#.##. => .
....# => .
...## => .
#.... => .
#..#. => .
..##. => .
###.# => #
#.#.. => #
##.#. => #
.###. => .
.#... => .
    """.trim

  val initial :: _ :: rules = input.lines.toList
  val live = rules.sorted.reverseIterator.map(_.last match{case'.'=>0 case'#'=>1}).toList

  var state = initial.replaceAll("[^.#]", "").map{case'.'=>0 case'#'=>1}.toList
  var offset = 0
  for (time <- 0 to 20) {
    if (time != 0) {
      state = 0 +: 0 +: 0 +: 0 +: state :+ 0 :+ 0 :+ 0 :+ 0
      state = state.sliding(5).map{_.reduce{ (acc, bit)=>acc*2+bit}}.map(live(_)).toList
      val firstOne = state.indexOf(1)
      offset += firstOne-2
      state = state.slice(firstOne, state.lastIndexOf(1)+1)
    }
    println(f"$time%2d: ($offset%2d)"+ " "*(offset+5) + state.map{case 0=>'.' case 1=>'#'}.mkString)
  }

  val part1 = state.zipWithIndex.filter(_._1==1).map{_._2+offset}.sum
  println(part1)
}
