object Day06 extends App {
  val input =
    """
132, 308
325, 300
310, 231
177, 248
111, 304
65, 135
227, 116
60, 80
182, 353
60, 42
314, 164
142, 50
90, 266
234, 219
68, 121
168, 153
258, 50
354, 92
126, 154
303, 324
90, 47
236, 316
316, 217
180, 110
70, 300
256, 221
56, 256
235, 190
56, 197
168, 145
250, 117
107, 77
259, 156
188, 301
183, 76
92, 224
41, 113
343, 90
162, 176
186, 77
312, 134
89, 98
191, 313
68, 225
85, 273
96, 161
260, 93
343, 153
247, 327
151, 197
    """.stripMargin.trim

  case class Point(x: Int, y: Int) {
    var score: Int = 0
    def manhattanDistance(ox: Int, oy: Int): Int = math.abs(x-ox) + math.abs(y-oy)
  }

  val lineRegex = """(\d+), (\d+)""".r
  val sources = input.lines.map { case lineRegex(x, y) => Point(x.toInt, y.toInt) }.toList

  var xs = sources.map(_.x).min to sources.map(_.x).max
  var ys = sources.map(_.y).min to sources.map(_.y).max
  // expand the bounding box to ensure counting qualifying areas partly outside
  xs = (xs.start - ys.length/2) to (xs.end + ys.length/2)
  ys = (ys.start - xs.length/2) to (ys.end + xs.length/2)

  for (x <- xs; y <- ys) {
    val closest :: nextClosest :: _ = sources.sortBy(_.manhattanDistance(x,y))
    if (closest.manhattanDistance(x,y) != nextClosest.manhattanDistance(x,y))
      closest.score += 1
  }

  // run around the outside and disqualify infinite areas
  for (x <- xs; y <- List(ys.start-1, ys.end+1))
    sources.minBy(_.manhattanDistance(x,y)).score = -1
  for (y <- ys; x <- List(xs.start-1, xs.end+1))
    sources.minBy(_.manhattanDistance(x,y)).score = -1

  val part1 = sources.map(_.score).max
  println(part1)

  var part2 = 0
  for (x <- xs; y <- ys) {
    if (sources.map(_.manhattanDistance(x,y)).sum < 10000)
      part2 += 1
  }
  println(part2)
}
