package main.scala.digitparser

class DigitParser[N](zero: N, succ: N => N) {
  def parse(d: String): N = {
    def loop(n: String, cur: N): N = {
      if (isZeroString(n)) return cur

      loop(pred(n), succ(cur))
    }

    loop(d, zero)
  }

  def pred(d: String): String = {
    val lst = d.reverse.toList
    if (lst.forall(_ == '0')) return "0"

    def loop(l: List[Char]): String = {
      if (l.head == '0') loop(l.tail) + "9"
      else predMap.get(l(0)) match {
        case Some(s) => cleanString((s :: l.tail).reverse)
        case None => throw new Exception("Invalid digit")
      }
    }

    loop(lst)
  }

  private def cleanString(l: List[Char]): String = l.dropWhile(_ == '0').mkString

  private def isZeroString(d: String) = d.dropWhile(_ == '0') == ""

  private def baseTenPreds = Map(
    '9' -> '8',
    '8' -> '7',
    '7' -> '6',
    '6' -> '5',
    '5' -> '4',
    '4' -> '3',
    '3' -> '2',
    '2' -> '1',
    '1' -> '0'
  )

  private def predMap = baseTenPreds
}

object StandardParser {
  val z = 0
  def succ(n: Int) = n + 1

  val intParser = new DigitParser[Int](z, succ)

  def parse(d: String) = intParser.parse(d)
}