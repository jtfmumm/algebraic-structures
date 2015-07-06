package main.scala.sets.ui
//import digitparser.DigitParser
import main.scala.sets._
import main.scala.digitparser.DigitParser

object SetUI {
  val digits = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
  val compOps = List(">", ">=", "<", "<=", "==")

  def isDigit(c: Char) = digits.contains(c)
  def isNumber(s: String) = s.forall(_.isDigit)
  def isComparisonOp(s: String): Boolean = compOps.contains(s)

  val natParser = new DigitParser[PureSet](PureSet.zero, PureSet.succ)

  def parseInt(s: String) = natParser.parse(s)
  def parseCalc(s: String): PureSet = {
    val op = getOp(s)
    val n1 = parseInt(getLeftOperand(s))
    val n2 = parseInt(getRightOperand(s))
    op match {
      case "+" => PureSet.plus(n1, n2)
      case "-" => PureSet.minus(n1, n2)
      case "*" => PureSet.times(n1, n2)
      case _ => throw new Exception("Invalid operator")
    }
  }
  def parseComp(s: String): Boolean = {
    val op = getOp(s)
    val n1 = parseInt(getLeftOperand(s))
    val n2 = parseInt(getRightOperand(s))
    op match {
      case ">" => PureSet.gt(n1, n2)
      case ">=" => PureSet.gte(n1, n2)
      case "<" => PureSet.lt(n1, n2)
      case "<=" => PureSet.lte(n1, n2)
      case "==" => PureSet.eq(n1, n2)
      case _ => throw new Exception("Invalid operator")
    }
  }

  def eval(s: String) = {
    if (isComparisonOp(getOp(s))) parseComp(s) else parseCalc(s).size
  }

  private def getOp(s: String): String = s.split(" ")(1)
  private def getLeftOperand(s: String) = s.split(" ")(0)
  private def getRightOperand(s: String) = s.split(" ")(2)
}