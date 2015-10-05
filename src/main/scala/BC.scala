package bc

import java.text.DecimalFormat


import scala.annotation.tailrec
import scala.io.StdIn

//operators
//+,-,*,/

//Base trait to deserialize each member of an expression to
trait BCInput {}

//Numeric types
case class BCNumber(value: BigDecimal) extends BCInput

//Base operator type
trait BCOperator extends BCInput {
  def precedence: Int
  def calc(a:BCNumber, b:BCNumber):BCNumber
}

//Addition operator
case object BCAdd extends BCOperator {
  def precedence = 1
  def calc(a:BCNumber, b:BCNumber) = BCNumber(a.value + b.value)
}

case object BCSubtract extends BCOperator {
  def precedence = 1
  def calc(a:BCNumber, b:BCNumber) = BCNumber(a.value - b.value)
}

//Multiplication operator
case object BCMult extends BCOperator {
  def precedence = 2
  def calc(a:BCNumber, b: BCNumber) :BCNumber = BCNumber(a.value * b.value)
}

//division rounds down
case object BCDivide extends BCOperator {
  def precedence = 2
  def calc(a:BCNumber, b: BCNumber) :BCNumber = {
    BCNumber(BigDecimal((a.value/b.value).toInt))
  }

}

/**
 * Main namespace
 */
object BC {

  //regex matchers. they will match at the beginning of the string
  val operator = """^([\+\-\*\/])(.*)""".r
  val int = """^(\d+)(.*)""".r
  val double = """^(\d*\.\d*)(.*)""".r

  /**
   * Converts a string expression (infix) into a deserialized form (still infix). Order is preserved
   *
   * @param input
   * @return
   */
  def toDeserializedForm (input: String): List[BCInput] = {

    //recursively parse the string
    @tailrec def getFormRecurse (iexpr:String, formList: List[BCInput]): List[BCInput] = {
      iexpr.trim match {
        case "" => formList
        case operator(op, rest) => {
          val dop = op match {
            case "+" => BCAdd
            case "-" => BCSubtract
            case "*" => BCMult
            case "/" => BCDivide
          }
          getFormRecurse(rest, formList :+ dop)
        }
        case double(num, rest) =>  getFormRecurse(rest, formList :+ BCNumber(BigDecimal(num)))
        case int(num, rest) => getFormRecurse(rest, formList :+ BCNumber(BigDecimal(num)))
      }
    }

    getFormRecurse(input, List())
  }

  /**
   * Converts an infix expression to postfix
   * 
   * @param expression an infix expression
   * @return the equivalent postfix expression
   */
  def toPostFix (expression: List[BCInput]): List[BCInput] = {

    val (pexp, operators) = expression.foldLeft(List[BCInput](), List[BCOperator]()) { (acc, item) =>

      val postFix = acc._1
      val operatorStack = acc._2

      item match {
        case num: BCNumber =>
          (postFix :+ num, operatorStack)
        case operator: BCOperator =>
          if (operatorStack.isEmpty) { //just push the operator on the (blank) stack
            (postFix, operator :: operatorStack)
          } else { //stack is not empty
            //pop elements off the stack
            val (popped, stackRemains) = operatorStack.span(topop => topop.precedence >= operator.precedence)
            (postFix ++ popped, operator :: stackRemains)
          }
      }
    }

    pexp ++ operators
  }

  /**
   * Evaluates a postfix expression
   *
   * @param postFix
   * @return
   */
  def evaluatePostfix(postFix: List[BCInput]):BigDecimal = {

    val result = postFix.foldLeft(List[BCNumber]()) { (stack, item) =>
      item match {
        case num: BCNumber =>
          num :: stack //prepend the stack with any operands
        case operator:BCOperator =>
          val (firstTwo, remainder) = stack.splitAt(2) //do some math on the top two operands on the stack
          val res = operator.calc(firstTwo(1), firstTwo(0)) // this can throw an IndexOutOfBoundsException if the expression is invalid
          res::remainder
      }
    }

    if (result.length == 1) {
      result.head.value
    } else {
      //something has gone horribly wrong - expression was invalid
      throw new RuntimeException("invalid expression")
    }
  }

  /**
   * Composed infix expression evaluation
   * First it deserializes the input to {BCInput}s, then converts it to postfix, then evaluates the postfix expression
   * @return Decimal
   */
  val evaluateInfixExpressionString = toDeserializedForm _ andThen toPostFix _ andThen evaluatePostfix

  /**
   * Prints the evaluation of str to stdout
   *
   * @param str infix expression to eval
   */
  def printEval (str: String) = {
    try {
      if (str.trim.isEmpty) {
        println("")
      } else {
        println(evaluateInfixExpressionString(str))
      }
    } catch {
      case _:Throwable => println("invalid expression")
    }
  }

  /**
   * Keeps the prompt going. Although, it's not really much of a REPL b/c variables aren't supported
   */
  @tailrec def replMode() {
    StdIn.readLine(">>") match {
      case "quit" => System.exit(0)
      case strExpr => printEval(strExpr)
    }

    replMode
  }

  def main(args: Array[String]) {
    if (args.isEmpty) {
      replMode
    } else {
      printEval(args(0))
    }
  }
}