package bc

import org.scalatest.FunSpec

class BCSpec extends FunSpec {

  describe("toDeserializedForm") {

    describe("singletons") {

      it("gets the deserialized form of an operator") {
        val x = BC.toDeserializedForm("+")
        assert(x.length === 1)
        assert(x(0).equals(BCAdd))
      }

      it("gets the deserialized form of a single digit") {
        val nums = BC.toDeserializedForm("2")
        assert(nums.length === 1)
        assert(nums(0).equals(BCNumber(2)))
      }

      it("gets the deserialized form of many digits") {
        val nums = BC.toDeserializedForm("2345")
        assert(nums.length === 1)
        assert(nums(0).equals(BCNumber(2345)))
      }

      it("can handle double") {
        val nums = BC.toDeserializedForm("2.2")
        assert(nums.length === 1)
        assert(nums(0) == BCNumber(2.2))

        val numsMore = BC.toDeserializedForm("2.345")

        assert(numsMore.length === 1)
        assert(numsMore(0) == BCNumber(2.345))
      }
    }

    describe("More operators/operands") {

      it ("yields the correct expression for '2.2 + 1 - 1 * 2.65'") {
        val expression = BC.toDeserializedForm("2.2 + 1 - 1 * 2.65")

        assert(expression ==
          List(BCNumber(2.2),
            BCAdd,
            BCNumber(1),
            BCSubtract,
            BCNumber(1),
            BCMult,
            BCNumber(2.65)))
      }
    }
  }

  describe ("toPostfix") {
    it ("trivial case") {
      val infix = BC.toDeserializedForm("2 + 1 + 3")
      val postfix = BC.toPostFix(infix)

      assert(postfix == List(
        BCNumber(2),
        BCNumber(1),
        BCAdd,
        BCNumber(3),
        BCAdd
      ))
    }

    it ("more operands and operators") {
      val infix = BC.toDeserializedForm("2 + 1")
      val postfix = BC.toPostFix(infix)

      assert(postfix == List(
        BCNumber(2),
        BCNumber(1),
        BCAdd
      ))
    }

    it ("mixes precedence") {
      val infix = BC.toDeserializedForm("1 + 2 * 3 - 4")
      val postfix = BC.toPostFix(infix)

      assert(postfix == List(
        BCNumber(1),
        BCNumber(2),
        BCNumber(3),
        BCMult,
        BCAdd,
        BCNumber(4),
        BCSubtract
      ))
    }
  }


  describe("evaluation") {
    it ("does evaluation correctly") {
      val infix = BC.toDeserializedForm("1 + 2 * 3 - 4")
      val postfix = BC.toPostFix(infix)
      val result = BC.evaluatePostfix(postfix)

      assert(result == 3)

    }
  }

  describe("evaluateSimpleExpression") {
    it ("1+3==4") {
      val x = BC.evaluateInfixExpressionString("1+3")
      assert(x == 4)
    }

    it ("1++3 throws an IndexOutOfBoundsException") {
      intercept[IndexOutOfBoundsException] {
        BC.evaluateInfixExpressionString("1++3")
      }
    }

    it ("1 3 throws a RuntimeException") {
      intercept[RuntimeException] {
        BC.evaluateInfixExpressionString("1 3")
      }
    }

    it ("4+6*10-5/2") {
      val x = BC.evaluateInfixExpressionString("4+6*10-5/2")
      assert(x==62)
    }

    it ("5/2") {
      val x = BC.evaluateInfixExpressionString("5/2")
      assert(x == 2)
    }

    it ("3.1 - 4.1 - 3.3 does not get confused with float epsilons") {
      val x = BC.evaluateInfixExpressionString("3.1 - 4.1 - 3.3")
      assert(x== -4.3)
    }

  }
}
