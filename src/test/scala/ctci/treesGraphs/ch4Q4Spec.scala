package ctci.treesGraphs

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ch4Q4.{isBalanced, isBalancedStackSafe}

class ch4Q4Spec extends AnyFlatSpec with Matchers:

    import BTree.*

    val leaf = Branch(0, End, End)
    val perfectTree = Branch(0, Branch(1, End, End), Branch(2, End, End))
    val completeTree = 
        Branch(0, 
                Branch(0, leaf, leaf),
                leaf
        )
    val imbalancedTree = 
        Branch(0,
            Branch(1, End, End),
            completeTree
        )

    "isBalanced" should "return true for a Leaf node" in {
        isBalanced(leaf) shouldBe true
    }

    it should "return true for a perfect tree" in {
        isBalanced(perfectTree) shouldBe true
    }

    it should "return true for a complete tree" in {
        isBalanced(completeTree) shouldBe true
    }

    it should "return false for an imbalanced tree" in {
        isBalanced(imbalancedTree) shouldBe false
    }


    "isBalancedStackSafe" should "return true for a Leaf node" in {
        isBalancedStackSafe(leaf) shouldBe true
    }

    it should "return true for a perfect tree" in {
        isBalancedStackSafe(perfectTree) shouldBe true
    }

    it should "return true for a complete tree" in {
        isBalancedStackSafe(completeTree) shouldBe true
    }

    it should "return false for an imbalanced tree" in {
        isBalancedStackSafe(imbalancedTree) shouldBe false
    }
