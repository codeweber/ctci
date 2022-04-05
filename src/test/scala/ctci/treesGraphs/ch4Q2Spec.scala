package ctci.treesGraphs

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class ch4Q2Spec extends AnyFlatSpec with Matchers:

    import ch4Q2.*


    "createMinimalBST" should "create a tree with a single leaf" in {

        createMinimalBST(Array(1)) shouldBe Tree.Leaf(1)

    }

    it should "create a tree with a branch and one leaf" in {

        createMinimalBST(Array(1,2)) shouldBe Tree.Branch(2, Tree.Leaf(1), null)

    }

    it should "create a tree with a branch and two leaves" in {

        createMinimalBST(Array(1,2,3)) shouldBe Tree.Branch(2, Tree.Leaf(1), Tree.Leaf(3))

    }

    it should "create a tree with a branch with two subtrees" in {

        createMinimalBST(Array(1,2,3,4,5)) shouldBe Tree.Branch(3, Tree.Branch(2, Tree.Leaf(1), null), Tree.Branch(5, Tree.Leaf(4), null))

    }