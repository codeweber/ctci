package ctci.treesGraphs

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ch4Q3.*
import BTree.*

class ch4Q3Spec extends AnyFlatSpec with Matchers:

    "listOfDepths" should "given a tree of depth 0, return a single element list" in {
        val testTree = Branch(0, End, End)
        listOfDepths(testTree) shouldBe List((0, List(testTree)))
    }

    it should "given a tree of depth 1, return a two element list" in {
        val testTree = Branch(0, Branch(2, End, End), Branch(1, End, End))
        val expected = List(
        (1, List(Branch(1, End, End), Branch(2,End,End))),
        (0, List(testTree))
        )
        listOfDepths(testTree) shouldBe expected
    }

    it should "given a tree of depth 1, return a two element list, excluding End" in {
        val testTree = Branch(0, End, Branch(1, End, End))
        val expected = List(
        (1, List(Branch(1, End, End))),
        (0, List(testTree))
        )
        listOfDepths(testTree) shouldBe expected
    }



