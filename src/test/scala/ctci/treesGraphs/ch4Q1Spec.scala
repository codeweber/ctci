package ctci.treesGraphs

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ch4Q1Spec extends AnyFlatSpec with Matchers:

    import Graphs.*
    import ch4Q1.*

    "existsRoute" should "return true for a direct path" in {

        existsRoute(nodeA, nodeB) shouldBe true

    }

    it should "return true for an indirect path" in {

        existsRoute(nodeA, nodeE) shouldBe true

    }

    it should "return false when no path exists" in {

        existsRoute(nodeB, nodeC) shouldBe false

    }
