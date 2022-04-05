package ctci.stacksQueues

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ch3Q1.*

class ch3Q1Spec extends AnyFlatSpec with Matchers:

    val numStacks = 3
    type testMultiStack = MultiStack[Int]

    val testValues = Map(
        0 -> Seq(0),
        1 -> Seq(10, 11),
        2 -> Seq(100, 101, 102)
    )

    def getTestStack(): testMultiStack = 
        val testMs = new testMultiStack(3)

        for
            (s, xs) <- testValues
            x <- xs
        do testMs.push(x, s)

        testMs


    "MultiStack" should "be instantiable" in {

        val ms = new MultiStack[Int](3)

    }

    it should "be possible to push values onto each stack" in {

        
        val ms = new testMultiStack(3)

        val unit = 
            for
                (s, xs) <- testValues
                x <- xs
            do ms.push(x, s)

        assert( unit == () )

    }



    it should "be possible to push values onto each stack and retrieve them through head" in {

        val testMs = getTestStack()
        
        val checkHead = testValues.map( (s, _) => testMs.head(s) ).toList
        val expectedHead = testValues.map( (s, xs) => xs.last).toList
        
        checkHead should contain theSameElementsInOrderAs expectedHead

    }

    it should "be possible to push values onto each stack and retrieve them through pop" in {

        val testMs = getTestStack()

        val checkPop = testValues.map( (s, _) => testMs.pop(s) ).toList
        val expectedPop = testValues.map( (s, xs) => xs.last).toList
        
        checkPop should contain theSameElementsInOrderAs expectedPop

    }

    it should "have an empty stack after popping single value" in {

        val testMs = getTestStack()

        testMs.pop(0)

        (0 until numStacks).map(testMs.isEmpty(_)) should contain theSameElementsInOrderAs Seq(true, false, false)
        

    }