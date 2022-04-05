package ctci.stacksQueues

import scala.collection.mutable.{Seq => mSeq}
import scala.reflect.ClassTag

object ch3Q1:


    // Implemenent three stacks using a single Array
    class MultiStack[T:ClassTag](numStacks: Int, defaultSize: Int = 100):

        require(numStacks > 0)
        require(defaultSize > 0)

        private val storeSize = defaultSize * numStacks
        private val store = new Array[T](storeSize)

        private val stackMin = (0 until numStacks).toSeq.map(n => n *  storeSize / numStacks)
        private val stackMax = stackMin.drop(1) :+ storeSize
        private val stackTop = mSeq.from(stackMin).map(_ - 1)

        def push(value: T, stack: Int): Unit = 
            val newTop = stackTop(stack) + 1

            if newTop >= stackMax(stack) then
                throw new RuntimeException(s"Stack overflow in stack $stack")
            else
                stackTop(stack) = newTop
                store(newTop) = value


        def pop(stack: Int): T = 

            if !isEmpty(stack) then
                val currentTop = stackTop(stack)
                val valueTop = store(currentTop)
                stackTop(stack) = currentTop - 1
                valueTop
            else
                throw new RuntimeException("Cannot remove element from an empty stack")

            
        def head(stack: Int): T = 
            
            if !isEmpty(stack) then
                store(stackTop(stack))
            else 
                throw new RuntimeException("Stack empty")

        def isEmpty(stack: Int): Boolean = stackTop(stack) < stackMin(stack)
            