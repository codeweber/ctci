package ctci.treesGraphs

import scala.collection.immutable.Queue
import scala.annotation.tailrec

object ch4Q3 {
  
    import BTree.*

    type DepthNode = [T] =>> (Int, BTree[T])
    type DepthNodes = [T] =>> (Int, List[BTree[T]])

    def listOfDepths[T](tree: BTree[T]): List[DepthNodes[T]] = 

        @tailrec
        def loop(queueSubTrees: Queue[DepthNode[T]], currentDepth: Int, nodeStack: List[BTree[T]], agg: List[DepthNodes[T]]): List[DepthNodes[T]] = 

            if queueSubTrees.isEmpty then
                if nodeStack.isEmpty then
                    agg 
                else 
                    (currentDepth, nodeStack) :: agg 
            else 
                val ((nextDepth, nextNode), queue) = queueSubTrees.dequeue
                if nextDepth == currentDepth then
                    nextNode match 
                        case End => loop(queue, currentDepth, nodeStack, agg)
                        case Branch(value, left, right) =>
                            val nextQueue = queue.enqueueAll(List((nextDepth+1, left), (nextDepth+1, right)))
                            loop(nextQueue, currentDepth, nextNode :: nodeStack, agg)
                else
                    val newAgg = (currentDepth, nodeStack) :: agg
                    nextNode match
                        case End => loop(queue, nextDepth, Nil, newAgg)
                        case Branch(value, left, right) => 
                            val nextQueue = queue.enqueueAll(List((nextDepth+1, left), (nextDepth+1, right)))
                            loop(nextQueue, nextDepth, List(nextNode), newAgg)


        loop(Queue((0, tree)), 0, Nil, Nil)
}
