package ctci.treesGraphs

import Graphs.*
import scala.collection.mutable.Queue
import scala.collection.mutable.Set

object ch4Q1:

    def existsRoute(start: Node, end: Node): Boolean =

        val frontier = Queue.empty[Node]
        val visited = Set.empty[Node]
        var isPath = false 

        frontier.enqueue(start)
        visited.add(start)

        while !frontier.isEmpty && !isPath do
            
            val nextNode = frontier.dequeue

            for
                child <- nextNode.children
            do 
                if child == end then 
                    isPath = true
                
                if visited.add(child) then 
                    frontier.enqueue(child)


        isPath



