package ctci.treesGraphs

object ch4Q2:

    enum Tree[T]:
        case Leaf(value: T)
        case Branch(value: T, left: Tree[T], right: Tree[T])


    def createMinimalBST[T](values: Array[T]): Tree[T] =

        def createSubtree(start: Int, end: Int): Tree[T] =

            val numElements = end - start
            if numElements <= 0 then 
                null
            else if numElements == 1 then 
                Tree.Leaf(values(start))
            else
                val middle = start + numElements/2 
                val value = values(middle)
                Tree.Branch(value, createSubtree(start, middle), createSubtree(middle+1, end))


        createSubtree(0, values.length)
