package ctci.treesGraphs

import scala.annotation.tailrec

object ch4Q4 {

    import BTree.*
  
    def getHeight[T](tree: BTree[T]): Int = 

        tree match
            case End => 0
            case Branch(_, End, End) => 0
            case Branch(_, left, right) => 1 + (getHeight(left) max getHeight(right))

    def isBalanced[T](tree: BTree[T]): Boolean = 

        tree match
            case End => true 
            case Branch(_, End, End) => true 
            case Branch(_, left, right) => 
                isBalanced(left) && isBalanced(right) && ((getHeight(left) - getHeight(right)).abs <= 1)

    def isBalancedStackSafe[T](tree: BTree[T]): Boolean = 

        @tailrec
        def loop(remainingSubTrees: List[BTree[T]], nodeStack: List[BTree[T]], heightStack: List[Int]): Boolean = 

            remainingSubTrees match
                case Nil => true 
                case node :: sts =>  
                    nodeStack match
                        case n :: ns if n == node =>
                            val rightHeight = heightStack.head 
                            val leftHeight = heightStack.tail.head 
                            if (rightHeight - leftHeight).abs > 1 then
                                false
                            else
                                val nodeHeight = 1 + (rightHeight max leftHeight)
                                loop(sts, ns, nodeHeight :: heightStack.tail.tail) 
                        case _ => 
                            node match
                                case End => loop(sts, nodeStack, 0 :: heightStack)
                                case Branch(_, End, End) => loop(sts, nodeStack, 0 :: heightStack)
                                case Branch(_, left, right) => loop(left :: right :: remainingSubTrees, node :: nodeStack, heightStack)
                        
        loop(List(tree), Nil, Nil)
}
