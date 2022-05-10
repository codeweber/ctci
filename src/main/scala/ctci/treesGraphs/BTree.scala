package ctci.treesGraphs

enum BTree[+T]:
  case Branch(value: T, left: BTree[T], right: BTree[T])
  case End

  def isLeaf: Boolean = 
    this match
      case Branch(_, End, End) => true
      case _ => false

  def isEmpty: Boolean = 
    this match
      case End => true 
      case _ => false