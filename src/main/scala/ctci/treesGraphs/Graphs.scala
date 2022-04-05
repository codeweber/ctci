package ctci.treesGraphs


object Graphs:

    case class Node(label: String):

        val children = scala.collection.mutable.Set.empty[Node]


    object Node:

        def createDirectedEdge(parent: Node, child: Node): Boolean =

            if parent == child then
                false
            else 
                parent.children.add(child)

        def createUndirectedEdge(node1: Node, node2: Node): Boolean =

            if node1 == node2 then 
                false
            else 
                node1.children.add(node2) & node2.children.add(node1)


    case class Graph(nodes: Array[Node])

    val nodeA = Node("A")
    val nodeB = Node("B")
    val nodeC = Node("C")
    val nodeD = Node("D")
    val nodeE = Node("E")
    Node.createDirectedEdge(nodeA, nodeB)
    Node.createDirectedEdge(nodeA, nodeC)
    Node.createDirectedEdge(nodeA, nodeD)
    Node.createDirectedEdge(nodeC, nodeD)
    Node.createDirectedEdge(nodeD, nodeB)
    Node.createDirectedEdge(nodeD, nodeE)
    Node.createDirectedEdge(nodeE, nodeB)
    Node.createDirectedEdge(nodeE, nodeC)
    val socialNetwork = Graph(Array(nodeA, nodeB, nodeC, nodeD, nodeE))


    @main def TryGraphs =

        val nA = Node("A")
        val nB = Node("B")
        val nC = Node("C")

        Node.createUndirectedEdge(nA, nB)
        Node.createUndirectedEdge(nA, nC)

        val g = Graph(Array(nA,nB,nC))


        println(s"Graph: $g")
        
        for 
            node <- g.nodes
            child <- node.children
        do println(s"$node has child $child")