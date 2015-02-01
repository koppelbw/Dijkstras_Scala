/* *****************************************************************************
* Project: Dijkstra's Algorithm in Scala
* File: Dijkstras.scala
* Description: Learning a new language while implementing Dijkstra's shortest
*   path algorithm.
* Author: William Koppelberger
* Date: 2/2/15
* *****************************************************************************/


/* Class to represent and hold given Edge information */
class Edge(edge: Map[String, Any]) {
  
  val startVertex = edge.apply("startLocation").asInstanceOf[String]
  val endVertex = edge.apply("endLocation").asInstanceOf[String]
  val distance = edge.apply("distance").asInstanceOf[Int]
}


/* Class to represent and hold derived Vertex information */
class Vertex(name: String) {
  
  val vertName = name
  var lowestDistance = Int.MaxValue                      // Closest Int to infinity
  var visited = false
  var previousVertex: Vertex = null                      // Linked list of Vertices leading backwards
  var neighborEdgeInfo: List[Map[String, Any]] = List()  // Keep list of neighbor edge info [Name, Distance, Vertex]
}


/* Creates and fills all Edge and Vertex objects with data */
class CreateGraph(startingLocation: String, targetLocation: String, edgesList: List[Map[String, Any]]) {
  
  var edgeObjects: List[Edge] = List()        // Contains all Edges
  var vertexObjects: List[Vertex] = List()    // Contains all Vertices
  var vertNames: List[String] = List()        // Mirrors vertexObjects
  var tmp: Vertex = null
  
  
  /* Input ERROR Checking */
  try {
    if(startingLocation == null) { throw new NullPointerException }  // If start is null
    if(targetLocation == null) { throw new NullPointerException }    // If end is null
    if(edgesList == null) { throw new NullPointerException }         // If edge list is null
    
    // Warn user if an edge contains a null value
    edgesList.foreach { i=>
      i.keys.foreach { j=>
        if(i(j) == null) { throw new IllegalStateException }
      }
    }
  } catch { 
    case nil: NullPointerException => println("Input not found!!!\nExiting Code . . ."); System.exit(1)
    case input: IllegalStateException => println(
        "\n***********************************************************" +
        "\nWARNING: Invalid Edge value detected! Output compromised!!!" + 
        "\n***********************************************************\n")
  }
  
  
  /* Create Edge objects */
  edgesList.foreach { i => edgeObjects = new Edge(i) :: edgeObjects }

  
  /* Find and create all Vertices */
  edgeObjects.foreach { i =>
    
    // Add all unique vertices from 'startLocation' attribute
    if(!vertNames.contains(i.startVertex)){
      vertNames = i.startVertex :: vertNames
      vertexObjects = new Vertex(i.startVertex) :: vertexObjects
    }
    
    // Add all unique vertices from 'endLocation' attribute
    if(!vertNames.contains(i.endVertex)) {
      vertNames = i.endVertex :: vertNames
      vertexObjects = new Vertex(i.endVertex) :: vertexObjects
    }
  } 

  
  /* Set neighboring Vertex info */
    edgeObjects.foreach { i =>
      vertexObjects.foreach { j =>
        
        // Find neighbors for 'startVert' attribute
        if(j.vertName == i.startVertex) {
          
          // Find neighboring Vertex Object
          vertexObjects.foreach { k=> if(k.vertName == i.endVertex) { tmp = k } }
          j.neighborEdgeInfo = Map("neighbor" -> i.endVertex, "distance" -> i.distance,
              "neighborVertex" -> tmp) :: j.neighborEdgeInfo
        }
          
        // Find neighbors for 'endVert' attribute
        if(j.vertName == i.endVertex) {
          
          // Find neighboring Vertex Object
          vertexObjects.foreach { k=> if(k.vertName == i.startVertex) { tmp = k } }
          j.neighborEdgeInfo = Map("neighbor" -> i.startVertex, "distance" -> i.distance,
              "neighborVertex" -> tmp) :: j.neighborEdgeInfo
        }
      }
    }
    
    
    /* Run Dijkstra's Algorithm on the graph */
    new Dijkstras(startingLocation, targetLocation, edgeObjects, vertexObjects)
}


/* Prints shortest path from start to end locations */
class PrintPath(endLocation: Vertex) {

  var shortestPath: List[String] = List()
  var tmpVertex = endLocation
  var output: Map[String, Any] = null
  
  shortestPath = endLocation.vertName :: shortestPath
  
  // Backtrack from 'endLocation' to 'startLocation'
  while (tmpVertex.previousVertex != null) {
    shortestPath = tmpVertex.previousVertex.vertName :: shortestPath  
    tmpVertex = tmpVertex.previousVertex
  }
  
  output = Map("distance" -> endLocation.lowestDistance, "path" -> shortestPath.mkString(" => "))
  println(output)
}


/* Performs Dijkstra's algorithm using the created Edge and Vertex objects */
class Dijkstras(startingLocation: String, targetLocation: String, edgesList: List[Edge], vertexList: List[Vertex]) {
  
  var startVertex: Vertex = null      // Vertex object of the start location
  var endVertex: Vertex = null        // Vertex object of the end location
  var currentVertex: Vertex = null    // Vertex object of the current location

  
  /* Set up starting Vertex */
  vertexList.foreach { i =>
    if (i.vertName == startingLocation) {
      startVertex = i
      startVertex.lowestDistance = 0
      startVertex.visited = true
      currentVertex = startVertex
    }
    if (i.vertName == targetLocation) { endVertex = i }
  }
  
  // Check for null starting or ending location errors
  if(startVertex == null) { println("Starting Location is not a part of this graph!\nExiting . . ."); System.exit(0) }
  if(endVertex == null) { println("Ending Location is not a part of this graph!\nExiting . . ."); System.exit(0) }

  
  /* Dijkstra's Algorithm */
  while (!endVertex.visited) {
    
    
    /* Calculate distance to each unvistied neighbor of currentVertex */
    currentVertex.neighborEdgeInfo.foreach { i =>
      if(!(i.apply("neighborVertex").asInstanceOf[Vertex].visited)) {
        
        
        /* Calculate and update new distances for each neighboring Vertex */
        var newDistance = (currentVertex.lowestDistance + (i.apply("distance")).asInstanceOf[Int])
        if (newDistance < (i.apply("neighborVertex").asInstanceOf[Vertex].lowestDistance)) {
          i.apply("neighborVertex").asInstanceOf[Vertex].lowestDistance = newDistance
          
          // Update previous vertex to the vertex that updated it's new lowest distance
          i.apply("neighborVertex").asInstanceOf[Vertex].previousVertex = currentVertex
        }
      }
    }

    
    /* Determine which Vertex to visit next */
    var lowestUnvisitedDistance: Int = Int.MaxValue
    var lowestUnvisitedVertex: Vertex = null
    vertexList.foreach { i =>
      if (!i.visited && i.lowestDistance != Int.MaxValue) {
        if(i.lowestDistance < lowestUnvisitedDistance) {
          lowestUnvisitedDistance = i.lowestDistance
          lowestUnvisitedVertex = i
        }
      }
    }
    
    // Test if start and end vertices are in same graph
    if(lowestUnvisitedVertex == null) {
      println("Starting and Ending Locations do not exist in the same graph!!\nExiting . . . ")
      System.exit(0)
    }
    
    currentVertex = lowestUnvisitedVertex
    currentVertex.visited = true
  }
  
  
  /* Print path */
  new PrintPath(endVertex)
}