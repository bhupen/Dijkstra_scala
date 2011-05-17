package Dijkstra;

import scala.collection.mutable._ ;

class WeightedGraph {

  // Map of from node to nodes and the weight for that edge
  private val edges = new HashMap[String, HashMap[String, java.lang.Integer]] ;
  private val nodes = new HashSet[String] ;

  // Add Edge to the WeightedGraph
  // Add node to the list of nodes
  def addEdge(from: String, to: String, weight: java.lang.Integer) = {
    //if Stringertex does not exist in the edge map then add
    if (!edges.contains(from)) {
      val adjMap = new HashMap[String, java.lang.Integer] ;
      adjMap += to -> weight ;
      edges += from -> adjMap
    } else {
      val adjMap = edges(from) ;
      adjMap += to -> weight
    }
    // add the 2 nodes to the list of nodes
    nodes += from ;
    nodes += to
  }

  // Calculate weights for all paths from start to end
  private def calcWeightToAllPaths(start: String, end: String) = {
    val listOfPaths = new HashMap[String, String] ;    // list of possible paths
    val path = new HashSet[String] ;       // best path
    val dist = new HashMap[String, java.lang.Integer] ;  // weigth for each path in listOfPaths 

    if(end == null)
       throw new Exception("calcWeightToAllPaths : end param cannot be null");
    if(start == null)
       throw new Exception("calcWeightToAllPaths : end param cannot be null");

    // start with start node
    dist += start -> 0 ;
    path += start ;

    var endReached = false ;

    while (!path.isEmpty && !endReached) {
      val u = path.iterator.foldLeft(path.iterator.next) {
        (u, v) => if (dist(u) <= dist(v)) u else v
      }
      path -= u ;
     // update distances and add to path
      for(v <- edges(u).keys) {
        val vNewDist = dist(u) + edges(u)(v) ;
        if (!dist.isDefinedAt(v) || vNewDist < dist(v)) {
          dist += v -> vNewDist ;
          listOfPaths += v -> u ;
          path += v ;
        }
      }
      endReached = (u == end) ;
    }
    Tuple3(dist, listOfPaths, endReached);
  }

  // Find the pickDesiredPath
  def pickDesiredPath(start: String, end: String) = {
    if (!edges.contains(start))
      throw new Exception("pickDesiredPath: "+start+" not in graph") ;
    if (!nodes.contains(end))
      throw new Exception("pickDesiredPath: "+end+" not in graph") ;

    // get shortest-distances, paths, end result
    val Tuple3(dist, paths, endReached) = calcWeightToAllPaths(start, end) ;

    // build path from start to end based on paths
    var path: List[Pair[String, java.lang.Integer]] = Nil ;
    if (endReached) {
      var v = end ;
      while (v != start) {
        path = Pair(v, dist(v)) :: path ;
        // iterate on paths
        v = paths(v);
      }
    }
    path;
  }
}
