package Dijkstra;

object DijkstraPath {
  /* Main */
  def main(args: Array[String]) = {
    
    val m_graph = new WeightedGraph ;
    
    Console.println("-------------------") ;
    for(line <- scala.io.Source.fromFile("Input.dat").getLines) {
      val temp = line.trim.split(' ');
      val from = temp(0).toString ;
      val to = temp(1).toString ;
      val cost = java.lang.Integer.parseInt(temp(2).toString) ;
      
      m_graph.addEdge(from, to, cost) ;
      m_graph.addEdge(to, from, cost) ;
      Console.println("Adding edge from " + from + " to " + to + " cost " + cost);
    }
  
    Console.println("-------------------") ;
    for(line <- scala.io.Source.fromFile("Test.dat").getLines) {
      val temp = line.trim.split(' ');
      val from = temp(0).toString ;
      val to = temp(1).toString ;
      var total  = 0;
        
      Console.println ("from node " + from) ;

      try {
        val paths = m_graph.pickDesiredPath(from, to) ;

        if (paths == Nil) {
          Console.println("No paths to " + to);
        }
        else for(Pair(to, weight) <- paths) {
	  total = weight + total;
	  Console.println("to " + to + " cost " + weight);
        }
      }
      catch {
        case m: Exception => Console.println(" Exception: "+ m.getMessage);
        case e: Throwable => Console.println(e.getMessage) ;
      }
      Console.println(" total to node " + to + " is " + total );
    }
    Console.println("-------------------") ;
  }
}

