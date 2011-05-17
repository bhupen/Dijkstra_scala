import org.specs._
import Dijkstra._

object newSpecification extends Specification {

    val m_graph = new WeightedGraph;

    for(line <- scala.io.Source.fromFile("Input.dat").getLines) {
      val temp = line.trim.split(' ');
      val from = temp(0).toString ;
      val to = temp(1).toString ;
      val cost = java.lang.Integer.parseInt(temp(2).toString) ;

      m_graph.addEdge(from, to, cost) ;
      m_graph.addEdge(to, from, cost) ;
    }
    def calcTotal(paths: List[Pair[String, java.lang.Integer]]) : java.lang.Integer = {
        var total = 0;
        for(Pair(to,weight) <- paths)
	    total = weight + total;
        total;
    }

    "B to A" in {
       calcTotal(m_graph.pickDesiredPath("B", "A")) must be(5)
    }
    "C to D" in {
       calcTotal(m_graph.pickDesiredPath("C", "D")) must be(21)
    }
    "A to H" in {
       calcTotal(m_graph.pickDesiredPath("A", "H")) must be(36)
    }
    "D to H" in {
       calcTotal(m_graph.pickDesiredPath("D", "H")) must be(8)
    }
    "A to G" in {
       calcTotal(m_graph.pickDesiredPath("A", "G")) must be(36)
    }
    "A to Z" in {
       calcTotal(m_graph.pickDesiredPath("A", "Z")) must be(44)
    }
}
