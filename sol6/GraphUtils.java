import java.io.*;
import java.util.*;

public abstract class GraphUtils {

  static <V> Graph<V> emptyGraph() {
    return new AdjSetGraph<V>();
  }

  static <V> Graph<V> emptyDirectedGraph() {
    // TODO: replace the following line with a call to the
    // constructor for the AdjSetDirectedGraphClass.
    // We are only using AdjSetGraph here to ensure that the initial
    // version of the code will compile.
    return new AdjSetDirectedGraph<V>();
  }

  static <V> String dumpGraph(Graph<V> g) {
    String s = "Vertex count = " + g.vertexCount() + "\n";
    for (V v : g.vertices()) {
      s += v + ":";
      for (V w : g.neighbors(v)) {
        s += " " + w;
      }
      s += "\n";
    }
    return s;
  }

  public static <V> void toDot(Graph <V> g, String gname) throws Exception {
    toDot(g, gname, "graph", "--");
  }

  public static <V> void directedToDot(Graph <V> g, String gname) throws Exception {
    toDot(g, gname, "digraph", "->");
  }

  public static <V> void toDot(Graph <V> g, String gname, String type, String arrow) throws Exception {
    java.io.PrintStream out = new java.io.PrintStream(gname + ".dot");
    out.println(type + " {");
    for (V v : g.vertices()) {
      for (V w : g.neighbors(v)) {
        out.println("\"" + v + "\" " + arrow + " \"" + w + "\";");
      }
    }
    out.println("}");
    out.close();
  }

  public static <V> Graph<V> flip(Graph<V> g) {
     // Construct a flipped version of g as a directed graph, using
     // a Java translation of the algorithm from Week 5, Slide 70.
     Graph<V> flipped = emptyDirectedGraph();
     for(V v: g.vertices())
     {
        flipped.addVertex(v);
     }
     for(V v: g.vertices())
     {
       for(V w: g.neighbors(v)){
         flipped.addEdge(w,v);
       }
     }

     return flipped;  // this is a stub, not the correct implementation!
  }

  public static <V> List<List<V>> scc(Graph<V> g) {
    // Calculate the list of strongly connected components in the given
    // graph, returning the result as a list of lists.  You should
    // Follow the algorithm that was presented using Python code on the
    // slides for Week 5, Slides 66-79, which was also presented at the
    // start of each of the Week 5 lab sessions.  It is not necessary
    // that you understand exactly how the algorithm works, just that
    // are able to determine how each part of the Python code can be
    // mapped into corresponding Java code that takes advantage of the
    // abstract datatypes for lists and sets in the Java Collections
    // Framework, as well as the abstract datatype for graphs that we
    // have been working with in the Week 8 labs and in the rest of
    // this assignment.

    // NOTE: It is expected that you will need to define multiple
    // auxiliary "private static" methods in this class to complete
    // your implementation.  (For example, you will need to implement
    // at least one "visit" method for each of the two depth first
    // searches that are required.)

    List<V> finished;
    finished = dfs(g);
    System.out.print("DFS order: " + finished+ '\n');
    Collections.reverse(finished);
    Graph<V> flipped;
    flipped = flip(g);
    Set<V> visited = new HashSet<V>();
    List<List<V>> sccs = new LinkedList<List<V>>();

    for(V v:finished){
      if(visited.contains(v) == false){
         List<V>scc = new LinkedList<V>();
         visitscc(v, scc, visited, flipped);
         sccs.add(scc);
      }
    }
    
    return sccs;


    //return new LinkedList<List<V>>();
    // This is a stub, not the correct implementation!  But it does
    // show how you can construct an empty list (in this case, an
    // empty list containing elements of type List<V>) using the
    // Java collections framework.
  }
    

    public static <V> void visitscc(V v, List<V> scc, Set<V> visited, Graph<V> flipped){
       visited.add(v);
       scc.add(v);
       for(V w:flipped.neighbors(v)){
	    if(!visited.contains(w)){
		visitscc(w, scc, visited, flipped);
	    }
       }
    }

    public static <V> List<V> dfs(Graph<V> g){
	Set<V> visited = new HashSet<V>();
	List<V> finished = new LinkedList<V>();
	for(V v:g.vertices()){
		visit(v,visited,g,finished);
	}
	return finished;
    }

    public static <V> void visit(V v, Set<V> visited, Graph<V> g, List<V> finished){
	if(!visited.contains(v)){
		visited.add(v);
		for(V w: g.neighbors(v)){
			visit(w, visited, g, finished);
		}
		finished.add(v);
	}
    }

}
