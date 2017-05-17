package dmitriypanasiuk;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.concurrent.ForkJoinPool;

public class FloydWarshall {
    private double[][] distTo;
    private DirectedEdge[][] edgeTo;

    public FloydWarshall(AdjMatrixEdgeWeightedDigraph G, boolean parallel) {
        int V = G.V();
        distTo = new double[V][V];
        edgeTo = new DirectedEdge[V][V];

        for (int v = 0; v < V; v++) {
            for (int w = 0; w < V; w++) {
                distTo[v][w] = Double.POSITIVE_INFINITY;
            }
        }

        for (int v = 0; v < G.V(); v++) {
            for (DirectedEdge e : G.adj(v)) {
                distTo[e.From()][e.To()] = e.Weight();
                edgeTo[e.From()][e.To()] = e;
            }
            if (distTo[v][v] >= 0.0) {
                distTo[v][v] = 0.0;
                edgeTo[v][v] = null;
            }
        }

        if (parallel) {
            new ForkJoinPool().invoke(new FloydWarshallParallel(distTo, edgeTo));
        } else {
            //sequential implementation
            for (int i = 0; i < V; i++) {
                for (int v = 0; v < V; v++) {
                    if (edgeTo[v][i] == null) continue;  // optimization
                    for (int w = 0; w < V; w++) {
                        if (distTo[v][w] > distTo[v][i] + distTo[i][w]) {
                            distTo[v][w] = distTo[v][i] + distTo[i][w];
                            edgeTo[v][w] = edgeTo[i][w];
                        }
                    }
                }
            }
        }
    }

    public boolean hasPath(int s, int t) {
        validateVertex(s);
        validateVertex(t);
        return distTo[s][t] < Double.POSITIVE_INFINITY;
    }

    public double dist(int s, int t) {
        validateVertex(s);
        validateVertex(t);
        return distTo[s][t];
    }

    public Iterable<DirectedEdge> path(int s, int t) {
        validateVertex(s);
        validateVertex(t);
        if (!hasPath(s, t)) return null;
        Deque<DirectedEdge> path = new ArrayDeque<>();
        for (DirectedEdge e = edgeTo[s][t]; e != null; e = edgeTo[s][e.From()]) {
            path.addFirst(e);
        }
        return path;
    }

    private void validateVertex(int v) {
        int V = distTo.length;
        if (v < 0 || v >= V)
            throw new IllegalArgumentException("vertex " + v + " is not between 0 and " + (V-1));
    }
}
