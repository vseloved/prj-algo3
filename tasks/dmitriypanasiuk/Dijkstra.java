package dmitriypanasiuk;

import java.util.ArrayDeque;
import java.util.Deque;

public class Dijkstra {
    private double[] distTo;
    private DirectedEdge[] edgeTo;
    private MinHeap heap;

    public Dijkstra(EdgeWeightedDigraph G, int s) {
        distTo = new double[G.getV()];
        edgeTo = new DirectedEdge[G.getV()];
        heap = new MinHeap(G.getV());

        for (int i = 0; i < distTo.length; i++) {
            distTo[i] = Double.POSITIVE_INFINITY;
        }
        distTo[s] = 0.0;
        heap.insert(s, distTo(s));
        while(!heap.isEmpty()) {
            int v = heap.delMin();
            for (DirectedEdge e : G.adj(v)) {
                relax(e);
            }
        }
    }

    private void relax(DirectedEdge e) {
        int from = e.From();
        int to = e.To();
        if (distTo[to] > distTo[from] + e.Weight()) {
            distTo[to] = distTo[from] + e.Weight();
            edgeTo[to] = e;
            if (heap.contains(to)) {
                heap.decreaseKey(to, distTo[to]);
            } else {
                heap.insert(to, distTo[to]);
            }
        }
    }

    public boolean hasPathTo(int v) {
        validateVertex(v);
        return distTo[v] < Double.POSITIVE_INFINITY;
    }

    public double distTo(int v) {
        validateVertex(v);
        return distTo[v];
    }

    public Iterable<DirectedEdge> pathTo(int v) {
        validateVertex(v);
        if (!hasPathTo(v)) return null;
        Deque<DirectedEdge> path = new ArrayDeque<>();
        for (DirectedEdge e = edgeTo[v]; e != null; e = edgeTo[e.From()]) {
            path.push(e);
        }
        return path;
    }

    private void validateVertex(int v) {
        if (v < 0 || v >= distTo.length) {
            throw new IllegalArgumentException("vertex " + v + " should be between 0 and " + (distTo.length-1));
        }
    }
}
