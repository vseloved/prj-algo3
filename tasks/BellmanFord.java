import java.util.ArrayDeque;
import java.util.Deque;

public class BellmanFord {
    private double[] distTo;
    private DirectedEdge[] edgeTo;
    private Deque<Integer> queue;
    boolean[] onQueue;

    public BellmanFord(EdgeWeightedDigraph G, int s) {
        distTo = new double[G.getV()];
        edgeTo = new DirectedEdge[G.getV()];
        onQueue = new boolean[G.getV()];

        for (int i = 0; i < distTo.length; i++) {
            distTo[i] = Double.POSITIVE_INFINITY;
        }
        distTo[s] = 0.0;


        queue = new ArrayDeque<>();
        queue.addLast(s);
        onQueue[s] = true;
        /* В сравнении с вариантом, рассмотренным на лекции, этот вариант оптимизирован. Делать релаксацию ребер имеет
        смысл только для тех узлов, кратчайший путь к которым изменился на предыдущей итерации. Для этого храним такие
        узлы в очереди и релаксируем ребра из этих узлов пока таковых не останется */
        while (!queue.isEmpty()) {
            int v = queue.pollFirst();
            onQueue[v] = false;
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
            if (!onQueue[to]) {
                queue.addLast(e.To());
                onQueue[to] = true;
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
