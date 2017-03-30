import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class EdgeWeightedDigraph {
    private static final String NEWLINE = System.getProperty("line.separator");
    private int V;
    private int E;

    private List<Set<DirectedEdge>> adj = new ArrayList<>();
    private List<Integer> postOrder = new ArrayList<>();
    private boolean[] marked;

    public EdgeWeightedDigraph() {
        this.V = 0;
        this.E = 0;
    }

    public EdgeWeightedDigraph(int V) {
        this.V = V;
        this.E = 0;
        for (int v = 0; v < V; v++) {
            adj.add(new HashSet<DirectedEdge>());
        }
    }

    public int getV() {
        return V;
    }

    public int getE() {
        return E;
    }

    public int addVertex() {
        adj.add(new HashSet<DirectedEdge>());
        V++;
        return V-1;
    }

    public void addEdge(int from, int to, double weight) {
        validateVertex(from);
        validateVertex(to);
        adj.get(from).add(new DirectedEdge(from, to, weight));
        E++;
    }

    public Iterable<DirectedEdge> adj(int v) {
        validateVertex(v);
        return adj.get(v);
    }

    private void validateVertex(int v) {
        if (adj.get(v) == null) {
            throw new IllegalArgumentException("vertex " + v + " does not exist");
        }
    }

    private void dfs() {
        marked = new boolean[this.V];
        for(int v = 0; v < this.V; v++) {
            if (!marked[v]) {
                dfs(v);
            }
        }
    }

    private void dfs(int v) {
        marked[v] = true;
        for (DirectedEdge e : adj(v)) {
            int to = e.To();
            if (!marked[to]) {
                dfs(to);
            }
        }
        postOrder.add(v);
    }

    public Iterable<Integer> topologicalOrder() {
        dfs();
        Collections.reverse(postOrder);
        return postOrder;
    }

    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append(V + " " + E + NEWLINE);
        for (int v = 0; v < V; v++) {
            s.append(v + ": ");
            for (DirectedEdge e : adj.get(v)) {
                s.append(e + " ");
            }
            s.append(NEWLINE);
        }
        return s.toString();
    }
}
