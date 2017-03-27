import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

public class EdgeWeightedDigraph {
    private static final String NEWLINE = System.getProperty("line.separator");
    private int V;
    private int E;

    private List<Set<DirectedEdge>> adj = new ArrayList<>();
    private List<String> vertexValues = new ArrayList<>();
    private Map<String, Integer> valueMappings = new HashMap<>();
    private Queue<Integer> postorder = new ArrayDeque<>();
    private boolean[] marked;

    public EdgeWeightedDigraph() {
        this.V = 0;
        this.E = 0;
    }

    public int getV() {
        return V;
    }

    public int getE() {
        return E;
    }

    public void addVertex(String value){
        vertexValues.add(value);
        valueMappings.put(value, V);
        adj.add(new HashSet<DirectedEdge>());
        V++;
    }

    public void addEdge(String from, String to, double weight) {
        validateVertex(from);
        validateVertex(to);
        int vFrom = valueMappings.get(from);
        int vTo = valueMappings.get(to);
        adj.get(vFrom).add(new DirectedEdge(vFrom, vTo, weight));
        E++;
    }

    private Iterable<DirectedEdge> adj(int v) {
        return adj.get(v);
    }

    private void validateVertex(String v) {
        if (valueMappings.get(v) == null) {
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
        postorder.add(v);
    }

    public Iterable<Integer> topologicalOrder() {
        dfs();
        Deque<Integer> reversePostOrder = new ArrayDeque<>();
        for (int v : postorder) {
            reversePostOrder.addFirst(v);
        }

        return reversePostOrder;
    }

    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append(V + " " + E + NEWLINE);
        for (int v = 0; v < V; v++) {
            s.append(vertexValues.get(v) + ": ");
            for (DirectedEdge e : adj.get(v)) {
                s.append(vertexValues.get(e.From()) + "->" + vertexValues.get(e.To()) + " " + e.Weight());
            }
            s.append(NEWLINE);
        }
        return s.toString();
    }
}
