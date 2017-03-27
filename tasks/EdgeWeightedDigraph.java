import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class EdgeWeightedDigraph {
    private static final String NEWLINE = System.getProperty("line.separator");
    private int V;
    private int E;

    private List<Set<DirectedEdge>> adj = new ArrayList<>();
    private List<String> vertexValues = new ArrayList<>();
    private Map<String, Integer> valueMappings = new HashMap<>();

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

    private void validateVertex(String v) {
        if (valueMappings.get(v) == null) {
            throw new IllegalArgumentException("vertex " + v + " does not exist");
        }
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
