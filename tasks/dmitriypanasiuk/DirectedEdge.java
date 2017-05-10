package dmitriypanasiuk;

public class DirectedEdge {
    private int from;
    private int to;
    private double weight;

    public DirectedEdge(int from, int to, double weight) {
        if (from < 0) throw new IllegalArgumentException("Vertex can't be less than zero");
        if (to < 0) throw new IllegalArgumentException("Vertex can't be less than zero");
        this.from = from;
        this.to = to;
        this.weight = weight;
    }

    public int From() {
        return from;
    }

    public int To() {
        return to;
    }

    public double Weight() {
        return weight;
    }

    public String toString() {
        return from + "->" + to + " " + String.format("%5.2f", weight);
    }

}
