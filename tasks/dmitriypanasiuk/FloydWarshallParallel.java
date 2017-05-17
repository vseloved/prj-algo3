package dmitriypanasiuk;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.RecursiveAction;

public class FloydWarshallParallel extends RecursiveAction {
    private double[][] distTo;
    private DirectedEdge[][] edgeTo;
    private int V;

    public FloydWarshallParallel(double[][] distTo, DirectedEdge[][] edgeTo) {
        this.distTo = distTo;
        this.edgeTo = edgeTo;
        this.V = distTo.length;
    }

    @Override
    protected void compute() {

        for (int i = 0; i < V; i++) {
            System.out.println("Executing iteration " + i);
            List<RecursiveAction> tasks = new ArrayList<>();
            for (int v = 0; v < V; v++) {
                RecursiveAction task = new SubTaskMiddleLoop(distTo, edgeTo, i, v);
                task.fork();
                tasks.add(task);
            }
            for (RecursiveAction task : tasks) {
                task.join();
            }

        }

    }
}

class SubTask extends RecursiveAction {
    private double[][] distTo;
    private DirectedEdge[][] edgeTo;
    private int i;
    private int V;

    public SubTask(double[][] distTo, DirectedEdge[][] edgeTo, int i) {
        this.distTo = distTo;
        this.edgeTo = edgeTo;
        this.i = i;
        this.V = distTo.length;
    }

    @Override
    protected void compute() {
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

class SubTaskInnerLoop extends RecursiveAction {
    private double[][] distTo;
    private DirectedEdge[][] edgeTo;
    private int i;
    private int v;
    private int w;
    private int V;

    public SubTaskInnerLoop(double[][] distTo, DirectedEdge[][] edgeTo, int i, int v, int w) {
        this.distTo = distTo;
        this.edgeTo = edgeTo;
        this.i = i;
        this.v = v;
        this.w = w;
        this.V = distTo.length;
    }

    @Override
    protected void compute() {
        if (distTo[v][w] > distTo[v][i] + distTo[i][w]) {
            distTo[v][w] = distTo[v][i] + distTo[i][w];
            edgeTo[v][w] = edgeTo[i][w];
        }
    }
}

class SubTaskMiddleLoop extends RecursiveAction {
    private double[][] distTo;
    private DirectedEdge[][] edgeTo;
    private int i;
    private int v;
    private int V;

    public SubTaskMiddleLoop(double[][] distTo, DirectedEdge[][] edgeTo, int i, int v) {
        this.distTo = distTo;
        this.edgeTo = edgeTo;
        this.i = i;
        this.v = v;
        this.V = distTo.length;
    }

    @Override
    protected void compute() {
        if (edgeTo[v][i] == null) return;  // optimization
        for (int w = 0; w < V; w++) {
            if (distTo[v][w] > distTo[v][i] + distTo[i][w]) {
                distTo[v][w] = distTo[v][i] + distTo[i][w];
                edgeTo[v][w] = edgeTo[i][w];
            }
        }
    }
}