package dmitriypanasiuk;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Locale;
import java.util.Scanner;

public class ShortestPaths {
    private static double NO = Double.POSITIVE_INFINITY;
    private static double[][] solutionMatrix = {
            {0, 4, 3, 5, 4, 7, NO, 5, NO, NO},
            {10, 0, 11, 8, 3, 7, NO, 4, NO, NO},
            {4, 1, 0, 2, 1, 4, NO, 2, NO, NO},
            {2, 6, 5, 0, 6, 2, NO, 7, NO, NO},
            {11, 9, 8, 9, 0, 4, NO, 1, NO, NO},
            {8, 5, 4, 6, 5, 0, NO, 6, NO, NO},
            {NO, NO, NO, NO, NO, NO, 0, 10, NO, NO},
            {NO, NO, NO, NO, NO, NO, NO, 0, NO, NO},
            {NO, NO, NO, NO, NO, NO, NO, NO, 0, 3},
            {NO, NO, NO, NO, NO, NO, NO, NO, NO, 0}};

    private static EdgeWeightedDigraph exampleGraph() {
        EdgeWeightedDigraph exampleGraph = new EdgeWeightedDigraph(10);
        exampleGraph.addEdge(0, 1, 7);
        exampleGraph.addEdge(0, 2, 3);
        exampleGraph.addEdge(1, 3, 8);
        exampleGraph.addEdge(1, 4, 3);
        exampleGraph.addEdge(2, 1, 1);
        exampleGraph.addEdge(2, 3, 2);
        exampleGraph.addEdge(2, 4, 1);
        exampleGraph.addEdge(3, 5, 2);
        exampleGraph.addEdge(3, 0, 2);
        exampleGraph.addEdge(4, 3, 9);
        exampleGraph.addEdge(4, 5, 4);
        exampleGraph.addEdge(5, 2, 4);
        exampleGraph.addEdge(6, 7, 10);
        exampleGraph.addEdge(4, 7, 1);
        exampleGraph.addEdge(8, 9, 3);

        return exampleGraph;
    }

    private static AdjMatrixEdgeWeightedDigraph exampleMatrixGraph() {
        AdjMatrixEdgeWeightedDigraph exampleGraph = new AdjMatrixEdgeWeightedDigraph(10);
        exampleGraph.addEdge(0, 1, 7);
        exampleGraph.addEdge(0, 2, 3);
        exampleGraph.addEdge(1, 3, 8);
        exampleGraph.addEdge(1, 4, 3);
        exampleGraph.addEdge(2, 1, 1);
        exampleGraph.addEdge(2, 3, 2);
        exampleGraph.addEdge(2, 4, 1);
        exampleGraph.addEdge(3, 5, 2);
        exampleGraph.addEdge(3, 0, 2);
        exampleGraph.addEdge(4, 3, 9);
        exampleGraph.addEdge(4, 5, 4);
        exampleGraph.addEdge(5, 2, 4);
        exampleGraph.addEdge(6, 7, 10);
        exampleGraph.addEdge(4, 7, 1);
        exampleGraph.addEdge(8, 9, 3);

        return exampleGraph;
    }

    private static EdgeWeightedDigraph largeGraph(String filename) {
        EdgeWeightedDigraph largeGraph;

        File file = new File(DynamicTask1.class.getResource(filename).getFile());

        try {
            FileInputStream fis = new FileInputStream(file);
            Scanner scanner = new Scanner(new BufferedInputStream(fis));
            scanner.useLocale(Locale.US);
            largeGraph = new EdgeWeightedDigraph(scanner.nextInt());
            int E = scanner.nextInt();
            for (int i = 0; i < E; i++) {
                int v = scanner.nextInt();
                int w = scanner.nextInt();
                double weight = scanner.nextDouble();
                largeGraph.addEdge(v, w, weight);
            }

        } catch (IOException ioe) {
            throw new IllegalArgumentException("Could not open " + file, ioe);
        }
        return largeGraph;
    }

    private static AdjMatrixEdgeWeightedDigraph largeGraphMatrix(String filename) {
        AdjMatrixEdgeWeightedDigraph matrixGraph;
        File file = new File(DynamicTask1.class.getResource(filename).getFile());

        try {
            FileInputStream fis = new FileInputStream(file);
            Scanner scanner = new Scanner(new BufferedInputStream(fis));
            scanner.useLocale(Locale.US);
            matrixGraph = new AdjMatrixEdgeWeightedDigraph(scanner.nextInt());
            int E = scanner.nextInt();
            for (int i = 0; i < E; i++) {
                int v = scanner.nextInt();
                int w = scanner.nextInt();
                double weight = scanner.nextDouble();
                matrixGraph.addEdge(v, w, weight);
            }

        } catch (IOException ioe) {
            throw new IllegalArgumentException("Could not open " + file, ioe);
        }
        return matrixGraph;
    }

    public static void checkDijkstraCorrectness() {
        EdgeWeightedDigraph exampleGraph = exampleGraph();
        double[][] resultMatrix = new double[solutionMatrix.length][solutionMatrix[0].length];
        for (int source = 0; source < exampleGraph.getV(); source++) {
            Dijkstra dijkstra = new Dijkstra(exampleGraph, source);
            for (int target = 0; target < exampleGraph.getV(); target++) {
                resultMatrix[source][target] = dijkstra.distTo(target);
                if (solutionMatrix[source][target] != resultMatrix[source][target]) {
                    System.out.println("ERROR, path from " + source + " to " + target + " is incorrect");
                }
            }
        }
    }

    public static void checkBellmanFordCorrectness() {
        EdgeWeightedDigraph exampleGraph = exampleGraph();
        double[][] resultMatrix = new double[solutionMatrix.length][solutionMatrix[0].length];
        for (int source = 0; source < exampleGraph.getV(); source++) {
            BellmanFord bellmanFord = new BellmanFord(exampleGraph, source);
            for (int target = 0; target < exampleGraph.getV(); target++) {
                resultMatrix[source][target] = bellmanFord.distTo(target);
                if (solutionMatrix[source][target] != resultMatrix[source][target]) {
                    System.out.println("ERROR, path from " + source + " to " + target + " is incorrect");
                }
            }
        }
    }

    public static void checkFloydWarshallCorrectness(boolean parallel) {
        AdjMatrixEdgeWeightedDigraph exampleGraph = exampleMatrixGraph();
        double[][] resultMatrix = new double[solutionMatrix.length][solutionMatrix[0].length];
        FloydWarshall FWarshall = new FloydWarshall(exampleGraph, parallel);
        for (int source = 0; source < exampleGraph.V(); source++) {
            for (int target = 0; target < exampleGraph.V(); target++) {
                resultMatrix[source][target] = FWarshall.dist(source, target);
                if (solutionMatrix[source][target] != resultMatrix[source][target]) {
                    System.out.println("ERROR, path from " + source + " to " + target + " is incorrect");
                }
            }
        }
    }

    public static void checkParallelCorrectness(FloydWarshall unparallel, FloydWarshall parallel, int size) {
        for (int source = 0; source < size; source++) {
            for (int target = 0; target < size; target++) {
                if (unparallel.dist(source, target) != parallel.dist(source, target)) {
                    System.out.println("dist from " + source + " to " + target + " is wrong!");
                }
            }
        }
    }

    public static void main(String[] args) {
        checkDijkstraCorrectness();

        checkBellmanFordCorrectness();

        checkFloydWarshallCorrectness(false);

        checkFloydWarshallCorrectness(true);
        // 1000000 узлов, 15172126 ребер - http://algs4.cs.princeton.edu/44sp/largeEWD.txt
        EdgeWeightedDigraph largeGraph = largeGraph("largeEWD.txt");
        double dijkstraAverage = 0;
        double bellmanFordAverage = 0;
        for (int i = 0; i < 10; i++) {
            StopWatchCPU clock1 = new StopWatchCPU();
            new Dijkstra(largeGraph, 0);
            dijkstraAverage += clock1.elapsedTime();
            StopWatchCPU clock2 = new StopWatchCPU();
            new BellmanFord(largeGraph, 0);
            bellmanFordAverage += clock2.elapsedTime();
        }
        System.out.println("Dijkstra average = " + dijkstraAverage / 10);
        System.out.println("BellmanFord average = " + bellmanFordAverage / 10);

        AdjMatrixEdgeWeightedDigraph testGraph = largeGraphMatrix("1000EWD.txt");
        double sequentialRuntime = 0.0;
        double parallelRuntime = 0.0;

        int N = 10;
        for (int i = 0; i < N; i++) {
            StopWatch clock1 = new StopWatch();
            FloydWarshall sequentialSolution = new FloydWarshall(testGraph, false);
            sequentialRuntime += clock1.elapsedTime();
            StopWatch clock2 = new StopWatch();
            FloydWarshall parallelSolution = new FloydWarshall(testGraph, true);
            parallelRuntime += clock2.elapsedTime();
            checkParallelCorrectness(sequentialSolution, parallelSolution, testGraph.V());
        }
        System.out.println("Sequential runtime average = " + sequentialRuntime / N);
        System.out.println("Parallel runtime average = " + parallelRuntime / N);
        System.out.println("Ratio is " + sequentialRuntime/parallelRuntime);
    }
}
