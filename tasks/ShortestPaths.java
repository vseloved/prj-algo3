public class ShortestPaths {
    public static void main(String[] args) {
        int V = 10;
        EdgeWeightedDigraph exampleGraph = new EdgeWeightedDigraph(V);
        exampleGraph.addEdge(0, 1, 7);exampleGraph.addEdge(0, 2, 3);exampleGraph.addEdge(1, 3, 8);exampleGraph.addEdge(1, 4, 3);
        exampleGraph.addEdge(2, 1, 1);exampleGraph.addEdge(2, 3, 2);exampleGraph.addEdge(2, 4, 1);exampleGraph.addEdge(3, 5, 2);
        exampleGraph.addEdge(3, 0, 2);exampleGraph.addEdge(4, 3, 9);exampleGraph.addEdge(4, 5, 4);exampleGraph.addEdge(5, 2, 4);
        exampleGraph.addEdge(6, 7, 10);exampleGraph.addEdge(4, 7, 1);exampleGraph.addEdge(8, 9, 3);
        double NO = Double.POSITIVE_INFINITY;
        double[][] solutionMatrix = {
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
        double[][] resultMatrix = new double[solutionMatrix.length][solutionMatrix[0].length];

        //checking result with provided solution matrix
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
}
