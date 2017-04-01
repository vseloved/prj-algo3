import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

/*
Слова читаем из файла и храним в HashSet words. Добавление и поиск О(1).
DAG - направленный граф. Узлы - слова, направленные ребра - биграм, вес ребра = -1 * log(P(биграм))
backtrack - вектор множеств, backtrack[i] хранит множество индексов узлов графа (узел графа -> слово), которые заканчиваются на i-том символе входной строки.
Заполняем backtrack проходя по входной строке за О(n**2). По ходу заполнения также заполняем DAG узлами и ребрами.
vertexStrings - ассоциативный массив, хранит соответствия между узлом DAG и словом.
Ищем кратчаший путь в DAG из корневого узла во все узлы, затем сравниваем вес путей в те узлы, из которых не исходят ребра (слова, стоящие в конце текста),
и формируем обратный путь. Сложность O(E+V).
Возможные оптимизации -
1. Узнать у словаря какая длина самого длинного слова и делать внутренний цикл заполнения backtrack до этого значения.
Это позволит заполнить backtrack за О(n * k) где k константа.
2. Xранить слова в префиксном дереве вместо HashSet. Это даст мозможность спросить у словаря, есть ли еще слова с даным
префиксом и прекратить внутренний цикл заполнения backtrack если таких слов нет. Это позволит заполнить backtrack за О(n * k) где k константа.
*/
public class DynamicTask1 {
    private static Map<String, Long> readFreqsFromFile(String path) {
        Map<String, Long> words = new HashMap<>();
        File file = new File(DynamicTask1.class.getResource(path).getFile());
        try (BufferedReader br = new BufferedReader(new FileReader(file))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] entry = line.split("\t");
                words.put(entry[0], Long.parseLong(entry[1]));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        return words;
    }

    private static Set<String> readWordsFromFile(String path) {
        Set<String> words = new HashSet<>();
        File file = new File(DynamicTask1.class.getResource(path).getFile());
        try (BufferedReader br = new BufferedReader(new FileReader(file))) {
            String line;
            while ((line = br.readLine()) != null) {
                words.add(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        return words;
    }

    private static ArrayList<Deque<String>> getSegmentations(char[] charArray,
                                                             ArrayList<HashSet<Integer>> backTrack,
                                                             int currentPosition,
                                                             ArrayList<ArrayList<Deque<String>>> memo) {
        if (currentPosition < 0) {
            Deque<String> emptyDeque = new ArrayDeque<>();
            ArrayList<Deque<String>> result = new ArrayList<>();
            result.add(emptyDeque);
            return result;
        }
        if (memo.get(currentPosition).size() != 0) {
            return memo.get(currentPosition);
        }
        ArrayList<Deque<String>> newResult = new ArrayList<>();
        for (int number : backTrack.get(currentPosition)) {
            String str = new String(charArray, currentPosition - number + 1, number);
            ArrayList<Deque<String>> subProblemResult = new ArrayList<>(getSegmentations(charArray, backTrack, currentPosition - number, memo));

            for (Deque<String> subProblemSolution : subProblemResult) {
                Deque<String> newSolution = new ArrayDeque<>(subProblemSolution);
                newSolution.add(str);
                newResult.add(newSolution);
            }
        }
        memo.set(currentPosition, newResult);
        return newResult;
    }

    private static double getWeight(long freq, long total){
        double percentage = (double)freq/total;
        return -Math.log(percentage);
    }

    private static long getTotalBigramCount(Map<String, Long> bigram) {
        long totalCount = 0;
        for (long freq : bigram.values()) {
            totalCount += freq;
        }

        return totalCount;
    }

    private static int shortestPathIndex(List<Integer> lastWords, double[] weights) {
        double min = Double.POSITIVE_INFINITY;
        int index = 0;
        for (int i : lastWords) {
            if (min > weights[i]) {
                min = weights[i];
                index = i;
            }
        }

        return index;
    }

    public static void main(String[] args) {
        String inputText = "himynameisjeremy";
        Set<String> words  = readWordsFromFile("test.txt");
        Map<String, Long> bigram = readFreqsFromFile("test_2gram.txt");
        long totalCount = getTotalBigramCount(bigram);
        Map<Integer, String> vertexStrings = new HashMap<>();
        ArrayList<HashSet<Integer>> backTrack = new ArrayList<>(inputText.length());
        EdgeWeightedDigraph DAG = new EdgeWeightedDigraph();
        List<Integer> lastWords = new ArrayList<>();

        for (int i = 0; i < inputText.length(); i++) {
            backTrack.add(new HashSet<Integer>());
        }

        int rootVertex = DAG.addVertex(); //корневой узел - начало строки

        for (int i = 0; i < inputText.length(); i++) {
            if (i == 0 || backTrack.get(i - 1).size() != 0) { // O(1)
                StringBuilder builder = new StringBuilder();
                for (int j = i; j < inputText.length(); j++) {
                    builder.append(inputText.charAt(j)); // O(1)
                    if (words.contains(builder.toString())) { // builder.toString -> O(n)
                        int to = DAG.addVertex();
                        if (j == inputText.length() - 1) {
                            lastWords.add(to);
                        }
                        vertexStrings.put(to, builder.toString());
                        if (i == 0){
                            DAG.addEdge(rootVertex, to, 0);
                        } else {
                            for (int v : backTrack.get(i - 1)) {
                                Long pairFreq =  bigram.get(vertexStrings.get(v) + " " + vertexStrings.get(to));
                                DAG.addEdge(v, to, getWeight((pairFreq == null) ? 0 : pairFreq, totalCount));
                            }
                        }
                        backTrack.get(j).add(to); // O(1)
                    }
                }
            }
        }

        //shortest path
        double[] pathWeights = new double[DAG.getV()];
        int[] paths = new int[DAG.getV()];
        for (int path = 0; path < DAG.getV(); path++) {
            pathWeights[path] = Double.POSITIVE_INFINITY;
        }
        pathWeights[0] = 0;
        paths[0] = 0;
        for (int from : DAG.topologicalOrder()) {
            for (DirectedEdge adjVertex : DAG.adj(from)) {
                int to = adjVertex.To();
                double weight = adjVertex.Weight();
                if (pathWeights[to] > pathWeights[from] + weight) {
                    pathWeights[to] = pathWeights[from] + weight;
                    paths[to] = from;
                }
            }
        }

        Deque<String> segmentation = new ArrayDeque<>();
        int v = shortestPathIndex(lastWords, pathWeights);
        while (v > 0) {
            segmentation.addFirst(vertexStrings.get(v));
            v = paths[v];
        }
        System.out.println(segmentation);
    }
}
