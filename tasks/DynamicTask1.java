import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
/*
Слова читаем из файла и храним в HashSet words. Добавление и поиск О(1).
backtrack - вектор множеств, backtrack[i] хранит множество длин строк, которые заканчиваются на i-том символе входной строки.
Заполняем backtrack проходя по входной строке за О(n**2).
Делаем обратный проход рекурсивно начиная с конца backtrack чтобы собрать все сегментации слов. Запоминаем результаты рекурсивных
вызовов в вектор memo, потому каждая подзадача вычисляется только один раз. Количество подзадач - n. Сложность решения подзадачи - О(k) где k длина слова,
длина слова в моем понимании величина статистическая, со срелним константным значением, то есть в нашем случае О(k) -> О(1).
Таким обратный проход работает за О(n)
Возможные оптимизации -
1. Узнать у словаря какая длина самого длинного слова и делать внутренний цикл заполнения backtrack до этого значения.
Это позволит заполнить backtrack за О(n * k) где k константа.
2. Xранить слова в префиксном дереве вместо HashSet. Это даст мозможность спросить у словаря, есть ли еще слова с даным
префиксом и прекратить внутренний цикл заполнения backtrack если таких слов нет. Это позволит заполнить backtrack за О(n * k) где k константа.
*/
public class DynamicTask1 {
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

    public static void main(String[] args) {
        Set<String> words = readWordsFromFile("dict_en.txt");
        String inputText = "himynameisjeremy";
        char[] charArray = inputText.toCharArray(); // O(1)

        ArrayList<HashSet<Integer>> backTrack = new ArrayList<>(charArray.length);
        ArrayList<ArrayList<Deque<String>>> memo = new ArrayList<>(charArray.length);
        for (char aCharArray : charArray) {
            backTrack.add(new HashSet<Integer>());
            memo.add(new ArrayList<Deque<String>>());
        }

        for (int i = 0; i < charArray.length; i++) {
            if (i == 0 || backTrack.get(i - 1).size() != 0) { // O(1)
                StringBuilder builder = new StringBuilder();
                for (int j = i; j < charArray.length; j++) {
                    builder.append(charArray[j]); // O(1)
                    if (words.contains(builder.toString())) { // builder.toString -> O(n)
                        backTrack.get(j).add(builder.length()); // O(1)
                    }
                }
            }
        }

        ArrayList<Deque<String>> segmentations = getSegmentations(charArray, backTrack, charArray.length - 1, memo);
        for (Deque segmentation : segmentations) {
            System.out.println(segmentation);
        }
    }
}
