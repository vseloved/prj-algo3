import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
/*
Слова читаем из файла и храним в HashSet. Добавление и поиск О(1).
Вектор backtrack хранит множества чисел, backtrack[i] хранит длины строк, которые заканчиваются на i-том символе входной строки.
Заполняем backtrack проходя по входной строке за О(n**2).
Делаем обратный проход рекурсивно начиная с конца backtrack чтобы собрать все сегментации слов за О(n**2)(?) в худшем случае.
Возможные оптимизации -
1. Узнать у словаря какая длина самого длинного слова и делать внутренний цикл заполнения backtrack до этого значения.
Это позволит заполнить backtrack за О(n * k) где k константа.
2. Xранить слова в префиксном дереве вместо HashSet. Это даст мозможность спросить у словаря, есть ли еще слова с даным
префиксом и прекратить внутренний цикл заполнения backtrack если таких слов нет.
Это позволит заполнить backtrack за О(n * k) где k константа.
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

    private static void getSegmentations(char[] charArray,
                                        ArrayList<HashSet<Integer>> backTrack,
                                        int currentPosition,
                                        Deque<String> currentSegmentation,
                                        ArrayList<Deque<String>> segmentations) {
        if (currentPosition < 0) {
            segmentations.add(new ArrayDeque<>(currentSegmentation)); // O(n)
            return;
        }
        for (int number : backTrack.get(currentPosition)) {
            String str = new String(charArray, currentPosition - number + 1, number); // O(n)
            currentSegmentation.push(str); // O(1)
            getSegmentations(charArray, backTrack, currentPosition - number, currentSegmentation, segmentations);
            currentSegmentation.pop(); // O(1)
        }
    }

    public static void main(String[] args) {
        Set<String> words = readWordsFromFile("dict_en.txt");
        String inputText = "himynameisjeremy";
        char[] charArray = inputText.toCharArray(); // O(1)

        ArrayList<HashSet<Integer>> backTrack = new ArrayList<>(charArray.length);
        for (char aCharArray : charArray) {
            backTrack.add(new HashSet<Integer>());
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
        ArrayList<Deque<String>> segmentations = new ArrayList<>();
        getSegmentations(charArray, backTrack, charArray.length - 1, new ArrayDeque<String>(), segmentations);
        for (Deque segmentation : segmentations) {
            System.out.println(segmentation);
        }
    }
}
