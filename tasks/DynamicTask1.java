import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

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
            segmentations.add(new ArrayDeque<>(currentSegmentation));
            return;
        }
        for (int number : backTrack.get(currentPosition)) {
            String str = new String(charArray, currentPosition - number + 1, number);
            currentSegmentation.push(str);
            getSegmentations(charArray, backTrack, currentPosition - number, currentSegmentation, segmentations);
            currentSegmentation.pop();
        }
    }

    public static void main(String[] args) {
        Set<String> words = readWordsFromFile("test.txt");
        String str = "himynameisjeremy";
        char[] charArray = str.toCharArray();

        ArrayList<HashSet<Integer>> backTrack = new ArrayList<>(str.length());
        for (int i = 0; i < charArray.length; i++) {
            backTrack.add(new HashSet<Integer>());
        }
        for (int i = 0; i < charArray.length; i++) {
            if (i == 0 || backTrack.get(i - 1).size() != 0) {
                StringBuilder builder = new StringBuilder();
                for (int j = i; j < charArray.length; j++) {
                    builder.append(charArray[j]);
                    if (words.contains(builder.toString())) {
                        backTrack.get(j).add(builder.length());
                    }
                }
            }
        }
        ArrayList<Deque<String>> segmentations = new ArrayList<>();
        getSegmentations(charArray, backTrack, charArray.length - 1, new ArrayDeque<String>(), segmentations);
        for (Deque d : segmentations) {
            System.out.println(d);
        }
    }
}
