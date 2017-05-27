# LCS, редакционное расстояние, выравнивание строк, diff

## Задача выравнивания строк

Динамическое программирование имеет множество применений к задачам работы со строками:

- LCS (longest common subsequence)
- edit distance (редакционное расстояние)
- выравнивание слов по редакционному расстоянию (word alignment)
- поиск ближайших кандидатов на исправления слова при орфографической коррекции
- diff
- многие алгоритмы вероятностного тегирования и парсинга

Для алгоритма LCS и основанных на нем используется метрика Левенштейна, которая расчитывает т.н. редакционное расстояние между двумя словами как количество элементарных операций морфинга (изменения одной буквы): удаления, вставки и замены (а также обмена местами в метрике Дамеро-Левенштейна). Используя эту метрику, можно построить таблицу динамического программирования длины наибольшей общей последовательности (а также минимального расстояния между словами) по рекуррентной формуле:

```
LCS(i, j) = LCS(i-1, j-1) + 1 if word1[i] = word2[j]
            max LCS(i-1, j), LCS(i, j-1), LCS(i-1, j-1) otherwise
```

## Литература

- http://algorithms.tutorialhorizon.com/dynamic-programming-longest-common-subsequence/
- http://algorithms.tutorialhorizon.com/dynamic-programming-edit-distance-problem/
- https://neil.fraser.name/writing/diff/
- https://stackoverflow.com/questions/42635889/myers-diff-algorithm-vs-hunt-mcilroy-algorithm
- https://blog.jcoglan.com/2017/02/12/the-myers-diff-algorithm-part-1/, https://blog.jcoglan.com/2017/02/15/the-myers-diff-algorithm-part-2/, https://blog.jcoglan.com/2017/02/17/the-myers-diff-algorithm-part-3/, https://blog.jcoglan.com/2017/03/22/myers-diff-in-linear-space-theory/, https://blog.jcoglan.com/2017/04/25/myers-diff-in-linear-space-implementation/
- http://www.cs.dartmouth.edu/~doug/diff.pdf
- https://stackoverflow.com/questions/2817255/where-can-i-find-the-diff-algorithm
- http://fabiensanglard.net/git_code_review/diff.php
- https://neil.fraser.name/writing/patch/
- https://stackoverflow.com/questions/9478023/is-the-git-binary-diff-algorithm-delta-storage-standardized
- http://cs.brynmawr.edu/Courses/cs330/spring2012/SpellingCheckers.pdf
