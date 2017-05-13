# Строки

## Представление строк

Строка — это последовательность букв (characters), которая может быть представлена в виде:

- массива
- связного списка (Erlang, Prolog, Haskell)
- дерева (rope)

Варианты учета длины строки:

- null-terminated
- префикс длина (netstring: `12:hello world!,`)
- метаинформация

Динамические строки - StringBuffer/StringBuilder


## Поиск подстроки

### Наивный алгоритм

Сложность наивного алгоритма - `O(n * m)`,
где `n` — длина строки, по которой осуществляется поиск,
а `m` — длина поискового шаблона

Более эффективные алгоритмы — это эвристические алгоритмы, которые направленны на то, чтобы проделать не более одного сравнения для каждой буквы текста, в котором происходит поиск. Это достигается за счет того или иного предпроцессинга поисковой строки.

- Knuth-Morris-Pratt (KMP) - расчет максимального сдвига для каждой буквы поисковой строки 
- Boyer-Moore - расчет масимального возможного сдвига для каждой буквы алфавита, а также для каждого суффикса поисковой строки
- Boyer-Moore-Horspool- только буквы алфавита
- двунаправленный - разбиение строки на 2 части по определенному принципу


## Литература

- https://unspecified.wordpress.com/2012/04/19/the-importance-of-language-level-abstract-unicode-strings/
- http://mrale.ph/blog/2016/11/23/making-less-dart-faster.html
- http://www.inf.fh-flensburg.de/lang/algorithmen/pattern/bmen.htm
- http://www-igm.univ-mlv.fr/~lecroq/string/node26.html
- http://www-igm.univ-mlv.fr/~mac/Articles-PDF/CP-1991-jacm.pdf
- http://old.blog.phusion.nl/2010/12/06/efficient-substring-searching/
- https://filipjaniszewski.wordpress.com/2016/03/03/string-searching-algorithms-comparison/
- http://www-igm.univ-mlv.fr/~lecroq/string/index.html
- https://arxiv.org/pdf/1012.2547v1.pdf
- https://pdfs.semanticscholar.org/e880/29ee3f1b881ad1d0fcdff97bfe6eebc1b645.pdf
- http://stackoverflow.com/questions/5603080/have-you-ever-used-kmp-or-bm-algorithms
