# Вероятностные структуры данных

Вероятностные струкутры данных и алгоритмы бывают двух типов:

- первый тип использует операции с элементом случайности для упрощения структуры данных при сохранении амортизированных гарантий ее свойств (т.е. в таких струкутурах, как правило, допускается, что худший случай будет иметь худшие характеристики, но его вероятность минимаизированна и ее можна расчитать, а, часто, и управлять ее за счет выбора параметров структуры). То же относится и к вероятностным алгоритмам
- второй тип — это специализированные структуры/функции, которые могут выполнять какую-то определенную операцию приближенно, при этом используя на порядки меньшше места, чем такие для точного выполнения этой операции, а также дающие возможность в зависимости от выбора параметров получать торговать вероятностью ошибки против занимаемого места/времени выполнения

К первому типу структур относятся:

- в какой-то мере хеш-таблицы (хотя и все их операции являются детерминированными, но заполнение таблицы в зависимости от значений элементов и их последовательности может приводить к разным характеристикам, вероятность которых можно оценить)
- рандомизированные бинарные поисковые деревья
- списки cо скачками (skip-list)

Ко второму типу относятся такие структуры, как:

- Блум-фильтры
- Гиперлоглог
- Count-min sketch

## Литература

- https://habrahabr.ru/post/230413/
- https://stackoverflow.com/questions/256511/skip-list-vs-binary-tree
- https://highlyscalable.wordpress.com/2012/05/01/probabilistic-structures-web-analytics-data-mining/
- http://billmill.org/bloomfilter-tutorial/
- http://www.michaelnielsen.org/ddi/why-bloom-filters-work-the-way-they-do/
- http://stackoverflow.com/questions/4282375/what-is-the-advantage-to-using-bloom-filters
- https://www.quora.com/What-are-the-best-applications-of-Bloom-filters
- https://research.neustar.biz/2012/10/25/sketch-of-the-day-hyperloglog-cornerstone-of-a-big-data-infrastructure/
- http://stackoverflow.com/questions/12327004/how-does-the-hyperloglog-algorithm-work
- http://antirez.com/news/75
