# Персистентные структуры данных

Персистентные/функциональные/иммутабельные структуры данных — это не только структура, но и сопутствующий набор операций ее модификации, который гарантирует сохранение предыдущей до выполнения операции версии.

На базовом уровне любая структура данных может быть персистентной, если модификация выполняется по принципу copy-on-write (т.е. сперва создания копии и затем ее изменения) либо же записи полной последовательности операций. Однако, такая реализация является неэффективной и очень редко практичной. Т.е. стоит задача эффективной реализации персистентных структур данных. Как правило, такая реализация возможно только для линкованных структур, таких как связный список или дерево.

Но даже эффективная реализация немутабельности, как правило, имеет худшие параметры сложности, чем мутабельная версия той же структуры. Оданко, она может иметь схожую сложность с поправкой на амортизацию "тяжелых" операций копирования (т.е. растягивание их во времени в фоновом режиме - scheduling - если это возможно, или же усреднении с другими операциями).  


## Структуры на связных списках

Базовая персистентная структура — это стопка (стек) с операциями push и pop. С использованием двух стеков можно реализовать т.ню очередь банкира или очередь реального времени (RT queue).

## Деревья пальцев

Деревья пальцев — это базовая структура, на основе которой могут быть реализованы персистентные массивы и другие структуры данных (например, приоритетные очереди). В таком дереве в листьях находятся значения, а в промежуточных узлах — метаинформация для быстрого выполнения тех или иных операций. Для массива каждый такой узелл содержит размер поддерева, а для приоритетной очереди — приоритетный (max/min) ключ.

## Траи с целочисленными ключами

Это префиксные деревья, которые используют в качестве ключа биты или же группы битов индекса. Как правило, они имеют очень небольшую глубину за счет использования большого фактора ветвистости (например, 32).

На их основе можно реализовать массивы, а также хеш-таблицы (HAMT - hash array-mapped trie), если в качестве индекса использовать хеш ключа.

## Зиппера

Зиппер — это функциональный способ обхода линкованных структур данных, который позволяет эффективно выполнять операции модификации по принципу копирования пути, т.к. в нем хранится путь, по которому мы дошли до текущего элемента.


## Литература

- http://www.toves.org/books/persist/
- https://cstheory.stackexchange.com/questions/1539/whats-new-in-purely-functional-data-structures-since-okasaki
- https://habrahabr.ru/post/208918/
- https://www.slideshare.net/knoldus/data-structuresscala?next_slideshow=1
- https://medium.com/@dtinth/immutable-js-persistent-data-structures-and-structural-sharing-6d163fbd73d2
- http://apfelmus.nfshost.com/articles/monoid-fingertree.html
- http://ittc.ku.edu/~andygill/papers/IntMap98.pdf
- https://idea.popcount.org/2012-07-25-introduction-to-hamt/
- https://www.cs.ox.ac.uk/ralf.hinze/publications/ICFP01.pdf
- http://hypirion.com/musings/understanding-persistent-vector-pt-1, http://hypirion.com/musings/understanding-persistent-vector-pt-2, http://hypirion.com/musings/understanding-persistent-vector-pt-3, http://hypirion.com/musings/understanding-clojure-transients, http://hypirion.com/musings/persistent-vector-performance-summarised
- http://gallium.inria.fr/~huet/PUBLIC/zip.pdf
- https://donsbot.wordpress.com/2007/05/17/roll-your-own-window-manager-tracking-focus-with-a-zipper/
- http://blog.ezyang.com/2010/04/you-could-have-invented-zippers/

## Книги

- http://www.cs.cmu.edu/~rwh/theses/okasaki.pdf
- https://www.cs.cmu.edu/~sleator/papers/making-data-structures-persistent.pdf

