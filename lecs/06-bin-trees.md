# Бинарные деревья

## Деревья

Применение деревьев:

- иерархии: файловая система, LDAP, иерархические БД
- деревья разбора: AST, parse tree
- DOM
- индексы в СУБД
- дерево принятия решений
- сортировка
- поиск ближайшего объекта, геометрические вычисления

Представление деревьев:

- связное дерево (структура Node с указателями на потомков и, возможно, родителя, соседей)
- массив (как для кучи) - эффективная, но не гибкая структура (не поддерживает эффективно операции поворота, которые необходимы для баллансировки BST)
- список списков (простое представление, удобное для сериализации)

Обход дерева:

- в пред порядке
- в порядке
- в пост порядке

Пример простой реализации бинарного дерева и вариантов его обхода:

```
(defstruct (node (:conc-name "T-"))
  d l r)

(defun traverse (fn tree &key (order :in) parent)
  (when (eql :pre order) (call fn tree))
  (when-it (t-l tree)
    (traverse fn it :parent tree :order order))
  (when (eql :in order) (call fn tree))
  (when-it (t-r tree)
    (traverse fn it :parent tree :order order))
  (when (eql :post order) (call fn tree)))
```


## Бинарные поисковые деревья

- обычное бинарное поисковое дерево не дает гарантии логарифмического времени операций, так как для некоторых распространенных сценариев вставки и удаления данных дерево может вырождаться в связные список
- AVL-деревья - идеально сбаллансированные бинарные деревья, у которых для любого узла высота левого и правого поддерева отличсается максимум на 1 - требует логарифмического времени для всех основных операций
- Красно-черное дерево - степень сбаллансированности меньше, чем у AVL-дерева: высота левого и правого деревьев может отличаться максимум в 2 раза, но вставка и удаление, как правило, работают быстрее
- Splay-дерево - несбаллансированное дерево, которое после каждой операции вставки или поиска перемещает вновь созданный/найденный узел в вершину дерева, что позволяет ускорить типичные варианты работы, в которых многие обращения повторяются, однако приводит к отсутствию гарантий логарифмического времени выполнения всех операций


## Литература

- https://www.quora.com/What-are-some-of-the-use-cases-of-various-traversal-methods-for-trees
- https://www.tutorialspoint.com/data_structures_algorithms/avl_tree_algorithm.htm
- https://www.cs.princeton.edu/~rs/talks/LLRB/RedBlack.pdf
- https://www.quora.com/Why-are-Red-Black-trees-used-more-often-in-industry-than-AVL-trees
- http://digital.cs.usu.edu/~allan/DS/Notes/Ch22.pdf
- http://benpfaff.org/papers/libavl.pdf
- https://www.cs.cmu.edu/~sleator/papers/self-adjusting.pdf
- https://www.cs.usfca.edu/~galles/visualization/AVLtree.html
- https://www.cs.usfca.edu/~galles/visualization/RedBlack.html
- https://www.cs.usfca.edu/~galles/visualization/SplayTree.html
