# Поиск нескольких подстрок

При одновременном поиске нескольких подстрок в тексте возможны следующие основоные подходы:

- в лоб: осуществлять поиск каждой подстороки отдельно. Плюсы: простота реализации и распараллеливания

```
(defun brute-force-search (text &rest pats)
  (let (rez)
    (dotimes (i (length text))
      (dolist (pat pats)
        (when (starts-with pat (slice text i))
          (push (pair i pat) rez))))
    rez))
```

- с помощью поиска "отпечатков пальцев" (хешей)
- с помощь конечных автоматов


## Алгоритм Рабина-Карпа

Алгоритм Рабина-Карпа использует скользящий хеш, пересчет которого в каждой новой позиции в строке требует константного количества операций, и таким образом сокращает время работы на порядка длины поисковых строк.

## Алгоритм Ахо-Корасик

Алгоритм Ахо-Корасик строит префиксное дерево из всех поисковых строк, которое также имеет "обратные" суффиксные ссылки, что позвоялет ускорить поиск для строк, которые имеют много общего (например, с его помощью можно реализовать поиск шаблонов с глобами).


## Литература

- http://net.pku.edu.cn/~course/cs101/2007/resource/Intro2Algorithm/book6/chap34.htm
- http://blog.ivank.net/aho-corasick-algorithm-in-as3.html
- https://www.cs.uku.fi/~kilpelai/BSA05/lectures/slides04.pdf
- http://cr.yp.to/bib/1975/aho.pdf
- https://habrahabr.ru/post/65944/
