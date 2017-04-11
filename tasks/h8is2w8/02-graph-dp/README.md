02 Dynamic Programming
============
Break text without spaces and find the best candidate using bi-grams' statistics.

Usage
-----
### Load project
`(load "./run.lisp")`

### Run code
```
(break-text:load-words "data/short_dict_en.txt")
(break-text:load-bi-grams "data/short_bi_grams.txt")
(break-text:break-text "Ihaveadream")
```

### Run tests
`(break-text-test::run-tests :all :break-text-test)`
