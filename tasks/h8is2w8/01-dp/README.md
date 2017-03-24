# Split text without spaces into list of all possible combinations

### Load project
`(load "./run.lisp")`

### Run code
```
(split-text:load-dict "../../dict_en.txt")
(split-text:SPLIT-TEXT "thisistest")
```

### Run tests
`(split-text-test::run-tests :all :split-text-test)`
