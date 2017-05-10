(pushnew "./" asdf:*central-registry* :test #'equal)

(ql:quickload :break-text)
(ql:quickload :break-text-test)
