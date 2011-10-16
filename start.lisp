;; Загрузчик системы
(let ((swank (ignore-errors (asdf:find-system :swank))))
  (when swank
    (swank:create-server :coding-system "utf-8-unix" :dont-close t)))
(let ((cl-mysql (ignore-errors (asdf:find-system :cl-mysql))))
  (when cl-mysql
    (asdf:operate 'asdf:load-op '#:cl-mysql)))
(require 'CL-STORE)
(require 'RESTAS)
(require 'CLOSURE-TEMPLATE)
(require 'RESTAS-DIRECTORY-PUBLISHER)
(require 'CL-JSON)
(load "lib.lisp")
(reload)
(load "init.lisp")
(restas:start '#:wizard :port 8081)
(restas:debug-mode-on)
