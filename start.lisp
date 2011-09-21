;; Загрузчик системы
;; Не забыть скачать в текущую директорию quicklisp
;; $ curl -O http://beta.quicklisp.org/quicklisp.lisp
;; (load "quicklisp.lisp")
;; (quicklisp-quickstart:install)
;; (ql:quickload "restas-directory-publisher")
(let ((swank (ignore-errors (asdf:find-system :swank))))
  (when swank
    (swank:create-server :coding-system "utf-8-unix" :dont-close t)))
(require 'RESTAS)
(require 'CLOSURE-TEMPLATE)
(require 'RESTAS-DIRECTORY-PUBLISHER)
(require 'CL-JSON)
(load "lib.lisp")
(reload)
(restas:start '#:wizard :port 8081)
(restas:debug-mode-on)


