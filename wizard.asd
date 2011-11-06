(asdf:defsystem #:wizard
  :version    "0.0.1"
  :author     "rigidus <i.am.rigidus@gmail.com>"
  :licence    "GPLv3"
  :depends-on (#:cl-mysql
               #:cl-store
               #:restas
               #:restas-directory-publisher
               #:cl-json
               #:cl-ppcre
               #:cl-smtp
               #:cl-mime
               #:arnesi
               #:closer-mop
               #:drakma)
  :serial      t
  :components ((:file "lib")
               (:file "ent")
               (:file "gen")
               (:file "grid")
               (:file "show-block")
               (:file "show-linear")
               (:file "perm")
               (:file "defmodule")
               (:file "init")
               (:static-file "migration.lisp")
               (:static-file "README")
               (:static-file "wizard.asd")
               (:static-file "templates.soy")
               ))




