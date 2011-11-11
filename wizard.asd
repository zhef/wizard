(asdf:defsystem #:wizard
  :version      "0.0.1"
  :author       "rigidus <i.am.rigidus@gmail.com>"
  :licence      "GPLv3"
  :description  "metacyclic codegenerator"
  :depends-on   (#:cl-mysql
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
  :serial        t
  :components   ((:file "lib")
                 (:file "ent")
                 (:file "gen")
                 (:file "grid")
                 (:file "searching")
                 (:file "grid-fltr")
                 (:file "show-block")
                 (:file "show-linear")
                 (:file "show-linear-elt")
                 (:file "show-grid")
                 (:file "perm")
                 (:file "defmodule")
                 (:file "init")
                 (:static-file "migration.lisp")
                 (:static-file "README")
                 (:static-file "wizard.asd")
                 (:module "tpl"
                          :serial t
                          ;; :pathname ""
                          :components ((:static-file "about.htm")
                                       (:static-file "contacts.htm")
                                       (:static-file "main.htm")
                                       (:static-file "root.htm")
                                       (:static-file "services.htm")
                                       (:static-file "templates.htm")))))




