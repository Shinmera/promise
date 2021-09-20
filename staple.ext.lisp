(asdf:load-systems :staple-markless)

(defpackage "promise-docs"
  (:use #:cl)
  (:local-nicknames
   (#:promise #:org.shirakumo.promise)))

(defmethod staple:document-package ((page staple:simple-page)) (find-package "promise-docs"))
