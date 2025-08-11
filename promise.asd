(asdf:defsystem promise
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A small, independent promise library for asynchronous frameworks"
  :homepage "https://shinmera.com/docs/promise"
  :bug-tracker "https://shinmera.com/project/promise/issues"
  :source-control (:git "https://shinmera.com/project/promise.git")
  :serial T
  :components ((:file "package")
               (:file "promise")
               (:file "documentation"))
  :depends-on (:documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :promise-test))))
