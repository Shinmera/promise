(asdf:defsystem promise
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A small, independent promise library for asynchronous frameworks"
  :homepage "https://shinmera.github.io/promise"
  :bug-tracker "https://github.com/Shinmera/promise/issues"
  :source-control (:git "https://github.com/Shinmera/promise.git")
  :serial T
  :components ((:file "package")
               (:file "promise")
               (:file "documentation"))
  :depends-on (:documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :promise-test))))
