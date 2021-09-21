#|
 This file is a part of promise
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem promise-test
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Tests for the promise system."
  :homepage "https://shinmera.github.io/promise"
  :bug-tracker "https://github.com/Shinmera/promise/issues"
  :source-control (:git "https://github.com/Shinmera/promise.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:parachute
               :promise)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :promise-test)))
