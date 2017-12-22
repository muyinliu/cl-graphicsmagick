(defsystem "cl-graphicsmagick-test"
  :name "cl-graphicsmagick-test"
  :description "test case for cl-graphicsmagick"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :depends-on ("cl-graphicsmagick" "prove" "ironclad")
  :defsystem-depends-on ("prove-asdf")
  :components ((:module "test"
                        :serial t
                        :components ((:file "cl-graphicsmagick-test"))))
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
    
