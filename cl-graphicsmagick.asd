(asdf:defsystem "cl-graphicsmagick"
  :name "cl-graphicsmagick"
  :description "Common Lisp CFFI wrapper for GraphicsMagick, a library to deal with images."
  :version "0.0.2"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :license "MIT"
  :depends-on (:cffi)
  :in-order-to ((test-op (test-op "cl-graphicsmagick-test")))
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:module "cffi"
                              :serial t
                              :components ((:file "load-foreign-library")
                                           (:file "common")
                                           (:file "types")
                                           (:file "pixel-wand")
                                           (:file "magick-wand")
                                           (:file "drawing-wand")))
                             (:file "utils")))))
