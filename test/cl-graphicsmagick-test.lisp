(in-package :cl-user)

(defpackage cl-graphicsmagick-test
  (:use :cl :prove))

(in-package :cl-graphicsmagick-test)

(defvar *test-directory-pathname*
  (merge-pathnames "test/"
                   (asdf:system-source-directory :cl-graphicsmagick)))

(defvar *resource-directory-pathname*
  (merge-pathnames "resource/"
                   *test-directory-pathname*))

;;; utilities

(defun digest-equal-p (pathspec-1 pathspec-2 &key (digest-spec :sha1))
  "Compare 2 files by compare digest result of the files."
  (equalp (ironclad:digest-file digest-spec pathspec-1)
          (ironclad:digest-file digest-spec pathspec-2)))

(defmacro validate-operate ((var
                             &key
                             input-path
                             output-path
                             init-form
                             compare-path
                             (delete-output-p t))
                            &body operate)
  (let ((input-path-symbol (gensym "input-path"))
        (output-path-symbol (gensym "output-path"))
        (compare-path-symbol (gensym "compare-path")))
    `(let ((,input-path-symbol (when ,input-path
                                 (namestring
                                  (merge-pathnames ,input-path
                                                   *resource-directory-pathname*))))
           (,output-path-symbol (when ,output-path
                                  (namestring
                                   (merge-pathnames ,output-path
                                                    *resource-directory-pathname*))))
           (,compare-path-symbol (when ,compare-path
                                   (namestring
                                    (merge-pathnames ,compare-path
                                                     *resource-directory-pathname*)))))
       (when (probe-file ,output-path-symbol)
         (delete-file ,output-path-symbol))
       (gm:with-magick-wand (,var :input-path ,input-path-symbol
                                  :output-path ,output-path-symbol
                                  :init-form ,init-form)
         (progn ,@operate))
       (when (and ,output-path-symbol
                  ,compare-path-symbol)
         (ok (digest-equal-p ,output-path-symbol ,compare-path-symbol)))
       ,(when delete-output-p
          `(delete-file ,output-path-symbol)))))

(defmacro with-test-magick-wand ((var &key input-path output-path)
                                 &body body)
  "Wrap input-path & output-path with *resource-directory-pathname* of gm:with-magick-wand."
  (let ((input-path-symbol (gensym))
        (output-path-symbol (gensym)))
    `(let ((,input-path-symbol (when ,input-path
                                 (namestring (merge-pathnames ,input-path
                                                              *resource-directory-pathname*))))
           (,output-path-symbol (when ,output-path
                                  (namestring (merge-pathnames ,output-path
                                                               *resource-directory-pathname*)))))
       (gm:with-magick-wand (,var :input-path ,input-path-symbol
                                  :output-path ,output-path-symbol)
         (progn ,@body)))))

(plan nil)

;;; test cases for magick-wand.lisp

(subtest "Testing magick-get-version(%MagickGetVersion)"
  (multiple-value-bind (version-string version-number)
      (gm:magick-get-version)
    (format t "    version-string: ~S~%    version-number: ~S~%"
            version-string
            version-number)
    (is t (typep version-string 'string))
    (is t (> (length version-string) 0))
    (ok (search "GraphicsMagick" version-string))
    (is t (typep version-number 'number))
    (is t (> version-number 0))))

(subtest "Testing %NewMagickWand and %DestroyMagickWand"
  (let ((wand (gm:%NewMagickWand)))
    (ok wand)
    (is t (cffi:pointerp wand))
    (is nil (cffi:null-pointer-p wand))
    (gm:%DestroyMagickWand wand)))

(subtest "Testing %MagickReadImage"
  ;; Note: gm:with-new-magick-wand is a macro wrap %NewMagickWand
  ;;   and make sure %DestroyMagickWand. See src/utils.lisp
  (gm:with-new-magick-wand (wand)
    (let* ((pathname (merge-pathnames "320x240_white.jpg" *resource-directory-pathname*))
           (path (namestring pathname)))
      (is t (gm:%MagickReadImage wand path)))))

(subtest "Testing %MagickWriteImage"
  (gm:with-new-magick-wand (wand)
    (let* ((input-pathname (merge-pathnames "320x240_white.jpg" *resource-directory-pathname*))
           (input-path (namestring input-pathname))
           (output-pathname (merge-pathnames "320x240_white.tmp.jpg"
                                             *resource-directory-pathname*))
           (output-path (namestring output-pathname)))
      (gm:%MagickReadImage wand input-path)
      (when (probe-file output-path)
        (delete-file output-path))
      (is t (gm:%MagickWriteImage wand output-path))
      (ok (probe-file output-path))
      (delete-file output-path))))

(subtest "Testing %MagickDescribeImage"
  ;; Note: gm:with-magick-wand is a macro wrap %NewMagickWand or exist magickwand
  ;;   and make sure %DestroyMagickWand. See src/utils.lisp
  (gm:with-magick-wand (wand
                        :input-path (namestring
                                     (merge-pathnames "320x240_white.jpg"
                                                      *resource-directory-pathname*)))
    (let ((description (gm:%MagickDescribeImage wand)))
      (is t (typep description 'string))
      (is t (> (length description) 0)))))

(subtest "Testing %MagickGetImageWidth"
  (with-test-magick-wand (wand :input-path "320x240_white.jpg")
    (is 320 (gm:%MagickGetImageWidth wand)))
  (with-test-magick-wand (wand :input-path "320x240_black.jpg")
    (is 320 (gm:%MagickGetImageWidth wand)))
  (gm:with-new-magick-wand (wand)
    (is 0 (gm:%MagickGetImageWidth wand))))

(subtest "Testing %MagickGetImageHeight"
  (with-test-magick-wand (wand :input-path "320x240_white.jpg")
    (is 240 (gm:%MagickGetImageHeight wand)))
  (with-test-magick-wand (wand :input-path "320x240_black.jpg")
    (is 240 (gm:%MagickGetImageHeight wand)))
  (gm:with-new-magick-wand (wand)
    (is 0 (gm:%MagickGetImageHeight wand))))

(subtest "Testing %MagickGetImageResolution"
  (with-test-magick-wand (wand :input-path "320x240_white.jpg")))

(subtest "Testing %MagickGetImageFormat"
  (with-test-magick-wand (wand :input-path "320x240_white.jpg")
    (is "JPEG" (gm:%MagickGetImageFormat wand)))
  (with-test-magick-wand (wand :input-path "320x240_black.gif")
    (is "GIF" (gm:%MagickGetImageFormat wand))))

(subtest "Testing %MagickGetImageDepth"
  (with-test-magick-wand (wand :input-path "320x240_white.jpg")
    (is 8 (gm:%MagickGetImageDepth wand))))

(subtest "Testing %MagickGetImageDepth"
  (with-test-magick-wand (wand :input-path "320x240_white.jpg")
    ;; TODO more test(such as GIF)
    (is 1 (gm:%MagickGetNumberImages wand))))

(subtest "Testing %MagickSetSize"
  (let ((output-path (namestring (merge-pathnames "320x240_set_size.jpg"
                                                  *resource-directory-pathname*))))
    (when (probe-file output-path)
      (delete-file output-path))
    (gm:with-new-magick-wand (wand)
      ;; set size only works before read from a empty image
      (ok (gm:%MagickSetSize wand 320 240))
      (ok (gm:%MagickReadImage wand "xc:none"))
      (ok (gm:%MagickWriteImage wand output-path)))
    (ok (probe-file output-path))
    (gm:with-magick-wand (wand :input-path output-path)
      (is 320 (gm:%MagickGetImageWidth wand))
      (is 240 (gm:%MagickGetImageHeight wand)))))

(subtest "Testing %MagickFlipImage"
  (validate-operate
      (wand :input-path "320x240_black_white_rectangle0_0-160_120.jpg"
            :output-path "320x240_black_white_rectangle0_0-160_120_flip_result.tmp.jpg"
            :compare-path "320x240_black_white_rectangle0_0-160_120_flip_result.jpg")
    (gm:%MagickFlipImage wand)))

(subtest "Testing %MagickFlopImage"
  (validate-operate
      (wand :input-path "320x240_black_white_rectangle0_0-160_120.jpg"
            :output-path "320x240_black_white_rectangle0_0-160_120_flop_result.tmp.jpg"
            :compare-path "320x240_black_white_rectangle0_0-160_120_flop_result.jpg")
    (gm:%MagickFlopImage wand)))


(subtest "Testing %MagickResampleImage")
;; TODO

(subtest "Testing %MagickResizeImage")
;; TODO

(subtest "Testing %MagickRollImage")
;; TODO

(subtest "Testing %MagickRotateImage"
  (dolist (degree '(45d0 90d0 135d0 180d0 225d0 270d0 315d0 360d0))
    (validate-operate
        (wand :input-path "320x240_black_white_rectangle0_0-160_120.jpg"
              :output-path (format nil 
                                   "320x240_black_white_rectangle0_0-160_120_rotate~A_result.tmp.jpg"
                                   degree)
              :compare-path (format nil 
                                    "320x240_black_white_rectangle0_0-160_120_rotate~A_result.jpg"
                                    degree))
      (gm:with-pixel-wand (pixel-wand :color "red")
        (gm:%MagickRotateImage wand pixel-wand degree)))))

(subtest "Testing %MagickSampleImage" 
  ;; Note: %MagickSampleImage will NOT keep the width/height ratio,
  ;;   and will NOT introduce any additional color into the scaled image
  (loop for (width height) in '((160 120)
                                (320 120)
                                (160 240))
     do (validate-operate
            (wand :input-path "320x240_black_white_rectangle0_0-160_120.jpg"
                  :output-path (format nil
                                       "320x240_black_white_rectangle0_0-160_120_sample~Ax~A.tmp.jpg"
                                       width
                                       height)
                  :compare-path (format nil
                                        "320x240_black_white_rectangle0_0-160_120_sample~Ax~A.jpg"
                                        width
                                        height))
          (gm:%MagickSampleImage wand width height))))

(subtest "Testing %MagickScaleImage"
  ;; Note: %MagickSampleImage will NOT keep the width/height ratio,
  ;;   and might introduce any additional color into the scaled image
  (loop for (width height) in '((160 120)
                                (320 120)
                                (160 240))
     do (validate-operate
            (wand :input-path "320x240_black_white_rectangle0_0-160_120.jpg"
                  :output-path (format nil
                                       "320x240_black_white_rectangle0_0-160_120_scale~Ax~A.tmp.jpg"
                                       width
                                       height)
                  :compare-path (format nil
                                        "320x240_black_white_rectangle0_0-160_120_scale~Ax~A.jpg"
                                        width
                                        height))
          (gm:%MagickScaleImage wand width height))))

;; (subtest "Testing %MagickResizeImage"
;;   (gm:with-magick-wand (wand :input-path input-path
;;                              :output-path output-path)
;;     ;; TODO
;;     ;; (gm:%MagickResizeImage wand)
;;     ))

(subtest "Testing %MagickBorderImage"
  (validate-operate
      (wand :input-path "320x240_black_white_rectangle0_0-160_120.jpg"
            :output-path "320x240_black_white_rectangle0_0-160_120_border_blue_5_10.tmp.jpg"
            :compare-path "320x240_black_white_rectangle0_0-160_120_border_blue_5_10.jpg")
    (gm:with-pixel-wand (blue-pixel-wand :color "blue")
      (gm:%MagickBorderImage wand blue-pixel-wand 5 10))))

(subtest "Testing %MagickChopImage"
  (validate-operate
      (wand :input-path "320x240_black_white_rectangle0_0-160_120.jpg"
            :output-path "320x240_black_white_rectangle0_0-160_120_chop180_120-40_80.tmp.jpg"
            :compare-path "320x240_black_white_rectangle0_0-160_120_chop180_120-40_80.jpg")
    (gm:%MagickChopImage wand 180 120 40 80)))

(subtest "Testing %MagickCompareImages")
;; TODO


(subtest "Testing %MagickCompositeImage"
  (validate-operate
      (composite-wand :input-path "320x240_black.jpg"
                      :output-path "320x240_black_composite_320x240_white_OverCompositeOp_50_100.tmp.jpg"
                      :compare-path "320x240_black_composite_320x240_white_OverCompositeOp_50_100.jpg")
    (with-test-magick-wand (wand :input-path "320x240_white.jpg")
      ;; "320x240_white.jpg" will be top of "320x240_black.jpg" at pointer (50, 100)
      (gm:%MagickCompositeImage composite-wand wand :%OverCompositeOp 50 100))))


;; TODO



;;; test cases for drawing-wand.lisp
;; TODO

(subtest "Testing %MagickDrawRectangle"
  (validate-operate (magick-wand :init-form (let ((magick-wand (gm:%NewMagickWand)))
                                              (gm:%MagickSetSize magick-wand 320 240)
                                              (gm:%MagickReadImage magick-wand "xc:none")
                                              magick-wand)
                                 :output-path "320x240_black_white_rectangle0_0-160_120.tmp.jpg"
                                 :compare-path "320x240_black_white_rectangle0_0-160_120.jpg")
    (gm:with-pixel-wand (white-pixel-wand :color "white")
      (gm:with-drawing-wand (drawing-wand)
        (gm:%MagickDrawRectangle drawing-wand 0d0 0d0 160d0 120d0)
        (gm:%MagickDrawSetFillColor drawing-wand white-pixel-wand)
        (gm:%MagickDrawImage magick-wand drawing-wand))))
  (validate-operate (magick-wand :init-form (let ((magick-wand (gm:%NewMagickWand)))
                                              (gm:%MagickSetSize magick-wand 320 240)
                                              (gm:%MagickReadImage magick-wand "xc:none")
                                              magick-wand)
                                 :output-path "320x240_black_white_rectangle0_120-160_240.tmp.jpg"
                                 :compare-path "320x240_black_white_rectangle0_120-160_240.jpg")
    (gm:with-pixel-wand (white-pixel-wand :color "white")
      (gm:with-drawing-wand (drawing-wand)
        (gm:%MagickDrawRectangle drawing-wand 0d0 120d0 160d0 240d0)
        (gm:%MagickDrawSetFillColor drawing-wand white-pixel-wand)
        (gm:%MagickDrawImage magick-wand drawing-wand)))))




;;; test cases for pixel-wand.lisp
;; TODO


(finalize)
