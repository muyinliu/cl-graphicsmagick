;; 获取图像信息
(describe-image "/Users/muyinliu/Downloads/1.png")
;; "Image: /Users/muyinliu/Downloads/1.png
;;   Format: PNG (Portable Network Graphics)
;;   Geometry: 99x99
;;   Class: DirectClass
;;   Type: true color with transparency
;;   Depth: 8 bits-per-pixel component
;;   Channel Depths:
;;     Red:      8 bits
;;     Green:    8 bits
;;     Blue:     8 bits
;;     Opacity:  1 bits
;;   Channel Statistics:
;;     Red:
;;       Minimum:                     0.00 (0.0000)
;;       Maximum:                   255.00 (1.0000)
;;       Mean:                      216.70 (0.8498)
;;       Standard Deviation:         54.02 (0.2118)
;;     Green:
;;       Minimum:                     0.00 (0.0000)
;;       Maximum:                   255.00 (1.0000)
;;       Mean:                      169.32 (0.6640)
;;       Standard Deviation:         62.09 (0.2435)
;;     Blue:
;;       Minimum:                     0.00 (0.0000)
;;       Maximum:                   255.00 (1.0000)
;;       Mean:                      136.35 (0.5347)
;;       Standard Deviation:         75.14 (0.2947)
;;     Opacity:
;;       Minimum:                     0.00 (0.0000)
;;       Maximum:                   255.00 (1.0000)
;;       Mean:                       31.79 (0.1247)
;;       Standard Deviation:         84.25 (0.3304)
;;   Opacity: (255,255,255,255)	  #FFFFFFFF
;;   Rendering-Intent: saturation
;;   Gamma: 0.45455
;;   Chromaticity:
;;     red primary: (0.64,0.33)
;;     green primary: (0.3,0.6)
;;     blue primary: (0.15,0.06)
;;     white point: (0.3127,0.329)
;;   Resolution: 28.35x28.35 pixels/centimeter
;;   Filesize: 16.7Ki
;;   Interlace: No
;;   Orientation: Unknown
;;   Background Color: white
;;   Border Color: #DFDFDF00
;;   Matte Color: #BDBDBD00
;;   Page geometry: 99x99+0+0
;;   Compose: Over
;;   Dispose: Undefined
;;   Iterations: 0
;;   Compression: Zip
;;   Png:IHDR.color-type-orig: 6
;;   Png:IHDR.bit-depth-orig: 8
;;   Signature: 22840ae27432c17cf141a0abbc0c892f85b16ffadd793d5caccc57b5744f40ca
;;   Profile-color: 3144 bytes
;;   Tainted: False
;;   User Time: 162.980u
;;   Elapsed Time: 223:34
;;   Pixels Per Second: 0
;; "

;; 剪裁图像（建议先 clone MagickWand）
(defvar *magick-wand2* (%CloneMagickWand *magick-wand-pointer*))
(magick-crop-image *magick-wand2* 30 40 25 35)



;; string name="*","icc","icm","cptc","8bim",genericProfiles
;; binary profile

(defun image-profile-test ()
  (with-magick-wand (magick-wand :input-path "/Users/muyinliu/Downloads/2.jpg")
    (with-foreign-pointer (length-pointer 64)
      (let* ((pointer (%MagickGetImageProfile magick-wand "ICC" length-pointer))
             (length (mem-ref length-pointer :unsigned-long)))
        (format t "length: ~A~%" length)
        (let ((vector (make-array length
                                  :element-type '(unsigned-byte 8))))
          (dotimes (index length)
            (setf (elt vector index)
                  (mem-ref pointer :unsigned-char index)))
          (with-open-file (stream "/Users/muyinliu/Downloads/2.jpg.txt"
                                  :direction :output
                                  :element-type '(unsigned-byte 8))
            (write-sequence vector stream)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MagickGetImageProfile
;; MagickGetImageAttribute

;; MagickCompositeImage
;; MagickFlattenImages

;; MagickGetImageIterations
;; MagickSetImageIterations
;; MagickResetIterator
;; MagickHasNextImage
;; MagickHasPreviousImage
;; MagickNextImage

;; MagickDeconstructImages
;; MagickSetImageDelay

;; MagickGetImagePage
;; MagickSetImagePage

;; MagickGetImageBackgroundColor
;; MagickSetImageBackgroundColor

;; MagickTrimImage
;; MagickExtentImage
;; MagickMatteFloodfillImage
;; MagickTransparentImage
;; MagickBorderImage
;; MagickSetImageAlphaChannel
;; ParseChannelOption
;; MagickShaveImage
;; MagickNegateImage
;; MagickSetImageClipMask


;; MagickDrawSetTextAntialias

;;; make GIF
;;     MagickWand *mw = NewMagickWand();
;;     MagickSetFormat(mw, "gif");
;;     NSLog(@"Going into ImageMagick stuff");
;;     for (UIImage *img in frames) {
;;         MagickWand *localWand = NewMagickWand();
;;         NSData *dataObj = UIImagePNGRepresentation(img);
;;         MagickReadImageBlob(localWand, [dataObj bytes], [dataObj length]);
;;         MagickSetImageDelay(localWand, 10);
;;         MagickAddImage(mw, localWand);
;;         DestroyMagickWand(localWand);
;;     }



(ql:quickload '(:cl-ppcre :rutils :cl-emoji))

(defun test-draw-watermark-with-emoji
    (input-path output-path text
     &key (color "red")
       (font-path "/Users/muyinliu/Documents/WebWorkspace/zhengjie0.4/data/fonts/SourceHanSansCN-Regular.ttf")
       (font-size 14)
       (gravity :%SouthEastGravity)
       (x 0.0d0)
       (y 35.0d0)
       (emoji-image-directory #P"/Users/muyinliu/Downloads/unicode_emoji_images/unicode_emoji_image_twitter_19x19/")
       (font "YaHei Coneole, Arial Unicode MS")) ;; or *watermark-font-path*
  "Test"
  (with-pixel-wands ((black-pixel-wand :color "#333") ;; black
                     (white-pixel-wand :color "white"))
    (with-magick-wand (watermark-wand)
      ;; :output-path "/Users/muyinliu/Downloads/draw_watermark_with_emoji.png"
      (let ((descender 0d0)
            (text-height 0d0))
        ;; Initial image size
        (%MagickReadImage watermark-wand "xc:none")
        
        (with-drawing-wand (drawing-wand)
          (%MagickDrawPushGraphicContext drawing-wand)
          (when font-path
            ;; (%DrawSetFont drawing-wand font-path)
            (%DrawSetFont drawing-wand font))
          
          (when font-size
            (%DrawSetFontSize drawing-wand font-size))

          (let ((pointer (%MagickQueryFontMetrics watermark-wand drawing-wand text)))
            (setf text-height (mem-aref pointer :double 5))
            (setf descender (mem-aref pointer :double 3))
            ;; Initial image size
            (%MagickSetSize watermark-wand
                            (floor (* (mem-aref pointer :double 4) ;; text-width
                                      2))
                            (floor (* text-height 3)))
            (%MagickReadImage watermark-wand "xc:none"))

          (let ((prev-end 0))
            (loop for (begin end) in (rutils:group 2 (cl-ppcre:all-matches em:*emoji-regex*
                                                                           text))
               do
                 (unless (= begin prev-end)
                   (let* ((text-part (subseq text prev-end begin))
                          (text-width (mem-aref (%MagickQueryFontMetrics watermark-wand
                                                                         drawing-wand
                                                                         text-part)
                                                :double 4)))
                     (%DrawSetFillColor drawing-wand black-pixel-wand)
                     (%DrawAnnotation drawing-wand (1+ x) (1+ y) text-part)

                     (%DrawSetFillColor drawing-wand white-pixel-wand)
                     (%DrawAnnotation drawing-wand x y text-part)
                     (incf x text-width)))
                 (let* ((emoji (subseq text begin end))
                        (emoji-image-path (format nil "~A~A.png"
                                                  (namestring emoji-image-directory)
                                                  (em:emoji-hex emoji))))
                   (format t "~A~%" (em:emoji-hex emoji))
                   (with-magick-wand (emoji-wand :input-path emoji-image-path)
                     (let* ((threshold 19)
                            (emoji-size (if (< font-size threshold)
                                            threshold
                                            font-size)))
                       (%MagickScaleImage emoji-wand emoji-size emoji-size)
                       (%MagickCompositeImage watermark-wand
                                              emoji-wand
                                              :%OverCompositeOp
                                              (floor (+ x 1))
                                              (floor (- y font-size
                                                        (if (< font-size threshold)
                                                            0d0
                                                            descender))))
                       (incf x emoji-size))))
                 (setf prev-end end))
            (unless (equal prev-end (length text))
              (let* ((text-part (subseq text prev-end))
                     (text-width (mem-aref (%MagickQueryFontMetrics watermark-wand
                                                                    drawing-wand
                                                                    text-part)
                                           :double 4)))
                (%DrawSetFillColor drawing-wand black-pixel-wand)
                (%DrawAnnotation drawing-wand x y text-part)

                (%DrawSetFillColor drawing-wand white-pixel-wand)
                (%DrawAnnotation drawing-wand (1+ x) (1+ y) text-part)
                ;; (%DrawAnnotation drawing-wand x y text-part)
                (incf x text-width))))

          (%DrawPopGraphicContext drawing-wand)
          (%MagickDrawImage watermark-wand drawing-wand)
          (%MagickTrimImage watermark-wand 1))

        
        (with-magick-wand (image-wand :input-path input-path
                                      :output-path output-path)
          (with-drawing-wand (drawing-wand)
            (%MagickDrawPushGraphicContext drawing-wand)
            (when font-path
              ;; (%DrawSetFont drawing-wand font-path)
              (%DrawSetFont drawing-wand font))
            
            (when font-size
              (%DrawSetFontSize drawing-wand font-size))
            (when color
              (with-pixel-wand (pixel-wand :color color)
                (%DrawSetFillColor drawing-wand pixel-wand)))
            
            (when gravity
              (%DrawSetGravity drawing-wand gravity))

            (%DrawSetFillColor drawing-wand black-pixel-wand)
            (%DrawAnnotation drawing-wand 10 10 "正解 | www.zhengjie.com")

            (%DrawSetFillColor drawing-wand white-pixel-wand)
            (%DrawAnnotation drawing-wand 11 11 "正解 | www.zhengjie.com")
            
            (%MagickDrawImage image-wand drawing-wand)
            (%DrawPopGraphicContext drawing-wand)

            (%MagickCompositeImage image-wand
                                   watermark-wand
                                   :%OverCompositeOp
                                   (- (%MagickGetImageWidth image-wand)
                                      (%MagickGetImageWidth watermark-wand)
                                      10)
                                   (- (%MagickGetImageHeight image-wand)
                                      (%MagickGetImageHeight watermark-wand)
                                      (floor text-height)
                                      10))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-image-width-hegiht (directory width height)
  (dolist (pathname (fad:list-directory directory))
    (let ((ext-name (pathname-type pathname)))
      (when (equal "png" ext-name)
        ;; (format t "~A~%" pathname)
        (multiple-value-bind (real-width real-height)
            (image-width-height (namestring pathname))
          (when (or (not (equal real-width width))
                    (not (equal real-height height)))
            (format t "target ~Ax~A but get ~Ax~A: ~A"
                    width height
                    real-width real-height
                    pathname)))))))


(defun test (path)
  (with-magick-wand (magick-wand)
    (multiple-value-bind (vector length)
        (image-byte-vector-x path)
      (with-foreign-pointer (blob-pointer length)
        (dotimes (index length)
          (setf (mem-ref blob-pointer :unsigned-char index)
                (elt vector index)))
        (%MagickReadImageBlob magick-wand blob-pointer length)))
    (%MagickGetImageHeight magick-wand)))
