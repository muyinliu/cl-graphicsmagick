(in-package :gm)

;;;; GraphicsMagick utilities

;;; macros

(defmacro with-new-magick-wand ((var) &body body)
  "Wrap %NewMagickWand and make sure %DestroyMagickWand."
  `(let ((,var (%NewMagickWand)))
     (unwind-protect
          (progn ,@body)
       (%DestroyMagickWand ,var))))

(defmacro with-magick-wand ((var &key input-path output-path init-form)
                            &body body)
  "Wrap %NewMagickWand or exist magickwand and make sure %DestroyMagickWand."
  (let ((result (gensym)))
    `(let ((,var ,(if init-form
                      init-form
                      `(%NewMagickWand)))
           ,result)
       (unwind-protect
            (progn
              (when ,input-path
                (%MagickReadImage ,var ,input-path))
              (setf ,result
                    (multiple-value-list (progn ,@body)))
              (when ,output-path
                (%MagickWriteImage ,var ,output-path)))
         (%DestroyMagickWand ,var))
       (values-list ,result))))

(defmacro with-pixel-wand ((var &key color) &body body)
  `(let ((,var (%NewPixelWand)))
     (unwind-protect
          (progn
            (when ,color
              (%PixelSetColor ,var ,color))
            ,@body)
       (%DestroyPixelWand ,var))))

(defmacro with-pixel-wands (params-list &body body)
  `(let (,@(mapcar #'(lambda (params)
                       (list (car params) '(%NewPixelWand)))
                   params-list))
     (unwind-protect
          (progn
            ,@(loop for params in params-list
                 when (getf (cdr params) :color)
                 collect (let ((var (car params))
                               (color (getf (cdr params) :color)))
                           `(%PixelSetColor ,var ,color)))
            ,@body)
       (progn
         ,@(mapcar #'(lambda (params)
                       `(%DestroyPixelWand ,(car params)))
                   params-list)))))

(defmacro with-drawing-wand ((var) &body body)
  `(let ((,var (%MagickNewDrawingWand)))
     (unwind-protect
          (progn ,@body)
       (%MagickDestroyDrawingWand ,var))))

;;; functions

(defun image-width-height (path)
  "Get Image's width and height."
  (with-magick-wand (magick-wand :input-path path)
    (values (%MagickGetImageWidth magick-wand)
            (%MagickGetImageHeight magick-wand))))

(defun describe-image (path)
  "Describes an image by formatting its attributes to an allocated string which must be freed by the user. Attributes include the image width, height, size, and others. The string is similar to the output of 'identify -verbose'."
  (with-magick-wand (magick-wand :input-path path)
    (%MagickDescribeImage magick-wand)))

(defun image-size (path)
  "Get image's size in byte."
  (with-magick-wand (magick-wand :input-path path)
    (%MagickGetImageSize magick-wand)))

(defun image-byte-vector (path)
  "Get image's blob data in '\(unsigned-byte 8) vector format."
  (let (vector)
    (with-magick-wand (magick-wand :input-path path)
      (with-foreign-pointer (length-pointer 64)
        (let* ((buffer-pointer (%MagickWriteImageBlob magick-wand length-pointer))
               (length (mem-ref length-pointer :unsigned-int)))
          (setf vector (make-array length
                                   :element-type '(unsigned-byte 8)))
          (dotimes (index length)
            (setf (elt vector index)
                  (mem-ref buffer-pointer :unsigned-char index)))
          (values vector length))))))

(defun image-byte-vector-x (path)
  "Get image's blob data by reading file directly.
Note: result is different from image-byte-vector."
  (let (vector
        length)
    (with-open-file (stream path :element-type '(unsigned-byte 8))
      (setf vector (make-array (file-length stream)
                               :element-type '(unsigned-byte 8)))
      (setf length (read-sequence vector stream)))
    (values vector length)))

(defun image-format (path)
  "Get image's format type\(string)."
  (with-magick-wand (magick-wand :input-path path)
    (%MagickGetImageFormat magick-wand)))

(defun image-depth (path)
  "Get image's depth(integer)."
  (with-magick-wand (magick-wand :input-path path)
    (%MagickGetImageDepth magick-wand)))

(defun image-resolution (path)
  "Get image's resolution."
  (with-magick-wand (magick-wand :input-path path)
    (with-foreign-pointer (x-pointer 64)
      (with-foreign-pointer (y-pointer 64)
        (%MagickGetImageResolution magick-wand x-pointer y-pointer)
        (values (mem-ref x-pointer :double)
                (mem-ref y-pointer :double))))))

(defun image-gamma (path)
  "Get image's gamma\(double)."
  (with-magick-wand (magick-wand :input-path path)
    (%MagickGetImageGamma magick-wand)))

(defun rotate-image (input-path output-path background degrees)
  "Rotate Image with background color and degrees."
  (with-magick-wand (magick-wand :input-path input-path
                                 :output-path output-path)
    (with-pixel-wand (pixel-wand)
      (%PixelSetColor pixel-wand background)
      (%MagickRotateImage magick-wand pixel-wand degrees)))
  output-path)

(defun flip-image (input-path output-path)
  "Flip\(upside down) Image."
  (with-magick-wand (magick-wand :input-path input-path
                                 :output-path output-path)
    (%MagickFlipImage magick-wand))
  output-path)

(defun flop-image (input-path output-path)
  "Flop\(horizontal flip, switch left and right) Image."
  (with-magick-wand (magick-wand :input-path input-path
                                 :output-path output-path)
    (%MagickFlopImage magick-wand))
  output-path)

(defun add-text-watermark (magick-wand text
                           &key color font-path font-size
                             (gravity :%SouthEastGravity)
                             (x 0) (y 0))
  "Add text watermark to MagickWand."
  (with-drawing-wand (drawing-wand)
    (%MagickDrawPushGraphicContext drawing-wand)
    (when color
      (with-pixel-wand (pixel-wand)
        (%PixelSetColor pixel-wand color)
        (%MagickDrawSetFillColor drawing-wand pixel-wand)))
    (when font-path
      (%MagickDrawSetFont drawing-wand font-path))
    (when font-size
      (%MagickDrawSetFontSize drawing-wand font-size))
    (when gravity
      (%MagickDrawSetGravity drawing-wand gravity))
    (%MagickDrawAnnotation drawing-wand x y text)
    (%MagickDrawPopGraphicContext drawing-wand)
    (%MagickDrawImage magick-wand drawing-wand)))

(defun add-text-watermarks (magick-wand watermark-options)
  "Add more than one text watermark to MagickWand."
  (dolist (watermark-option watermark-options)
    (apply #'add-text-watermark magick-wand watermark-option)))

(defun add-text-watermark-to-image (input-path output-path text
                                    &key color font-path font-size
                                      (gravity :%SouthEastGravity)
                                      (x 0) (y 0))
  "Add text watermark to image."
  (with-magick-wand (magick-wand :input-path input-path :output-path output-path)
    (add-text-watermark magick-wand text
                        :color color
                        :font-path font-path
                        :font-size font-size
                        :gravity gravity
                        :x x
                        :y y))
  output-path)

(defun add-text-watermarks-to-image (input-path output-path watermark-options)
  "Add text watermarks to image."
  (with-magick-wand (magick-wand :input-path input-path :output-path output-path)
    (add-text-watermarks magick-wand watermark-options))
  output-path)

(defun adaptive-threshhold-image (input-path output-path width height &optional (offset 0))
  (with-magick-wand (magick-wand :input-path input-path
                                 :output-path output-path)
    (%MagickAdaptiveThresholdImage magick-wand width height offset))
  output-path)

(defun fit-width-height (origin-width origin-height max-width max-height)
  "Return image fit width and height limit with max-width and max-height."
  (if (or (> origin-width max-width)
          (> origin-height max-height))
      (if (> (/ origin-width origin-height)
             (/ max-width max-height))
             (values max-width
                     (truncate (* origin-height (/ max-width origin-width))))
             (values (truncate (* origin-width (/ max-height origin-height)))
                     max-height))
      (values origin-width origin-height)))

(defun image-fit-width-height (magick-wand max-width max-height)
  "Return image fit width and height limit with max-width and max-height."
  (let ((origin-width (%MagickGetImageWidth magick-wand))
        (origin-height (%MagickGetImageHeight magick-wand)))
    (fit-width-height origin-width origin-height max-width max-height)))

(defun resize-image (input-path output-path columns rows
                     &key (filter :%UndefinedFilter)
                       (blur 1)
                       (fit-p t))
  "Resize image with columns(width), rows(height), filter and blur."
  (with-magick-wand (magick-wand :input-path input-path
                                 :output-path output-path)
    (when fit-p
      (multiple-value-bind (fit-width fit-height)
          (image-fit-width-height magick-wand columns rows)
        (setf columns fit-width)
        (setf rows fit-height)))
    (%MagickResizeImage magick-wand columns rows
                        filter
                        blur))
  output-path)

(defun sample-image (input-path output-path columns rows &key (fit-p t))
  "Resize image with columns(width) and rows(height).
Unlike other scaling methods, this method does not introduce any additional color into the scaled image"
  (with-magick-wand (magick-wand :input-path input-path
                                 :output-path output-path)
    (when fit-p
      (multiple-value-bind (fit-width fit-height)
          (image-fit-width-height magick-wand columns rows)
        (setf columns fit-width)
        (setf rows fit-height)))
    (%MagickSampleImage magick-wand columns rows))
  output-path)

(defun scale-image (input-path output-path columns rows &key (fit-p t))
  "Resize image with columns(width) and rows(height)."
  (with-magick-wand (magick-wand :input-path input-path
                                 :output-path output-path)
    (when fit-p
      (multiple-value-bind (fit-width fit-height)
          (image-fit-width-height magick-wand columns rows)
        (setf columns fit-width)
        (setf rows fit-height)))
    (%MagickScaleImage magick-wand columns rows))
  output-path)

(defun crop-image (input-path output-path width height x y)
  "Extracts(cut) a region(with width, height, x and y) of the image."
  (with-magick-wand (magick-wand :input-path input-path
                                 :output-path output-path)
    (%MagickCropImage magick-wand width height x y))
  output-path)

(defun compress-image (input-path output-path quality)
  "Compress Image by setting compression quality.
JPEG
  0-100, default: 75, best: 100
MIFF/TIFF
  0-9, use ZIP compression, best: 9
JPEG-2000
  The default quality value 75 results in a request for 16:1 compression. 
  The quality value 100 results in a request for non-lossy compres- sion.
PNG/MNG
  0-100, default: 75, best: 100
"
  (with-magick-wand (magick-wand :input-path input-path
                                 :output-path output-path)
    (%MagickSetCompressionQuality magick-wand quality)
    (values output-path quality)))

(defun gif-cover (input-path output-path)
  "Get GIF animation image's cover image."
  (with-magick-wand (magick-wand :input-path input-path)
    (%MagickSetImageIndex magick-wand 0)
    (with-magick-wand (cover-magick-wand :init-form (%MagickGetImage magick-wand)
                                         :output-path output-path)))
  output-path)

(defun gif-decompose (input-path output-path-formater)
  "Decompose GIF to save all frame images into files.
Example:
  \(gif-decompose \"/path/to/image.gif\" \"/path/to/output/file_~A.gif\")
Note: \"~A\" is required in the output-path-formater string, it will be replaced by the index number. The result image files's path will be \"/path/to/output/file_0.gif\" \"/path/to/output/file_1.gif\" and so on."
  (ensure-directories-exist (directory-namestring (parse-namestring output-path-formater)))
  (with-magick-wand (magick-wand :input-path input-path)
    (loop for i from 0 below (%MagickGetNumberImages magick-wand)
       do (%MagickSetImageIndex magick-wand i)
         (with-magick-wand (cover-magick-wand :init-form (%MagickGetImage magick-wand)
                                              :output-path (format nil output-path-formater
                                                                   i)))))
  output-path-formater)

(defun image-format-p (magick-wand format)
  "Whether a magick-wand is in specific format."
  (assert (string format))
  (string-equal format (%MagickGetImageFormat magick-wand)))

(defun jpeg-p (magick-wand)
  "Whether a magick-wand is a JPEG.
Note: Both *.jpg and *.jpeg is JPEG."
  (image-format-p magick-wand "JPEG"))

(defun gif-p (magick-wand)
  "Whether a magick-wand is a GIF."
  (image-format-p magick-wand "GIF"))

(defun dynamic-gif-p (magick-wand)
  "Whether a magick-wand is a dynamic GIF."
  (and (gif-p magick-wand)
       (> (%MagickGetNumberImages magick-wand) 1)))

(defun compare-images (input-path1 input-path2 output-path
                       &key (metric :%UndefinedMetric))
  "Compares one or more images and returns the specified distortion metric."
  (with-magick-wand (magick-wand1 :input-path input-path1)
    (with-magick-wand (magick-wand2 :input-path input-path2)
      (with-foreign-pointer (distortion-pointer 64)
        (with-magick-wand (result-magick-wand
                           :init-form (%MagickCompareImages magick-wand1
                                                            magick-wand2
                                                            metric
                                                            distortion-pointer)
                           :output-path output-path)
          (values output-path
                  (mem-ref distortion-pointer :double)))))))

(defun composite-image (input-path1 input-path2 output-path
                        &key (compose :%OverCompositeOp)
                          (x 0)
                          (y 0))
  "Composite one image onto another at the specified offset(merge two image).
compose:
  This operator affects how the composite is applied to the
image.  The default is Over.  Choose from these operators:
    :%OverCompositeOp       :%InCompositeOp         :%OutCompositeOP
    :%AtopCompositeOP       :%XorCompositeOP        :%PlusCompositeOP
    :%MinusCompositeOP      :%AddCompositeOP        :%SubtractCompositeOP
    :%DifferenceCompositeOP :%BumpmapCompositeOP    :%CopyCompositeOP
    :%DisplaceCompositeOP"
  (with-magick-wand (magick-wand1 :input-path input-path1 :output-path output-path)
    (with-magick-wand (magick-wand2 :input-path input-path2)
      (%MagickCompositeImage magick-wand1 magick-wand2 compose x y)))
  output-path)

(defun contrast-image (input-path output-path sharpen)
  "Enhances the intensity differences between the lighter and darker elements of the image. Set sharpen to a value other than 0 to increase the image contrast otherwise the contrast is reduced."
  (with-magick-wand (magick-wand :input-path input-path :output-path output-path)
    (%MagickContrastImage magick-wand sharpen))
  output-path)

(defun query-formats (&optional (pattern "*"))
  "Returns any image formats that match the specified pattern.
For Example:
```lisp
\(query-formats \"*\")
```
```=>
\(\"3FR\" \"8BIM\" \"8BIMTEXT\" \"8BIMWTEXT\" \"APP1\" \"APP1JPEG\" \"ART\" \"ARW\" \"AVS\"
\"B\" \"BIGTIFF\" \"BMP\" \"BMP2\" \"BMP3\" \"C\" \"CACHE\" \"CAL\" \"CALS\" \"CAPTION\" 
\"CIN\" \"CMYK\" \"CMYKA\" \"CR2\" \"CRW\" \"CUR\" \"CUT\" \"DCM\" \"DCR\" \"DCX\" \"DIB\" 
\"DNG\" \"DPX\" \"EPDF\" \"EPI\" \"EPS\" \"EPS2\" \"EPS3\" \"EPSF\" \"EPSI\" \"EPT\" \"EPT2\" \"EPT3\" \"ERF\" \"EXIF\" \"FAX\" \"FILE\" \"FITS\" \"FRACTAL\" \"FTP\" \"G\" \"G3\" \"GIF\" 
\"GIF87\" \"GRADIENT\" \"GRANITE\" \"GRAY\" \"GRAYA\" \"GROUP4RAW\" \"H\" \"HISTOGRAM\" 
\"HRZ\" \"HTM\" \"HTML\" \"HTTP\" \"ICB\" \"ICC\" \"ICM\" \"ICO\" \"ICODIB\" \"ICON\" 
\"IDENTITY\" \"IMAGE\" \"INFO\" \"IPTC\" \"IPTCTEXT\" \"IPTCWTEXT\" \"JNG\" \"JNX\" \"JPEG\" 
\"JPG\" \"K\" \"K25\" \"KDC\" \"LABEL\" \"LOCALE\" \"LOCALEC\" \"LOCALEH\" \"LOCALEMC\" 
\"LOGO\" \"M\" \"M2V\" \"MAC\" \"MAP\" \"MAT\" \"MATTE\" \"MEF\" \"MIFF\" \"MNG\" \"MONO\" 
\"MPC\" \"MPEG\" \"MPG\" \"MPR\" \"MPRI\" \"MRW\" \"MSL\" \"MTV\" \"MVG\" \"NEF\" 
\"NETSCAPE\" \"NULL\" \"O\" \"ORF\" \"OTB\" \"P7\" \"PAL\" \"PALM\" \"PAM\" \"PATTERN\" 
\"PBM\" \"PCD\" \"PCDS\" \"PCL\" \"PCT\" \"PCX\" \"PDB\" \"PDF\" \"PEF\" \"PFA\" \"PFB\"
\"PGM\" \"PICON\" \"PICT\" \"PIX\" \"PLASMA\" \"PM\" \"PNG\" \"PNG00\" \"PNG24\" \"PNG32\" 
\"PNG48\" \"PNG64\" \"PNG8\" \"PNM\" \"PPM\" \"PREVIEW\" \"PS\" \"PS2\" \"PS3\" \"PTIF\" 
\"PWP\" \"R\" \"RAF\" \"RAS\" \"RGB\" \"RGBA\" \"RLA\" \"RLE\" \"ROSE\" \"SCT\" \"SFW\" 
\"SGI\" \"SHTML\" \"SR2\" \"SRF\" \"STEGANO\" \"SUN\" \"SVG\" \"SVGZ\" \"TEXT\" \"TGA\" 
\"TIF\" \"TIFF\" \"TILE\" \"TIM\" \"TOPOL\" \"TTF\" \"TXT\" \"UIL\" \"UYVY\" \"VDA\" 
\"VICAR\" \"VID\" \"VIFF\" \"VST\" \"WBMP\" \"WPG\" \"X3F\" \"XBM\" \"XC\" \"XCF\" \"XMP\" 
\"XPM\" \"XV\" \"Y\" \"YUV\")
196
```
Note: `196` is fonts count.

WARN: MagickQueryFormats(including v1.3.27) never use parameter `pattern`,
        never rely on this, please use your own filter instead."
  (with-foreign-pointer (number_formats-pointer 64)
    (let* ((result-pointer (%MagickQueryFormats pattern number_formats-pointer))
           (count (mem-ref number_formats-pointer :unsigned-long)))
      (values (loop for index from 0 below count
                 collect (mem-aref result-pointer :string index))
              count))))

(defun query-fonts (&optional (pattern "*"))
  "Returns any font that match the specified pattern.
For Example:
```lisp
\(query-fonts \"*\")
```
```=>
\(\"AvantGarde-Book\" \"AvantGarde-BookOblique\" \"AvantGarde-Demi\"
 \"AvantGarde-DemiOblique\" \"Bookman-Demi\" \"Bookman-DemiItalic\" \"Bookman-Light\"
 \"Bookman-LightItalic\" \"Courier\" \"Courier-Bold\" \"Courier-Oblique\"
 \"Courier-BoldOblique\" \"Helvetica\" \"Helvetica-Bold\" \"Helvetica-Oblique\"
 \"Helvetica-BoldOblique\" \"Helvetica-Narrow\" \"Helvetica-Narrow-Oblique\"
 \"Helvetica-Narrow-Bold\" \"Helvetica-Narrow-BoldOblique\"
 \"NewCenturySchlbk-Roman\" \"NewCenturySchlbk-Italic\" \"NewCenturySchlbk-Bold\"
 \"NewCenturySchlbk-BoldItalic\" \"Palatino-Roman\" \"Palatino-Italic\"
 \"Palatino-Bold\" \"Palatino-BoldItalic\" \"Times-Roman\" \"Times-Bold\"
 \"Times-Italic\" \"Times-BoldItalic\" \"Symbol\")
33
```
Note: `33` is fonts count.

```lisp
\(gm::query-fonts \"Time*\")
```
=>
```=>
\(\"Times-Roman\" \"Times-Bold\" \"Times-Italic\" \"Times-BoldItalic\")
4
```

Note: parameter `pattern` of MagickQueryFonts works.
"
  (with-foreign-pointer (number_formats-pointer 64)
    (let* ((result-pointer (%MagickQueryFonts pattern number_formats-pointer))
           (count (mem-ref number_formats-pointer :unsigned-long)))
      (values (loop for index from 0 below count
                 collect (mem-aref result-pointer :string index))
              count))))

(defun magick-get-version ()
  "Returns the ImageMagick API version as a string and as a number."
  (with-foreign-pointer (version-pointer 64)
    (let ((version-string (%MagickGetVersion version-pointer)))
      (values version-string
              (mem-ref version-pointer :unsigned-long)))))

