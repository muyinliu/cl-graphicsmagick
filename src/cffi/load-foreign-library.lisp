(in-package :gm)

;;; Load library and initial

(define-foreign-library graphicsmagick-wand
  (:darwin (:or "libGraphicsMagickWand.dylib" "/usr/lib/libGraphicsMagickWand.dylib" "/usr/local/lib/libGraphicsMagickWand.dylib"))
  (:linux (:or "libGraphicsMagickWand.so" "/usr/lib/libGraphicsMagickWand.so" "/usr/local/lib/libGraphicsMagickWand.so"))
  (:windows "CORE_RL_wand_.dll")
  (t (:default "libGraphicsMagickWand")))

(use-foreign-library graphicsmagick-wand)

;;; InitializeMagick doesn't work on Windows unless its dll is loaded specifically
#+windows
(progn
  (define-foreign-library graphicsmagick
    (:windows "CORE_RL_magick_.dll"))
  (use-foreign-library graphicsmagick))
