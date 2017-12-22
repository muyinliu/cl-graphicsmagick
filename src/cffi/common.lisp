(in-package :gm)

;;; common condition

(define-condition magick-error (error)
  ((code :initarg :code :reader code)
   (name :initarg :name :reader name)
   (message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream
                     "GraphicsMagick ERROR(~A) ~A: ~A"
                     (code condition)
                     (name condition)
                     (message condition))))
  (:documentation "GraphicsMagick ERROR condition."))

(define-condition magick-warning (warning)
  ((code :initarg :code :reader code)
   (name :initarg :name :reader name)
   (message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream
                     "GraphicsMagick WARNING(~A) ~A: ~A"
                     (code condition)
                     (name condition)
                     (message condition))))
  (:documentation "GraphicsMagick WARNING condition."))


;;; magick_wand

(defun magick-get-exception (magick-wand)
  "Return last exception's code and message."
  (with-foreign-object (severity-pointer :int)
    (let* ((message (%MagickGetException magick-wand severity-pointer))
           (exception-code (mem-ref severity-pointer :int)))
      (values exception-code
              (foreign-enum-keyword '%ExceptionType exception-code)
              message))))

(define-foreign-type magick-status-type ()
  nil
  (:actual-type :unsigned-int)
  (:simple-parser %magick-status))

(defmethod expand-from-foreign (form (type magick-status-type))
  (let ((status (gensym))
        (code (gensym))
        (name (gensym))
        (message (gensym)))
    `(let ((,status ,form))
       (if (equal ,status +magick-pass+)
           t
           (multiple-value-bind (,code ,name ,message)
               (magick-get-exception ,(cadr (caddr form)))
             (cond ((>= ,code 400)
                    (error 'magick-error :code ,code :name ,name :message ,message))
                   ((>= ,code 300)
                    (warn 'magick-warning :code ,code :name ,name :message ,message))
                   ((=  ,code 0)
                    (warn "GraphicsMagick UndefinedException"))
                   (t
                    (error "GraphicsMagick Unknow ERROR"))))))))


;;; pixel_wand

(defun pixel-get-exception (magick-wand)
  "Return last exception's code and message."
  (with-foreign-object (description-pointer :string)
    (let* ((exception-code (%PixelGetException magick-wand description-pointer))
           (description (cffi:foreign-string-to-lisp description-pointer)))
      (values exception-code
              (foreign-enum-keyword '%ExceptionType exception-code)
              description))))

(define-foreign-type pixel-status-type ()
  nil
  (:actual-type :unsigned-int)
  (:simple-parser %pixel-status))

(defmethod expand-from-foreign (form (type pixel-status-type))
  (let ((status (gensym))
        (code (gensym))
        (name (gensym))
        (message (gensym)))
    `(let ((,status ,form))
       (if (equal ,status +magick-pass+)
           t
           (multiple-value-bind (,code ,name ,message)
               (magick-get-exception ,(cadr (caddr form)))
             (cond ((>= ,code 400)
                    (error 'magick-error :code ,code :name ,name :message ,message))
                   ((>= ,code 300)
                    (warn 'magick-warning :code ,code :name ,name :message ,message))
                   ((=  ,code 0)
                    (warn "GraphicsMagick UndefinedException"))
                   (t
                    (error "GraphicsMagick Unknow ERROR"))))))))
