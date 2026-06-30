(in-package #:dev.zxul767.web)

(define-html-macro :standard-page ((&key title) &body body)
  `(:html
     (:head (:title ,title))
     (:body
      (:h1 ,title)
      ,@body)))

(defgeneric parse-as (type value))

(defmethod parse-as ((type (eql 'string)) value)
  (and (plusp (length value)) value))

(defmethod parse-as ((type (eql 'integer)) value)
  (and (not (null value))
       (parse-integer value :junk-allowed t)))

(defmethod parse-as ((type (eql 'keyword)) value)
  (and (plusp (length value))
       (intern (string-upcase value) :keyword)))

(defmethod parse-as ((type (eql :base64)) value)
  (let ((obj (base64->obj value)))
    (if (listp obj) obj)))

;; TODO: move to (nonexisting) `data-conversion' package
(defun base64->obj (string)
  (ignore-errors
   (with-safe-io-syntax (read-from-string (base64-decode string)))))

(defun get-cookie-value (name request)
  (cdr (assoc name (get-cookie-values request) :test #'string=)))

(defun symbol->query-name (symbol)
  (string-downcase symbol))

(defun symbol->cookie-name (symbol function-name sticky)
  (let ((package-name (package-name (symbol-package function-name))))
    (when sticky
      (ecase sticky
        (:global
         (string-downcase symbol))
        (:package
         (format nil "~(~a:~a~)" package-name symbol))
        (:local
         (format nil "~(~a:~a:~a~)" package-name function-name symbol))))))

(defun normalize-param (param)
  (etypecase param
    (list param)
    ;; nonsticky, string parameter with no default value
    (symbol `(,param string nil nil))))

(defun gen-handler-registration (name)
  `(publish :path ,(format nil "/~(~a~)" name) :function ',name))

(defun gen-request-param-bindings (function-name request-params request)
  (with-labels
      (loop for param in request-params
            collect (gen-request-param-binding param function-name request))

    (gen-request-param-binding (param function-name request)
      (destructuring-bind (name type &optional default-value sticky) param
        (let ((cookie-name (symbol->cookie-name name function-name sticky)))
          `(,name (or ,(gen-value-from-query-string name type request)
                      ,(gen-value-from-cookie cookie-name type request)
                      ,default-value)))))

    (gen-value-from-query-string (param-name type request)
      (let ((query-name (symbol->query-name param-name)))
        `(parse-as ',type (request-query-value ,query-name ,request))))

    (gen-value-from-cookie (cookie-name type request)
      `(parse-as ',type (get-cookie-value ,cookie-name ,request)))))

(defun gen-set-cookies-code (function-name request-params request)
  (with-labels
      (loop for param in request-params
            when (gen-set-cookie-code function-name param request) collect it)))

(defun gen-set-cookie-code (function-name param request)
  (destructuring-bind (name type &optional default-value sticky) param
    (declare (ignore type default-value))
    (if sticky
        `(when ,name
           (set-cookie-header
            ,request
            :name ,(symbol->cookie-name name function-name sticky)
            :value (princ-to-string ,name))))))

(defun strip-headers-code (body)
  (let ((headers-code (member :headers body :key #'car)))
    (if headers-code
        (cdr headers-code)
        body)))

(defun extract-headers-code (body)
  (cdr (assoc :headers body)))

(defun gen-custom-headers (body request)
  (declare (ignore request))
  (extract-headers-code body))

(defun gen-html-handler (name request request-params body)
  (with-gensyms (entity)
    `(defun ,name (,request ,entity)
       (with-http-response (,request ,entity :content-type "text/html")
         (let* (,@(gen-request-param-bindings name request-params request))
           ,@(gen-set-cookies-code name request-params request)
           ,@(gen-custom-headers body request)
           (with-http-body (,request ,entity)
             (with-html-output ((request-reply-stream ,request))
               (html ,@(strip-headers-code body)))))))))

(defmacro define-html-handler (name (request &rest params) &body body)
  (let ((params (mapcar #'normalize-param params)))
    `(progn
       ,(gen-html-handler name request params body)
       ,(gen-handler-registration name))))
