(in-package :dev.zxul767.web)

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

(defun generate-handler-registration (name)
  `(publish :path ,(format nil "/~(~a~)" name) :function ',name))

(defun generate-request-param-bindings (function-name request-params request)
  (with-labels
      (loop for param in request-params
            collect (generate-request-param-binding param function-name request))

    (generate-request-param-binding (param function-name request)
      (destructuring-bind (name type &optional default-value sticky) param
        (let ((cookie-name (symbol->cookie-name name function-name sticky)))
          `(,name (or ,(generate-value-from-query-string name type request)
                      ,(generate-value-from-cookie cookie-name type request)
                      ,default-value)))))

    (generate-value-from-query-string (param-name type request)
      (let ((query-name (symbol->query-name param-name)))
        `(parse-as ',type (request-query-value ,query-name ,request))))

    (generate-value-from-cookie (cookie-name type request)
      `(parse-as ',type (get-cookie-value ,cookie-name ,request)))))

(defun generate-set-cookies-code (function-name request-params request)
  (with-labels
      (loop for param in request-params
            when (generate-set-cookie-code function-name param request) collect it)))

(defun generate-set-cookie-code (function-name param request)
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

(defun generate-custom-headers (body request)
  (declare (ignore request))
  (extract-headers-code body))

(defun generate-html-handler (name request request-params body)
  (with-gensyms (entity)
    `(defun ,name (,request ,entity)
       (with-http-response (,request ,entity :content-type "text/html")
         (let* (,@(generate-request-param-bindings name request-params request))
           ,@(generate-set-cookies-code name request-params request)
           ,@(generate-custom-headers body request)
           (with-http-body (,request ,entity)
             (with-html-output ((request-reply-stream ,request))
               (html ,@(strip-headers-code body)))))))))

(defmacro define-html-handler (name (request &rest params) &body body)
  (let ((params (mapcar #'normalize-param params)))
    `(progn
       ,(generate-html-handler name request params body)
       ,(generate-handler-registration name))))
