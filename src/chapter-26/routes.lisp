(in-package :dev.zxul767.web)

(defvar *seconds-in-a-day* (* 60 60 24))

(define-html-handler random-number (request (limit integer 1000))
  (:html
    (:head (:title "Random"))
    (:body
     (:p "Random Number v2.0: " (:print (random limit))))))

(define-html-handler show-query-params (request)
  (:standard-page
   (:title "Query Parameters")
   (if (request-query request)
       (html
         (:table :border 1
                 (:tr (:th "key") (:th "value"))
                 (loop for (k . v) in (request-query request)
                       do (html (:tr (:td k) (:td v))))))
       (html (:p "No query parameters.")))))

(define-html-handler simple-form (request)
  (:html
    (:head (:title "Simple Form"))
    (:body
     (:form :method "POST" :action "/show-query-params"
            (:table
             (:tr (:td "Username")
                  (:td (:input :name "username" :size 20)))
             (:tr (:td "Password")
                  (:td (:input :name "password" :type "password" :size 20))))
            (:p (:input :name "submit" :type "submit" :value "ready")
                (:input :type "reset" :value "Reset"))))))

(define-html-handler show-cookies (request)
  (:standard-page
   (:title "Cookies")
   (if (null (get-cookie-values request))
       (html (:p "No cookies."))
       (html
         (:table :border 1
                 (:tr (:th "name") (:th "value"))
                 (loop for (key . value) in (get-cookie-values request)
                       do (html (:tr (:td key) (:td value)))))))))

(define-html-handler set-cookie (request (my-cookie string "tasty" :local))
  (:standard-page
   (:title "Set Cookie")
   (:p (:print (format nil "Cookie set: [~a: ~a]" "my-cookie" "tasty")))
   (:p (:a :href "/show-cookies" "look at cookie jar."))))

(defun yesterday ()
  (- (get-universal-time) *seconds-in-a-day*))

(defun clear-all-cookies (request)
  (let ((cookies (get-cookie-values request)))
    (loop for (name . value) in cookies
          do (set-cookie-header request
                                :name name
                                :value value
                                ;; a past expiry date purges the cookie from the browser
                                :expires (yesterday)))))

(define-html-handler clear-cookies (request)
  (:headers (clear-all-cookies request))
  (:standard-page
   (:title "Cookies Grinder")
   (:p "All cookies have been erased")))
