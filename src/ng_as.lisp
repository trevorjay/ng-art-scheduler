(ql:quickload :parenscript)
(defpackage "NG" (:use "COMMON-LISP" "PARENSCRIPT"))
(in-package "NG")

(defmacro js-src (&rest body)
  `(define-symbol-macro js
       (ps-inline ((lambda () (block js ,@body))))))

(js-src
 (defmacro log$ (&rest body)
   `((chain console log) ,@body))

 (define-symbol-macro sched " Schedule ")
 (define-symbol-macro running "running")
 (define-symbol-macro off "off")
  
 (let ((submit-button ((chain document get-element-by-id) "art-submit")))
   (when (or (= submit-button nil)
	     (@ submit-button hidden))
     (return-from js))
   (setf (@ submit-button hidden) "true")
   (let ((row (@ submit-button parent-element))
	 (picker ((chain document create-element) "input"))
	 (action-button ((chain document create-element) "button"))
	 (spacer ((chain document create-element) "span"))
	 (attempts 0))

     (setf (@ picker id) "picker")
     (setf (@ picker type) "datetime-local")
     (setf (@ action-button id) "actionButton")
     (setf (@ action-button inner-h-t-m-l) sched)
     (setf (@ action-button dataset state) off)
     (setf (@ spacer inner-h-t-m-l) "&nbsp;")

     (setf (@ action-button onclick)
	   (lambda ()
	     (setf (@ action-button disabled) "true")
	     (if (= (@ action-button dataset state) running)
		 (setf (@ action-button dataset state) off)
		 (setf (@ action-button dataset state) running))
	     f))

     ((chain row append-child) action-button)
     ((chain row append-child) spacer)
     ((chain row append-child) picker)

     (set-interval
      (lambda ()
	(let ((remaining-time (@ picker value)))
	  (try (setf remaining-time (- (+ (/ ((chain -date parse) remaining-time) 1000) (* attempts 90)) (floor (/ ((chain -date now)) 1000))))
	       (:catch (e)
		 (setf remaining-time -na-n)))

	  (when ((chain -number is-na-n) remaining-time)
	    (setf (@ action-button dataset state) off))

	  (if (= (@ action-button dataset state) running)
	      (let* ((hrs (floor (/ remaining-time 3600)))
		     (mins (floor (/ (- remaining-time (* hrs 3600)) 60)))
		     (secs (floor (- remaining-time (* hrs 3600) (* mins 60)))))
		(when (< hrs 10) (setf hrs (concatenate 'string "0" hrs)))
		(when (< mins 10) (setf mins (concatenate 'string "0" mins)))
		(when (< secs 10) (setf secs (concatenate 'string "0" secs)))
		(setf (@ action-button inner-h-t-m-l) (concatenate 'string hrs ":" mins ":" secs)))
	      (setf (@ action-button inner-h-t-m-l) sched))

	  (when (and (= (@ action-button dataset state) running) (<= remaining-time 0))
	    (setf attempts (+ attempts 1))
	    ((chain submit-button click)))

	  ((chain action-button remove-attribute) "disabled")))
      500))))

(with-open-file
    (idx
     "ng_as.js"
     :direction :output
     :if-does-not-exist :create
     :if-exists :supersede)
  (format idx "~a~%" js))
