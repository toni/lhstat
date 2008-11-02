(defun unit (x)
  (setf units '("b" "k" "M" "G"))
  (setf y 0)
  (defvar x_unit)
  (loop while (>= (/ x 1024) 1) do
       (setf y (+ y 1))
       (setf x (floor (float (/ x 1024))))
     y)
  (cond 
    ((>= x 1) 
     (format nil "~A~A" x (nth y units)))
    ((= x 0) 
     (format nil ""))
    (t (format nil "~1$~A" x (nth y units)))))

;;         my @units = qw/b k M G/;
;;         my $o = 0;
;;         while ( ( $v / 10000 ) >= 1 ) { $o++; $v /= 1024; }
;;         if ( $v >= 1 ) {
;;                 return sprintf("%d%s", $v, $units[$o]);
;;         } elsif ( $v == 0 ) {                                                 
;;                 return ''; 
;;         } else { 
;;                 return sprintf("%.1f%s", $v, $units[$o]);
;;         }
;; }  

(defun total (L)
  (if (null L) 0 (+ (car L) (total (cdr L)))))

(defun average (x) (float(/ (total x) (length x))))

(defconstant *day-names* '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defconstant *month-names* '("Jan" "Feb" "Mar" "Apr" "May" "Jun" 
			     "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun get-time ()
    (multiple-value-bind
	  (second minute hour date month year day-of-week dst-p tz)
	(get-decoded-time)
      (format nil "~2,'0d:~2,'0d ~a ~d ~d"
	      hour
	      minute
	      (nth day-of-week *day-names*)
	      date
	      (nth (- month 1) *month-names*)
	      )))

(defun divide-get-float (number1 number2 &optional decimals)
  "Returns float from division. Takes two integers (or strings that
can be parsed into integers) and an optional integer for number of
decimals for the float - defaults to 2 decimals."
;;   (print (integerp number2))
  (unless decimals (setf decimals 2))
  (float (read-from-string 
	  (format nil (concatenate 'string "~," (write-to-string decimals) "F")
		  (/ (if (integerp number1)
 			 number1
 			 (parse-integer number1))
		     (if (integerp number2)
 			 number2
 			 (parse-integer number2)))))))

(defun remove-extra-spaces (string)
  "Leave only one space between non-space characters of argument string."
  (let* ((len (length string))
         (new-string (make-array len :element-type 'character :fill-pointer 0)))
    (vector-push (char string 0) new-string)
    (loop for i from 1 to (1- len)
          unless (and (char= #\Space (char string i))
                      (char= #\Space (char string (1- i))))
          do (vector-push (char string i) new-string))
    (string-trim " " new-string)))

(defun split (string &optional max (ws '(#\Space #\Tab)))
  (flet ((is-ws (char) (find char ws)))
    (nreverse
     (let ((list nil) (start 0) (words 0) end)
       (loop
	  (when (and max (>= words (1- max)))
	    (return (cons (subseq string start) list)))
	  (setf end (position-if #'is-ws string :start start))
	  (push (subseq string start end) list)
	  (incf words)
	  (unless end (return list))
	  (setf start (1+ end)))))))

(defun read-oneline-file (file)
  (with-open-file (stream file)
    (read-line stream nil)))

(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname :directory (append (pathname-directory wildcard) (list :wild))
		 :name nil :type nil :defaults wildcard))

(defun dir-list (wildcard)
    (nconc (directory (clisp-subdirectories-wildcard wildcard))))

