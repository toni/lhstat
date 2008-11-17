;; code for this server taken from:
;; http://paste.lisp.org/display/27402

(defvar *connected-users* '())
(defvar *fds* '())
(defvar *sock* nil)

(defun setnonblocking (socket)
  (socket:socket-options socket :so-keepalive))

(defun add-fd (socket)
  (car (push (cons socket (cons :io nil)) *fds*)))

(defun remove-fd (socket)
  (setf *fds* (delete socket *fds* :key #'car)))

(defun insert-connection (conn)
  (format conn "Welcome to clisp-echo LHSTAT server~%v.0.5.5~%> ")
  (finish-output conn)
  (format *trace-output* "~a ~a~%" "connect from" (socket:socket-stream-peer conn))
  (add-fd conn))

(defun remove-connection (conn)
  (format *trace-output* "~a ~a~%" "disconnect from" (socket:socket-stream-peer conn))
  (remove-fd conn))

(let ((exit-value nil))
  (defun init-exit-value()
    (setf exit-value nil))
  (defun set-exit-value()
    (setf exit-value t))
  (defun exit-value-p()
    exit-value))

(defun dispatch (fd)
  (let* ((connection (car fd))
         (line (read-line connection)))
    (cond ((equal line "QUIT") 
	   (remove-connection connection) 
	   (close connection :abort t) (return-from dispatch))
          ((equal line "SHUTDOWN") (set-exit-value))
          ((equal line "SHOWCONN") (dump-fds connection))
 	  ((and (> (length line) 5) 
		(string= line "conf:" :start1 0 :end1 5))
	   (configure-linux connection line))
          (t  (format connection "Command not recognised: ~a~%" line)))
    (format connection "> ")
    (finish-output connection)))

(defun configure-linux (stream line)
  (setf config (split (first (last (split line 2 ":"))) 2 "="))
  (format stream "~a ~%" 
	  (lhstat_configure *linux* (first config) 
			    (first (last config)))))

(defun dump-fds (stream)
  (dolist (fd *fds*)
    (format stream "~a   ~a        ~a~%" (first fd) (second fd) (cddr fd))))

(defun close-server(server)
  (cleanup-fds)
  (socket:socket-server-close server)
  (format *trace-output* "Shutting down clisp-echo server...~%"))

(defun cleanup-fds ()
  (mapcar #'(lambda (fd)
              (remove-connection (car fd))
              (close (car fd) :abort t)) *fds*))

(defmacro start-server-with ((port ip) &body body)
  (init-exit-value)
  (setf sock (socket:socket-server port :interface ip))
  (setnonblocking sock)
  (print (format nil "~a ~a:~a" "Creating server at" ip port))
  `(do () ((exit-value-p) (close-server sock))
    (,@body)
    (when (socket:socket-status sock 0 1)
      (insert-connection (socket:socket-accept sock :buffered t)))
    (when *fds*
      (socket:socket-status *fds* 0 1)
      (mapcar #'(lambda (connection)
                  (when (eq :io (cddr connection))
                    (dispatch connection))) *fds*))))

