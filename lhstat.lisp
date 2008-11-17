#!/usr/bin/clisp -on-error debug

;; Writing this was a way to learn some Lisp basics for the first
;; time, and to solve the problem of monitoring some properties of my
;; linux laptops. I use it in the window manager Awesome v2.3.4, but,
;; given its text output, it shouldn't be too hard to reuse it in
;; other window managers, or textual environments. I've been using it
;; on daily basis for months on couple of different laptops.
;;
;; Toni Prug, tony@irational.org

;; we'll supply selected_display on the command line, or even better
;; on an open socket, which would alow for different displays, which
;; means that detailed stats of various system components could be
;; displayed (for example: with a shortcut to cycle through available
;; displays). which means that a socket to which we print a display
;; name could be a good idea.

(require "lib/warnings.lisp")
(require "lib/utils.lisp")
(require "lib/class.linux.lisp")
(setf *linux* (make-instance 'linux))
(setf (slot-value *linux* 'sleep_sec) 3)
(setf (slot-value *linux* 'selected_display) "awesome.lisp")

(defun lhstat ()
  (lhstat_checksystem *linux*)
  (require (concatenate 'string "display/" (slot-value *linux* 'selected_display)))
  (display_stats *linux*)
  (if (= (slot-value *linux* 'power_status_critical) 1)
      (warning_low_battery *linux*))
  (sleep (slot-value *linux* 'sleep_sec)))

(require "lib/socket.server.lisp")

;;(defconstant *port* 9980)
;;(defconstant *ip* "127.0.0.1")
;;(start-server-with (*port* *ip*) lhstat)

(start-server-with (9980 "127.0.0.1") lhstat)
