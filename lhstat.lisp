#!/usr/bin/clisp -on-error debug

;; Writing this was a way to learn some Lisp basics for the first
;; time, and to solve the problem of monitoring some properties of my
;; linux laptops. I use it in the window manager Awesome v2.3.4, but,
;; given its text output, it shouldn't be too hard to reuse it in
;; other window managers, or textual environments. I've been using it
;; on daily basis for months on couple of different laptops.
;;
;; Toni Prug, tony@irational.org

 
(require "lib/utils.lisp")
(require "lib/class.linux.lisp")
(setf *linux* (make-instance 'linux))
(setf (slot-value *linux* 'sleep_sec) 3)
(loop 
   (lhstat_checksystem *linux*)
;;   (setf out (format nil "~A | ~A | ~A | ~A | ~A | ~A (~A) ~A%" 
   (setf out (format nil "~A | ~A | ~A | ~A | ~A | ~A" 
		     (slot-value *linux* 'cpu_temp_display_max)
		     (slot-value *linux* 'load_display_max)
		     (get-time)
		     (slot-value *linux* 'storage_traffic_display_simple)
		     (slot-value *linux* 'net_traffic_display)
;;		     (slot-value *linux* 'remaining_time) 
;;		     (slot-value *linux* 'status )
;;		     (slot-value *linux* 'charge_percentage)))
		     (slot-value *linux* 'power_display_simple)))
   (run-shell-command
    (format nil "~A ~A ~A" 
	    "echo \"0 widget_tell mybar tb_all text" out "\" |
	    'awesome-client'"))
   (sleep (slot-value *linux* 'sleep_sec)))
