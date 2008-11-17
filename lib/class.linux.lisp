(require "class.linux.power.lisp")
(require "class.linux.processing.lisp")
(require "class.linux.storage.lisp")
(require "class.linux.network.lisp")

(defclass linux (power processing storage network) 
  (reload_processing reload_power reload load_current load_display_max
		     (load_max :initform 0) (reload_all :initform nil) 
		     (load_path :accessor
				load_path :initform "/proc/loadavg")
		     sleep_sec selected_display))

(defmethod lhstat_configure (mylinux slot value) 
  "Given a linux object, slot and value, sets slot to value on given
linux object."
  (setf myslot (intern slot))
  (if (parse-integer value :junk-allowed t)
      (setf (slot-value mylinux myslot) (parse-integer value))
      (setf (slot-value mylinux myslot) value))
  (print (format nil "Linux slot ~A set to value ~A" myslot (slot-value mylinux myslot))))

(defmethod lhstat_checksystem (mylinux) 
  "Given a linux object, loads current state of its components."
  (lhstat_getload mylinux)
  (lhstat_getpower mylinux)
  (lhstat_getprocessing mylinux)
  (lhstat_getstorage mylinux)
  (lhstat_getnetwork mylinux))


(defmethod initialize-instance :after ((mylinux linux) &key)
  "Upon initialization of a linux object, it sets reload times"
  (lhstat_set_reloads mylinux))

(defmethod lhstat_getload (mylinux) 
  "Given a linux object, sets load slot"
  (setf myload (read-oneline-file (slot-value mylinux 'load_path)))
  (setf (slot-value mylinux 'load_current) 
	(read-from-string (subseq myload 0 (position #\Space myload))))
  (setf (slot-value mylinux 'load_max) 
	(max (slot-value mylinux 'load_current) 
	     (slot-value mylinux 'load_max)))
  (setf (slot-value mylinux 'load_display_max)
	(format nil "~A (~A)" (slot-value mylinux 'load_current)
		(slot-value mylinux 'load_max))))

(defmethod lhstat_set_reloads (mylinux) 
  "Given a linux object, sets reload slots for all components."
  (if (slot-value mylinux 'reload_all)
      (progn 
	(setf (slot-value mylinux 'reload_processing) reload_all)
	(setf (slot-value mylinux 'reload_power) reload_all)))) 

