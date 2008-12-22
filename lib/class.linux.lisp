(require "class.linux.power.lisp")
(require "class.linux.processing.lisp")
(require "class.linux.storage.lisp")
(require "class.linux.network.lisp")

(defclass linux (power processing storage network) 
  (reload_processing reload_power reload load_current 
   load_display_max sleep_sec selected_display
   (load_max :initform 0) (reload_all :initform nil) 
   (load_path :accessor
	      load_path :initform "/proc/loadavg")
   power_display_simple power_display_warning cpu_temp_display 
   cpu_temp_display_max cpu_temp_selected_display
   storage_traffic_display storage_traffic_display_simple
   net_traffic_display net_traffic_display_simple))

(defmethod lhstat_configure (mylinux slot value) 
  "Given a linux object, slot and value, sets slot to value on given
linux object."
  (setf myslot (intern slot))
  (if (parse-integer value :junk-allowed t)
      (setf (slot-value mylinux myslot) (parse-integer value))
      (setf (slot-value mylinux myslot) value))
  (print (format nil "Linux slot ~A set to value ~A" myslot (slot-value mylinux myslot))))

(defmethod lhstat_checksystem (mylinux) 
  "Given a linux object, loads current state of its components. Called
from the main loop."
  (lhstat_getload mylinux)
  (lhstat_getpower mylinux)
  (lhstat_getprocessing mylinux)
  (lhstat_getstorage mylinux)
  (lhstat_getnetwork mylinux)
  (lhstat_setdisplays mylinux))

(defmethod lhstat_setdisplays (mylinux) 
  "Given a linux object, sets displays for all components."
  ;; load
  (setf (slot-value mylinux 'load_display_max)
	(format nil "~A (~A)" (slot-value mylinux 'load_current)
		(slot-value mylinux 'load_max)))
  ;; remaining time
  (when (and (= (slot-value mylinux 'power_calibrating_mode) 0)
	   (= (slot-value mylinux 'power_status_full) 0))
	(setf (slot-value mylinux 'remaining_time) 
	      (format nil "~Ah:~Am"  
		      (slot-value mylinux 'remaining_hour)  
		      (slot-value mylinux 'remaining_min))))
  ;; power
  (setf (slot-value mylinux 'power_display_simple) 
	(format nil "~A (~A) ~A%" 
		(slot-value mylinux 'remaining_time)
		(subseq (slot-value mylinux 'status) 0 3)
		(slot-value mylinux 'charge_percentage)))
  (setf (slot-value mylinux 'power_display_warning) 
	(format nil "~A ~A%" 
		(slot-value mylinux 'remaining_time)
		(slot-value mylinux 'charge_percentage)))
  ;; network 
  ;; TODO: use default route interface for displaying stats
  ;; currently, it's the last interface that gets displayed,
  ;; accidentaly being a correct one on this system, wlan0
  (dolist (myint (slot-value mylinux 'net_interfaces))
     (setf (slot-value mylinux 'net_traffic_display_simple)
	   (format nil "^~4A N ~4A " 
		   (unit (slot-value myint 'int_stat_tx))
		   (unit (slot-value myint 'int_stat_rx))))
     (setf (slot-value mylinux 'net_traffic_display)
	   (format nil "^~4A (~A) ~4A " 
		   (unit (slot-value myint 'int_stat_tx))
		   (slot-value myint 'int_sys_name)
		   (unit (slot-value myint 'int_stat_rx)))))
  ;; cpu temp
  (setf (slot-value mylinux 'cpu_temp_display)
	(format nil "~AC" (slot-value mylinux 'cpu_temp_average)))
  (setf (slot-value mylinux 'cpu_temp_display_max)
	(format nil "~AC (~A)" 
		(subseq (write-to-string 
			 (slot-value mylinux 'cpu_temp_average)) 0 2)
		(subseq (write-to-string 
			 (slot-value mylinux 'cpu_temp_average_max)) 0 2)))
  ;; storage
  (dolist (myhd (slot-value mylinux 'storage_components))
     (setf (slot-value mylinux 'storage_traffic_display_simple)
	   (format nil "^~4A D ~4A " 
		   (unit (slot-value myhd 'hd_stat_r))
		   (unit (slot-value myhd 'hd_stat_w))))
     (setf (slot-value mylinux 'storage_traffic_display)
	   (format nil "^~4A (~A) ~4A " 
		   (unit (slot-value myhd 'hd_stat_r))
		   (slot-value myhd 'hd_sys_name)
		   (unit (slot-value myhd 'hd_stat_w))))))

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
	     (slot-value mylinux 'load_max))))

(defmethod lhstat_set_reloads (mylinux) 
  "Given a linux object, sets reload slots for all components."
  (when (slot-value mylinux 'reload_all)
	(setf (slot-value mylinux 'reload_processing) reload_all)
	(setf (slot-value mylinux 'reload_power) reload_all)))

