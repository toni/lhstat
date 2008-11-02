(require "class.linux.int.lisp")

(defclass network () 
  (net_interfaces (net_path :accessor net_path :initform "/sys/class/net/")
       net_traffic net_traffic_display net_traffic_display_simple))

(defmethod initialize-instance :after ((mynetwork network) &key)
  "Upon initialization of a storage object, it automatically sets and
calculates slots."
  (setf (slot-value mynetwork 'net_interfaces)  ;; get list of network interfaces
	(lhstat_interfaces_find mynetwork))
  (print (format nil "Interface list: ~A"
		 (mapcar #'(lambda (x) (slot-value x 'int_sys_name))
			 (slot-value mynetwork 'net_interfaces)))))

(defmethod lhstat_interfaces_find (mynetwork)
  "Returns list of network interfaces, with some stats collected."

  ;; TODO: get default route interface name ie, one we care about
  ;; ip route | grep "default via" | awk '{print $5}'

  (let ((collected_interfaces '()))
    (dolist (store_dir (dir-list (net_path mynetwork))) ;; all interfaces
      ;;(print store_dir)
      (setf dir_name (car (last (butlast 
				 (split (format nil "~A" store_dir) 100 "/")))))
      ;; (print (format nil "+ found ~A" dir_name))   
      (setf myint (make-instance 'int))
      (setf (slot-value myint 'int_location) store_dir)
      (setf (slot-value myint 'int_sys_name) dir_name)
      (push myint collected_interfaces))
collected_interfaces ))

(defmethod lhstat_getnetwork (mynetwork)
  ;; TODO: use default route interface for displaying stats
  ;; currently, it's the last interface that gets displayed,
  ;; accidentaly being a correct one on this system, wlan0
  
   (dolist (myint (slot-value mynetwork 'net_interfaces))
      (net_collect_stats myint (slot-value mynetwork 'sleep_sec))
      (setf (slot-value mynetwork 'net_traffic_display_simple)
  	   (format nil "^~4A N ~4A " 
  		   (unit (slot-value myint 'int_stat_tx))
  		   (unit (slot-value myint 'int_stat_rx))))
      (setf (slot-value mynetwork 'net_traffic_display)
  	   (format nil "^~4A (~A) ~4A " 
  		   (unit (slot-value myint 'int_stat_tx))
  		   (slot-value myint 'int_sys_name)
  		   (unit (slot-value myint 'int_stat_rx))))))
