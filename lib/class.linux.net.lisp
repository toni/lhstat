(require "class.linux.net.lisp")

(defclass network () 
  (net_interfaces (net_path :accessor net_path :initform "/sys/block/")
       storage_traffic storage_traffic_display (hd_types :initform '("hd" "sd"))))

(defmethod initialize-instance :after ((mystorage storage) &key)
  "Upon initialization of a storage object, it automatically sets and
calculates slots."
  (setf (slot-value mystorage 'storage_components)  ;; get list of storage objects
	(lhstat_storage_find mystorage))
  (print (format nil "Storage list: ~A" 
		 (slot-value (first (slot-value mystorage 'storage_components)) 
			     'hd_sys_name))))

(defmethod lhstat_storage_find (mystorage)
  "Returns list of storage objects, with stats collected."
  (let ((collected_storage '()))
    (dolist (store_dir (dir-list (storage_path mystorage))) ;; all storage
      (setf dir_name (car (last (butlast 
				 (split (format nil "~A" store_dir) 100 "/")))))
;;      (print dir_name)
      (if (= (length dir_name) 3)
	  (dolist (store_type (slot-value mystorage 'hd_types))
	    (print (format nil "~A ~A" store_type (subseq dir_name 0 2)))
	    (if (string= (subseq dir_name 0 2) store_type) ;; is it hd storage?
	      (progn
		(print (format nil "+ found ~A" dir_name))   
		(setf myhd (make-instance 'hd))
		(setf (slot-value myhd 'hd_location) store_dir)
		(setf (slot-value myhd 'hd_sys_name) dir_name)
		(push myhd collected_storage) ))))) 
    collected_storage ))

(defmethod lhstat_getstorage (mystorage)
  (dolist (myhd (slot-value mystorage 'storage_components))
     (hd_collect_stats myhd (slot-value mystorage 'sleep_sec))
     (setf (slot-value mystorage 'storage_traffic_display)
	   (format nil "^~4A (~A) ~4A " 
		   (unit (slot-value myhd 'hd_stat_r))
		   (slot-value myhd 'hd_sys_name)
		   (unit (slot-value myhd 'hd_stat_w))))))