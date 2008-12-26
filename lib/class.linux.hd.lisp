(defclass hd () 
  (hd_temp hd_label hd_location hd_sys_name
	   hd_vendor hd_model hd_modalias hd_stat hd_blk_size 
	   hd_stat_r_now hd_stat_w_now hd_stat_r hd_stat_w
	   (hd_stat_r_last :initform 0)  
	   (hd_stat_w_last :initform 0)))

(defun lhstat_make_hd ((store_dir string) (dir_name string))
  "Returns a hd object"
  (let  ((myhd))
    (print (format nil "+ making HD object from '~A'" dir_name))   
    (setf myhd (make-instance 'hd))
    (setf (slot-value myhd 'hd_location) store_dir)
    (setf (slot-value myhd 'hd_sys_name) dir_name)
  myhd))

(defmethod hd_collect_stats (myhd sleep_sec)
  "Collects stats for given hd object."
  ;; should we not be getting block size with something like
  ;; /sbin/dumpe2fs /dev/sda3 | grep "Block size" | awk '{print $3}'
  (setf (slot-value myhd 'hd_blk_size) 512)
  (setf vendor_path 
	(make-pathname :directory 
		       (append (pathname-directory 
				(format nil "~A~A/"
					(slot-value myhd 'hd_location) "device")))
		       :name "vendor"))
  (setf (slot-value myhd 'hd_vendor) 
	(if (probe-file vendor_path)
	    (read-oneline-file vendor_path)))

  ;; all the stats in a messy space separated line cleaned up
  (setf (slot-value myhd 'hd_stat)
	(remove-extra-spaces
	 (read-oneline-file 
	 (make-pathname :directory 
			(append (pathname-directory 
				 (slot-value myhd 'hd_location)))
			:name "stat"))))
  ;; current read value from the stats line
  (setf (slot-value myhd 'hd_stat_r_now)
	(* (slot-value myhd 'hd_blk_size) 
	   (parse-integer (nth 2 (split (slot-value myhd 'hd_stat) 100 )))))
  ;; current write value from the stats line
  (setf (slot-value myhd 'hd_stat_w_now)
	(* (slot-value myhd 'hd_blk_size) 
	   (parse-integer (nth 4 (split (slot-value myhd 'hd_stat) 100 )))))
  ;; READ RATE per sec = (current - last read) / seconds between current/last
  (setf (slot-value myhd 'hd_stat_r)
	(floor (- (slot-value myhd 'hd_stat_r_now)
		  (slot-value myhd 'hd_stat_r_last)) 
	       sleep_sec))
  ;; WRITE RATE per sec = (current - last write) / seconds between current/last
  (setf (slot-value myhd 'hd_stat_w)
	(floor (- (slot-value myhd 'hd_stat_w_now)
			     (slot-value myhd 'hd_stat_w_last)) 
			  sleep_sec))
  ;; update last rates with current ones
  (setf (slot-value myhd 'hd_stat_r_last) (slot-value myhd 'hd_stat_r_now))
  (setf (slot-value myhd 'hd_stat_w_last) (slot-value myhd 'hd_stat_w_now)))
