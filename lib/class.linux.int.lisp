(defclass int () 
  (int_location int_sys_name int_stat int_operstate
	   int_stat_rx_now int_stat_tx_now int_stat_rx int_stat_tx
	   (int_stat_rx_last :initform 0)  
	   (int_stat_tx_last :initform 0)))

(defmethod net_collect_stats (myint sleep_sec)
  "Collects stat for given network interface"

  ;; all the stats in a messy space separated line cleaned up
  (setf (slot-value myint 'int_operstate)
	 (read-oneline-file 
	 (make-pathname :directory 
			(append (pathname-directory 
				 (slot-value myint 'int_location)))
			:name "operstate")))

  (setf stats_dir   ;; statistics directory
	(make-pathname :directory 
		       (append 
			(pathname-directory 
			 (format nil "~A~A/"
				 (slot-value myint 'int_location) "statistics")))))

  ;; total bytes received (RX) current value
  (setf (slot-value myint 'int_stat_rx_now)
	(parse-integer 
	 (read-oneline-file 
	  (make-pathname :directory stats_dir :name "rx_bytes"))))

  ;; total bytes transmited (TX) current value
  (setf (slot-value myint 'int_stat_tx_now)
	(parse-integer 
	 (read-oneline-file 
	  (make-pathname :directory stats_dir :name "tx_bytes"))))

  ;; READ RATE per sec = (current - last read) / seconds between current/last
   (setf (slot-value myint 'int_stat_rx)
 	(floor (- (slot-value myint 'int_stat_rx_now)
 		  (slot-value myint 'int_stat_rx_last)) 
 	       sleep_sec))
   ;; WRITE RATE per sec = (current - last write) / seconds between current/last
   (setf (slot-value myint 'int_stat_tx)
 	(floor (- (slot-value myint 'int_stat_tx_now)
		  (slot-value myint 'int_stat_tx_last)) 
	       sleep_sec))
   ;; update last rates with current ones
   (setf (slot-value myint 'int_stat_rx_last) 
	 (slot-value myint 'int_stat_rx_now))
   (setf (slot-value myint 'int_stat_tx_last) 
	 (slot-value myint 'int_stat_tx_now)))
