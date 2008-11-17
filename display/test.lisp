(defun display_stats (linux) 
    (setf lhstat_bar (format nil "~A | ~A | ~A | ~A | ~A" 
			     (slot-value linux 'cpu_temp_display_max)
			     (slot-value linux 'load_display_max)
			     (slot-value linux 'storage_traffic_display)
			     (slot-value linux 'net_traffic_display)
			     (slot-value linux 'power_display_warning)))
  (run-shell-command
   (format nil "~A ~A ~A" 
	   "echo \"0 widget_tell mybar tb_all text" lhstat_bar "\" |
	    'awesome-client'")))