(defun warning_low_battery (linux) 
  (setf mycmd (format nil "~A ~A ~A" 
		      "echo \" battery C R I T I C A L ... " 
		      (slot-value linux 'power_display_warning) "\" | 
osd_cat -p middle -A center -f -adobe-helvetica-bold-*-*-*-64-*-*-*-*-*-*-* -s 4 -d 2"))
  (run-shell-command mycmd))