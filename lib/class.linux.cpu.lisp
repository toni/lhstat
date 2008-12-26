(defclass cpu () 
  (cpu_temp cpu_label cpu_location cpu_sys_name))

(defmethod cpu_collect_stats (mycpu)
  "Collects stat for given cpu object."
  ;;(print (format nil "cpu_location: ~A" (slot-value mycpu 'cpu_location)))
  (setf label_path (make-pathname :directory  
				  (append (pathname-directory 
					   (slot-value mycpu 'cpu_location)))
		       :name "temp1_label"))

  (setf (slot-value mycpu 'cpu_label)
	(if (probe-file label_path)
	    (read-oneline-file label_path)
	    (slot-value mycpu 'cpu_sys_name)))

  (setf temp_path (make-pathname :directory 
				 (append  (pathname-directory 
					   (slot-value mycpu 'cpu_location)))
				 :name "temp1_input"))

  (setf (slot-value mycpu 'cpu_temp) 
	(if (probe-file temp_path)
	    (parse-integer (read-oneline-file temp_path)))))

(defun lhstat_make_cpu ((cpu_dir string) (dir_name string))
  "Returns a cpu object"
  (let  ((mycpu))
    (print (format nil "+ making CPU object from '~A'" dir_name))   
    (setf mycpu (make-instance 'cpu))
    (setf (slot-value mycpu 'cpu_location) cpu_dir)
    (setf (slot-value mycpu 'cpu_sys_name) dir_name)
    (cpu_collect_stats mycpu)
     (print (format nil "~A ~A" 
		    (slot-value mycpu 'cpu_label) 
		    (slot-value mycpu 'cpu_temp)))
  mycpu))
