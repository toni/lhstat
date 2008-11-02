(defclass cpu () 
  (cpu_temp cpu_label cpu_location cpu_sys_name))

(defmethod cpu_collect_stats (mycpu)
  "Collects stat for given cpu object."
  ;;(print (format nil "cpu_location: ~A" (slot-value mycpu 'cpu_location)))
  (setf label_path (make-pathname :directory  (append (pathname-directory (slot-value mycpu 'cpu_location)))
		       :name "temp1_label"))
  (if (probe-file label_path)
      (setf (slot-value mycpu 'cpu_label) (read-oneline-file label_path))
      (setf (slot-value mycpu 'cpu_label) (slot-value mycpu 'cpu_sys_name)))

  (setf temp_path (make-pathname :directory (append (pathname-directory (slot-value mycpu 'cpu_location)))
				 :name "temp1_input"))
  (if (probe-file temp_path)
      (setf (slot-value mycpu 'cpu_temp) (parse-integer (read-oneline-file temp_path)))))
