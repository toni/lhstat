(require "class.linux.cpu.lisp")

(defclass processing () 
  (cpu cpu_temp_average (cpu_temp_average_max :initform 0)
       (cpu_path :accessor cpu_path :initform "/sys/devices/platform/")))

(defmethod lhstat_getprocessing (myproc)
  "Calculate average temperature for all cpus, then set max tempterature and
display strings."
  ;; get latest stats first
  (dolist (mycpu (slot-value myproc 'cpu))
		(cpu_collect_stats mycpu))
  ;; Feed list of all cpu objects to the lambada that returns a list of their
  ;; temperatures that finally gets averaged.
  ;; IF more than one CPU
  (if (> (list-length (slot-value myproc 'cpu)) 1)
      (setf (slot-value myproc 'cpu_temp_average)  
	    (average (mapcar 
		      #'(lambda (x) (parse-integer (subseq (write-to-string (slot-value x 'cpu_temp)) 0 2)))
		      (slot-value myproc 'cpu))))
      (setf (slot-value myproc 'cpu_temp_average) 
	    (slot-value (first (slot-value myproc 'cpu)) 'cpu_temp)))
  (setf (slot-value myproc 'cpu_temp_average_max) 
	(max (slot-value myproc 'cpu_temp_average) 
	     (slot-value myproc 'cpu_temp_average_max))))

(defmethod initialize-instance :after ((myproc processing) &key)
  "Upon initialization of a processing object, it automatically sets and
calculates slots."
  (setf (slot-value myproc 'cpu) (lhstat_cpu_find myproc)) ;; get list of cpu
							   ;; objects
)
;;  (print (format nil "CPU list: ~A" 
;;		 (slot-value (first (slot-value myproc 'cpu)) 'cpu_temp))))

(defmethod lhstat_cpu_find (myproc)
  "Returns list of cpu objects, with stats collected."
  (print "+ looking for cpus")
  (let ((collected_cpus '()))
    (dolist (cpu_dir (dir-list (cpu_path myproc))) ;; all cpus
      (setf dir_name (car (last (butlast 
				 (split (format nil "~A" cpu_dir) 100 "/")))))
      (print (format nil "   + ~A" dir_name))
      (when (or (and (> (length dir_name) 7)
		     (string= (subseq dir_name 0 8) "coretemp")) ;; HP 8510, thinkpad x61    
		(string= dir_name "thinkpad_hwmon"))             ;; thinkpad x31
	(push (lhstat_make_cpu cpu_dir dir_name) collected_cpus)))
    collected_cpus))
