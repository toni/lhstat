(defclass power () 
  (alarm serial_number manufacturer model_name charge_now remaining_min remaining_hour	 
	 charge_full charge_full_design current_now voltage_now voltage_min_design 
	 energy_now energy_full energy_full_design power_files_prefix 
	 (power_status_full :initform 0)
	 technology present status type uevent charge_percentage remaining_time
	 (power_status_critical :initform 0) (power_calibrating_mode :initform 0)
	 (sys_power_path :accessor sys_power_path :initform "/sys/class/power_supply/")
	 (battery_path :accessor battery_path ) power_files ))

(defmethod lhstat_getpower (mypower)
  "No point in collecting it all when only couple of stats are
used. This should take a list as an argument instead, which would
solve the problem."
  ;; populate power object slots by reading each file inside the battery directory
  (lhstat_power_getattr (slot-value mypower 'power_files) mypower)
  ;; on some systems, battery /sys files will have 'energy' prefix, on others 'change'
  (setf charge_full (intern (concatenate 'string (slot-value mypower 'power_files_prefix) "_FULL")))
  (setf charge_now  (intern (concatenate 'string (slot-value mypower 'power_files_prefix) "_NOW")))
  (setf charge_full_design  (intern (concatenate 'string (slot-value mypower 'power_files_prefix) "_FULL_DESIGN")))
  (setf (slot-value mypower 'charge_percentage)
	(floor (* (divide-get-float (slot-value mypower charge_now) 
			     (slot-value mypower charge_full_design)) 100)))
  (if (= (slot-value mypower 'charge_percentage) 100)
      ;; status: FULL
      (power-full mypower)
      (progn
	;; status: NOT FULL
	(setf (slot-value mypower 'power_status_full) 0)
	(if (< (parse-integer (slot-value mypower 'current_now)) 600000)
	    ;; status: CALIBRATING
	    ;; after plugging power supply in the laptop, current_now value in /sys
	    ;; falls to 0 (or 4000, or 25000) for up to 50 seconds period, after which
	    ;; the value jumps to the expected recharging level (355800 on my HP
	    ;; 8510w) - hence display "Calibrating..." mode. Inaccurate, for now.
	    (remaining-time-calibrating mypower)
	    ;; NOT FULL and NOT CALIBRATING
	    (progn
	      (setf (slot-value mypower 'power_calibrating_mode) 0)
	      (set-remaining-time mypower)))))
  (set-status-critical mypower))

(defmethod lhstat_power_find (mypower)
  "Returns a path to the /sys directory containing power_supply stats
for the Battery supply."
  (setf (slot-value mypower 'power_files_prefix) "CHARGE")
  (dolist (power_dir (dir-list (sys_power_path mypower))) 
    ;; all power_supply options
      (setf power_type
	    (read-oneline-file (make-pathname 
				:directory (append power_dir) :name "type"))) 
      (when (string= power_type "Battery") ;; Battery found
	    (if (probe-file (make-pathname 
			     :directory (append power_dir) :name "charge_now"))
		(setf (slot-value mypower 'power_files_prefix) "CHARGE")
		(setf (slot-value mypower 'power_files_prefix) "ENERGY"))
	    (return-from lhstat_power_find power_dir)))) ;; return path

(defmethod lhstat_power_getattr (files mypower) 
  "Given a list of file paths to Linux /sys power_supply attributes,
and a power object, set all the slots (names after the actual file
names) of the object to the content of files."
  (loop for power_attr in files
     do ( 
   	 setf (slot-value mypower 
   		 (intern (string-upcase 
			  (car (last (split (format nil "~A" power_attr) 100 "/"))))))
   		 (read-oneline-file power_attr))
       ))

(defmethod initialize-instance :after ((mypower power) &key)
  "Upon initialization of a power object, it automatically sets and
calculates some slots."
  ;; find a battery power source
  (setf (slot-value mypower 'battery_path) (lhstat_power_find mypower))
  (print (format nil "+ found battery at ~A " (slot-value mypower 'battery_path)))
  ;; collect all the files/attributes
  (setf (slot-value mypower 'power_files)
	(directory (make-pathname :directory 
				  (append (battery_path mypower)) :name "*")))
  ;; get all the stats
  (lhstat_getpower mypower))

(defmethod set-status-critical (mypower)
  "Given a power object, set the power critical status if the charge
falls under the designated percentage and if the power status is
Dischcharging"
   (if (< (slot-value mypower 'charge_percentage) 8)
       (if (string= (slot-value mypower 'status) "Discharging")
	   (setf (slot-value mypower 'power_status_critical) 1)
	   (setf (slot-value mypower 'power_status_critical) 0)))
   (print (format nil "power_status_critical ~A" (slot-value mypower 'power_status_critical))))

(defmethod power-full (mypower)
  "Given a power object, sets reamining time slots for FULL case."
  (setf (slot-value mypower 'power_status_full) 1)
  (setf (slot-value mypower 'remaining_time) "")
  (power-empty-rem-hourmin mypower))

(defmethod remaining-time-calibrating (mypower)
  "Given a power object, sets reamining time slots for CALIBRATING case."
  (setf (slot-value mypower 'power_calibrating_mode) 1)
  (setf (slot-value mypower 'remaining_time) "Calibrating ... ")
  (power-empty-rem-hourmin mypower))

(defmethod power-empty-rem-hourmin (mypower)
  (setf (slot-value mypower 'remaining_hour) "")
  (setf (slot-value mypower 'remaining_min) ""))

(defmethod set-remaining-time (mypower)
  "Given a power object, set slots for the reamining hour, minutes, and
remaining_time string for displaying."
  (setf (slot-value mypower 'power_calibrating_mode) 0)
  (setf charge_full_design  (intern (concatenate 'string (slot-value mypower 'power_files_prefix) "_FULL_DESIGN")))
  (setf charge_now  (intern (concatenate 'string (slot-value mypower 'power_files_prefix) "_NOW")))
  (print (format nil "current_now: ~A" (slot-value mypower 'current_now)))
  (print (format nil "~A: ~A" charge_full_design (slot-value mypower charge_full_design)))
  (print (format nil "~A: ~A" charge_now (slot-value mypower charge_now)))
  (if (string= (slot-value mypower 'status) "Discharging")
      (setf remtime (divide-get-float (slot-value mypower charge_now)
				      (slot-value mypower 'current_now) 6))
      (setf remtime 
	    (divide-get-float
	     (- (parse-integer (slot-value mypower charge_full_design)) 
		(parse-integer (slot-value mypower charge_now)))
	     (slot-value mypower 'current_now) 6)))
  (setf (slot-value mypower 'remaining_hour) (floor remtime))
  (setf (slot-value mypower 'remaining_min)
	(floor (* (- remtime (floor remtime)) 60))))


