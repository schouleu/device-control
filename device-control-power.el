(require 'device-control)

(defvar power-exec "~/outlet.sh")
(defvar-local dctrl-power-device nil)

(defun dctrl-power-run (&rest args)
  (dctrl-run-process
   (nconc (list power-exec) args)))

(defun dctrl-power-action-powerswitch (&optional action)
  (let ((action (or action (ido-completing-read "Action: " '("ON" "OFF")))))
    (dctrl-power-run dctrl-power-device action)))

(defun dctrl-power-action-powercycle ()
  (append (dctrl-power-action-powerswitch "OFF")
	  (dctrl-action-wait 4)
	  (dctrl-power-action-powerswitch "ON")
	  (dctrl-action-wait 1)))

(defun dctrl-power-init ()
  (setq dctrl-power-device (ido-completing-read "PowerSupply number: " nil)))

(defun dctrl-power-get-actions ()
    (dctrl-build-fun-list "dctrl-power-action-" 'success))

(dctrl-register-backend
 (make-dctrl-backend :name "power"
		     :create 'dctrl-power-init
		     :get-actions 'dctrl-power-get-actions))

(provide 'device-control-power)
