(require 'device-control)

(defvar dctrl-relay-dnx-combo '(vol-down vol-up))
(defvar dctrl-relay-dnx-delay 6)
(defvar dctrl-relay-bootloader-combo '(vol-down))
(defvar dctrl-relay-bootloader-delay 6)
(defvar dctrl-relay-device-prefix "/dev/serial/by-id/")
(defvar dctrl-relay-default-devices '("usb-Devantech_Ltd._USB-RLY08*"
				      "usb-FTDI_FT232R_USB_UART_A100*"))
(defvar dctrl-relay-default-rate 19200)

(defvar dctrl-quick-press-time "0.2")

(defcustom dctrl-relay-map
  '((power	.	( "i" . "s" ))
    (plug	.	( "orpq" . "ehfg" ))
    (vol-down	.	( "l" . "v" ))
    (vol-up	.	( "k" . "u" ))
    (combo	.	( "lk" . "vu" ))
    )
  "Relay board mapping"
  )

(defvar dctrl-relay-status-default
  (mapcar (lambda (x) (cons (car x) nil)) dctrl-relay-map))

(defvar-local dctrl-relay-device nil)
(defvar dctrl-relay-default-status (mapcar (lambda (x) (cons (car x) nil)) dctrl-relay-map))
(defvar-local dctrl-relay-status '())

(defun dctrl-relay-run-double (k1 k2)
  (dctrl-run-process
   (nconc (list "sh" "-c"
		(mapconcat 'identity
			   (append (list "echo") (list k1) (list ">" dctrl-relay-device)
				   (list (format "; sleep %s; " dctrl-quick-press-time))
				   (list "echo") (list k2) (list ">" dctrl-relay-device))
			   " ")))))

(defun dctrl-relay-run (&rest args)
  (dctrl-run-process
   (nconc (list "sh" "-c"
		(mapconcat 'identity
			   (append (list "echo") args (list ">" dctrl-relay-device))
			   " ")))))

(defun dctrl-relay-send-command (cmd push-type)
  (let ((keys (assoc-default cmd dctrl-relay-map)))
    (if (eq push-type 'quick-press)
	(dctrl-relay-run-double (car keys) (cdr keys))
	(progn
	  (setcdr (assoc cmd dctrl-relay-status) push-type)
	  (dctrl-relay-run (if push-type (car keys) (cdr keys)))))))


(defun dctrl-relay-toggle-command (cmd)
  (unless dctrl-relay-status
    (setq dctrl-relay-status dctrl-relay-default-status))
  (let ((new-status (not (assoc-default cmd dctrl-relay-status))))
    (setcdr (assoc cmd dctrl-relay-status) new-status)
    (dctrl-relay-send-command cmd new-status)))

(defun dctrl-relay-action-force-shutdown ()
  (nconc (dctrl-relay-send-command 'power t)
	 (dctrl-action-wait 11)
	 (dctrl-relay-send-command 'power nil)))

(defun dctrl-relay-action-power-on ()
  (nconc (dctrl-relay-send-command 'power t)
	 (dctrl-action-wait 3)
	 (dctrl-relay-send-command 'power nil)))

(defun dctrl-relay-action-force-reboot ()
  (nconc (dctrl-relay-action-force-shutdown)
	 (dctrl-relay-action-power-on)))

(defun dctrl-relay-action-usb-plug ()
  (dctrl-relay-send-command 'plug t))

(defun dctrl-relay-action-usb-unplug ()
  (dctrl-relay-send-command 'plug nil))

(defun dctrl-relay-action-keypress ()
  (let ((key (ido-completing-read "Key: " (mapcar (lambda(x) (symbol-name (car x))) dctrl-relay-map))))
    (dctrl-relay-send-command (intern key) 'quick-press)))

(defun dctrl-relay-action-holdkey ()
  (let ((key (ido-completing-read "Key: " (mapcar (lambda(x)(symbol-name (car x))) dctrl-relay-map))))
    (dctrl-relay-send-command (intern key) t)))

(defun dctrl-relay-action-releasekey ()
  (let ((key (ido-completing-read "Key: " (mapcar (lambda(x)(symbol-name (car x))) dctrl-relay-map))))
    (dctrl-relay-send-command (intern key) nil)))

(defun dctrl-relay-force (combo delay)
  (nconc (dctrl-relay-action-force-shutdown)
	 (apply 'nconc (mapcar (rcurry 'dctrl-relay-send-command t) combo))
	 (dctrl-relay-action-power-on)
	 (dctrl-action-wait delay)
	 (apply 'nconc (mapcar (rcurry 'dctrl-relay-send-command nil) combo))))

(defun dctrl-relay-action-force-bootloader ()
  (dctrl-relay-force dctrl-relay-bootloader-combo dctrl-relay-bootloader-delay))

(defun dctrl-relay-action-force-dnx ()
  (dctrl-relay-force dctrl-relay-dnx-combo dctrl-relay-dnx-delay))

(defun dctrl-relay-select-device ()
  (let* ((prefix (concat (dctrl-get-tramp-prefix) dctrl-relay-device-prefix))
	 (files (mapcar 'file-expand-wildcards
			(mapcar (curry 'concat prefix)
				dctrl-relay-default-devices)))
	 (files (apply 'append (delq nil files))))
    (when files
      (let ((dev (ido-completing-read "Relay device: "
				      (append (mapcar 'file-name-nondirectory files) '("none"))
				      nil t)))
	(cond ((string= dev "none") (progn (setq dctrl-relay-device 'none)
					   nil))
	      ((setq dctrl-relay-device (concat dctrl-relay-device-prefix dev))
	       (message "default-directory: %s" default-directory)
	       (process-file "stty" nil nil nil "-F" dctrl-relay-device
			     (number-to-string dctrl-relay-default-rate))))))))

(defun dctrl-relay-connected-p ()
  (cond ((eq dctrl-relay-device 'none) nil)
	(dctrl-relay-device dctrl-relay-device)
	(t (dctrl-relay-select-device))))

(defun dctrl-relay-get-actions ()
  (when (dctrl-relay-connected-p)
    (dctrl-build-fun-list "dctrl-relay-action-" 'success)))

(defun dctrl-relay-init ()
  (setq dctrl-relay-status dctrl-relay-default-status))

(dctrl-register-backend
 (make-dctrl-backend :name "relay"
		     :create 'dctrl-relay-init
		     :get-actions 'dctrl-relay-get-actions))

(provide 'device-control-relay)
