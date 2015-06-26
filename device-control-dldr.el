(require 'device-control)

(defvar dldr-exec "~/dldr_ifwi.sh")
(defvar dldr-device-id ".*8087:0a82.*")

(defun dctrl-dldr-run (&rest args)
  (dctrl-run-process
   (nconc (list dldr-exec) args)))

(defun dctrl-dldr-action-part (&optional dnx_file part_file)
  (let ((dnx-file (read-file-name "DnX file: " nil nil t))
	(part-file (read-file-name "CfgPart file: " nil nil t))
	tramp-cmd part-filename dnx-filename)
    (multiple-value-setq (tramp-cmd dnx-filename)
      (dctrl-untramp-file dnx-file))
    (multiple-value-setq (tramp-cmd part-filename)
      (dctrl-untramp-file part-file))
    (append tramp-cmd
	    (dctrl-dldr-run "part" dnx-filename part-filename))))

(defun dctrl-dldr-action-flash-ifwi (&optional dnx_file ifwi_file)
  (let ((dnx-file (read-file-name "DnX file: " nil nil t))
	(ifwi-file (read-file-name "IFWI file: " nil nil t))
	tramp-cmd1 tramp-cmd2 ifwi-filename dnx-filename)
    (multiple-value-setq (tramp-cmd1 dnx-filename)
      (dctrl-untramp-file dnx-file))
    (multiple-value-setq (tramp-cmd2 ifwi-filename)
      (dctrl-untramp-file ifwi-file))
    (append tramp-cmd1 tramp-cmd2
	    (dctrl-dldr-run "ifwi" dnx-filename ifwi-filename))))

(defun dctrl-dldr-action-startover ()
    (dctrl-dldr-run "startover"))

(defun dctrl-dldr-device-present-p ()
  (delq nil (mapcar (lambda (line)
		      (numberp (string-match dldr-device-id line)))
		    (split-string (shell-command-to-string "lsusb") "\n" t))))

(defun dctrl-dldr-get-actions ()
;;  (when (dctrl-dldr-device-present-p)
    (dctrl-build-fun-list "dctrl-dldr-action-" 'success))

(dctrl-register-backend
 (make-dctrl-backend :name "dldr"
		     :get-actions 'dctrl-dldr-get-actions))

(provide 'device-control-dldr)
