;;; init-local.el --- Local personal configuration -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This file contains personal configuration settings that are
;; specific to this machine and user setup.
;;
;;; Code:

;; Org agenda files configuration
;; Set org-agenda-files to search for org files in ~/org/ directory
(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))

;; Set default notes file for org-capture
(setq org-default-notes-file "~/org/inbox.org")

;; Ensure org-agenda-files is updated when new org files are added
(defun my/update-org-agenda-files ()
  "Update org-agenda-files with all org files in ~/org/ directory."
  (interactive)
  (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
  (message "Updated org-agenda-files with %d files" (length org-agenda-files)))

;; Auto-update agenda files when opening org files
(add-hook 'org-mode-hook
          (lambda ()
            (when (string-prefix-p (expand-file-name "~/org/") (buffer-file-name))
              (my/update-org-agenda-files))))

(provide 'init-local)
;;; init-local.el ends here