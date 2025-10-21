;;; test-java-setup.el --- Test Java development setup -*- lexical-binding: t -*-

;;; Commentary:
;; Test script for Java development environment

;;; Code:

(defun test-java-setup ()
  "Run comprehensive tests for Java development setup."
  (interactive)
  (message "=== Java Development Environment Test ===")

  ;; Test JDTLS installation
  (let ((jdtls-bin (executable-find "jdtls"))
        (jdtls-home "/opt/homebrew/Cellar/jdtls/1.47.0/libexec"))
    (message "JDTLS binary: %s" (if jdtls-bin "✓ Found" "✗ Not found"))
    (when jdtls-bin
      (message "  Location: %s" jdtls-bin))

    (message "JDTLS home directory: %s" (if (file-exists-p jdtls-home) "✓ Found" "✗ Not found"))

    ;; Test launcher JAR
    (let ((launcher-jar (car (file-expand-wildcards (concat jdtls-home "/plugins/org.eclipse.equinox.launcher_*.jar")))))
      (message "Launcher JAR: %s" (if launcher-jar "✓ Found" "✗ Not found"))
      (when launcher-jar
        (message "  Location: %s" launcher-jar)))

    ;; Test config directory
    (let ((config-dir (concat jdtls-home "/config_mac")))
      (message "Config directory: %s" (if (file-exists-p config-dir) "✓ Found" "✗ Not found"))))

  ;; Test Lombok
  (let ((lombok-jar "/Users/wangzhixiong/repo/org/projectlombok/lombok/1.18.34/lombok-1.18.34.jar"))
    (message "Lombok JAR: %s" (if (file-exists-p lombok-jar) "✓ Found" "✗ Not found"))
    (when (file-exists-p lombok-jar)
      (message "  Location: %s" lombok-jar)))

  ;; Test Java
  (let ((java-bin (executable-find "java")))
    (message "Java binary: %s" (if java-bin "✓ Found" "✗ Not found"))
    (when java-bin
      (message "  Location: %s" java-bin)))

  ;; Test eglot
  (require 'eglot nil t)
  (message "Eglot package: %s" (if (featurep 'eglot) "✓ Available" "✗ Not available"))

  ;; Test current configuration
  (when (featurep 'eglot)
    (let ((java-server (alist-get 'java-mode eglot-server-programs)))
      (message "Java eglot configuration: %s" (if java-server "✓ Found" "✗ Not found"))
      (when java-server
        (message "  Server function: %s" java-server))))

  (message "=== Test Complete ==="))

(provide 'test-java-setup)
;;; test-java-setup.el ends here