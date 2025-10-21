;;; verify-eglot-config.el --- Verify Eglot Lombok configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Quick verification script for the Eglot + Lombok configuration

;;; Code:

(defun verify-eglot-lombok-config ()
  "Verify that the Eglot Lombok configuration is properly set up."
  (interactive)
  (message "=== Verifying Eglot + Lombok Configuration ===")

  ;; Check if function is defined
  (if (fboundp 'my/jdtls-command-contact)
      (message "✓ my/jdtls-command-contact function is defined")
    (message "✗ my/jdtls-command-contact function not found"))

  ;; Check if dependencies exist
  (let ((lombok-jar "/Users/wangzhixiong/repo/org/projectlombok/lombok/1.18.34/lombok-1.18.34.jar")
        (debug-jar "/Users/wangzhixiong/repo/com/microsoft/java/com.microsoft.java.debug.plugin/0.53.0/com.microsoft.java.debug.plugin-0.53.0.jar"))

    (if (file-exists-p lombok-jar)
        (message "✓ Lombok JAR found: %s" (file-name-nondirectory lombok-jar))
      (message "✗ Lombok JAR missing: %s" lombok-jar))

    (if (file-exists-p debug-jar)
        (message "✓ Debug Plugin JAR found: %s" (file-name-nondirectory debug-jar))
      (message "✗ Debug Plugin JAR missing: %s" debug-jar)))

  ;; Check eglot-server-programs configuration
  (when (require 'eglot nil t)
    (let ((java-config (assoc 'java-mode eglot-server-programs))
          (java-ts-config (assoc 'java-ts-mode eglot-server-programs)))
      (if (and java-config java-ts-config)
          (message "✓ Eglot server programs configured for java-mode and java-ts-mode")
        (message "✗ Eglot server programs not properly configured"))))

  (message "\n=== Configuration Summary ===")
  (message "To test Lombok @Data annotations:")
  (message "1. Open a Java file with @Data annotations")
  (message "2. Eglot should start automatically")
  (message "3. Try autocompleting getter/setter methods")
  (message "4. Check that @Data annotation has no errors"))

(provide 'verify-eglot-config)
;;; verify-eglot-config.el ends here