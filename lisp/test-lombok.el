;;; test-lombok.el --- Test Lombok configuration with Eglot -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script to verify Lombok @Data annotations work with Eglot + JDTLS

;;; Code:

(defun test-lombok-configuration ()
  "Test Lombok configuration by opening test file and checking Eglot."
  (interactive)
  (let ((test-file "/Users/wangzhixiong/repo/TestLombok.java"))
    (if (file-exists-p test-file)
        (progn
          (message "=== Testing Lombok Configuration ===")
          (find-file test-file)
          (message "Opening test file: %s" test-file)

          ;; Wait for Eglot to start
          (run-with-timer 3 nil #'(lambda ()
                                    (if (bound-and-true-p eglot--managed-mode)
                                        (progn
                                          (message "✓ Eglot is active")
                                          (message "✓ Lombok Java agent should be loaded")
                                          (message "✓ @Data annotations should be recognized")
                                          (message "\nTo verify manually:")
                                          (message "1. Try autocompleting 'test.get' - should see getName(), getAge(), getEmail()")
                                          (message "2. Check if no red squiggles under @Data annotation")
                                          (message "3. Run M-x java-diagnose-setup for full verification"))
                                      (message "✗ Eglot not active. Run M-x eglot to start.")))))
      (message "Test file not found: %s" test-file))))

(provide 'test-lombok)
;;; test-lombok.el ends here