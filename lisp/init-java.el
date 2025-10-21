;;; init-java.el --- Java development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Additional Java development utilities and configuration

;;; Code:

;; Declare functions to avoid circular dependencies
(declare-function eglot-ensure 'init-eglot)
(declare-function dape-mode 'init-dape)

(defcustom java-lombok-jar-path "/Users/wangzhixiong/repo/org/projectlombok/lombok/1.18.34/lombok-1.18.34.jar"
  "Path to lombok JAR file for Java development."
  :type 'file
  :group 'java)

(defcustom java-debug-plugin-jar-path "/Users/wangzhixiong/repo/com/microsoft/java/com.microsoft.java.debug.plugin/0.53.0/com.microsoft.java.debug.plugin-0.53.0.jar"
  "Path to Java debug plugin JAR file."
  :type 'file
  :group 'java)

(defun java-check-dependencies ()
  "Check if required Java dependencies are available."
  (interactive)
  (let ((lombok-exists (file-exists-p (expand-file-name java-lombok-jar-path)))
        (debug-exists (file-exists-p (expand-file-name java-debug-plugin-jar-path))))
    (message "Java Dependencies Check:")
    (message "  Lombok JAR (%s): %s"
             java-lombok-jar-path
             (if lombok-exists "✓ Found" "✗ Missing"))
    (message "  Debug Plugin JAR (%s): %s"
             java-debug-plugin-jar-path
             (if debug-exists "✓ Found" "✗ Missing"))

    (unless (and lombok-exists debug-exists)
      (message "\nTo install missing dependencies:")
      (unless lombok-exists
        (message "  Lombok: Add to pom.xml:")
        (message "    <dependency>")
        (message "      <groupId>org.projectlombok</groupId>")
        (message "      <artifactId>lombok</artifactId>")
        (message "      <version>1.18.34</version>")
        (message "      <scope>provided</scope>")
        (message "    </dependency>"))
      (unless debug-exists
        (message "  Debug Plugin: Add to pom.xml:")
        (message "    <dependency>")
        (message "      <groupId>com.microsoft.java</groupId>")
        (message "      <artifactId>com.microsoft.java.debug.plugin</artifactId>")
        (message "      <version>0.53.0</version>")
        (message "    </dependency>"))
      (message "\nThen run: mvn dependency:resolve"))))

(defun java-setup-project ()
  "Setup Java project with proper configuration."
  (interactive)
  (let ((project-root (or (locate-dominating-file default-directory "pom.xml")
                         (locate-dominating-file default-directory "build.gradle")
                         (locate-dominating-file default-directory "build.gradle.kts")
                         default-directory)))
    (if project-root
        (progn
          (message "Java project detected at: %s" project-root)
          (java-check-dependencies)
          ;; Enable eglot for Java (only if not already enabled)
          (unless (bound-and-true-p eglot--managed-mode)
            (eglot-ensure))
          ;; Enable dape for debugging (only if not already enabled)
          (unless dape-mode
            (dape-mode 1)))
      (message "No Java project detected in current directory."))))

(defun java-run-main-class (main-class &optional args)
  "Run Java MAIN-CLASS with optional ARGS."
  (interactive
   (list (completing-read "Main class: "
                          (when (fboundp 'dape--java-get-main-classes)
                            (dape--java-get-main-classes)))
         (read-string "Arguments: ")))
  (let ((default-directory (or (locate-dominating-file default-directory "pom.xml")
                               (locate-dominating-file default-directory "build.gradle")
                               (locate-dominating-file default-directory "build.gradle.kts")
                               default-directory)))
    (compile (format "java %s %s" (or args "") main-class))))

;; Auto-setup when opening Java files (but only once per buffer)
(defvar java--setup-done nil
  "Track if Java setup has been completed for current buffer.")

(defun java--setup-once ()
  "Setup Java project only once per buffer."
  (unless java--setup-done
    (setq-local java--setup-done t)
    (when (locate-dominating-file default-directory "pom.xml")
      (java-setup-project))))

;; Add server crash monitoring
(add-hook 'eglot-server-connected-hook
          (lambda ()
            (when (derived-mode-p 'java-mode java-ts-mode)
              (message "JDTLS server connected successfully"))))

(add-hook 'eglot-server-shutdown-hook
          (lambda ()
            (when (derived-mode-p 'java-mode java-ts-mode)
              (when (y-or-n-p "JDTLS server disconnected. Restart? ")
                (java-handle-server-crash)))))

(add-hook 'java-mode-hook #'java--setup-once)
(add-hook 'java-ts-mode-hook #'java--setup-once)

;; Enhanced error handling and diagnostics
(defun java-handle-m-plus-error ()
  "Handle M+ related errors in Java debugging/LSP context."
  (interactive)
  (let ((error-msg (current-message)))
    (when (and error-msg (string-match-p "M\\+" error-msg))
      (message "Detected M+ error - this is typically related to Java debugging or LSP operations")
      (when (y-or-n-p "Clear LSP cache and restart eglot to fix M+ errors? ")
        (java-clear-lsp-cache)
        (eglot-shutdown-all)
        (eglot-ensure)))))

(defun java-handle-server-crash ()
  "Handle JDTLS server crashes and provide recovery options."
  (interactive)
  (message "JDTLS server crashed. Attempting recovery...")
  (java-clear-lsp-cache)
  (when (bound-and-true-p eglot--managed-mode)
    (eglot-shutdown))
  (run-with-timer 2 nil #'(lambda ()
                           (condition-case err
                               (eglot-ensure)
                             (error
                              (message "Failed to restart JDTLS: %s" (error-message-string err))
                              (when (y-or-n-p "Try fallback JDTLS configuration? ")
                                (java-use-fallback-jdtls)))))))

(defun java-use-fallback-jdtls ()
  "Use a simplified fallback JDTLS configuration."
  (interactive)
  (let ((fallback-config (lambda ()
                          (list "jdtls"
                                "-data" (expand-file-name "~/.cache/jdtls-fallback")))))
    (message "Using fallback JDTLS configuration...")
    (setf (alist-get 'java-mode eglot-server-programs) fallback-config)
    (setf (alist-get 'java-ts-mode eglot-server-programs) fallback-config)
    (eglot-ensure)))

(defun java-verify-lombok-annotation-processing ()
  "Verify that Lombok annotation processing is working correctly."
  (interactive)
  (let ((lombok-jar (expand-file-name java-lombok-jar-path)))
    (if (file-exists-p lombok-jar)
        (progn
          (message "✓ Lombok JAR found at: %s" lombok-jar)
          (message "✓ Lombok annotation processing should be working")
          (message "  If @Data annotations are still not recognized:")
          (message "  1. Restart Emacs to ensure the Java agent is loaded")
          (message "  2. Check your project's pom.xml for Lombok dependency")
          (message "  3. Run: M-x java-clear-lsp-cache then reopen Java files"))
      (message "✗ Lombok JAR not found: %s" lombok-jar)
      (message "  Install Lombok: Add to pom.xml and run 'mvn dependency:resolve'"))))

(defun java-verify-dape-integration ()
  "Verify DAPE integration with JDTLS is working."
  (interactive)
  (let ((debug-plugin (expand-file-name java-debug-plugin-jar-path)))
    (if (file-exists-p debug-plugin)
        (progn
          (message "✓ Microsoft Java Debug Plugin found at: %s" debug-plugin)
          (message "✓ DAPE debugging should be working")
          (message "  If DAPE is not working:")
          (message "  1. Ensure Eglot is connected (M-x eglot)")
          (message "  2. Check that breakpoints are set correctly")
          (message "  3. Use M-x dape-java-debug-run to start debugging"))
      (message "✗ Microsoft Java Debug Plugin not found: %s" debug-plugin)
      (message "  Install Debug Plugin: Add to pom.xml and run 'mvn dependency:resolve'"))))

(defun java-diagnose-setup ()
  "Run comprehensive diagnostics for Java development setup."
  (interactive)
  (message "=== Java Development Environment Diagnostics ===")
  (java-check-dependencies)
  (java-verify-lombok-annotation-processing)
  (java-verify-dape-integration)
  (let ((eglot-active (bound-and-true-p eglot--managed-mode))
        (dape-active dape-mode))
    (message "\n=== Service Status ===")
    (message "Eglot (LSP): %s" (if eglot-active "✓ Active" "✗ Inactive"))
    (message "DAPE (Debug): %s" (if dape-active "✓ Active" "✗ Inactive"))
    (when (and (not eglot-active) (y-or-n-p "Start Eglot for Java LSP support? "))
      (eglot-ensure))
    (when (and (not dape-active) (y-or-n-p "Enable DAPE for debugging? "))
      (dape-mode 1))))

(defun java-clear-lsp-cache ()
  "Clear Java LSP cache to resolve potential M+ errors."
  (interactive)
  (let ((cache-dir (file-name-concat user-emacs-directory "cache" "lsp-cache")))
    (when (file-exists-p cache-dir)
      (delete-directory cache-dir t)
      (message "Cleared Java LSP cache: %s" cache-dir))))

;; Key bindings for Java development
(with-eval-after-load 'java-mode
  (define-key java-mode-map (kbd "C-c C-j c") 'java-check-dependencies)
  (define-key java-mode-map (kbd "C-c C-j s") 'java-setup-project)
  (define-key java-mode-map (kbd "C-c C-j r") 'java-run-main-class)
  (define-key java-mode-map (kbd "C-c C-j e") 'java-handle-m-plus-error)
  (define-key java-mode-map (kbd "C-c C-j x") 'java-clear-lsp-cache)
  (define-key java-mode-map (kbd "C-c C-j l") 'java-verify-lombok-annotation-processing)
  (define-key java-mode-map (kbd "C-c C-j d") 'java-verify-dape-integration)
  (define-key java-mode-map (kbd "C-c C-j z") 'java-diagnose-setup)
  (define-key java-mode-map (kbd "C-c C-j f") 'java-use-fallback-jdtls)
  (define-key java-mode-map (kbd "C-c C-j h") 'java-handle-server-crash))

(with-eval-after-load 'java-ts-mode
  (define-key java-ts-mode-map (kbd "C-c C-j c") 'java-check-dependencies)
  (define-key java-ts-mode-map (kbd "C-c C-j s") 'java-setup-project)
  (define-key java-ts-mode-map (kbd "C-c C-j r") 'java-run-main-class)
  (define-key java-ts-mode-map (kbd "C-c C-j e") 'java-handle-m-plus-error)
  (define-key java-ts-mode-map (kbd "C-c C-j x") 'java-clear-lsp-cache)
  (define-key java-ts-mode-map (kbd "C-c C-j l") 'java-verify-lombok-annotation-processing)
  (define-key java-ts-mode-map (kbd "C-c C-j d") 'java-verify-dape-integration)
  (define-key java-ts-mode-map (kbd "C-c C-j z") 'java-diagnose-setup)
  (define-key java-ts-mode-map (kbd "C-c C-j f") 'java-use-fallback-jdtls)
  (define-key java-ts-mode-map (kbd "C-c C-j h") 'java-handle-server-crash))

(provide 'init-java)
;;; init-java.el ends here