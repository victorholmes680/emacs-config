;;; init-dape.el --- Debug Adapter Protocol support -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for DAPE (Debug Adapter Protocol for Emacs)
;; with support for Java debugging via jdtls

;;; Code:

(when (maybe-require-package 'dape)

  ;; Enable dape mode for debugging
  (with-eval-after-load 'dape
    ;; Key bindings for dape
    (define-key dape-mode-map (kbd "C-c C-d C-c") 'dape-continue)
    (define-key dape-mode-map (kbd "C-c C-d C-b") 'dape-toggle-breakpoint)
    (define-key dape-mode-map (kbd "C-c C-d C-n") 'dape-next)
    (define-key dape-mode-map (kbd "C-c C-d C-s") 'dape-step-in)
    (define-key dape-mode-map (kbd "C-c C-d C-o") 'dape-step-out)
    (define-key dape-mode-map (kbd "C-c C-d C-r") 'dape-restart)
    (define-key dape-mode-map (kbd "C-c C-d C-q") 'dape-disconnect-quit)
    (define-key dape-mode-map (kbd "C-c C-d C-l") 'dape-locals)
    (define-key dape-mode-map (kbd "C-c C-d C-e") 'dape-evaluate-expression)

    ;; Ensure dape works with eglot/jdtls
    (setq dape-debug-template "java")

    ;; Java debug configuration template
    (dape-register-debug-provider
     "java"
     (lambda (config)
       (let* ((project-root (or (locate-dominating-file default-directory "pom.xml")
                                (locate-dominating-file default-directory "build.gradle")
                                (locate-dominating-file default-directory "build.gradle.kts")
                                default-directory))
              (project-name (file-name-nondirectory (directory-file-name project-root))))
         `(:type "java"
           :name "Java Debug"
           :request "launch"
           :mainClass ,(or (dape-config-get config 'mainClass)
                          (completing-read "Main class: "
                                          (dape--java-get-main-classes)))
           :projectName ,(or (dape-config-get config 'projectName) project-name)
           :args ,(or (dape-config-get config 'args) "")
           :vmArgs ,(or (dape-config-get config 'vmArgs) "")
           :cwd ,(or (dape-config-get config 'cwd) project-root)
           :env ,(dape-config-get config 'env)
           :console ,(or (dape-config-get config 'console) "internalTerminal")
           :stopOnEntry ,(dape-config-get config 'stopOnEntry)
           :internalConsoleOptions ,(dape-config-get config 'internalConsoleOptions)))))

    ;; Helper function to find main classes in project
    (defun dape--java-get-main-classes ()
      "Find main classes in the current Java project."
      (let ((project-root (or (locate-dominating-file default-directory "pom.xml")
                             (locate-dominating-file default-directory "build.gradle")
                             (locate-dominating-file default-directory "build.gradle.kts")
                             default-directory)))
        (when project-root
          (let ((main-classes '()))
            ;; Search for files containing main method
            (when (file-exists-p project-root)
              (dolist (file (directory-files-recursively project-root "\\.java$"))
                (when (with-temp-buffer
                        (insert-file-contents file)
                        (goto-char (point-min))
                        ;; Look for standard main method and handle Lombok generated code
                        (or (re-search-forward "public static void main" nil t)
                            (and (re-search-forward "@.*?Main" nil t)
                                 (re-search-forward "public static void main" nil t))))
                  (let ((class-name (dape--java-file-to-class-name file project-root)))
                    (when class-name
                      (push class-name main-classes))))))
            (nreverse main-classes)))))

    ;; Helper function to convert file path to class name
    (defun dape--java-file-to-class-name (file project-root)
      "Convert Java FILE path to class name relative to PROJECT_ROOT."
      (when (and file project-root)
        (let ((relative-path (file-relative-name file project-root)))
          (when (string-suffix-p ".java" relative-path)
            (let* ((java-file (substring relative-path 0 -5)) ; remove .java
                   (class-name (replace-regexp-in-string "/" "." java-file)))
              class-name)))))

    ;; Java debugging commands
    (defun dape-java-debug-run ()
      "Start debugging current Java project."
      (interactive)
      (let* ((project-root (or (locate-dominating-file default-directory "pom.xml")
                              (locate-dominating-file default-directory "build.gradle")
                              (locate-dominating-file default-directory "build.gradle.kts")
                              default-directory))
             (main-class (completing-read "Main class: " (dape--java-get-main-classes)))
             (project-name (file-name-nondirectory (directory-file-name project-root))))
        (dape `(:type "java"
                 :name "Java Debug"
                 :request "launch"
                 :mainClass ,main-class
                 :projectName ,project-name
                 :args ""
                 :vmArgs ""
                 :cwd ,project-root))))

    ;; Auto-configure breakpoints for Java
    (add-hook 'java-mode-hook
              (lambda ()
                (setq-local dape-breakpoint-same-line nil)
                ;; Performance improvements for Java debugging
                (setq-local dape-buffer-auto-kill t
                           dape-info-variable-min-length 0
                           dape-info-variable-max-length 30
                           dape-stack-trace-frames 20))))

  ;; Enable dape for Java buffers
  (add-hook 'java-mode-hook 'dape-mode)
  (add-hook 'java-ts-mode-hook 'dape-mode))

(provide 'init-dape)
;;; init-dape.el ends here