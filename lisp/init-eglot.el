;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when (maybe-require-package 'eglot)
  (maybe-require-package 'consult-eglot)
  
  (with-eval-after-load 'eglot
    (defun my/jdtls-command (&optional interactive)
      (let* ((project-root (or (locate-dominating-file default-directory "pom.xml")
                               (locate-dominating-file default-directory "build.gradle")
                               (locate-dominating-file default-directory "build.gradle.kts")
                               (locate-dominating-file default-directory ".project")
                               default-directory))
             (project-name (file-name-nondirectory (directory-file-name project-root)))
             (workspace-dir (expand-file-name (concat "~/.cache/jdtls-workspace/" project-name)))
             (jdtls-home "/opt/homebrew/Cellar/jdtls/1.47.0/libexec")
             (jar (car (file-expand-wildcards (concat jdtls-home "/plugins/org.eclipse.equinox.launcher_*.jar"))))
             (config (concat jdtls-home "/config_mac")))
        (list "java"
              "-Declipse.application=org.eclipse.jdt.ls.core.id1"
              "-Dosgi.bundles.defaultStartLevel=4"
              "-Declipse.product=org.eclipse.jdt.ls.core.product"
              "-Xmx1G"
              "--add-modules=ALL-SYSTEM"
              "--add-opens" "java.base/java.util=ALL-UNNAMED"
              "--add-opens" "java.base/java.lang=ALL-UNNAMED"
              "-jar" jar
              "-configuration" config
              "-data" workspace-dir)))
    
    (add-to-list 'eglot-server-programs '(java-mode . my/jdtls-command)))
  
  (add-hook 'java-mode-hook 'eglot-ensure))

(provide 'init-eglot)
;;; init-eglot.el ends here