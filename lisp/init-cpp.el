;;; init-cpp.el --- Support for working with C/C++ -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'cmake-mode)

;; (require-package 'eglot)
;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)


;; (require-package 'ccls)
;; (setq ccls-executable "/opt/homebrew/bin/ccls")


;; (setq package-selected-packages-for-cpp '(lsp-mode yasnippet lsp-treemacs helm-lsp
;;                                                    projectile hydra flycheck company avy which-key helm-xref dap-mode))
;; (when (cl-find-if-not #'package-installed-p package-selected-packages-for-cpp)
;;   (package-refresh-contents)
;;   (mapc #'package-install package-selected-packages-for-cpp))

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
;; (helm-mode)
;; (require 'helm-xref)
;; (define-key global-map [remap find-file] #'helm-find-files)
;; (define-key global-map [remap execute-extended-command] #'helm-M-x)
;; (define-key global-map [remap switch-to-buffer] #'helm-mini)

;; (which-key-mode)
;; (add-hook 'c-mode-hook 'lsp)
;; (add-hook 'c++-mode-hook 'lsp)

;; (setq gc-cons-threshold (* 100 1024 1024)
;;       read-process-output-max (* 1024 1024)
;;       treemacs-space-between-root-nodes nil
;;       company-idle-delay 0.0
;;       company-minimum-prefix-length 1
;;       lsp-idle-delay 0.1)  ;; clangd is fast

;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;;   (require 'dap-cpptools)
;;   (yas-global-mode))


(provide 'init-cpp)
;;; init-php.el ends here
