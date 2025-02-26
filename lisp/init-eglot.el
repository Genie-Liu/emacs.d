;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when (maybe-require-package 'eglot)
  (maybe-require-package 'consult-eglot)
  )

(require 'eglot)

(add-to-list 'eglot-server-programs '(racket-mode . ("racket" "-l" "racket-langserver")))
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-to-list 'eglot-server-programs '((python-mode) "pylsp"))

;; (require 'init-metal)
;; (add-to-list 'eglot-server-programs '((scala-mode) . "metals-emacs"))
;; (add-hook 'scala-mode-hook 'eglot-ensure)

(provide 'init-eglot)
;;; init-eglot.el ends here
