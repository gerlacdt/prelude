(require 'prelude-lsp)
(require 'prelude-programming)

(defun prelude-c-mode-common-defaults ()
  (setq c-default-style "k&r"
        c-basic-offset 2)
  (c-set-offset 'substatement-open 0)

  ;; enable lsp server, clangd
  (lsp))

(setq prelude-c-mode-common-hook 'prelude-c-mode-common-defaults)

;; this will affect all modes derived from cc-mode, like
;; java-mode, php-mode, etc
(add-hook 'c-mode-common-hook (lambda ()
                                (run-hooks 'prelude-c-mode-common-hook)))

(defun prelude-makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t ))

(setq prelude-makefile-mode-hook 'prelude-makefile-mode-defaults)

(add-hook 'makefile-mode-hook (lambda ()
                                (run-hooks 'prelude-makefile-mode-hook)))

;; autoformatting before save
(add-hook 'c-mode-common-hook #'clang-format+-mode)

(provide 'prelude-c)
