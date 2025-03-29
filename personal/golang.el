(require 'go-projectile)

;; Ignore go test -c output files
(add-to-list 'completion-ignored-extensions ".test")
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))

(with-eval-after-load 'go-ts-mode

  (defun prelude-go-mode-defaults ()
    (setq tab-width 4)
    (setq indent-tabs-mode 1)

    ;; stop whitespace being highlighted
    (whitespace-toggle-options '(tabs))

    ;; enable snippets
    (yas-minor-mode)

    ;; CamelCase aware editing operations
    (subword-mode +1)

    (add-hook 'before-save-hook #'eglot-format-buffer t t))

  (setq prelude-go-mode-hook 'prelude-go-mode-defaults)
  (add-hook 'go-ts-mode-hook (lambda ()
                            (run-hooks 'prelude-go-mode-hook)))

  (add-hook 'go-ts-mode-hook 'eglot-ensure))
