(prelude-require-packages '(go-mode))

(require 'go-projectile)

;; Ignore go test -c output files
(add-to-list 'completion-ignored-extensions ".test")

(with-eval-after-load 'go-mode

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
  (add-hook 'go-mode-hook (lambda ()
                            (run-hooks 'prelude-go-mode-hook)))

  (add-hook 'go-mode-hook 'eglot-ensure))
