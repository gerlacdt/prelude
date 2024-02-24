(require 'prelude-programming)

;;; prerequisites: gem install solargraph rubocop

(prelude-require-packages '(inf-ruby
                            yari
                            tree-sitter
                            tree-sitter-langs))

(require 'tree-sitter)
(require 'tree-sitter-langs)

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

;; Map yari to C-h R
(define-key 'help-command (kbd "R") 'yari)

(with-eval-after-load 'ruby-mode
  (add-hook 'ruby-mode-hook #'tree-sitter-mode)
  (add-hook 'ruby-mode-hook #'tree-sitter-hl-mode)

  (defun prelude-ruby-mode-defaults ()
    ;; Don't auto-insert encoding comments
    ;; Those are almost never needed in Ruby 2+
    (setq ruby-insert-encoding-magic-comment nil)
    (inf-ruby-minor-mode +1)
    ;; CamelCase aware editing operations
    (subword-mode +1)

    (add-hook 'before-save-hook #'eglot-format-buffer t t))

  (setq prelude-ruby-mode-hook 'prelude-ruby-mode-defaults)
  (add-hook 'ruby-mode-hook (lambda ()
                              (run-hooks 'prelude-ruby-mode-hook)))

  (add-hook 'ruby-mode-hook 'eglot-ensure))

(provide 'prelude-ruby)
;;; prelude-ruby.el ends here
