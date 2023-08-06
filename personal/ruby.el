;;; prerequisites: gem install solargraph rubocop
(prelude-require-packages '(inf-ruby
                            yari
                            rubocop))


;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

;; Map yari to C-h R
(define-key 'help-command (kbd "R") 'yari)

(setq rubocop-autocorrect-on-save t)

(with-eval-after-load 'ruby-mode
  (add-hook 'ruby-mode-hook #'tree-sitter-mode)
  (add-hook 'ruby-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'ruby-mode-hook #'rubocop-mode)

  (defun prelude-ruby-mode-defaults ()
    ;; Don't auto-insert encoding comments
    ;; Those are almost never needed in Ruby 2+
    (setq ruby-insert-encoding-magic-comment nil)
    (inf-ruby-minor-mode +1)
    ;; CamelCase aware editing operations
    (subword-mode +1))

  (setq prelude-ruby-mode-hook 'prelude-ruby-mode-defaults)
  (add-hook 'ruby-mode-hook (lambda ()
                              (run-hooks 'prelude-ruby-mode-hook)))

  (add-hook 'ruby-mode-hook 'eglot-ensure))

(provide 'prelude-ruby)
;;; prelude-ruby.el ends here
