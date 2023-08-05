(require 'prelude-programming)

;;; prerequisites: yaml-language-server
;;; npm install -g yaml-language-server

(prelude-require-packages '(yaml-mode))

(require 'myeglot)

(with-eval-after-load 'yaml-mode
  (defun prelude-yaml-mode-defaults ()
    ;; CamelCase aware editing operations
    (subword-mode +1)

    ;; yaml-mode doesn't derive from prog-mode, but we can at least enable
    ;; whitespace-mode and apply cleanup.
    (whitespace-mode +1)

    (add-hook 'before-save-hook 'prelude-cleanup-maybe nil t))


  (setq prelude-yaml-mode-hook 'prelude-yaml-mode-defaults)
  (add-hook 'yaml-mode-hook (lambda ()
                              (run-hooks 'prelude-yaml-mode-hook)))

  (add-hook 'yaml-mode-hook 'eglot-ensure))


(provide 'prelude-yaml)
;;; prelude-yaml.el ends here
