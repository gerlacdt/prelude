(require 'prelude-programming)

;;; prerequisites: yaml-language-server
;;; npm install -g yaml-language-server

(prelude-require-packages '(yaml-mode))



(with-eval-after-load 'yaml-mode
  (defun prelude-yaml-mode-defaults ()
    ;; some config
    )

  (add-hook 'yaml-mode-hook 'whitespace-mode)
  (add-hook 'yaml-mode-hook 'subword-mode)
  (add-hook 'yaml-mode-hook 'lsp)

  (add-hook 'yaml-mode-hook
            (lambda () (add-hook 'before-save-hook 'prelude-cleanup-maybe nil t)))

  (setq prelude-yaml-mode-hook 'prelude-yaml-mode-defaults)
  (add-hook 'yaml-mode-hook (lambda ()
                              (run-hooks 'prelude-yaml-mode-hook)))

  )

;; yaml-mode doesn't derive from prog-mode, but we can at least enable
;; whitespace-mode and apply cleanup.

(provide 'prelude-yaml)
;;; prelude-yaml.el ends here
