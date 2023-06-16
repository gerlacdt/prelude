(require 'prelude-programming)

;;; prerequisites: yaml-language-server
;;; npm install -g yaml-language-server

(prelude-require-packages '(yaml-mode))


(defun prelude-yaml-mode-defaults ()
  ;; enable lsp
  (lsp))

;; yaml-mode doesn't derive from prog-mode, but we can at least enable
;; whitespace-mode and apply cleanup.
(add-hook 'yaml-mode-hook 'whitespace-mode)
(add-hook 'yaml-mode-hook 'subword-mode)
(add-hook 'yaml-mode-hook
          (lambda () (add-hook 'before-save-hook 'prelude-cleanup-maybe nil t)))

(setq prelude-yaml-mode-hook 'prelude-yaml-mode-defaults)
(add-hook 'yaml-mode-hook (lambda ()
                            (run-hooks 'prelude-yaml-mode-hook)))

(provide 'prelude-yaml)
;;; prelude-yaml.el ends here
