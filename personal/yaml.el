(require 'prelude-programming)

;;; prerequisites: yaml-language-server
;;; npm install -g yaml-language-server

(prelude-require-packages '(yaml-mode))


(defun prelude-yaml-mode-defaults ()
  ;; enable lsp
  (lsp)

  ;; format buffer
  (add-hook 'before-save-hook #'lsp-format-buffer t t))

(setq prelude-yaml-mode-hook 'prelude-yaml-mode-defaults)
(add-hook 'yaml-mode-hook (lambda ()
                            (run-hooks 'prelude-yaml-mode-hook)))

(provide 'prelude-yaml)
;;; prelude-yaml.el ends here
