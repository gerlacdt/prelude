(require 'prelude-programming)

;;; prerequisites: lua-language-server
;;; https://github.com/LuaLS/lua-language-server
;;; create bash wrapper script https://github.com/LuaLS/lua-language-server/wiki/Getting-Started#command-line

(prelude-require-packages '(lua-mode
                            tree-sitter
                            tree-sitter-langs))


(require 'tree-sitter)
(require 'tree-sitter-langs)

(setq lsp-clients-lua-language-server-bin "~/.local/bin/lua-language-server")

(defun prelude-lua-mode-defaults ()
  ;; enable lsp
  (lsp)

  ;; format buffer
  (add-hook 'before-save-hook #'lsp-format-buffer t t)

  ;; CamelCase aware editing operations
  (subword-mode +1))

(setq prelude-lua-mode-hook 'prelude-lua-mode-defaults)
(add-hook 'lua-mode-hook (lambda ()
                           (run-hooks 'prelude-lua-mode-hook)))

(provide 'prelude-lua)
;;; prelude-lua.el ends here
