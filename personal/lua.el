(require 'prelude-programming)

;;; prerequisites: lua-language-server
;;; https://github.com/LuaLS/lua-language-server
;;; create bash wrapper script https://github.com/LuaLS/lua-language-server/wiki/Getting-Started#command-line

(prelude-require-packages '(lua-mode
                            tree-sitter
                            tree-sitter-langs))


(require 'tree-sitter)
(require 'tree-sitter-langs)
(require 'myeglot)


(defun prelude-lua-mode-defaults ()
  ;; format buffer
  (add-hook 'before-save-hook #'eglot-format-buffer t t)

  ;; CamelCase aware editing operations
  (subword-mode +1))

(setq prelude-lua-mode-hook 'prelude-lua-mode-defaults)
(add-hook 'lua-mode-hook (lambda ()
                           (run-hooks 'prelude-lua-mode-hook)))

(add-hook 'lua-mode-hook 'eglot-ensure)

(provide 'prelude-lua)
;;; prelude-lua.el ends here
