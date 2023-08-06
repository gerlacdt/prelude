;; This is a copy of the prelude cpp-module but uses lsp-mode.
;; instead of anaconda-mode. Requires clang installed.
;;
;; requires the following packages:
;; > sudo apt-get install clang clangd-12 clang-format clang-tidy clang-tools
;;
;; for clang formatting to work, create a clang-format file in project folder:
;; > clang-format -style=llvm -dump-config > .clang-format


;; c, cpp hooks
(defun prelude-c-mode-common-defaults ()
  (add-hook 'before-save-hook #'eglot-format-buffer t t))

(setq prelude-c-mode-common-hook 'prelude-c-mode-common-defaults)

(add-hook 'c-mode-hook (lambda ()
                         (run-hooks 'prelude-c-mode-common-hook)))

(add-hook 'c++-mode-hook (lambda ()
                         (run-hooks 'prelude-c-mode-common-hook)))

;; makefile hooks
(defun prelude-makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t ))

(setq prelude-makefile-mode-hook 'prelude-makefile-mode-defaults)

(add-hook 'makefile-mode-hook (lambda ()
                                (run-hooks 'prelude-makefile-mode-hook)))

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
