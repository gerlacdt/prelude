;; This is a copy of the prelude python module but uses lsp-mode
;; instead of anaconda-mode. Requires python3 installed.
;;
;; requires the following packages:
;; npm i -g pyright
;; pip install ruff
;; pip install ipython

(require 'electric)

;; Copy pasted from ruby-mode.el
(defun prelude-python--encoding-comment-required-p ()
  (re-search-forward "[^\0-\177]" nil t))

(defun prelude-python--detect-encoding ()
  (let ((coding-system
         (or save-buffer-coding-system
             buffer-file-coding-system)))
    (if coding-system
        (symbol-name
         (or (coding-system-get coding-system 'mime-charset)
             (coding-system-change-eol-conversion coding-system nil)))
      "ascii-8bit")))

(defun prelude-python--insert-coding-comment (encoding)
  (let ((newlines (if (looking-at "^\\s *$") "\n" "\n\n")))
    (insert (format "# coding: %s" encoding) newlines)))

(defun prelude-python-mode-set-encoding ()
  "Insert a magic comment header with the proper encoding if necessary."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (prelude-python--encoding-comment-required-p)
      (goto-char (point-min))
      (let ((coding-system (prelude-python--detect-encoding)))
        (when coding-system
          (if (looking-at "^#!") (beginning-of-line 2))
          (cond ((looking-at "\\s *#\\s *.*\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)")
                 ;; update existing encoding comment if necessary
                 (unless (string= (match-string 2) coding-system)
                   (goto-char (match-beginning 2))
                   (delete-region (point) (match-end 2))
                   (insert coding-system)))
                ((looking-at "\\s *#.*coding\\s *[:=]"))
                (t (prelude-python--insert-coding-comment coding-system)))
          (when (buffer-modified-p)
            (basic-save-buffer-1)))))))

(defun prelude-python-mode-defaults ()
  "Defaults for Python programming."
  (subword-mode +1)
  (setq-local electric-layout-rules
              '((?: . (lambda ()
                        (and (zerop (first (syntax-ppss)))
                             (python-info-statement-starts-block-p)
                             'after)))))
  (when (fboundp #'python-imenu-create-flat-index)
    (setq-local imenu-create-index-function
                #'python-imenu-create-flat-index))
  (add-hook 'post-self-insert-hook
            #'electric-layout-post-self-insert-function nil 'local)
  (add-hook 'after-save-hook 'eglot-format)
  (add-hook 'after-save-hook 'prelude-python-mode-set-encoding nil 'local))

(setq prelude-python-mode-hook 'prelude-python-mode-defaults)
(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'prelude-python-mode-hook)))

(add-hook 'python-mode-hook 'eglot-ensure)

;; set ipython as default shell
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")
(add-hook 'inferior-python-mode-hook
          #'(lambda ()
             (smartparens-mode)))


(setq flycheck-python-pycompile-executable "python3")
