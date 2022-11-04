(prelude-require-packages '(csv-mode
                            dockerfile-mode
                            markdown-mode
                            protobuf-mode
                            terraform-mode
                            rg
                            org-drill
                            prettier-js
                            emojify
                            unicode-fonts))

;; OS specific stuff
(defvar gerlacdt/font "DejaVu Sans Mono-12")

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(when (eq system-type 'gnu/linux)
  ;; do nothing
  )

;; OSX modifier keys and font
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-right-option-modifier nil
        gerlacdt/font "Menlo-16"))

(when (window-system)
  ;; do nothing
  )

(defun ms-windows? ()
  "Check if current OS is Microsoft Windows."
  (or (equal system-type 'cygwin) (equal system-type 'windows-nt)))

(when (ms-windows?)
  ;; flycheck does not work in ms-windows
  (global-flycheck-mode -1))

;; set font after OS-specific logic
(set-default 'default-frame-alist (\` ((font \, gerlacdt/font))))


;; defines alias
(defalias 'qrr 'anzu-query-replace-regexp)
(defalias 'jpp 'json-pretty-print)

;; set some prelude defaults
(setq prelude-guru nil)
(setq prelude-flyspell nil)
(setq which-function-mode nil)

;; hide minor-modes in modeline
(diminish 'which-key-mode)
(diminish 'whitespace-mode)
(diminish 'prelude-mode)
(diminish 'smartparens-mode)

;; general stuff
(set-scroll-bar-mode nil)
(menu-bar-mode -1)
(setq tags-table-list nil)
(setq compile-command "make")
(setq line-number-display-limit-width 1000) ; show line number even if lines are really long
(setq ediff-split-window-function 'split-window-horizontally)


;; find grep
(setq grep-find-command "find . -type f -print0 | xargs -0 grep -inH -e '' ")
(setq grep-command "grep -inH -e '' ")
(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-directories "target")
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories "vendor")))

;; auto backups
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-visited-mode t)
(auto-save-visited-mode +1)
(setq auto-save-visited-interval 60)
(setq create-lockfiles nil)

;;gdb user interface with multiple windows
(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)
(setq compilation-scroll-output t)

;; org-mode
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-reload-all)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

;; need to disable GNOME keybinding in settings -> keyboard
(define-key yas-minor-mode-map (kbd "s-<tab>") #'yas-expand)

(setq org-startup-folded nil)
(setq org-log-done t)
(add-hook 'org-mode-hook (lambda ()
                           (yas-minor-mode)
                           (add-to-list 'org-agenda-files "~/.org")))

;; ivy settings
(global-set-key (kbd "C-c a") 'counsel-rg)
(setq ivy-height 20)

;; helm settings
;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; company-mode
;; aligns annotation to the right hand side
(setq company-dabbrev-downcase nil)
(setq company-idle-delay 0.3)
(setq company-minimum-prefix-length 1)

;; projectile native indexing, much faster on windows
(setq projectile-indexing-method 'alien)

;; disable smartparens keybinding, it clashes with (xref/lsp-ui)-find-references
(add-hook 'smartparens-enabled-hook
          (lambda ()
            (define-key smartparens-mode-map (kbd "M-?") nil)) t)

;; smartparens for protobuf-mode
(add-hook 'protobuf-mode-hook #'smartparens-mode)

;; smartparens for haskell-interactive-mode-hook
(add-hook 'haskell-interactive-mode-hook #'smartparens-mode)

;; terraform
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)


;; GNU Octave
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; store recent files regularly
(run-at-time nil (* 5 60) 'recentf-save-list)

;; configure whitespace-mode, no highlights of too long lines at the end
(setq whitespace-line-column 120)
(setq whitespace-style '(face tabs empty trailing))

;; max chars in one line for auto-fill-mode
(setq-default fill-column 80)


;; enable emojis everywhere
(add-hook 'after-init-hook #'global-emojify-mode)

;; disable super-save-mode globally, because it causes some strange behaviour when auto-format after save
(super-save-mode -1)


;; active prettier format on save for some modes
(setq prettier-js-args '("--trailing-comma" "all"))
(setq prettier-js-command "prettier")
(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'json-mode-hook 'prettier-js-mode)
;; (add-hook 'markdown-mode-hook 'prettier-js-mode)
