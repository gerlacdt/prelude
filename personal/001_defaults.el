(prelude-require-packages '(anzu
                            blacken
                            csv-mode
                            dockerfile-mode
                            keycast
                            markdown-mode
                            protobuf-mode
                            restclient
                            terraform-mode))

;; OS specific stuff
(defvar gerlacdt/font "Inconsolata-16")

(when (eq system-type 'gnu/linux)
  ;; do nothing
  )

;; OSX modifier keys and font
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-right-option-modifier nil
        gerlacdt/font "Menlo-16"))

(when (eq window-system nil)
  ;; needed ubuntu for line highlighting in emacs -nw mode (terminal)
  (set-face-foreground 'highlight nil)
  (set-face-background 'hl-line "#3e4446"))

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

;; disable auto backups
(setq make-backup-files nil)
(setq auto-save-default nil)

;;gdb user interface with multiple windows
(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)
(setq compilation-scroll-output t)

;; org-mode
(setq org-startup-folded nil)
(setq org-log-done t)

;; ivy settings
;; (setq ivy-height 20)
;; override from counsel keymaps
;; (global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)

;; helm settings
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z



;; company-mode
(global-set-key (kbd "C-<tab>") 'company-complete-common-or-cycle)
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
(setq company-dabbrev-downcase nil)
(setq company-show-numbers t)

;; projectile
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; projectile native indexing, much faster on windows
(setq projectile-indexing-method 'alien)


;; smartparens
(setq sp-base-key-bindings 'paredit)
(sp-use-paredit-bindings)

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

;;don't highlight the end of long lines
(setq whitespace-line-column 120)
