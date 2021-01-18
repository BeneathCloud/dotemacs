;;-*- lexical-binding: t -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(beacon edwina lsp-ui lsp-haskell fuz snail dante haskell-mode sml-mode swift-mode which-key vterm use-package undo-fu selectrum-prescient rg olivetti mini-frame magit lsp-java key-chord general exec-path-from-shell evil-surround evil-commentary evil-collection eglot company-lsp centered-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nano-face-strong ((t (:foreground "#37474F" :weight bold :family "Roboto Mono"))) t))

(setq package-user-dir "~/.emacs.d/packages")
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; <Nano
(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/nano-emacs"))
(require 'nano-layout)
;; Theming Command line options (this will cancel warning messages)
(add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
(add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
(add-to-list 'command-switch-alist '("-compact" . (lambda (args))))

(cond
 ((member "-default" command-line-args) t)
 ((member "-dark" command-line-args) (require 'nano-theme-dark))
 (t (require 'nano-theme-light)))

(require 'nano-theme-dark)

;; Theme
(require 'nano-base-colors)
(require 'nano-faces)
(nano-faces)

(require 'nano-theme)
(nano-theme)

;; Nano default settings (optional)
;; (require 'nano-defaults)

;; Nano session saving (optional)
(require 'nano-session)

;; Nano header & mode lines (optional)
(require 'nano-modeline)

;; Nano key bindings modification (optional)
(require 'nano-bindings)

;; Compact layout (need to be loaded after nano-modeline)
(when (member "-compact" command-line-args)
  (require 'nano-compact))

;; Welcome message (optional)
(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

;; Splash (optional)
(unless (member "-no-splash" command-line-args)
  (require 'nano-splash))
;; Nano>


;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(global-visual-line-mode)
(electric-pair-mode)
(blink-cursor-mode -1)
;; indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)


(use-package general
  :ensure t
  :after evil
  :config
  ;; for leader
  (general-define-key
   :keymaps 'evil-insert-state-map
   (general-chord "jk") 'evil-normal-state
   (general-chord "kj") 'evil-normal-state)
  (general-define-key
   :prefix "SPC"
   :states '(normal visual)
   :keymaps 'override
   "f" 'find-file
   "s" 'save-buffer
   "r" 'recentf-open-files+
   "d" 'dired
   "b" 'switch-to-buffer
   "S" 'im/search-rg+
   "o" 'olivetti-mode
   "`" (lambda () (interactive) (switch-to-buffer (other-buffer (current-buffer) 1))))
  ;; for wm.el
  ;; (general-define-key
  ;;  :states '(normal visual insert emacs)
  ;;  :keymaps 'override
  ;;  "s-<return>" (lambda () (interactive) (wm-push-window) (wm-focus-last) (vterm))
  ;;  "s-w" 'wm-delete-window
  ;;  "M-RET" 'wm-cycle-layout
  ;;  "M-=" 'wm-push-window
  ;;  "M-+" 'wm-insert-window
  ;;  "M--" 'wm-pop-window
  ;;  "M-_" 'wm-delete-next-window
  ;;  "M-0" 'wm-delete-window
  ;;  "M-[" 'wm-move-window-backward
  ;;  "M-]" 'wm-move-window-forward
  ;;  "M-{" 'wm-move-window-to-front
  ;;  "M-}" 'wm-move-window-to-back
  ;;  "M-;" 'wm-focus-previous-window
  ;;  "M-'" 'wm-focus-next-window
  ;;  "M-TAB" 'wm-focus-next-window
  ;;  ;; 需要修改 wm.el，否则切换焦点时无法保持修改后的大小
  ;;  ;; "M-C-j" (lambda () (interactive) (enlarge-window 1))
  ;;  ;; "M-C-k" (lambda () (interactive) (enlarge-window -1))
  ;;  ;; "M-C-h" (lambda () (interactive) (enlarge-window -1 t))
  ;;  ;; "M-C-l" (lambda () (interactive) (enlarge-window 1 t))
  ;;  "M-1" (lambda () (interactive) (wm-focus-window 0))
  ;;  "M-2" (lambda () (interactive) (wm-focus-window 1))
  ;;  "M-3" (lambda () (interactive) (wm-focus-window 2))
  ;;  "M-4" (lambda () (interactive) (wm-focus-window 3))
  ;;  "M-5" (lambda () (interactive) (wm-focus-window 4))
  ;;  "M-6" (lambda () (interactive) (wm-focus-window 5))
  ;;  "M-7" (lambda () (interactive) (wm-focus-window 6))
  ;;  "M-8" (lambda () (interactive) (wm-focus-window 7))
  ;;  "M-9" (lambda () (interactive) (wm-focus-window 8))
  ;;  "M-s-1" (lambda () (interactive) (wm-switch-workspace 0))
  ;;  "M-s-2" (lambda () (interactive) (wm-switch-workspace 1))
  ;;  "M-s-3" (lambda () (interactive) (wm-switch-workspace 2))
  ;;  "M-s-4" (lambda () (interactive) (wm-switch-workspace 3))
  ;;  )
  ;; for other
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'override
   "C-a" 'beginning-of-visual-line
   "C-e" 'end-of-visual-line
   )

  (general-define-key
   :states '(insert emacs)
   :keymaps 'override
   "C-u" (lambda () (interactive) (kill-line 0)))

  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'xwidget-webkit-mode-map
   "s-c" 'xwidget-webkit-copy-selection-as-kill)
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'override
   "M-SPC" 'snails)
  )

 (use-package evil
   :ensure t
   :init
   (setq evil-undo-system 'undo-redo)
   (setq evil-want-keybinding nil)
   (setq evil-want-C-u-scroll t)
   :config
   (evil-global-set-key 'insert (kbd "C-n") 'next-line)
   (evil-global-set-key 'insert (kbd "C-p") 'previous-line)
   (evil-global-set-key 'normal (kbd "C-n") 'evil-next-visual-line)
   (evil-global-set-key 'normal (kbd "C-p") 'evil-previous-visual-line)
   (evil-mode))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

 (use-package evil-commentary
   :ensure t
   :config
   (evil-commentary-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package avy
  :ensure t
  :bind
  ("C-;" . avy-goto-char-2))

(use-package selectrum
  :ensure t
  ;; :disabled t
  :config
  (selectrum-mode 1))

(use-package selectrum-prescient
  :ensure t
  ;; :disabled t
  :after selectrum
  :config
  (selectrum-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package mini-frame
  :disabled
  :ensure t
  :config
  (mini-frame-mode)
  (custom-set-variables
   '(mini-frame-show-parameters
     '((top . 200)
       (width . 0.7)
       (left . 0.5)))))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  ;; (which-key-setup-minibuffer)
  )

(use-package recentf
  :ensure t
  :init
  (defun recentf-open-files+ ()
    "Use `completing-read' to open a recent file."
    (interactive)
    (let ((files (mapcar 'abbreviate-file-name recentf-list)))
      (find-file (completing-read "Find recent file: " files nil t))))
  :config
  (recentf-mode t))

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1))

(use-package exec-path-from-shell
  :ensure t
  ;; :disabled
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("GOPATH" "NPMBIN" "LC_ALL" "LANG"
     "LC_TYPE" "SSH_AGENT_PID" "SSH_AUTH_SOCK" "SHELL"
     "JAVA_HOME" "CLASSPATH")))

(use-package dired
  :init
  (when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil)))

(use-package olivetti
  :ensure t
  :init
  (setq olivetti-body-width 80))

(use-package wm
  :disabled t
  :ensure nil
  :load-path "~/.emacs.d/packages/wm"
  :config
  (wm-mode))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  ((java-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :init
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  :ensure t
  :config)

(use-package lsp-java
  :ensure t)

(use-package company
  :ensure t
  :hook
  (prog-mode . company-mode)
  :bind
  ;; (:map company-active-map
  ;;       ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.0))

(use-package vterm
  :ensure t)

(use-package paren
  :config
  (show-paren-mode 1))

(use-package magit
  :ensure t)

(use-package org
  :init
  ;; org <s
  (require 'org-tempo)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t))

(use-package swift-mode
  :ensure t)

(use-package sml-mode
  :ensure t)

(use-package haskell-mode
  :ensure t
  :config
  (electric-pair-local-mode -1))

(use-package lsp-haskell
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp))

(use-package dante
  :ensure t
  :disabled t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  ;; OR for flymake support:
  ;; (add-hook 'haskell-mode-hook 'flymake-mode)
  ;; (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

  (add-hook 'haskell-mode-hook 'dante-mode)
  )

(use-package snails
  :load-path "packages/snails/"
  ;; :commands (require 'snails)
  :config
  (add-to-list 'evil-emacs-state-modes 'snails-mode)
  (setq snails-prefix-backends
        '((">"
          '(snails-backend-command))
         ("@"
          '(snails-backend-imenu))
         ("#"
          '(snails-backend-current-buffer))
         ("!"
          '(snails-backend-rg))
         ("^"
          '(snails-backend-mdfind))
         ("?"
          '(snails-backend-mdfind snails-backend-projectile snails-backend-fd snails-backend-everything))))
  
  (setq snails-default-backends
        '(snails-backend-buffer snails-backend-recentf snails-backend-directory-files)))
;; (setq snails-show-with-frame nil)

;; fuzzy search dependency for Snails
(use-package fuz
  :load-path "packages/fuz.el/"
  ;; :commands (require 'fuz)
  :config
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod)))

(use-package edwina
  :ensure t
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-setup-dwm-keys 'super)
  (edwina-mode 1))

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))
