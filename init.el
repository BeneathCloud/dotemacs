;;-*- lexical-binding: t -*-

;; TODO
;; 1. remove useless packages
;; 2. remove system tool dependencies like Rust, ripgrep
;;    - add a new backend for Snails.el using selectrum for fuzzing search
;; 3. make edwina work with treemacs


;; straght.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package nano-layout
  ;; :disabled t
  :straight (nano-emacs :type git :host github :repo "rougier/nano-emacs"
                        :fork (:host github
                                     :repo "BeneathCloud/nano-emacs")
                        :no-byte-compile t)
  :demand t
  :init
  ;; Theming Command line options (this will cancel warning messages)
  (add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
  (add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
  (add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
  (add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
  (add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
  (add-to-list 'command-switch-alist '("-compact" . (lambda (args))))
  :config
  (cond
   ((member "-default" command-line-args) t)
   ((member "-dark" command-line-args) (require 'nano-theme-dark))
   (t (require 'nano-theme-light)))
  (require 'nano-theme-dark)
  (require 'nano-layout)
  (require 'nano-base-colors)
  (require 'nano-faces)
  (nano-faces)
  (require 'nano-theme)
  (nano-theme)
  ;; Nano session saving (optional)
  (require 'nano-session)
  ;; Nano header & mode lines (optional)
  (require 'nano-modeline)
  ;; Nano key bindings modification (optional)
  ;; (require 'nano-bindings)
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
  (defun nano-theme-light ()
    (interactive)
    (nano-theme-set-light)
    (nano-faces)
    (nano-theme))
  (defun nano-theme-night ()
    (interactive)
    (nano-theme-set-dark)
    (nano-faces)
    (nano-theme)))

(global-visual-line-mode)
(electric-pair-mode)
(blink-cursor-mode -1)
(delete-selection-mode nil)
;; (desktop-save-mode nil)
(setq ring-bell-function 'ignore)
;; (setq confirm-kill-processes nil)
;; indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq mac-option-key-is-meta t
      mac-command-key-is-meta nil
      mac-command-modifier　'super
      mac-option-modifier 'meta
      mac-use-title-bar nil)

(use-package general
  :after evil
  :config
  ;; for leader
  (general-define-key
   :keymaps 'evil-insert-state-map
   (general-chord "jk") 'evil-normal-state
   (general-chord "kj") 'evil-normal-state)
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'override
   "s-o" 'delete-other-windows
   "s-a" 'mark-whole-buffer
   "s-v" 'yank
   "s-c" 'kill-ring-save
   "s-s" 'save-buffer
   "s-w" 'delete-window
   "s-W" 'kill-current-buffer
   "s-z" 'undo-fu-only-undo
   "s-Z" 'undo-fu-only-redo
   "s-q" 'save-buffers-kill-terminal
   "s-=" 'text-scale-increase
   "s--" 'text-scale-decrease
   "s-0" 'text-scale-set)
  (general-define-key
   :prefix "SPC"
   :states '(normal visual)
   :keymaps 'override
   "'s" 'consult-register-store
   "'l" 'consult-register-load
   "''" 'consult-register
   "e" 'consult-flycheck
   "E" 'flycheck-list-errors
   "k" 'consult-focus-lines
   "K" 'consult-focus-lines-quit
   "a" 'iedit-mode
   "s" 'consult-isearch
   "L" 'consult-ripgrep
   "l" 'consult-line
   "o" 'consult-outline
   "i" 'consult-imenu
   "I" 'lsp-ui-imenu
   "SPC" 'projectile-find-file
   "x" 'execute-extended-command
   "g" 'magit
   "pp" 'projectile-switch-project
   "pd" 'projectile-remove-known-project
   "pa" 'projectile-add-known-project
   "pf" 'projectile-find-file
   "f" 'find-file
   ;; "s" 'save-buffer
   "r" 'consult-recent-file
   "d" 'dired
   "D" 'ranger
   "b" 'switch-to-buffer
   "S" 'im/search-rg+
   "O" 'olivetti-mode
   "`" (lambda () (interactive) (switch-to-buffer (other-buffer (current-buffer) 1))))
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
   "M-SPC" 'consult-buffer)
  (general-define-key
   :states '(emacs)
   :keymaps 'snails-mode-map
   "C-j" 'snails-select-next-backend
   "C-k" 'snails-select-prev-backend
)
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'eyebrowse-mode-map
   "s-1" 'eyebrowse-switch-to-window-config-1
   "s-2" 'eyebrowse-switch-to-window-config-2
   "s-3" 'eyebrowse-switch-to-window-config-3
   "s-4" 'eyebrowse-switch-to-window-config-4
   "s-5" 'eyebrowse-switch-to-window-config-5
   "s-<up>" 'eyebrowse-close-window-config
   "s-<down>" 'eyebrowse-rename-window-config
   "s-<left>" 'eyebrowse-prev-window-config
   "s-<right>" 'eyebrowse-next-window-config)
  )

 (use-package evil
   :init
   (setq evil-undo-system 'undo-fu)
   (setq evil-want-keybinding nil)
   (setq evil-want-C-u-scroll t)
   :config
   (evil-global-set-key 'insert (kbd "C-n") 'next-line)
   (evil-global-set-key 'insert (kbd "C-p") 'previous-line)
   (evil-global-set-key 'normal (kbd "C-n") 'evil-next-visual-line)
   (evil-global-set-key 'normal (kbd "C-p") 'evil-previous-visual-line)
   (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package avy
  :bind
  ("C-;" . avy-goto-char-2))

(use-package selectrum
  :config
  (selectrum-mode 1))

(use-package selectrum-prescient
  :after selectrum
  :config
  (selectrum-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package which-key
  :config
  (which-key-mode)
  ;; (which-key-setup-minibuffer)
  )

(use-package recentf
  :init
  (defun recentf-open-files+ ()
    "Use `completing-read' to open a recent file."
    (interactive)
    (let ((files (mapcar 'abbreviate-file-name recentf-list)))
      (find-file (completing-read "Find recent file: " files nil t))))
  :config
  (recentf-mode t))

(use-package key-chord
  :config
  (key-chord-mode 1))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("GOPATH" "NPMBIN" "LC_ALL" "LANG"
     "LC_TYPE" "SSH_AGENT_PID" "SSH_AUTH_SOCK" "SHELL"
     "JAVA_HOME" "CLASSPATH")))

(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

(use-package olivetti
  :init
  (setq olivetti-body-width 80))

(use-package lsp-mode
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  :hook
  ((java-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :init
  (setq lsp-ui-doc-enable nil)
  ;; (setq lsp-ui-show-hover t)
  (setq lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions nil)
  )

(use-package lsp-java)

(use-package company
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
  ;; :disabled t
  :init
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (setq vterm-kill-buffer-on-exit t))

(use-package paren
  :config
  (show-paren-mode 1))

(use-package magit)

(use-package org
  :init
  ;; org <s
  (require 'org-tempo)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t))

(use-package swift-mode)

(use-package sml-mode)

(use-package haskell-mode
 :config
 (electric-pair-local-mode -1))

(use-package lsp-haskell
  ;; :disabled t
 :config
 (add-hook 'haskell-mode-hook #'lsp)
 (add-hook 'haskell-literate-mode-hook #'lsp))

;; !need to install ripgrep command line tool in your system
(use-package snails
  ;; :disabled t
  :straight (snails :type git :host github :repo "manateelazycat/snails" :no-byte-compile t)
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
        '(snails-backend-buffer
          snails-backend-recentf
          snails-backend-projectile)))
;; fuzzy search dependency for Snails
;; !needs to install Rust on your system
(use-package fuz
  :straight (fuz :type git :host github :repo "rustify-emacs/fuz.el")
  :config
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod)))

(use-package edwina
  ;; :disabled t
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-setup-dwm-keys 'super)
  (edwina-mode 1))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package eyebrowse
  :demand t
  ;; :disabled t
  :custom
  (eyebrowse-wrap-around t)
  :hook
  ((eyebrowse-post-window-switch . get-eyebrowse-status)
   (eyebrowse-post-window-delete . get-eyebrowse-status))
  :config
  (defun get-eyebrowse-status ()
    (interactive)
    (message (eyebrowse-mode-line-indicator)))
  (eyebrowse-mode))

(use-package dired+
  :disabled t)

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mac-command-modifier 'super))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package all-the-icons)

(use-package treemacs
  ;; :demand t
  :disabled t
  )

(use-package treemacs-icons-dired
  :disabled t
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package perspective
  :disabled t
  :config
  (persp-mode))

(use-package ranger
  :init
  (ranger-override-dired-mode t)
  (setq ranger-parent-depth 0)
  (setq ranger-show-hidden t)
  )

(use-package all-the-icons-dired
  :hook
  ((dired-mode . all-the-icons-dired-mode)
   (deer-mode . all-the-icons-dired-mode)))

(use-package projectile
  :init
  (setq projectile-auto-discover nil)
  (setq projectile-project-root-functions
        '(projectile-root-local
          projectile-root-top-down
          projectile-root-bottom-up
          projectile-root-top-down-recurring
          ))
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

(use-package mini-frame
  :config
  (setq mini-frame-resize t)
  ;; (setq mini-frame-internal-border-color nano-color-subtle)
  (mini-frame-mode +1)
  (custom-set-variables `(mini-frame-internal-border-color ,nano-color-subtle))
  (custom-set-variables
 `(mini-frame-show-parameters
   `((top . 0.2)
     (width . 0.6)
     (left . 0.5)
     (background-color . ,nano-color-background)
     ;; (background-color . ,nano-color-subtle) 
     ;; (foreground-color . "#D08770")
     ;; (border-color . "#677691")
     ;; (height . 16)
     )))
  ;; workaround for not showing candidates if no typed characters, should be fixed in Emacs 27.2
  (define-advice fit-frame-to-buffer (:around (f &rest args) dont-skip-ws-for-mini-frame)
  (cl-letf* ((orig (symbol-function #'window-text-pixel-size))
             ((symbol-function #'window-text-pixel-size)
              (lambda (win from to &rest args)
                (apply orig
                       (append (list win from 
                                     (if (and (window-minibuffer-p win)
                                              (frame-root-window-p win)
                                              (eq t to))
                                         nil
                                       to))
                               args)))))
    (apply f args))))

(use-package consult
  :init
  (defun consult-focus-lines-quit ()
    (interactive)
    (consult-focus-lines -1))
  :bind
  ("M-'" . consult-register-store)
  :config
  (setq consult-project-root-function #'projectile-project-root)
  (defun find-fd (&optional dir initial)
    (interactive "P")
    (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
      (consult-find dir initial))))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("C-M-a" . marginalia-cycle)
         ;; When using the Embark package, you can bind `marginalia-cycle' as an Embark action!
         ;;:map embark-general-map
         ;;     ("A" . marginalia-cycle)
        )

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle' to
  ;; switch between the annotators.
  ;; (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
)

(use-package embark
  :ensure t
  :bind
  ("C-S-a" . embark-act))             

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
  ("C--" . er/contract-region)))

(use-package iedit)

(use-package flycheck
  :hook
  ((prog-mode . flycheck-mode)
   (emacs-lisp-mode . (lambda () (flycheck-mode -1)))))

(use-package consult-flycheck
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))

(use-package flycheck-pos-tip
  :disabled t
  :after
  flycheck)

(use-package flycheck-inline
  :disabled t
  :after
  flycheck)

(use-package undo-fu)
