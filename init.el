;;-*- lexical-binding: t -*-

;; TODO
;; 1. remove useless packages
;; 2. remove system tool dependencies like Rust, ripgrep
;;    - add a new backend for Snails.el using selectrum for fuzzing search


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
(delete-selection-mode 1)
(desktop-save-mode 1)
;; indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)


(use-package general
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
  ;; :disabled t
  :config
  (selectrum-mode 1))

(use-package selectrum-prescient
  ;; :disabled t
  :after selectrum
  :config
  (selectrum-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package mini-frame
  :disabled
  :config
  (mini-frame-mode)
  (custom-set-variables
   '(mini-frame-show-parameters
     '((top . 200)
       (width . 0.7)
       (left . 0.5)))))

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
  ;; :disabled
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
  :commands lsp
  :hook
  ((java-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :init
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  :config)

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
  :config
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp))

(use-package dante
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

;; !need to install ripgrep command line tool in your system
(use-package snails
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
        '(snails-backend-buffer snails-backend-recentf snails-backend-directory-files)))

;; fuzzy search dependency for Snails
;; !needs to install Rust on your system
(use-package fuz
  :straight (fuz :type git :host github :repo "rustify-emacs/fuz.el")
  :config
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod)))

(use-package edwina
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-setup-dwm-keys 'super)
  (edwina-mode 1))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package eyebrowse
  :demand t
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
