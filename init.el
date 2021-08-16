;;-*- lexical-binding: t -*-
;; TODO
;; 2. remove system tool dependencies like Rust, ripgrep
;;    - add a new backend for Snails.el using selectrum for fuzzing search
;; 3. make edwina work with treemacs
;; 4. add textbunlde support

(require 'cl)
;; straght.el
(defvar bootstrap-version)
(setq straight-disable-native-compile t)
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
;; -----------------------------------------------------

(use-package emacs
  :demand
  :config

  ;; required libs
  (require 'cl)
  (setq-default with-editor-emacsclient-executable
                "/opt/homebrew/bin/emacsclient")
  ;; emacs server
  (server-start)

  ;; font settings
  ;; (setq my/default-font "pragmatapro mono liga 1.125-20")
  (setq my/default-font "pragmatapro mono liga-20")
  ;; (setq my/default-font "pragmatapro mono liga 1.75-20")
  ;; (setq my/default-font "Monoid HalfTight Retina-16")
  ;; (setq-default line-spacing 0.125)

  ;; trash settings
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")

  (setq frame-resize-pixelwise t)

  (global-visual-line-mode)

  (delete-selection-mode nil)

  (setq ring-bell-function 'ignore)

  ;; indent
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab)

  ;; mac command key and option key
  (when (eq system-type 'darwin)
    (setq mac-option-modifier 'meta
          mac-command-modifier 'super
          mac-option-key-is-meta t))

  ;; fullscreen
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

  (when (eq system-type 'darwin)
    (if (fboundp 'mac-auto-operator-composition-mode)
        (mac-auto-operator-composition-mode))
    ;; default Latin font (e.g. Consolas)
    ;; (set-face-attribute 'default nil :font "sf mono-20")
    (set-face-attribute 'default nil :font my/default-font)
    (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
    ;; (set-face-attribute 'mode-line nil :font "Monaco-14")
    )

  (setq ps-print-header nil) ; 去除 wysiwyg print 的 header （C-u M-x ps-print-buffer-with-faces 打印成ps文件， M-x 直接发送到打印机）

  (defun my/put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

  (defun My/add-org-task-to-reminder ()
    (interactive)
    (when (eq major-mode 'org-mode)
      (let* ((reminder-list-name "Inbox")
            (title (org-element-property
                    :title
                    (org-element-at-point)))
            (command (format "/opt/homebrew/bin/reminders add %s \"%s\" -d today" reminder-list-name title)))
        (shell-command-to-string command))))
  )

(use-package diminish)

(use-package nano-theme
  ;; :disabled t
  :straight (nano-theme :type git :host github
                        :repo "rougier/nano-theme")
  :init
  ;; write a function to do the spacing
  (defun simple-mode-line-render (left right)
    "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
    (let* ((available-width (- (window-total-width)
                               (length left)
                               2
                               (/ (window-right-divider-width)
                                  (window-font-width nil 'default)))))
      (format (format " %%s %%%ds " available-width) left right)))

  (defun setup-mode-line (dol)
    (setq-default mode-line-format
                  '((:eval (simple-mode-line-render
                            ;; left
                            (format-mode-line "%b %* ")
                            ;; (format-mode-line "%b %m %*")
                            ;; right  
                            (format-mode-line "%l:%c ")))))

    (set-face-attribute 'mode-line nil
                        :background (eval (intern (concat "nano-" dol "-subtle")))
                        :foreground (eval (intern (concat "nano-" dol "-foreground")))
                        :box (list :line-width 2 :color (eval (intern (concat "nano-" dol "-faded"))))
                        :overline nil	
                        :underline nil)

    (set-face-attribute 'mode-line-inactive nil
                        :background (eval (intern (concat "nano-" dol "-background")))
                        :foreground (eval (intern (concat "nano-" dol "-foreground")))
                        :box (list :line-width 2 :color (eval (intern (concat "nano-" dol "-subtle"))))
                        :overline nil	
                        :underline nil))
  (defun polish@nano-light (old-fn &rest args)
    (apply old-fn args)
    (setup-mode-line "light")
    (set-face-attribute 'default nil :font "pragmatapro mono liga-20"))
  (defun polish@nano-dark (old-fn &rest args)
    (apply old-fn args)
    (setup-mode-line "dark")
    (set-face-attribute 'default nil :font "pragmatapro mono liga-20"))
  (advice-add 'nano-light :around 'polish@nano-light)
  (advice-add 'nano-dark :around 'polish@nano-dark)
  (nano-light)
  ;; (nano-dark)
  (nano-setup)
  (tool-bar-mode -1)
  )

(use-package nano-modeline
  :disabled t
  :straight (nano-modeline :type git :host github
                           :repo "rougier/nano-modeline")
  :config
  (nano-modeline))

(use-package nano-splash
  :disabled t
  :straight (nano-splash  :type git :host github
                          :repo "rougier/nano-splash")
  :config
  (nano-splash))

(use-package gruvbox-theme
  :disabled t
  :init
  (add-to-list 'load-path "~/.emacs.d/straight/repos/nano-emacs/")
  (require 'nano-layout)
  (load-theme 'gruvbox t))

(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")
(setq frame-resize-pixelwise t)
(global-visual-line-mode)
(blink-cursor-mode -1)
(delete-selection-mode nil)
;; (desktop-save-mode nil)
(setq ring-bell-function 'ignore)
;; (setq confirm-kill-processes nil)
;; indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
;; (setq mac-option-key-is-meta t
;;       mac-command-key-is-meta nil
;;       mac-command-modifier　'super
;;       ;; mac-command-modifier　'hyper
;;       mac-option-modifier 'meta
;;       mac-use-title-bar nil)
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-option-key-is-meta t))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(scroll-bar-mode -1)
;; (add-to-list 'default-frame-alist '(font . "Monaco-18"))
;; (set-face-attribute 'default nil :font "JetBrains Mono-18")
;; (set-face-attribute 'default nil :font "Monaco-18") 
;; (set-face-attribute 'default nil :font "Menlo-16")
(when (eq system-type 'darwin)

  (if (fboundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode))

  ;; default Latin font (e.g. Consolas)
  ;; (set-face-attribute 'default nil :font "sf mono-20")
  (set-face-attribute 'default nil :font "pragmatapro mono liga-20")
  ;; default font size (point * 10)
  ;; 
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly. 
  ;; (set-face-attribute 'default nil :height 180)

  ;; use specific font for Korean charset.
  ;; if you want to use different font size for specific charset,
  ;; add :size POINT-SIZE in the font-spec.
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
  ;; (set-face-attribute 'mode-line nil :font "Monaco-14")
  ;; you may want to add different for other charset in this way.
  )
                                        ;(define-key minibuffer-local-map (kbd "s-v") 'yank)
(setq ps-print-header nil) ; 去除 wysiwyg print 的 header （C-u M-x ps-print-buffer-with-faces 打印成ps文件， M-x 直接发送到打印机）

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
   ;; "s-t" 'vterm-other-window
   ;; "s-t" 'eshell-other-window
   "s-u" 'update-progress-bar-at-point
   "s-y" 'backward-progress-bar-at-point
   "s-i" 'forward-progress-bar-at-point
   ;; "s-[" 'previous-buffer
   ;; "s-]" 'next-buffer
   "s-[" 'winner-undo
   "s-]" 'winner-redo
   "s-f" 'consult-line
   "s-;" 'eval-expression
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
   "s-0" 'text-scale-mode)

  (general-define-key
   :keymaps 'minibuffer-local-map
   "s-v" 'yank
   "C-u" (lambda () (interactive) (kill-line 0))
   )

  (general-define-key
   :prefix "SPC"
   :states '(normal visual)
   :keymaps 'override
   "nb" 'org-roam-buffer-toggle
   "nn" 'org-roam-node-find
   "ni" 'org-roam-node-insert
   "ng" 'org-roam-graph
   "nc" 'org-roam-capture 
   "nj" 'org-roam-dailies-capture-today
   ";" 'eval-expression
   "'s" 'consult-register-store
   "'l" 'consult-register-load
   "''" 'consult-register
   "e" 'consult-flycheck
   "E" 'flycheck-list-errors
   "k" 'consult-focus-lines
   "K" 'consult-focus-lines-quit
   "a" 'iedit-mode
   "A" 'org-agenda
   "s" 'consult-isearch
   "L" 'consult-ripgrep
   "l" 'consult-line
   "o" 'consult-outline
   "i" 'consult-imenu
   "I" 'lsp-ui-imenu
   "SPC" 'consult-buffer
   ;; "SPC" 'projectile-find-file
   "x" 'execute-extended-command
   "g" 'magit
   "pp" 'projectile-switch-project
   "pd" 'projectile-remove-known-project
   "pa" 'projectile-add-known-project
   "pf" 'projectile-find-file
   "f" 'find-file
   "F" (lambda () (interactive) (shell-command (concat "open -R " (buffer-name))))
   "q" (lambda () (interactive) (let ((default-directory "~/Space/Drafts/")) (call-interactively 'find-file)))
   ;; "s" 'save-buffer
   "r" 'consult-register-load
   "R" 'consult-register-store
   "d" 'dired
   "D" (lambda () (interactive) (shell-command "open ."))
   ;; "D" 'ranger
   "b" 'switch-to-buffer
   "S" 'im/search-rg+
   "O" 'olivetti-mode
   "`" (lambda () (interactive) (switch-to-buffer (other-buffer (current-buffer) 1))))
  ;; for other
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'global
   "C-a" 'beginning-of-visual-line
   "C-e" 'end-of-visual-line
   "C-k" 'kill-line
   "C-\\" 'universal-argument
   )

  (general-define-key
   :states '(insert normal emacs visual)
   :keymaps '(lispy-mode-map emacs-lisp-mode-map)
   "M-<return>" 'eval-last-sexp)

  (general-define-key
   :states '(insert emacs)
   :keymaps 'global
   "C-u" (lambda () (interactive) (kill-line 0)))

  (general-define-key
   :state '(insert emacs)
   :keymaps 'vterm-mode-map
   "C-u" 'vterm-send-C-u)

  (general-define-key
   :state '(normal)
   :keymaps 'markdown-mode-map
   "<tab>" 'markdown-cycle)

  (general-define-key
   :state '(insert emacs)
   :keymaps 'vterm-mode-map
   "C-u" 'vterm-send-C-u)
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'xwidget-webkit-mode-map
   "s-c" 'xwidget-webkit-copy-selection-as-kill)
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'override
   "M-SPC" 'consult-buffer)
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
  :diminish
  ;; :disabled t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :diminish
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :diminish
  :config
  (global-evil-surround-mode 1))

(use-package avy
  :diminish
  :after flyspell
  :bind
  ("C-'" . avy-goto-char))

(use-package prescient
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :disabled t
  :after ivy
  :config
  (ivy-prescient-mode 1))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

(use-package ivy
  :disabled t
  :diminish
  :config
  (ivy-mode))

(use-package selectrum
  :diminish
  :config
  (selectrum-mode 1))

(use-package selectrum-prescient
  :diminish
  :after selectrum
  :config
  (selectrum-prescient-mode 1))

(use-package which-key
  :diminish
  :config
  (which-key-mode)
  ;; (which-key-setup-minibuffer)
  )

(use-package recentf
  :diminish
  :init
  (defun recentf-open-files+ ()
    "Use `completing-read' to open a recent file."
    (interactive)
    (let ((files (mapcar 'abbreviate-file-name recentf-list)))
      (find-file (completing-read "Find recent file: " files nil t))))
  :config
  (recentf-mode t))

(use-package key-chord
  :diminish
  :config
  (key-chord-mode 1))

(use-package cider
  :diminish
  :after org
  :bind
  (:map clojure-mode-map
        ("M-<return>" . cider-eval-last-sexp)
        ("C-c C-s" . cider-jack-in)))

(use-package exec-path-from-shell
  :diminish
  :if (memq window-system '(mac ns))
  :config
  ;; (setq exec-path-from-shell-arguments '("-l"))
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH" "NODE_PATH" "GOPATH" "NPMBIN" "LC_ALL" "LANG"
     "LC_TYPE" "SSH_AGENT_PID" "SSH_AUTH_SOCK" "SHELL"
     "JAVA_HOME" "CLASSPATH" "PKG_CONFIG_PATH")))

(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

(use-package olivetti
  :diminish
  :init
  (setq olivetti-body-width 80))

(use-package lsp-mode
  :diminish
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  :hook
  ((java-mode . lsp)
   (ruby-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :diminish
  :init
  (setq lsp-ui-doc-enable nil)
  ;; (setq lsp-ui-show-hover t)
  (setq lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions nil)
  )

(use-package lsp-java
  :diminish
  )

(use-package company
  :diminish
  :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
  :bind
  (:map
   company-active-map
   ;; ("C-n"   . company-select-next)
   ;; ("C-p"   . company-select-previous)
   ;; ("C-d"   . company-show-doc-buffer)
   ;; ;; ("<return>" . company-complete-selection)
   ;; ;; ("SPC" . company-complete-selection)
   ;; ("<tab>" . company-complete-selection)
   :map company-mode-map
   ("<tab>" . company-indent-or-complete-common)
   )
  :custom
  (company-auto-complete t)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode eaf-mode org-mode))
  ;; Trigger completion immediately.
  (company-idle-delay 0.1)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  :config
  ;; (unless clangd-p (delete 'company-clang company-backends))
  (global-company-mode 1)
  ;; (add-to-list 'company-backends 'company-yasnippet)
  )


;; (use-package company
;;   :disabled t
;;   :hook
;;   (prog-mode . company-mode)
;;   ;; :bind
;;   ;; ;; (:map company-active-map
;;   ;; ;;       ("<tab>" . company-complete-selection))
;;   ;; (:map lsp-mode-map
;;   ;;       ("<tab>" . company-indent-or-complete-common))
;;   ;; :custom
;;   ;; (company-minimum-prefix-length 2)
;;   ;; (company-idle-delay 0.0)
;;   )

(use-package vterm
  :diminish
  ;; :disabled t
  :init
  ;; (evil-define-key 'insert vterm-mode-map "C-u" 'vterm-send-C-u)
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (setq vterm-kill-buffer-on-exit t))

(use-package paren
  :diminish
  :config
  (show-paren-mode 1))

(use-package magit
  :diminish
  )

(use-package swift-mode
  :diminish
  )

(use-package sml-mode
  :diminish
  )

(use-package haskell-mode
  :diminish
  :config
  (electric-pair-local-mode -1))

(use-package lsp-haskell
  :diminish
  ;; :disabled t
  :config
  ;; (add-hook 'haskell-mode-hook #'lsp)
  ;; (add-hook 'haskell-literate-mode-hook #'lsp)
  )

;; !need to install ripgrep command line tool in your system
(use-package snails
  :diminish
  :disabled t
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
  :diminish
  :disabled t
  :straight (fuz :type git :host github :repo "rustify-emacs/fuz.el")
  :config
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod)))

(use-package edwina
  ;; :disabled t
  :diminish
  :custom
  (edwina-mfact 0.55)
  (edwina-narrow-threshold 115)
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-setup-dwm-keys 'super)
  (edwina-mode 1))

(use-package beacon
  :diminish
  ;; :disabled t
  :config
  (beacon-mode 1)
  (setq beacon-dont-blink-major-modes (append beacon-dont-blink-major-modes
                                              '(vterm-mode shell-mode eshell-mode term-mode)))
  (add-hook 'beacon-dont-blink-predicates
            (lambda () (bound-and-true-p org-tree-slide-mode))))

(use-package eyebrowse
  :diminish
  :demand t
  ;; :disabled t
  :custom
  (eyebrowse-wrap-around t)
  :bind
  (:map
   eyebrowse-mode-map
   ("s-1" . 'eyebrowse-switch-to-window-config-1)
   ("s-2" . 'eyebrowse-switch-to-window-config-2)
   ("s-3" . 'eyebrowse-switch-to-window-config-3)
   ("s-4" . 'eyebrowse-switch-to-window-config-4)
   ("s-5" . 'eyebrowse-switch-to-window-config-5)
   ("s-<up>" . 'eyebrowse-close-window-config)
   ("s-<down>" . 'eyebrowse-rename-window-config)
   ("s-<left>" . 'eyebrowse-prev-window-config)
   ("s-<right>" . 'eyebrowse-next-window-config))
  :hook
  ((eyebrowse-post-window-switch . get-eyebrowse-status)
   (eyebrowse-post-window-delete . get-eyebrowse-status))
  :config
  (defun get-eyebrowse-status ()
    (interactive)
    (message (eyebrowse-mode-line-indicator)))
  (eyebrowse-mode))

(use-package dired+
  :diminish
  :disabled t)

(use-package yaml-mode
  :diminish
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(use-package all-the-icons
  :diminish
  )

(use-package treemacs-icons-dired
  :diminish
  :disabled t
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package perspective
  :diminish
  :disabled t
  :config
  (persp-mode))

(use-package ranger
  :diminish
  :disabled t
  :init
  (ranger-override-dired-mode t)
  (setq ranger-parent-depth 1)
  (setq ranger-max-parent-width 0.12)
  (setq ranger-show-hidden t)
  ;; :hook
  ;; (
  ;;  (change-major-mode . (lambda () (when (not (eq major-mode 'ranger-mode)) (edwina-mode -1))))
  ;;  (after-change-major-mode . (lambda () (when (not (and (eq major-mode 'dired-mode)
  ;;   													 (eq major-mode 'ranger-mode))) (message (symbol-name major-mode)))))
  ;;  ;(after-change-major-mode . (lambda () (when (not (eq major-mode 'ranger-mode)) (edwina-mode))))
  ;;  ;(after-change-major-mode . (lambda () (when (not (eq major-mode 'ranger-mode)) (edwina-mode nil))))
  ;; )
  )
;; (setq debug-on-error t)

(use-package rainbow-delimiters
  :diminish
  :disabled t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons-dired
  :diminish
  :hook
  ((dired-mode . (lambda ()
                   (interactive)
                   (unless (file-remote-p default-directory)
                     (all-the-icons-dired-mode))))
   (deer-mode . all-the-icons-dired-mode)))

(use-package projectile
  :diminish
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
  :diminish
  :disabled t
  :config
  (setq mini-frame-resize t)
  (mini-frame-mode +1)
  ;; (custom-set-variables `(mini-frame-internal-border-color ,nano-color-subtle))
  (custom-set-variables
   `(mini-frame-show-parameters
     `((top . 0.2)
       (width . 0.6)
       (left . 0.5)
       ;; (background-color . ,nano-color-background)
       )))
  ;; workaround for not showing candidates if no typed characters, should be fixed in Emacs 27.2
  ;; (define-advice fit-frame-to-buffer (:around (f &rest args) dont-skip-ws-for-mini-frame)
  ;; (cl-letf* ((orig (symbol-function #'window-text-pixel-size))
  ;;            ((symbol-function #'window-text-pixel-size)
  ;;             (lambda (win from to &rest args)
  ;;               (apply orig
  ;;                      (append (list win from 
  ;;                                    (if (and (window-minibuffer-p win)
  ;;                                             (frame-root-window-p win)
  ;;                                             (eq t to))
  ;;                                        nil
  ;;                                      to))
  ;;                              args)))))
  ;; (apply f args)))
  )

(use-package consult
  :diminish
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
  :diminish
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
  :diminish
  :disabled t
  :ensure t
  :bind
  ("C-S-a" . embark-act))             

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :diminish
  :disabled t
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package expand-region
  :diminish
  :bind
  (("C-=" . er/expand-region)
   ("C--" . er/contract-region)))

(use-package iedit
  :diminish
  )

(use-package flycheck
  :diminish
  :hook
  ((prog-mode . flycheck-mode)
   (emacs-lisp-mode . (lambda () (flycheck-mode -1)))))

(use-package consult-flycheck
  :diminish
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))

(use-package flycheck-pos-tip
  :diminish
  :disabled t
  :after
  flycheck)

(use-package flycheck-inline
  :diminish
  :disabled t
  :after
  flycheck)

(use-package undo-fu
  :diminish
  )

(use-package tao-theme
  :diminish
  ;; :config
  ;; (set-face-attribute 'default nil :height 180)
  ;; (load-theme 'tao-yang t)
  )

(use-package minimal-theme
  :diminish
  )

;;; Install epdfinfo via 'brew install pdf-tools --HEAD' and then install the
;;; pdf-tools elisp via the use-package below. To upgrade the epdfinfo
;;; server, just do 'brew upgrade pdf-tools' prior to upgrading to newest
;;; pdf-tools package using Emacs package system. If things get messed
;;; up, just do 'brew uninstall pdf-tools', wipe out the elpa
;;; pdf-tools package and reinstall both as at the start.
(use-package pdf-tools
  :diminish
  :disabled t
  :straight (pdf-tools :type git :host github :repo "politza/pdf-tools"
                       :fork (:host github
                                    :repo "flatwhatson/pdf-tools"))
  :init
  (setq pdf-view-use-scaling t)
  :config
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (pdf-tools-install))

(use-package markdown-mode
  :diminish
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ;; ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  ;; ("\\.markdown\\'" . gfm-mode))
  :hook
  (markdown-mode . variable-pitch-mode)
  (markdown-mode . (lambda ()
                     ;; (setq markdown-hide-urls t)
                     (markdown-display-inline-images)
                     (setq markdown-hide-markup nil)
                     (markdown-enable-header-scaling)
                     (setq markdown-enable-prefix-prompts nil)
                     (setq markdown-enable-math t)))
  :init
  (defun markdown-enable-header-scaling ()
    (interactive)
    (setq markdown-header-scaling t)
    (markdown-update-header-faces t  '(1.3 1.2 1.1 1.0 1.0 1.0)))
  (setq markdown-xhtml-header-content
        (concat "<script type=\"text/javascript\" async"
                " src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjax/"
                "2.7.1/MathJax.js?config=TeX-MML-AM_CHTML\">"
                "</script>"))
  (setq markdown-command "multimarkdown")
  (setq markdown-asymmetric-header t)
  (setq markdown-indent-on-enter 'indent-and-new-item)
  (setq markdown-display-remote-images t)
  (setq markdown-electric-backquote t)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-wiki-links t)
  (setq markdown-enable-math t)
  ;; (setq markdown-live-preview-window-function 'markdown-live-preview-window-xwidget)
  (setq markdown-open-command "/usr/local/bin/mark")
  (setq markdown-max-image-size '(500 . 500))
  ;; (evil-define-key 'normal 'markdown-mode-map (kbd "RET") 'markdown-follow-wiki-link-at-point)
  :bind
  (:map markdown-mode-map
        ("C-<left>" . markdown-promote)
        ("C-<right>" . markdown-demote)
        ("C-<up>" . markdown-move-up)
        ("C-<down>" . markdown-move-down)))

(use-package edit-indirect
  :diminish
  )

(use-package iscroll
  :diminish
  :disabled t
  :straight (iscroll :type git :host: github :repo "casouri/iscroll")
  :bind
  (:map evil-normal-state-map
        ("j" . iscroll-next-line)
        ("k" . iscroll-previous-line)
        ("C-n" . iscroll-next-line)
        ("C-p" . iscroll-previous-line)))

(use-package deft
  :diminish
  :commands (deft deft-open-file deft-new-file-named)
  :config
  (setq deft-directory "~/Space/"
        deft-recursive t
        deft-extensions '("md" "txt" "org" "tex")
        deft-use-filter-string-for-filename nil
        deft-use-filename-as-title t
        deft-markdown-mode-title-level 1
        deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase))))

(use-package eshell
  :diminish
  :init
  (defun set-eshell-prompt-path ()
    (interactive)
    (let* ((pwd (eshell/pwd))
           (splited (split-string pwd "/"))
           (prefix (string-join (seq-take splited 3) "/"))
           (rest (string-join (seq-drop splited 3) "/"))
           (home (getenv "HOME")))
      (if (equal home prefix)
          (if (equal home pwd) "~" (concat "~/" rest))
        pwd)))
  ;; (setq eshell-prompt-function
  ;;       (lambda nil
  ;;         (concat
  ;;          "\n"
  ;;          (set-eshell-prompt-path)
  ;;          "\nλ ")))
  (defun eshell-other-window ()
    "Open a `shell' in a new window."
    (interactive)
    (let ((buf (eshell)))
      (switch-to-buffer (other-buffer buf))
      (switch-to-buffer-other-window buf)))
  :hook
  (eshell-mode . (lambda ()
                   (evil-define-key 'insert eshell-mode-map
                     (kbd "C-u") 'eshell-kill-input
                     (kbd "C-a") 'eshell-bol
                     (kbd "C-p") 'eshell-previous-input
                     (kbd "<up>") 'eshell-previous-input
                     (kbd "C-n") 'eshell-next-input
                     (kbd "<down>" 'eshell-next-input)))))

(use-package restclient
  :diminish
  )

(use-package ob-restclient
  :diminish
  :after org)

(use-package plantuml-mode
  :after org
  :init
  (setq org-plantuml-jar-path "/opt/homebrew/Cellar/plantuml/1.2021.8/libexec/plantuml.jar")
  (setq org-plantuml-default-exec-mode 'jar)
  (add-to-list
  'org-src-lang-modes '("plantuml" . plantuml)))

(use-package org
  :diminish
  ;; :hook
  ;; (org-mode . org-num-mode)
  :init
  (setq org-latex-compiler "lualatex")
  (setq org-export-preserve-breaks t)
  (setq org-ditaa-jar-path "/opt/homebrew/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar")
  (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
  (setq org-display-remote-inline-images 'cache)
  (setq org-latex-create-formula-image-program 'dvisvgm)
  (defun modi/org-entity-get-name (char)
    "Return the entity name for CHAR. For example, return \"ast\" for *."
    (let ((ll (append org-entities-user
                      org-entities))
          e name utf8)
      (catch 'break
        (while ll
          (setq e (pop ll))
          (when (not (stringp e))
            (setq utf8 (nth 6 e))
            (when (string= char utf8)
              (setq name (car e))
              (throw 'break name)))))))

  (defun modi/org-insert-org-entity-maybe (&rest args)
    "When the universal prefix C-u is used before entering any character,
    insert the character's `org-entity' name if available.

    If C-u prefix is not used and if `org-entity' name is not available, the
    returned value `entity-name' will be nil."
    ;; It would be fine to use just (this-command-keys) instead of
    ;; (substring (this-command-keys) -1) below in emacs 25+.
    ;; But if the user pressed "C-u *", then
    ;;  - in emacs 24.5, (this-command-keys) would return "^U*", and
    ;;  - in emacs 25.x, (this-command-keys) would return "*".
    ;; But in both versions, (substring (this-command-keys) -1) will return
    ;; "*", which is what we want.
    ;; http://thread.gmane.org/gmane.emacs.orgmode/106974/focus=106996
    (let ((pressed-key (substring (this-command-keys) -1))
          entity-name)
      (when (and (listp args) (eq 4 (car args)))
        (setq entity-name (modi/org-entity-get-name pressed-key))
        (when entity-name
          (setq entity-name (concat "\\" entity-name "{}"))
          (insert entity-name)
          (message (concat "Inserted `org-entity' "
                           (propertize entity-name
                                       'face 'font-lock-function-name-face)
                           " for the symbol "
                           (propertize pressed-key
                                       'face 'font-lock-function-name-face)
                           "."))))
      entity-name))

  ;; Run `org-self-insert-command' only if `modi/org-insert-org-entity-maybe'
  ;; returns nil.
  (advice-add 'org-self-insert-command :before-until #'modi/org-insert-org-entity-maybe)
  (setq org-pretty-entities t)
  (setq org-image-actual-width nil)
  (defun my/get-one-layer-subdirs (bases)
    (flatten-list
     (mapcar
      (lambda (base)
        (let ((base (file-name-as-directory base)))
          (mapcar
           (apply-partially 'concat base)
           (seq-filter
            (lambda (file)
              (and (file-directory-p (concat base file))
                   (not (equal file ".."))
                   (not (equal file "."))))
            (directory-files base)))))
      bases)))
  (require 'org-tempo)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)" "DELEGATED(D)")))
  (setq org-tag-alist '(("flag" . ?f)))
  (setq org-log-done 'time)
  ;; (setq org-agenda-files
  ;; (my/get-one-layer-subdirs org-roam-para-dirs))
  ;; don't show deadline befeore scheduled day
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (setq org-log-into-drawer t)
  (setq org-columns-default-format
        "%25ITEM %TODO %3PRIORITY %SCHEDULED %DEADLINE")
  ;; (setq org-agenda-window-setup 'only-window)
  :config
  (setq haskell-process-type 'stack-ghci)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((haskell . t)
     (ditaa . t)
     (dot . t)
     (plantuml . t)))
  (add-to-list 'org-export-backends 'md)
  (require 'ob-js)
  (require 'ob-clojure)
  (setq org-babel-clojure-backend 'cider)
  (require 'cider)
  (require 'ob-scheme)
  (require 'ob-restclient)
  (require 'ob-ruby))

(use-package evil-org
  :diminish
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-ql
  :diminish
  :disabled t
  :after org
  :init
  (setq org-agenda-custom-commands
        '(("c" "custom"
           ((org-ql-block '(or (and (todo) (deadline) (not (scheduled))) ;; ->|
                               (and (todo) (scheduled :to 0)) ;; |^->
                               (and (todo) (not (planning)) (ts-active :on 0)) ;; <^>
                               (and (todo) (not (planning)) (ts-active :to 0) (ts-active :from 0))
                               ) ;; <>-^-<>
                          ((org-ql-block-header "Today")))
            ;; (org-ql-block '(or (and (todo) (deadline))
            ;;                    (and (todo) (scheduled))
            ;;                    (and (todo) (not (planning)) (ts-active)))
            ;;               ((org-ql-block-header "Scheduled")))
            (org-ql-block '(and (todo) (ts-active))
                          ((org-ql-block-header "Scheduled")))
            (org-ql-block '(or (and (todo) (tags-all "flag")))
                          ((org-ql-block-header "Flagged")))
            (org-ql-block '(and (todo) (not (ts-active)))
                          ((org-ql-block-header "Someday")))
            (org-ql-block '(or (and (todo) (not (planning)) (ts-active :to -1) (not (ts-active :from 0))) ;; 错过的严格日期或时间段
                               (and (todo) (deadline :to -1))) ;; 错过的 deadline
                          ((org-ql-block-header "Missed")))
            ;; (org-ql-block '(or (and (todo) (ts :to -1) (not (ts :to 1))) ;; 没有错过的严格日期或时间段
            ;;                    (and (todo) (scheduled :to 0)) ;; |^->
            ;;                    (and (todo) (not (planning)) (ts :on 0)) ;; <^>
            ;;                    (and (todo) (not (planning)) (ts :to 0) (ts :from 0))) ;; <>-^-<>
            ;;               ((org-ql-block-header "Projects")))
            (org-ql-block '(todo) ;; <>-^-<>
                          ((org-ql-block-header "All")))
            ;; (agenda)
            )))))

(use-package org-super-agenda
  :diminish
  :disabled t
  :init
  (setq org-super-agenda-groups
        '(;; Each group has an implicit boolean OR operator between its selectors.
          (:name "Today"  ; Optionally specify section name
                 :todo "TODAY"
                 :scheduled past
                 :scheduled today
                 :deadline past
                 :deadline today
                 )  ; Items that have this TODO keyword
          (:name "Scheduled"
                 :scheduled t
                 :deadline t)
          (:name "Flagged"
                 ;; Single arguments given alone
                 :tag "flag")
          (:name "Someday"
                 :todo "SOMEDAY")
          ))
  :config
  (org-super-agenda-mode))

(use-package org-download
  :diminish
  :hook
  (dired-mode . org-download-enable)
  :init
  (setq org-download-image-org-width 500)
  ;; (setq-default org-download-image-dir "note_assets")
  (setq org-download-method 'attach)
  (setq-default org-download-heading-lvl nil)
  ;; (setq org-download-method 'directory)
  (setq org-download-screenshot-method "/usr/sbin/screencapture -i %s"))

(use-package md-roam
  :diminish
  :disabled t
  :straight (md-roam :type git :host github :repo "nobiot/md-roam")
  :init
  (setq md-roam-file-extension-single "md")
  (setq org-roam-title-sources '((mdtitle title mdheadline headline) (mdalias alias)))
  (setq org-roam-tag-sources '(md-frontmatter))
  (setq md-roam-use-org-file-links nil)
  (defun my-md-insert-file ()
    "select a file and insert to copy to resources dir and insert md link"
    (interactive)
    (let* ((filename (read-file-name "select a file: "))
           (extension (file-name-extension filename))
           (new-filename-nondir (concat (format-time-string "%Y-%m-%d_%H-%M-%S") "." extension))
           (new-filename (concat slipbox-resources-directory new-filename-nondir)))
      (copy-file filename new-filename)
      (insert (concat "[](../Assets/" new-filename-nondir ")"))))
  (defun my-insert-image-from-clipboard (format)
    "paste image from clipboard to org-roam-directory/resources
and insert the markdown link to current position.
require pastepng installed.
argument: format, can be png, jpg, gif, pdf, tif, jpeg (string)"
    (interactive)
    (if (string= "pdf" format)
        (setq format "png"))
    (setq filename
          (concat
           (format-time-string "%Y-%m-%d_%H-%M-%S") "." format))
    (start-process "" nil "pngpaste"
                   (concat (expand-file-name org-roam-directory)
                           "/Assets/"
                           filename))
    (insert (concat "![](../Assets/" filename ")"))
    (markdown-display-inline-images))
  (defun my-insert-jpg-from-clipboard ()
    (interactive)
    (my-insert-image-from-clipboard "jpg"))
  (defun my-insert-gif-from-clipboard ()
    (interactive)
    (my-insert-image-from-clipboard "gif"))
  (defun delete-resource ()
    "delete resources file from current line's markdown link"
    (interactive)
    (let* ((line (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position)))
           (start (+ (string-match "(" line) 1))
           (end (string-match ")" line)))
      (delete-file (expand-file-name (substring line start end)))
      (kill-whole-line)))
  (defun my/all-occur (regexp string)
    "Get a list of all regexp matches in a string"
    (interactive)
    (save-match-data
      (let ((pos 0)
            matches)
        (while (string-match regexp string pos)
          (push (match-string 0 string) matches)
          (setq pos (match-end 0)))
        matches)))
  (defun my/file-contents (filename)
    "Return the contents of FILENAME."
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-string)))
  (defun my/all-md-link (filename)
    (interactive)
    (my/all-occur "\\[.*\\]\\(.*\\)" (my/file-contents filename)))
  (defun my/export-md-to-textbundle-structure ()
    "export a selected md file, copy md file and its assets to Downloads dir,
and process the md link inside to adapt to textbundle dir strucuture,
remove the md and assets file to a trash dir
requires that the original md file has a structure of SlipBox"
    (interactive)
    (let* ((filename (read-file-name ""))
           (default-directory (file-name-directory filename))
           (filename-nondir (file-name-nondirectory filename))
           (filename-sans (file-name-sans-extension filename-nondir))
           (all-md-links (my/all-md-link filename))
           (output-dir (concat (expand-file-name "~/Downloads/") filename-sans "/"))
           (assets-dir (concat output-dir "assets/"))
           (output-md-file-name (concat output-dir filename-nondir))
           (all-links
            (mapcar (lambda (link) (expand-file-name (substring (string-trim-right link) 3 -1) ))
                    all-md-links)))
      (when (not (file-directory-p output-dir)) (make-directory output-dir))
      (when (not (file-directory-p assets-dir)) (make-directory assets-dir))
      (copy-file filename output-md-file-name)
      (dolist (filename all-links)
        (copy-file filename (concat assets-dir (file-name-nondirectory filename)))
        (message (concat "file " filename " copied successfully"))
        ;; (move-file-to-trash filename)
        ;; won't delete files, will build a fun to scan whole files and remove useless link,
        ;; for intentionlly useage, for performance
        (message (concat "file " filename " deleted successfully")))
      (start-process "" nil "sed" "-i" "" "s|](\\.\\./|](|" output-md-file-name)
      ;; (move-file-to-trash filename)
      (message (concat "file " filename " copied and deleted successfully")))))

(use-package geiser
  :diminish
  :init
  (setq geiser-active-implementations '(chez Racket MIT/GNU guile))
  (setq geiser-chez-binary "chez"))

(use-package electric
  :diminish
  ;; :disabled t
  :config
  (electric-pair-mode)
  :hook
  (org-mode
   . (lambda ()
       (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                      (if (char-equal c ?<)
                          t
                        (,electric-pair-inhibit-predicate c)))))))

(use-package org-fragtog
  :diminish
  :hook
  (org-mode . org-fragtog-mode))

(use-package w3m
  :diminish
  :init
  (setq w3m-command "/opt/homebrew/bin/w3m"))

(use-package tidal
  :diminish
  :init
  (setq tidal-interpreter "/Users/las/.ghcup/bin/ghci"))

(use-package clojure-mode
  :diminish
  )

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(internal-border ((t (:background "#282828"))))
;;  '(window-divider ((t (:foreground "#282828"))))
;;  '(window-divider-first-pixel ((t (:foreground "gray25"))))
;;  '(window-divider-last-pixel ((t (:foreground "gray25")))))

(use-package telephone-line
  :diminish
  :disabled t
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-flat
        telephone-line-primary-right-separator telephone-line-primary-left-separator
        telephone-line-secondary-left-separator 'telephone-line-flat
        telephone-line-secondary-right-separator telephone-line-secondary-left-separator)
  (telephone-line-mode 1))

(use-package lispy
  :disabled t
  :diminish
  :hook
  ((clojure-mode . (lambda () (lispy-mode 1)))
   (emacs-lisp-mode . (lambda () (lispy-mode 1)))
   (scheme-mode . (lambda () (lispy-mode)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(company-auto-commit t nil nil "Customized with use-package company")
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "31302cb8f88ee2ca06fa2324b3fc31366443db6d066626154ef0dd64f267cbc4" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "515e9dd9749ef52a8e1b63427b50b4dc68afb4a16b1a9cabfbcf6b4897f2c501" "e3b2bad7b781a968692759ad12cb6552bc39d7057762eefaf168dbe604ce3a4b" default))
 '(frame-background-mode 'light)
 '(menu-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))

(use-package dired
  :diminish
  :straight nil
  :init
  (setq dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|^.DS_STORE$\\|^.projectile$\\|^.git$")
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . dired-omit-mode))

(use-package dired-hacks-utils)

(use-package dired-open
  :diminish
  :init
  (setq dired-open-extensions '(("pdf" . "open"))))

(use-package diredfl
  :diminish
  :disabled t)

(use-package dired-subtree
  :diminish
  :init
  (setq dired-subtree-use-backgrounds nil)
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map)
  (advice-add 'dired-subtree-toggle :after (lambda ()
                                             (interactive)
                                             (when all-the-icons-dired-mode
                                               (revert-buffer))))
  (advice-add 'dired-subtree-cycle :after (lambda ()
                                            (interactive)
                                            (when all-the-icons-dired-mode
                                              (revert-buffer))))
  )

(use-package treemacs
  :diminish
  :disabled t)

(use-package dired-sidebar
  :diminish
  :disabled t
  :commands (dired-sidebar-toggle-sidebar))

(use-package hide-mode-line
  :diminish
  :hook
  (dired-mode . hide-mode-line-mode))

;; (set-face-attribute 'mode-line nil
;;                     :background nano-dark-background
;;                     :foreground "white"
;;                     ;; :box '(:line-width 8 :color "#353644")
;;                     ;; :overline nil
;;                     ;; :underline nil
;;                     )

;; (set-face-attribute 'mode-line-inactive nil
;;                     :background "#565063"
;;                     :foreground "white"
;;                     ;; :box '(:line-width 8 :color "#565063")
;;                     :overline nil



;; (custom-set-faces cus
;;  '(mode-line ((t (:underline t))))
;;  '(mode-line-inactive ((t (:underline t)))))
(use-package simple-modeline
  :diminish
  :disabled t
  :hook (after-init . simple-modeline-mode))

(diminish 'auto-revert-mode)
(diminish 'eldoc-mode)
(diminish 'visual-line-mode)

;; write a function to do the spacing
(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))




(use-package shell-pop
  :custom
  (shell-pop-shell-type '("vterm" "vterm" (lambda nil (vterm))))
  (shell-pop-universal-key "s-t")
  :hook
  (shell-pop-in-after . (lambda () (edwina-arrange))))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package flyspell
  :init
  (setq ispell-program-name "aspell")
  ;; You could add extra option "--camel-case" for since Aspell 0.60.8 
  ;; @see https://github.com/redguardtoo/emacs.d/issues/796
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16")))

(use-package wucuo
  :init
  (setq wucuo-spell-check-buffer-predicate
        (lambda ()
          (not (memq major-mode
                     '(dired-mode
                       log-edit-mode
                       compilation-mode
                       help-mode
                       profiler-report-mode
                       speedbar-mode
                       gud-mode
                       calc-mode
                       Info-mode)))))
  :hook
  (prog-mode . wucuo-start)
  (text-mode . wucuo-start)
  )

(use-package org-roam
  ;; :straight (org-roam :type git :host github :repo "org-roam/org-roam" :branch "v2")
  :init
  (setq org-roam-v2-ack t)
  (defun org-hide-properties ()
    "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
        (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put ov_this 'display "")
          (overlay-put ov_this 'hidden-prop-drawer t))))
    (put 'org-toggle-properties-hide-state 'state 'hidden))

  (defun org-show-properties ()
    "Show all org-mode property drawers hidden by org-hide-properties."
    (interactive)
    (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
    (put 'org-toggle-properties-hide-state 'state 'shown))

  (defun org-toggle-properties ()
    "Toggle visibility of property drawers."
    (interactive)
    (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
        (org-show-properties)
      (org-hide-properties)))
  :custom
  (org-roam-directory "~/Space/Data/Org Roam/")
  :config
  (org-roam-setup)
  (require 'org-roam-protocol ))

(use-package progress-bar
  :straight (:repo "BeneathCloud/progress-bar"))

(use-package org-devonthink
  :straight (:repo "BeneathCloud/org-devonthink"))

(use-package org-tree-slide
  :init
  (setq org-tree-slide-heading-emphasis t)
  :bind
  (:map org-tree-slide-mode-map
        ("s-<left>" . org-tree-slide-move-previous-tree)
        ("s-<right>" . org-tree-slide-move-next-tree)
        ("s-<up>" . org-tree-slide-content))
  :hook
  (org-tree-slide-play . (lambda ()
                           (org-display-inline-images)
                           (setq text-scale-mode-amount 3)
                           (text-scale-mode)
                           (hide-mode-line-mode)))
  (org-tree-slide-stop . (lambda ()
                           (text-scale-mode -1)
                           (hide-mode-line-mode -1)))
  :config
  (org-tree-slide-simple-profile))

(use-package company-math
  :disabled t
  :after company
  :hook
  (org-mode . (lambda ()
                (setq-local company-backends
                            '((company-math-symbols-latex company-latex-commands))))))

;; 显示 emoji
(set-fontset-font t 'symbol 
                  (font-spec :family "Apple Color Emoji") 
                  nil 'prepend)

;; (set-face-attribute 'hl-line nil
;;                         :foreground "alternateSelectedControlTextColor"
;;                         :background "selectedContentBackgroundColor")
;; (set-face-attribute 'selectrum-current-candidate nil
;;                     :background "selectedContentBackgroundColor"
;;                     :foreground "alternateSelectedControlTextColor")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(selectrum-current-candidate ((t (:inherit nano-subtle :extend t)))))

(use-package org-link-beautify
  :disabled t
  :config
  (org-link-beautify-mode 1))

(use-package org-mind-map
  :init
  (require 'ox-org)
  :config
  (setq org-mind-map-engine "dot"))

(use-package graph
  :straight (graph :host github :repo "storax/graph.el"))

(use-package alda-mode)

(use-package lorem-ipsum)

(defun my/delete-current-file ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (when (y-or-n-p (concat "delete file " filename "?"))
            (progn
              (delete-file filename t)
              (message "%s deleted" filename)
              (kill-buffer)
              (when (> (length (window-list)) 1)
                (delete-window))))
      (message "it's not a file"))))

(use-package winner
  :config
  (winner-mode +1))

(use-package git-timemachine)
