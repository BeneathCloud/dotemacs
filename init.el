;;-*- lexical-binding: t -*-

;; TODO

;; 2. remove system tool dependencies like Rust, ripgrep
;;    - add a new backend for Snails.el using selectrum for fuzzing search
;; 3. make edwina work with treemacs
;; 4. add textbunlde support
(require 'cl)
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
(setq-default with-editor-emacsclient-executable "/opt/homebrew/bin/emacsclient")

(use-package nano
  :straight (nano-emacs :type git :host github :repo "rougier/nano-emacs"
                        :fork (:host github
                                     :repo "BeneathCloud/nano-emacs")
                        :no-byte-compile t)
  :config
  ;; (require 'nano-base-colors)
  ;; (require 'nano-colors)
  ;; (require 'nano-faces)
  ;; (require 'nano-theme)
  ;; (require 'nano-theme-light)
  ;; (require 'nano-splash)
  ;; (require 'nano-modeline)
  ;; (require 'nano-layout)
  ;; (require 'nano-minibuffer)
  ;; (require 'nano-compact)
  ;; (defun update-mini-frame-color ()
  ;;   (custom-set-variables `(mini-frame-internal-border-color ,nano-color-subtle))
  ;;   (custom-set-variables
  ;;    `(mini-frame-show-parameters
  ;;      `((top . 0.2)
  ;;        (width . 0.6)
  ;;        (left . 0.5)
  ;;        (background-color . ,nano-color-background)
  ;;        ))))
  ;; (defun nano-theme-light ()
  ;;   (interactive)
  ;;   (nano-theme-set-light)
  ;;   (nano-faces)
  ;;   (nano-theme)
  ;;   (update-mini-frame-color))
  ;; (defun nano-theme-dark ()
  ;;   (interactive)
  ;;   (nano-theme-set-dark)
  ;;   (nano-faces)
  ;;   (nano-theme)
  ;;   (update-mini-frame-color))
  ;; (nano-theme-light)
  )

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
(setq mac-option-key-is-meta t
      mac-command-key-is-meta nil
      mac-command-modifier　'super
      mac-option-modifier 'meta
      mac-use-title-bar nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(scroll-bar-mode -1)

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
   "s-t" 'eshell-other-window
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
   "F" (lambda () (interactive) (let ((default-directory "~/Space/Drafts/")) (call-interactively 'find-file)))
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
   :keymaps 'global
   "C-a" 'beginning-of-visual-line
   "C-e" 'end-of-visual-line
   "C-k" 'kill-line
   "C-\\" 'universal-argument
   )

  (general-define-key
   :states '(insert normal emacs visual)
   :keymaps 'emacs-lisp-mode-map
   "C-<return>" 'eval-last-sexp)

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
  ;; :disabled t
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
  ("C-;" . avy-goto-char))

(use-package ivy
  :disabled
  :config
  (ivy-mode))

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

(use-package cider
  :after org
  :bind
  (:map clojure-mode-map
        ("C-<return>" . cider-eval-last-sexp)
        ("C-c C-s" . cider-jack-in)))

(use-package exec-path-from-shell
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
  :init
  (setq olivetti-body-width 80))

(use-package lsp-mode
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  :hook
  ((java-mode . lsp)
   (ruby-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :init
  (setq lsp-ui-doc-enable nil)
  ;; (setq lsp-ui-show-hover t)
  (setq lsp-ui-sideline-show-diagnostics nil
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
  ;; (evil-define-key 'insert vterm-mode-map "C-u" 'vterm-send-C-u)
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (setq vterm-kill-buffer-on-exit t))

(use-package paren
  :config
  (show-paren-mode 1))

(use-package magit)

(use-package swift-mode)

(use-package sml-mode)

(use-package haskell-mode
 :config
 (electric-pair-local-mode -1))

(use-package lsp-haskell
  ;; :disabled t
 :config
 ;; (add-hook 'haskell-mode-hook #'lsp)
 ;; (add-hook 'haskell-literate-mode-hook #'lsp)
 )

;; !need to install ripgrep command line tool in your system
(use-package snails
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
  :disabled t
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

(use-package all-the-icons)

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
  :disabled t
  :ensure t
  :bind
  ("C-S-a" . embark-act))             

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :disabled t
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

(use-package tao-theme
  ;; :config
  ;; (set-face-attribute 'default nil :height 180)
  ;; (load-theme 'tao-yang t)
  )

(use-package minimal-theme)

;;; Install epdfinfo via 'brew install pdf-tools --HEAD' and then install the
;;; pdf-tools elisp via the use-package below. To upgrade the epdfinfo
;;; server, just do 'brew upgrade pdf-tools' prior to upgrading to newest
;;; pdf-tools package using Emacs package system. If things get messed
;;; up, just do 'brew uninstall pdf-tools', wipe out the elpa
;;; pdf-tools package and reinstall both as at the start.
(use-package pdf-tools
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

(use-package edit-indirect)

(use-package iscroll
  :disabled t
  :straight (iscroll :type git :host: github :repo "casouri/iscroll")
  :bind
  (:map evil-normal-state-map
        ("j" . iscroll-next-line)
        ("k" . iscroll-previous-line)
        ("C-n" . iscroll-next-line)
        ("C-p" . iscroll-previous-line)))

(use-package deft
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
  (setq eshell-prompt-function
        (lambda nil
          (concat
           "\n"
           (set-eshell-prompt-path)
           "\nλ ")))
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

(use-package restclient)

(use-package ob-restclient
  :after org)

(use-package org-roam
  :init
  (setq org-roam-directory "~/Space/")
  (setq org-roam-para-dirs 
        (mapcar (lambda (arg) (concat org-roam-directory arg))
                '("Projects" "Areas" "Resources" "Archives")))
  (setq org-roam-tag-sources '(all-directories prop))
  (setq org-roam-dailies-directory (concat org-roam-directory "Areas/Daily/"))
  (setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         #'org-roam-capture--get-point
         "* %?"
         :file-name "Areas/Daily/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n\n* Today\n\n")))
  (setq org-roam-capture-templates
        '(("l" "literature" plain (function org-roam--capture-get-point)
     "%?"
     :file-name "%(read-directory-name \"dir:\" org-roam-directory)/%<%Y%m%d%H%M%S>-${slug}"
     :head "#+roam_alias: \n#+roam_key: %^{PROMPT}\n#+roam_tags: literature growing\n\n* ${title}\n\n"
     :unnarrowed t)
          ("d" "style literature" plain (function org-roam--capture-get-point)
     "%?"
     :file-name "Areas/搭配/style_at_%<%Y%m%d%H%M%S>"
     :head "#+roam_alias: \n#+roam_key:\n#+roam_tags: literature\n\n* style_at_%<%Y%m%d%H%M%S>\n\n"
     :unnarrowed t)
          ("p" "permanent" plain (function org-roam--capture-get-point)
     "%?"
     :file-name "Permanents/%<%Y%m%d%H%M%S>-${slug}"
     :head "#+roam_alias: \n#+roam_tags: permanent\n\n* ${title}\n\n"
     :unnarrowed t)
          ("s" "subnent" plain (function org-roam--capture-get-point)
     "%?"
     :file-name "%(read-directory-name \"dir:\" org-roam-directory)/%<%Y%m%d%H%M%S>-${slug}"
     :head "#+roam_alias: \n#+roam_tags: subnent\n\n* ${title}\n\n"
     :unnarrowed t)
          ("o" "output" plain (function org-roam--capture-get-point)
     "%?"
     :file-name "%(read-directory-name \"dir:\" org-roam-directory)/%<%Y%m%d%H%M%S>-${slug}"
     :head "#+roam_alias: \n#+roam_tags: output growing\n\n* ${title}\n\n"
     :unnarrowed t)
          ("e" "excerpt" plain (function org-roam--capture-get-point)
     "%?"
     :file-name "%(read-directory-name \"dir:\" org-roam-directory)/%<%Y%m%d%H%M%S>-${slug}"
     :head "#+roam_alias: \n#+roam_tags: excerpt\n\n* ${title}\n\n"
     :unnarrowed t)
          ("f" "fleet" plain (function org-roam--capture-get-point)
     "%?"
     :file-name "%(read-directory-name \"dir:\" org-roam-directory)/%<%Y%m%d%H%M%S>-${slug}"
     :head "#+roam_alias: \n#+roam_tags: fleet growing\n\n* ${title}\n\n"
     :unnarrowed t)
          ("g" "log" plain (function org-roam--capture-get-point)
     "%?"
     :file-name "%(read-directory-name \"dir:\" org-roam-directory)/%<%Y%m%d%H%M%S>-${slug}"
     :head "#+roam_alias: \n#+roam_tags: log\n\n* ${title}\n\n"
     :unnarrowed t)))
  :general
  (:prefix "SPC"
   :states '(normal visual)
   :keymaps 'override
   "nt" 'org-roam-dailies-find-today
   "nT" 'org-roam-dailies-find-directory
   "nd" 'deft
   "nD" (lambda () (interactive) (dired org-roam-directory))
   "nn" 'org-roam-find-file
   "ni" 'org-roam-insert
   ;; "nij" 'my-insert-jpg-from-clipboard
   ;; "nig" 'my-insert-gif-from-clipboard
   "nI" 'org-roam-insert-immediate
   "nl" 'org-roam
   "ng" 'org-roam-graph))

(use-package org
  :init
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
  (setq org-agenda-files
        (my/get-one-layer-subdirs org-roam-para-dirs))
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
    '((haskell . t)))
  (add-to-list 'org-export-backends 'md)
  (require 'ob-js)
  (require 'ob-clojure)
  (setq org-babel-clojure-backend 'cider)
  (require 'cider)
  (require 'ob-scheme)
  (require 'ob-restclient)
  (require 'ob-ruby))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-ql
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
  :init
  (setq org-download-image-org-width 500)
  (setq-default org-download-image-dir "note_assets")
  (setq-default org-download-heading-lvl nil)
  (setq org-download-method 'directory))

(use-package md-roam
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
  :init
  (setq geiser-active-implementations '(chez Racket MIT/GNU guile))
  (setq geiser-chez-binary "chez"))

(use-package electric
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
  :hook
  (org-mode . org-fragtog-mode))

(use-package w3m
  :init
  (setq w3m-command "/opt/homebrew/bin/w3m"))

(use-package tidal
  :init
  (setq tidal-interpreter "/Users/las/.ghcup/bin/ghci"))

(use-package clojure-mode)

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
  :disabled t
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-flat
        telephone-line-primary-right-separator telephone-line-primary-left-separator
        telephone-line-secondary-left-separator 'telephone-line-flat
        telephone-line-secondary-right-separator telephone-line-secondary-left-separator)
  (telephone-line-mode 1))

(use-package lispy
  :hook
  ((clojure-mode . (lambda () (lispy-mode 1)))
   (emacs-lisp-mode . (lambda () (lispy-mode 1)))
   (scheme-mode . (lambda () (lispy-mode)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "515e9dd9749ef52a8e1b63427b50b4dc68afb4a16b1a9cabfbcf6b4897f2c501" "e3b2bad7b781a968692759ad12cb6552bc39d7057762eefaf168dbe604ce3a4b" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
