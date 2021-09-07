;-*- lexical-binding: t -*-
;; -----------------------------------------------------
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
(use-package nano-theme
  :straight (nano-theme :type git :host github
                        :repo "rougier/nano-theme")
  :init
  ;; write a function to do the spacing
  (defun simple-mode-line-render (left right)
    "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
    (let* ((available-width (- (window-total-width)
                               (length left)

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
    (set-face-attribute 'default nil :font my/default-fixed-pitch-font)
    ;; (setup-mode-line "light")
    )
  (defun polish@nano-dark (old-fn &rest args)
    (apply old-fn args)
    (set-face-attribute 'default nil :font my/default-fixed-pitch-font)
    ;; (setup-mode-line "dark")
    )
  :config
  ;; (advice-add 'nano-light :around 'polish@nano-light)
  ;; (advice-add 'nano-dark :around 'polish@nano-dark)
  (nano-light)
  )

(use-package emacs
  :after nano-theme
  :custom
  (face-font-family-alternatives
   '(("Monospace" "pragmatapro mono liga" "courier" "fixed")
     ("Consolas" "Monaco" "Roboto Mono" "PT Mono" "Terminus" "Monospace")
     ("Monospace Serif" "CMU Typewriter Text" "Courier 10 Pitch" "Monospace")
     ("Serif" "CMU Serif" "Georgia" "Cambria" "Times New Roman" "DejaVu Serif" "serif")
     ("Variable Serif" "PingFang SC" "Helvetica")))
  (delete-by-moving-to-trash t)
  (trash-directory "~/.Trash")
  (frame-resize-pixelwise t)
  :custom-face
  (mode-line ((t (:family "Monospace" :height 150
                          :inherit nano-subtle
                          ;; :box (:line-width 3 :inherit nano-subtle-i)))))
                          :box (:line-width 3 :color ,nano-light-subtle)))))
  (mode-line-inactive ((t (:family "Monospace" :height 150
                          ;; :inherit nano-subtle-i
                           :background ,nano-light-subtle
                           :foreground ,nano-light-faded
                          ;; :box (:line-width 3 :color ,nano-light-subtle)))))
                          :box (:line-width 3 :color ,nano-light-subtle)))))
  (variable-pitch ((t (:family "Variable Serif" :height 200))))
  (fixed-pitch ((t (:family "Monospace" :height 200))))
  (default ((t (:family "Monospace" :height 200))))
  :init
  ;; emacs-mac no-title-bar patch
  (setq mac-use-title-bar t)
  
  (setq system-time-locale "en_US.UTF-8")
  ;; No startup  screen
  (setq inhibit-startup-screen t)
  ;; No startup message
  (setq inhibit-startup-message t)
  (setq inhibit-startup-echo-area-message t)
  ;; No message in scratch buffer
  (setq initial-scratch-message nil)
  ;; Initial buffer
  (setq initial-buffer-choice nil)
  ;; No frame title
  (setq frame-title-format nil)
  ;; No file dialog
  (setq use-file-dialog nil)
  ;; No dialog box
  (setq use-dialog-box nil)
  ;; No popup windows
  ;; (setq pop-up-windows nil)
  (setq pop-up-windows t)
  ;; No empty line indicators
  (setq indicate-empty-lines nil)
  ;; No cursor in inactive windows
  (setq cursor-in-non-selected-windows nil)
  ;; Text mode is initial mode
  (setq initial-major-mode 'text-mode)
  ;; Text mode is default major mode
  (setq default-major-mode 'text-mode)
  ;; Moderate font lock
  (setq font-lock-maximum-decoration nil)
  ;; No limit on font lock
  (setq font-lock-maximum-size nil)
  ;; No line break space points
  (setq auto-fill-mode nil)
  ;; Fill column at 80
  (setq fill-column 80)
  ;; Bar cursor
  (setq-default cursor-type '(hbar .  2))
  (setq-default cursor-in-non-selected-windows nil)
  (setq blink-cursor-mode nil)
  ;; No scroll bars
  (if (fboundp 'scroll-bar-mode)
      (scroll-bar-mode -1))
  ;; No toolbar
  (if (fboundp 'tool-bar-mode)
      (tool-bar-mode nil))
  ;; Default frame settings
  (setq default-frame-alist
        (append (list
	             '(min-height . 1)  '(height . 45)
	             '(min-width  . 1)  '(width  . 81)
                 '(vertical-scroll-bars . nil)
                 '(internal-border-width . 24)
                 '(left-fringe . 0)
                 '(right-fringe . 0)
                 '(tool-bar-lines . 0)
                 '(menu-bar-lines . 0))))
  ;; Line spacing (in pixels)
  (setq line-spacing 0)
  ;; Vertical window divider
  (setq window-divider-default-right-width 24)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode 1)
  ;; Nicer glyphs for continuation and wrap 
  (set-display-table-slot standard-display-table
			              'truncation (make-glyph-code ?… 'nano-faded))
  (set-display-table-slot standard-display-table
			              'wrap (make-glyph-code ?… 'nano-faded))

  :config
  (require 'cl)
  ;; emacs server
  (server-start)
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

(use-package dashboard
  :after org
  :init
  (setq dashboard-startup-banner "~/.config/emacs/banner.png")
  (setq dashboard-image-banner-max-height 256)
  (setq dashboard-week-agenda t)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        ;; (projects . 5)
                        (agenda . 5)
                        ;; (registers . 5))
        ))
  :hook
  (dashboard-mode . (lambda () (turn-on-olivetti-mode)))
  :config
  (dashboard-setup-startup-hook))

(use-package el-patch
  :init
  (setq el-patch-enable-use-package-integration t))

(use-package key-chord
  :config
  (key-chord-mode 1))

(use-package avy
  :after key-chord
  ;; :general
  ;; (:keymaps 'override
  ;;  :states '(insert normal visual emacs)
  ;;          (general-chord "''") 'avy-goto-char-2)
  ;; (:states '(insert)
  ;;          (general-chord "jk") 'evil-normal-state
  ;;          (general-chord "kj") 'evil-normal-state)
  :config
  (avy-setup-default))

(use-package evil
  ;;:disabled
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-global-set-key 'insert (kbd "C-n") 'next-line)
  (evil-global-set-key 'insert (kbd "C-p") 'previous-line)
  (evil-global-set-key 'normal (kbd "C-n") 'evil-next-visual-line)
  (evil-global-set-key 'normal (kbd "j") 'evil-next-visual-line)
  (evil-global-set-key 'normal (kbd "C-p") 'evil-previous-visual-line)
  (evil-global-set-key 'normal (kbd "k") 'evil-previous-visual-line)
  (evil-mode))

(use-package evil-collection
  ;;:disabled
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  ;;:disabled
  :config
  (evil-commentary-mode))

(use-package evil-surround
  ;;:disabled
  :config
  (global-evil-surround-mode 1))

(use-package general
  ;; :after evil
  :config
  ;; for leader
  (general-define-key
   :keymaps 'evil-insert-state-map
   (general-chord "jk") 'evil-normal-state
   (general-chord "kj") 'evil-normal-state)
  ;; (general-define-key
  ;;  :keymaps '(insert normal emacs visual)
  ;;  (general-chord "''") 'avy-goto-word-0
  ;;  (general-chord ";'") 'avy-goto-line
  ;;  (general-chord "';") 'avy-goto-line)
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'override
   "s-n" 'make-frame
   "s-m" 'maximize-window
   "s-j" 'other-window
   "s-k" (lambda () (interactive) (other-window -1))
   "s-<return>" (lambda () (interactive)(split-window-horizontally) (other-window 1))
   "s-S-<return>" (lambda () (interactive)(split-window-vertically) (other-window 1))
   "s-J" 'enlarge-window
   "s-K" 'shrink-window
   "s-H" 'shrink-window-horizontally
   "s-L" 'enlarge-window-horizontally
   "s-d" 'osx-dictionary-search-word-at-point
   "s-D" 'osx-dictionary-search-input
   ;; "s-t" 'vterm-other-window
   "s-t" 'eshell-other-window
   "s-u" 'update-progress-bar-at-point
   "s-y" 'backward-progress-bar-at-point
   "s-i" 'forward-progress-bar-at-point
   ;; "s-[" 'previous-buffer
   ;; "s-]" 'next-buffer
   "s-[" 'winner-undo
   "s-]" 'winner-redo
   "s-f" 'consult-line
   "s-;" 'symex-mode-interface
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
   "mm" 'mu4e
   "mu" (lambda () (interactive) (mu4e-headers-search "flag:unread AND NOT flag:trashed"))
   "nb" 'org-roam-buffer-toggle
   "nn" 'org-roam-node-find
   "ni" 'org-roam-node-insert
   "ng" 'org-roam-graph
   "nc" 'org-roam-capture 
   "nj" 'org-roam-dailies-capture-today
   "ndd" 'org-roam-dailies-goto-today
   "ndD" 'org-roam-dailies-capture-today
   "ndy" 'org-roam-dailies-goto-yesterday
   "ndY" 'org-roam-dailies-capture-yesterday
   "ndt" 'org-roam-dailies-goto-tomorrow
   "ndT" 'org-roam-dailies-capture-tomorrow
   "ndg" 'org-roam-dailies-goto-date
   "ndG" 'org-roam-dailies-capture-date
   "ndn" 'org-roam-dailies-goto-next-note
   "ndp" 'org-roam-dailies-goto-previous-note
   ";" 'eval-expression
   ;; "e" 'consult-flycheck
   "e" 'elfeed
   "E" 'flycheck-list-errors
   "k" 'consult-focus-lines
   "K" 'consult-focus-lines-quit
   "A" 'iedit-mode
   "a" 'org-agenda
   "ss" 'slack-select-unread-rooms
   "si" 'slack-im-select
   "sc" 'slack-channel-select
   "L" 'consult-ripgrep
   "l" 'consult-line
   "o" 'consult-outline
   "i" 'consult-imenu
   "I" 'lsp-ui-imenu
   "SPC" 'consult-buffer
   ;; "SPC" 'projectile-find-file
   "x" 'execute-extended-command
   "g" 'magit
   ;; "pp" 'projectile-switch-project
   ;; "pd" 'projectile-remove-known-project
   ;; "pa" 'projectile-add-known-project
   ;; "pf" 'projectile-find-file
   "f" 'find-file
   ;; "F" (lambda () (interactive) (shell-command (concat "open -R " (buffer-name))))
   "F" 'reveal-in-osx-finder
   ;; "q" (lambda () (interactive) (let ((default-directory "~/Space/Drafts/")) (call-interactively 'find-file)))
   ;; "s" 'save-buffer
   "r" 'consult-register-load
   "R" 'consult-register-store
   "d" 'dired
   "D" (lambda () (interactive) (shell-command "open ."))
   ;; "D" 'ranger
   ;; "b" 'switch-to-buffer
   "b" 'consult-bookmark
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
   "M-<return>" 'eval-last-sexp
   "M-S-<return>" 'eval-defun
   )

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
   :states '(normal visual)
   :keymaps '(prog-mode-map text-mode-map fundamental-mode-map org-mode-map eshell-mode-map vterm-mode-map)
   "`" 'beacon-blink
   "f" 'avy-goto-word-0
   "F" 'avy-goto-char-2
   "C-f" 'evil-avy-goto-line
   "J" (lambda () (interactive) (scroll-up-command 1) (forward-line 1))
   "K" (lambda () (interactive) (scroll-up-command -1) (forward-line -1)))

  (general-define-key
   ;; :keymaps 'pdf-continuous-scroll-mode-map
   :keymaps 'pdf-view-mode-map
   :states '(normal visual)
   "j" 'pdf-continuous-scroll-forward
   "k" 'pdf-continuous-scroll-backward
   "J" 'pdf-continuous-next-page
   "K" 'pdf-continuous-previous-page
   "C-d" 'pdf-view-scroll-up-or-next-page
   "C-u" 'pdf-view-scroll-down-or-previous-page
   "M" 'pdf-cscroll-toggle-mode-line
   "gt" 'pdf-cscroll-view-goto-page
   "gg" 'pdf-cscroll-first-page
   "G" 'pdf-cscroll-last-page
   "q" 'pdf-cscroll-kill-buffer-and-windows)

  (general-define-key
   :keymaps 'pdf-outline-buffer-mode-map
   :states '(normal visual)
   "<return>" (lambda ()
                (interactive)
                (let ((link (pdf-outline-link-at-pos (point))))
                  (pdf-outline-quit)
                  (unless link
                    (error "Nothing to follow here"))
                  (pdf-cscroll-view-goto-page (cdr (assoc 'page link))))))
  )

(use-package company-prescient
  ;; :disabled t
  :after company
  :config
  (company-prescient-mode 1))

(use-package vertico
  :disabled t
  :custom-face
  (vertico-current ((t (:inherit nil :background "#ECEFF4" :foreground "black" :extended t))))
  :init
  (setq enable-recursive-minibuffers t)
  (setq vertico-resize t)
  (setq vertico-cycle t)
  (vertico-mode))

(use-package corfu
  :disabled
  :straight (:type git :host github :repo "minad/corfu")
  :custom-face
  (corfu-current ((t (:inherit nil :background "#ECEFF4" :foreground "black" :extended t))))
  (corfu-background ((t (:inherit nil :background "white"))))
  (corfu-border ((t (:inherit nil :background "#D8DEE9"))))
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto nil)                 ;; Enable auto completion
  (corfu-commit-predicate t)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary nil)     ;; Automatically quit at word boundary
  (corfu-quit-no-match nil)        ;; Automatically quit if there is no match
  ;; (corfu-echo-documentation nil) ;; Do not show documentation in the echo area

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous))

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (corfu-global-mode))

;; Dabbrev works with Corfu
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(use-package orderless
  ;; :disabled t
  :custom
  (completion-styles '(orderless))
  :init
  (setq orderless-skip-highlighting (lambda () selectrum-is-active))
  (setq selectrum-refine-candidates-function #'orderless-filter)
 (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
)

(use-package savehist
  :disabled t
  :init
  (savehist-mode))

(use-package selectrum
  ;; :disabled t
  :custom-face
  ;; (selectrum-current-candidate ((t (:inherit nano-faded-i :extended t))))
  (selectrum-current-candidate ((t (:inherit nano-subtle :extended t))))
  ;; (selectrum-current-candidate ((t (:inherit nil :background "#ECEFF4" :foreground "black" :extended t))))
  ;; (selectrum-current-candidate ((t (:background "red" :extend t))))
  :init
  (setq selectrum-extend-current-candidate-highlight t)
  :config
  (selectrum-mode 1))

(use-package selectrum-prescient
  ;; :disabled t
  :after selectrum
  :config
  (setq selectrum-prescient-enable-filtering nil)
  (selectrum-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package marginalia
  ;; :disabled t ;; marginalia will corrupt vertico, disabledtemporarily
  :init
  (marginalia-mode))

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

(use-package cider
  :after org
  :bind
  (:map clojure-mode-map
        ("M-<return>" . cider-eval-last-sexp)
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
  ;; (setq olivetti-body-width 80)
  (setq olivetti-body-width 0.65)
  (setq olivetti-minimum-body-width 72)
  )

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
  ;; :disabled t
  :hook ((prog-mode) . company-mode)
  :bind
  (:map
   company-active-map
   ("C-n"   . company-select-next)
   ("C-p"   . company-select-previous)
   ("C-d"   . company-show-doc-buffer)
   ;; ;; ("<return>" . company-complete-selection)
   ;; ;; ("SPC" . company-complete-selection)
   ;; ("<tab>" . company-complete-selection)
   ("<tab>" . company-complete-selection)
   ;; :map company-mode-map
   ;; ("<tab>" . company-indent-or-complete-common)
   )
  :custom
  (company-auto-complete t)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not magit-mode magit-popup-mode shell-mode org-mode dired-mode))
  ;; Trigger completion immediately.
  (company-idle-delay 0.1)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  :config
  ;; (unless clangd-p (delete 'company-clang company-backends))
  (global-company-mode 1)
  ;; (add-to-list 'company-backends 'company-yasnippet)
  )

(use-package vterm
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
  :config
  ;; (add-hook 'haskell-mode-hook #'lsp)
  ;; (add-hook 'haskell-literate-mode-hook #'lsp)
  )

(use-package edwina
  :disabled
  :custom
  (edwina-mfact 0.55)
  (edwina-narrow-threshold 115)
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-setup-dwm-keys 'super)
  (edwina-mode 1))

(use-package beacon
  :init
  (setq beacon-blink-when-window-scrolls nil)
  :config
  (beacon-mode 1)
  (setq beacon-dont-blink-major-modes (append beacon-dont-blink-major-modes
                                              '(vterm-mode shell-mode eshell-mode term-mode elfeed-show-mode)))
  (add-hook 'beacon-dont-blink-predicates
            (lambda () (bound-and-true-p org-tree-slide-mode))))

(use-package eyebrowse
  :demand t
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

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook
  ((dired-mode . (lambda ()
                   (interactive)
                   (unless (file-remote-p default-directory)
                     (all-the-icons-dired-mode))))
   (deer-mode . all-the-icons-dired-mode))
  :config/el-patch
  (defun all-the-icons-dired--setup ()
    "Setup `all-the-icons-dired'."
    (setq-local tab-width (el-patch-swap 1 2))
    (pcase-dolist (`(,file ,sym ,fn) all-the-icons-dired-advice-alist)
      (with-eval-after-load file
        (advice-add sym :around fn)))
    (all-the-icons-dired--refresh)))

(use-package projectile
  :disabled t
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
  ;; :hook
  ;; (after-init . mini-frame-mode)
  :custom
  ((mini-frame-show-parameters `((top . 0.2)
                                 (width . 0.6)
                                 (left . 0.5)
                                 (left-fringe . 16)
                                 (right-fringe .16)
                                 ;; (child-frame-border-width . 2)
                                 (child-frame-border-width . 2)
                                 (internal-border-width . 2)
                                 (foreground-color . ,nano-light-foreground)
                                 (background-color . "white")
                                 ;; (background-color . ,nano-light-subtle)
                                 ;; (background-color . "#ECEFF4")
                                 ))
   ;; (mini-frame-color-shift-step 10)
   (mini-frame-internal-border-color "#D8DEE9")
   ;; (mini-frame-internal-border-color "black")
   ;; (mini-frame-advice-functions '(selectrum-completing-read))
   (mini-frame-advice-functions '(read-from-minibuffer))
   (resize-mini-frame t))
  :config
  (mini-frame-mode))

(use-package consult
  :custom
  (consult-find-command "fd -I -t f ")
  :init
  (defun consult-focus-lines-quit ()
    (interactive)
    (consult-focus-lines -1))
  :bind
  ("M-'" . consult-register-store)
  :config
  ;; (setq consult-project-root-function #'projectile-project-root)
  (setq consult-project-root-function nil)
  (defun find-fd (&optional dir initial)
    (interactive "P")
    (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
      (consult-find dir initial))))

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

(use-package undo-fu)

;;; Install epdfinfo via 'brew install pdf-tools --HEAD' and then install the
;;; pdf-tools elisp via the use-package below. To upgrade the epdfinfo
;;; server, just do 'brew upgrade pdf-tools' prior to upgrading to newest
;;; pdf-tools package using Emacs package system. If things get messed
;;; up, just do 'brew uninstall pdf-tools', wipe out the elpa
;;; pdf-tools package and reinstall both as at the start.
(use-package pdf-tools
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

(use-package org-noter)

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

(use-package deft
  :commands (deft deft-open-file deft-new-file-named)
  :config
  (setq deft-directory "~/Space/Notes/Org Roam/"
        deft-recursive t
        deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:"
        deft-use-filename-as-title t))

(use-package eshell
  :custom-face
  ;; (eshell-prompt ((t (:inherit nano-subtle :extend t))))
  (eshell-ls-directory ((t (:inherit nano-strong :extend t))))
  (eshell-ls-backup ((t (:inherit nano-faded :extend t))))
  :init
  ;; (defun set-eshell-prompt-path ()
  ;;   (interactive)
  ;;   (let* ((pwd (eshell/pwd))
  ;;          (splited (split-string pwd "/"))
  ;;          (prefix (string-join (seq-take splited 3) "/"))
  ;;          (rest (string-join (seq-drop splited 3) "/"))
  ;;          (home (getenv "HOME")))
  ;;     (if (equal home prefix)
  ;;         (if (equal home pwd) "~" (concat "~/" rest))
  ;;       pwd)))
  (setq eshell-prompt-function
        (lambda nil
          (concat
           "\n"
           (propertize (eshell/pwd) 'face `(:inherit nano-faded))
           "\nε ")))
  (setq eshell-prompt-regexp "^ε ")
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

(use-package plantuml-mode
  :after org
  :init
  (setq org-plantuml-jar-path "/opt/homebrew/Cellar/plantuml/1.2021.8/libexec/plantuml.jar")
  (setq org-plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path "/opt/homebrew/Cellar/plantuml/1.2021.8/libexec/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  (add-to-list
  'org-src-lang-modes '("plantuml" . plantuml)))

(use-package org
  ;; :hook
  ;; (org-mode . org-num-mode)
  :hook
  (org-mode . org-indent-mode)
  :bind
  (:map org-mode-map
        ("C-c C-c" . (lambda ()
                     (interactive)
                     (org-ctrl-c-ctrl-c)
                     (org-display-inline-images))))
  :init
  (setq org-bookmark-names-plist nil) ; don't add bookmark when capture, refile... the highlight is annoying 
  ;; (setq org-agenda-window-setup 'current-window)
  (setq org-attach-use-inheritance t)
  (setq org-tags-column -80)
  (setq org-cycle-open-archived-trees nil)
  (setq org-startup-folded 'show2levels)
  (setq org-default-notes-file "~/Space/Notes/PARA/para.org")
  (setq org-export-with-tags nil)
  (setq org-latex-toc-command "\\tableofcontents \\clearpage") ; force new page after toc
  (defun my/list-attachments ()
    (interactive)
    (require 'org-attach)
    (insert (mapconcat (lambda (name)
                      (concat "+ [[attachment:"
                              name
                              "]]"))
                       (org-attach-file-list (org-attach-dir))
                       "\n")))
  (defun my/buffer-local-face (font)
   "Sets a fixed width (monospace) font in current buffer"
   (interactive "sFont: ")
   (setq buffer-face-mode-face `(:family ,font))
   (buffer-face-mode))
  ;; :hook
  ;; (org-mode . (lambda ()
  ;;               (my/buffer-local-face "pragmatapro mono liga 1.75")
  ;;               (setq line-spacing 0)))
  :init
  ;; (setq org-latex-compiler "lualatex")
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
  (require 'org-tempo) ; enable <s, <e ... abbrev
  (setq org-ellipsis " ⭭ ")
  (setq org-agenda-hide-tags-regexp "ARCHIVE\\|para\\|ATTACH")
  (setq org-special-ctrl-a/e nil) ; C-e moves to before the ellipses, not after.
  (setq org-cycle-separator-lines 2)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!)" "HOLD(h!)" "DISCARDED(D!)")))
  (setq org-tag-alist '(("flag" . ?f)))
  (setq org-log-done 'time)
  (setq org-agenda-files '("~/Space/Notes/PARA/para.org"))
  ;; (setq org-agenda-files
  ;; (my/get-one-layer-subdirs org-roam-para-dirs))
  ;; don't show deadline befeore scheduled day
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (setq org-log-into-drawer t)
  (setq org-columns-default-format
        "%25ITEM %TODO %3PRIORITY %SCHEDULED %DEADLINE")
  ;; (setq org-agenda-window-setup 'only-window)
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-capture-templates '(("i" "Inbox"
                                 entry (file+headline "~/Space/Notes/PARA/para.org" "Inbox")
                                 "\n* %?\n\n"
                                 :prepend t
                                 :empty-lines 1)))
  (setq org-capture-bookmark nil)
  :config
  (setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
    (setq org-format-latex-header
        "\\documentclass{article}
\\usepackage[usenames]{color}
[PACKAGES]
[DEFAULT-PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\setlength\parindent{0pt}
\\addtolength{\\topmargin}{-2.54cm}"
)

  (defface org-checkbox-done-text
    '((t (:inherit 'org-headline-done)))
    "Face for the text part of a checked org-mode checkbox.")

  (font-lock-add-keywords
   'org-mod
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-done-text prepend))
   'append)
  (setq haskell-process-type 'stack-ghci)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((haskell . t)
     (ditaa . t)
     (dot . t)
     (latex . t)
     (plantuml . t)))
  (add-to-list 'org-export-backends 'md)
  ;; agenda view custom
  (require 'org-agenda)
  (setq org-agenda-prefix-format '(
                                   (agenda  . "  • %i %?-12t% s")
                                   ;; (agenda  . " %i %-12:c%?-12t% s") file name + org-agenda-entry-type
                                   (timeline  . "  % s")
                                   (todo  . " %i")
                                   ;; (todo  . " %i %-12:c")
                                   (tags  . " %i %-12:c")
                                   (search . " %i %-12:c")))

  (require 'org-attach)
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
  :disabled t
  :init
  (setq org-super-agenda-groups
        '(;; Each group has an implicit boolean OR operator between its selectors.
          (
           :name ""  ; Optionally specify section name
                 :todo "NEXT"
                 ;; :scheduled past
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
          ;; (:name "Someday"
          ;;        :todo "SOMEDAY")
          ))
  :config
  (org-super-agenda-mode))

(use-package org-download
  :after org
  :hook
  (dired-mode . org-download-enable)
  :init
  (setq org-download-image-org-width 500)
  ;; (setq-default org-download-image-dir "note_assets")
  (setq org-download-method 'attach)
  (setq-default org-download-heading-lvl nil)
  ;; (setq org-download-method 'directory)
  (setq org-download-screenshot-method "/usr/sbin/screencapture -i %s"))

(use-package geiser
  :init
  (setq geiser-active-implementations '(chez Racket MIT/GNU guile))
  (setq geiser-chez-binary "chez"))

(use-package electric
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(company-auto-commit t nil nil "Customized with use-package company")
 '(custom-safe-themes
   '("249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b" "5a00018936fa1df1cd9d54bee02c8a64eafac941453ab48394e2ec2c498b834a" "a131602c676b904a5509fff82649a639061bf948a5205327e0f5d1559e04f5ed" "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "f00a605fb19cb258ad7e0d99c007f226f24d767d01bf31f3828ce6688cbdeb22" "733ef3e3ffcca378df65a5b28db91bf1eeb37b04d769eda28c85980a6df5fa37" "d516f1e3e5504c26b1123caa311476dc66d26d379539d12f9f4ed51f10629df3" "06ed754b259cb54c30c658502f843937ff19f8b53597ac28577ec33bb084fa52" "11cc65061e0a5410d6489af42f1d0f0478dbd181a9660f81a692ddc5f948bf34" "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336" "e8567ee21a39c68dbf20e40d29a0f6c1c05681935a41e206f142ab83126153ca" "d9a28a009cda74d1d53b1fbd050f31af7a1a105aa2d53738e9aa2515908cac4c" "2ce76d65a813fae8cfee5c207f46f2a256bac69dacbb096051a7a8651aa252b0" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "8d7b028e7b7843ae00498f68fad28f3c6258eda0650fe7e17bfb017d51d0e2a2" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "7eea50883f10e5c6ad6f81e153c640b3a288cd8dc1d26e4696f7d40f754cc703" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "b186688fbec5e00ee8683b9f2588523abdf2db40562839b2c5458fcfb322c8a4" "5f619a1e4a4827b0e0011c914f870e7a09252b3f4e117c64330af0a5bc0e3e1f" "d6d4e0512dcaae663f7bd304557d6bc8b78c576be5af9c0b62b8447fb79b5fde" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "03f28a4e25d3ce7e8826b0a67441826c744cbf47077fb5bc9ddb18afe115005f" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "31302cb8f88ee2ca06fa2324b3fc31366443db6d066626154ef0dd64f267cbc4" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "515e9dd9749ef52a8e1b63427b50b4dc68afb4a16b1a9cabfbcf6b4897f2c501" "e3b2bad7b781a968692759ad12cb6552bc39d7057762eefaf168dbe604ce3a4b" default))
 '(elfeed-feeds
   '("http://www3.nhk.or.jp/rss/news/cat0.xml" "http://news.yahoo.co.jp/pickup/rss.xml" "http://sspai.me/feed" "https://rsshub.app/gamersky/news" "http://feeds.feedburner.com/butshesagirl" "https://d12frosted.io/atom.xml" "https://eli.thegreenplace.net/feeds/all.atom.xml" "https://omny.fm/shows/future-of-coding/playlists/podcast.rss" "https://www.extrema.is/articles/tag/index:haskell-books.rss" "http://okmij.org/ftp/rss.xml" "http://hypirion.com/rss/all" "https://protesilaos.com/codelog.xml" "http://sachachua.com/wp/category/emacs/feed/" "https://blog.tecosaur.com/tmio/rss.xml" "https://ag91.github.io/rss.xml" "https://www.with-emacs.com/rss.xml" "http://christiantietze.de/feed.atom" "https://rsshub.app/blogs/wangyin" "http://nullprogram.com/feed/" "https://planet.emacslife.com/atom.xml"))
 '(frame-background-mode 'light)
 '(hsys-org-enable-smart-keys 'buttons)
 '(menu-bar-mode nil)
 '(pdf-tools-handle-upgrades nil)
 '(safe-local-variable-values
   '((org-roam-db-location . "~/Space/Notes/PARA/org-roam.db")
     (org-roam-directory . "~/Space/Notes/PARA/")
     (org-roam-db-location expand-file-name "./org-roam.db")
     (org-roam-directory expand-file-name ".")))
 '(send-mail-function 'smtpmail-send-it)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(warning-suppress-types '((use-package) (use-package))))

(use-package dired
  :straight nil
  :hook
  (dired-mode . (lambda () (setq tab-width 2)))
  :init
  (setq dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|^.DS_STORE$\\|^.projectile$\\|^.git$")
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . dired-omit-mode))

(use-package dired-hacks-utils)

(use-package dired-open
  :init
  (setq dired-open-extensions '(("pdf" . "open"))))

(use-package dired-subtree
  :init
  (setq dired-subtree-use-backgrounds nil)
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-cycle))
  :config
  ;; (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  ;; (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map)
  (advice-add 'dired-subtree-toggle :after (lambda ()
                                             (interactive)
                                             (when all-the-icons-dired-mode
                                               (revert-buffer))))
  (advice-add 'dired-subtree-cycle :after (lambda ()
                                            (interactive)
                                            (when all-the-icons-dired-mode
                                              (revert-buffer))))
  )

(use-package hide-mode-line
  :hook
  (dired-mode . hide-mode-line-mode))

(use-package shell-pop
  :custom
  ;; (shell-pop-shell-type '("vterm" "vterm" (lambda nil (vterm))))
  (shell-pop-shell-type '("eshell" "*eshell*" (lambda nil (eshell))))
  (shell-pop-universal-key "s-t")
  :hook
  (shell-pop-in-after . (lambda () (edwina-arrange))))

(use-package yasnippet
  :disabled t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :disabled t
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
  :straight (:host github :repo "org-roam/org-roam"
             :files (:defaults "extensions/*"))
  :init
  (setq org-roam-v2-ack t)
  ;; (setq org-roam-db-node-include-function
  ;;       (lambda ()
  ;;         (not (-any? (lambda (x) (eq 'para (read x))) (org-get-tags)))))
  ;; (setq org-roam-db-node-include-function
  ;;     (lambda ()
  ;;       (not (member "ATTACH" (org-get-tags)))))
  ;; (add-to-list 'load-path "~/.config/emacs/straight/repos/org-roam/extensions/")
  ;; (require 'org-roam-dailies)
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
  (org-roam-directory "~/Space/Notes/Org Roam/")
  (org-roam-capture-templates '(("d" "default" plain "%?" :if-new
                                 (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                 :unnarrowed t)
                              ("l" "literature note" plain "%?" :if-new
                                 (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+filetags: :literature:\n#+title: ${title}\n")
                                 :unnarrowed t)
                              ("c" "concept note" plain "%?" :if-new
                                 (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+filetags: :concept:\n#+title: ${title}\n")
                                 :unnarrowed t)
                              ))
  (org-roam-dailies-capture-templates '(("d" "default" entry "* %<%I:%M %p>: %?" :if-new
                                         (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :config
  (org-roam-setup))

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
                           (make-local-variable 'previous-line-spacing)
                           (setq previous-line-spacing line-spacing)
                           (setq line-spacing 1.0)
                           ;; (setq line-spacing 0)
                           (org-display-inline-images)
                           (setq text-scale-mode-amount 3)
                           (text-scale-mode)
                           (hide-mode-line-mode)))
  (org-tree-slide-stop . (lambda ()
                           (setq line-spacing previous-line-spacing)
                           (text-scale-mode -1)
                           (hide-mode-line-mode -1)))
  :config
  (org-tree-slide-simple-profile))

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
 '(default ((t (:family "Monospace" :height 200))))
 '(bookmark-face ((t (:inherit nano-subtle))))
 '(corfu-background ((t (:inherit nil :background "white"))))
 '(corfu-border ((t (:inherit nil :background "#D8DEE9"))))
 '(corfu-current ((t (:inherit nil :background "#ECEFF4" :foreground "black" :extended t))))
 '(eshell-ls-backup ((t (:inherit nano-faded :extend t))))
 '(eshell-ls-directory ((t (:inherit nano-strong :extend t))))
 '(eshell-prompt ((t (:inherit nano-salient :extend t))))
 '(fixed-pitch ((t (:family "Monospace" :height 200))))
 '(hbut-flash ((t (:inherit nano-popout-i))))
 '(iedit-occurrence ((t (:inherit nano-subtle))))
 '(mode-line ((t (:family "Monospace" :height 150 :inherit nano-subtle :box (:line-width 3 :color "#ECEFF1")))))
 '(mode-line-inactive ((t (:family "Monospace" :height 150 :background "#ECEFF1" :foreground "#B0BEC5" :box (:line-width 3 :color "#ECEFF1")))))
 '(org-indent ((t (:foreground "white"))))
 '(selectrum-current-candidate ((t (:inherit nano-subtle :extended t))))
 '(variable-pitch ((t (:family "Variable Serif" :height 200))))
 '(window-divider-first-pixel ((t (:foreground "#ECEFF1"))))
 '(window-divider-last-pixel ((t (:foreground "#ECEFF1")))))

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
        (when (y-or-n-p (concat "Delete file " filename "?"))
            (progn
              (delete-file filename t)
              (message "%s deleted" filename)
              (kill-buffer)
              (when (> (length (window-list)) 1)
                (delete-window))))
      (message "It's not a file."))))

(defun my/rename-current-file ()
  "rename current file name"
  (interactive)
  (let ((name (buffer-name))
        (file-name (buffer-file-name)))
    (if file-name
        (let ((new-name (read-from-minibuffer
                         (concat "New name for: ")
                         file-name)))
          (if (get-buffer new-name)
              (message "A buffer named %s already exists." new-name)
            (progn
              (rename-file file-name new-name)
              (set-visited-file-name new-name)
              (set-buffer-modified-p nil))))
      (message "This buffer is not visiting a file."))))
      
(use-package winner
  :config
  (winner-mode +1))

(use-package git-timemachine)

(use-package ts)

(use-package lua-mode)

(defun my/hs-reload ()
  "Reload hammerspoon config"
  (interactive)
  (if (executable-find "hs")
      (shell-command "hs -c \"hs.timer.doAfter(1, hs.reload)\"")
    (message "Hammerspoon command line binary not installed.")))

(global-unset-key (kbd "M-c")) ;; for hammerspoon console
(global-unset-key (kbd "M-j")) ;; for hammerspoon console
(global-unset-key (kbd "M-k")) ;; for hammerspoon console
(global-unset-key (kbd "M-h")) ;; for hammerspoon console
(global-unset-key (kbd "M-l")) ;; for hammerspoon console

(use-package svg-tag-mode
  :straight (:repo "rougier/svg-tag-mode")
  :after org
  :hook
  (org-mode . (lambda () (svg-tag-mode)))
  :init
  (defface svg-tag-default-face
    `((t :foreground "black" :background "white" :box "black"
         :family "menlo" :weight light :height 160))
    "Default face" :group nil)
  (defface svg-tag-todo-face
    `((t :foreground "#ffffff" :background "#FFAB91" :box (:line-width 1 :color "#FFAB91" :style nil)
         :family "menlo" :weight light :height 160))
    "TODO face" :group nil)
  (defface svg-tag-next-face
    `((t :foreground "white" :background "#673AB7" :box (:line-width 1 :color "#673AB7" :style nil)
         :family "menlo" :weight light :height 160))
    "TODO face" :group nil)
  (defface svg-tag-date-active-face
    '((t :foreground "white" :background "#673AB7"
         :box (:line-width 1 :color "#673AB7" :style nil)
         :family "menlo" :weight regular :height 160))
    "Face for active date svg tag" :group nil)

  (defface svg-tag-time-active-face
    '((t :foreground "#673AB7" :background "#ffffff"
         :box (:line-width 1 :color "#673AB7" :style nil)
         :family "menlo" :weight light :height 160))
    "Face for active time svg tag" :group nil)

  (defface svg-tag-date-inactive-face
    '((t :foreground "#ffffff" :background "#B0BEC5"
         :box (:line-width 1 :color "#B0BEC5" :style nil)
         :family "menlo" :weight regular :height 160))
    "Face for inactive date svg tag" :group nil)

  (defface svg-tag-time-inactive-face
    '((t :foreground "#B0BEC5" :background "#ffffff"
         :box (:line-width 2 :color "#B0BEC5" :style nil)
         :family "menlo" :weight light :height 160))
    "Face for inactive time svg tag" :group nil)
  :config
  (defun svg-tag-default (text)
    (svg-tag-make (substring text 1 -1) 'svg-tag-default-face 1 1 2))
  (defun svg-tag-priority (text)
    (svg-tag-make (substring text 2 -1) 'svg-tag-default-face 1 0 2))
  (setq svg-tag-todo
        (svg-tag-make "TODO" 'svg-tag-todo-face 1 1 2))
  (setq svg-tag-next
        (svg-tag-make "NEXT" 'svg-tag-next-face 1 1 2))

  (defun svg-tag-make-org-date-active (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-date-active-face 1 0 0))
  (defun svg-tag-make-org-time-active (text)
    (svg-tag-make (substring text 0 -1) 'svg-tag-time-active-face 1 0 0))
  (defun svg-tag-make-org-range-active (text)
    (svg-tag-make (substring text 0 -1) 'svg-tag-time-active-face 1 0 0))

  (defun svg-tag-make-org-date-inactive (text)
    (svg-tag-make (substring text 1 -1) 'svg-tag-date-inactive-face 1 0 0))
  (defun svg-tag-make-org-time-inactive (text)
    (svg-tag-make (substring text 0 -1) 'svg-tag-time-inactive-face 1 0 0))
  (defun svg-tag-make-org-range-inactive (text)
    (svg-tag-make (substring text 0 -1) 'svg-tag-time-inactive-face 1 0 0))

  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")

  (setq svg-tag-tags
         `((" TODO " . svg-tag-todo)
          (" NEXT " . svg-tag-next)
          (" DONE " . svg-tag-default)
          (" DISCARDED " . svg-tag-default)
          (" HOLD " . svg-tag-default)

          (":ARCHIVE:" . svg-tag-default)
          (":ATTACH:" . svg-tag-default)

          ("\\[#[ABC]\\]" . svg-tag-priority)

          (,(concat "<" date-re  "[ >]")             . svg-tag-make-org-date-active)    
          (,(concat "<" date-re " " day-re "[ >]")   . svg-tag-make-org-date-active)    
          (,(concat time-re ">")                     . svg-tag-make-org-time-active)
          (,(concat time-re "-" time-re ">")         . svg-tag-make-org-range-active)

          (,(concat "\\[" date-re  "[] ]")           . svg-tag-make-org-date-inactive)    
          (,(concat "\\[" date-re " " day-re "[] ]") . svg-tag-make-org-date-inactive)    
          (,(concat time-re "\\]")                   . svg-tag-make-org-time-inactive)
          (,(concat time-re "-" time-re "\\]")       . svg-tag-make-org-range-inactive)
          ))
  )

(use-package org-cliplink)

(defun my/finder-path ()
  "Return path of the frontmost Finder window, or the empty string.

Asks Finder for the path using AppleScript via `osascript', so
  this can take a second or two to execute."
  (let ($applescript $result)
    ;; Script via:  https://brettterpstra.com/2013/02/09/quick-tip-jumping-to-the-finder-location-in-terminal/
    (setq $applescript "tell application \"Finder\" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)")
    (setq $result (ns-do-applescript $applescript))
    (if $result
        (string-trim $result)
      "")))
(defun my/mac-finder-in-dired ()
  (dired (my/finder-path)))

(use-package emacs-everywhere)

(use-package websocket)

(use-package simple-httpd)

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :hook (org-roam . org-roam-ui-mode))

(use-package reveal-in-osx-finder)

(use-package org-superstar
  :hook
  (org-mode . (lambda () (org-superstar-mode 1)))
  :init
  ;; (setq org-superstar-headline-bullets-list '(?◉ ?◈ ?○))
  (setq org-superstar-headline-bullets-list '(?◉ ?◈ ?◌))
  ;; (setq org-superstar-item-bullet-alist '((?* . ?❖) (?+ . ?▣) (?- . ?•)))
  (setq org-superstar-item-bullet-alist '((?* . ?❖) (?+ . ?▢) (?- . ?•)))
  ;; This is usually the default, but keep in mind it must be nil
  (setq org-hide-leading-stars nil)
  ;; This line is necessary.
  (setq org-superstar-leading-bullet ?\s)
  ;; If you use Org Indent you also need to add this, otherwise the
  ;; above has no effect while Indent is enabled.
  (setq org-indent-mode-turns-on-hiding-stars nil)
  )

(use-package nov
  :straight (:type git :host nil :url "https://depp.brause.cc/nov.el/")
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :config
  ;; Fix the Unicode error
  (with-no-warnings
    (defun my-nov-content-unique-identifier (content)
      "Return the unique identifier for CONTENT."
      (when-let* ((name (nov-content-unique-identifier-name content))
                  (selector (format "package>metadata>identifier[id='%s']"
                                    (regexp-quote name)))
                  (id (car (esxml-node-children (esxml-query selector content)))))
        (intern id)))
    (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier)))

(use-package osx-dictionary)

(use-package consult-dir
  :straight (:type git :host github :repo "karthink/consult-dir")
  :bind (("C-x C-d" . consult-dir)
         ;; :map vertico-map
         :map selectrum-minibuffer-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package mu4e
  :ensure nil
  :defer t
  :load-path "/opt/homebrew/Cellar/mu/1.6.3/share/emacs/site-lisp/mu/mu4e"
  ;; :general
  ;; (:keymaps 'mu4e-main-mode-map
  ;;           :states '(normal)
  ;;           "q"  (lambda () (interactive) (kill-buffer)))
  ;; :init
  ;; (defun mu4e-action-view-in-browser-webkit (msg)
  ;;   (let ((url (concat "file://" (mu4e~write-body-to-html msg))))
  ;;     (xwidget-webkit-browse-url url)))
  :config
  (general-define-key :keymaps 'mu4e-main-mode-map
                      :states 'normal
                      "q" (lambda () (interactive) (kill-buffer)))
  
  (setq message-send-mail-function 'smtpmail-send-it)

  ;; Make sure plain text email flow, not hard new line
  (setq mu4e-compose-format-flowed t)
  
  (setq mu4e-view-show-images t)
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; (add-to-list 'mu4e-view-actions
  ;; '("ViewInBrowser" . mu4e-action-view-in-browser-webkit) t)
  ;; ;; for Emacs don't support webkit, using built-in external browser function
  ;; ;; (add-to-list 'mu4e-view-actions
  ;; ;; '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  (setq mu4e-completing-read-function 'completing-read)
  ;; mbsync specific settings
  (setq mu4e-change-filenames-when-moving t)
  ;; Refresh every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")

  (setq mu4e-drafts-folder "/gmail/[Gmail]/Drafts")
  (setq mu4e-sent-folder "/gmail/[Gmail]/Sent Mail")
  (setq mu4e-refile-folder "/gmail/[Gmail]/All Mail")
  (setq mu4e-trash-folder "/gmail/[Gmail]/Trash")

  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'ask)
  ;; (setq mu4e-compose-context-policy 'pick-first)

  (setq mu4e-bookmarks '(("flag:flagged" "Flagged" ?f)
                         (:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key 117)
                         (:name "Today's messages" :query "date:today..now" :key 116)
                         (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key 119)
                         (:name "Messages with images" :query "mime:image/*" :key 112)))

  ;; (setq mu4e-maildir-shortcuts
  ;;       '(("/gmail/Inbox" . ?i)
  ;;         ("/gmail/[Gmail]/Sent Mail" . ?s)
  ;;         ("/gmail/[Gmail]/Trash" . ?t)
  ;;         ("/gmail/[Gmail]/Drafts" . ?d)
  ;;         ("/gmail/[Gmail]/All Mail" . ?a)
  ;;         ))
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "gmail"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "lasviceju@gmail.com")
                  (user-full-name . "Laz")
                  (mu4e-compose-signature . "Lasvice")
                  (mu4e-drafts-folder  . "/gmail/[Gmail]/Drafts")
                  (mu4e-sent-folder  . "/gmail/[Gmail]/Sent Mail")
                  (mu4e-refile-folder  . "/gmail/[Gmail]/All Mail")
                  (mu4e-trash-folder  . "/gmail/[Gmail]/Trash")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type . ssl)
                  ))
         (make-mu4e-context
          :name "outlook"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/outlook" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "lasviceju@outlook.com")
                  (user-full-name . "JU GUIYUAN")
                  (mu4e-refile-folder . "/outlook/Archive")
                  (mu4e-sent-folder   . "/outlook/Sent")
                  (mu4e-drafts-folder . "/outlook/Drafts")
                  (mu4e-trash-folder  . "/outlook/Deleted")
                  (smtpmail-smtp-server . "smtp-mail.outlook.com") ;; Different from the official instruction, don't know why
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type . starttls)
                  ))
         (make-mu4e-context
          :name "yeah"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/yeah" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "undercloud@yeah.net")
                  (user-full-name . "Undercloud")
                  (mu4e-drafts-folder  . "/yeah/&g0l6P3ux-")
                  (mu4e-sent-folder  . "/yeah/&XfJT0ZAB-")
                  (mu4e-refile-folder  . "/yeah/Archive")
                  (mu4e-trash-folder  . "/yeah/&XfJSIJZk-")
                  (smtpmail-smtp-server . "smtp.yeah.net")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type . ssl)
                  ))
         (make-mu4e-context
          :name "jp-gmail"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/jp-gmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "lasvice.jp@gmail.com")
                  (user-full-name . "lasvice")
                  (mu4e-drafts-folder  . "/jp-gmail/[gmail]/drafts")
                  (mu4e-sent-folder  . "/jp-gmail/[gmail]/sent mail")
                  (mu4e-refile-folder  . "/jp-gmail/[gmail]/all mail")
                  (mu4e-trash-folder  . "/jp-gmail/[gmail]/trash")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type . ssl)
                  ))
         (make-mu4e-context
          :name "tokyo-tech"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/tokyo-tech" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "ju.g.aa@m.titech.ac.jp")
                  (user-full-name . "JU GUIYUAN")
                  (mu4e-drafts-folder  . "/tokyo-tech/&Tgtm+DBN-") ; 下書き
                  (mu4e-sent-folder  . "/tokyo-tech/&kAFP4Q-BOX") ; 送信 BOX
                  ;; (mu4e-refile-folder  . "/jp-gmail/[gmail]/all mail") ; doesn't exists
                  (mu4e-trash-folder  . "/tokyo-tech/&MLQw33ux-") ; ゴミ箱
                  (smtpmail-smtp-server . "smtpv3.m.titech.ac.jp")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type . ssl)
                  ))
         (make-mu4e-context
          :name "anonymous-gmail"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/anonymous-gmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "lazjunos@gmail.com")
                  (user-full-name . "lazjunos")
                  (mu4e-drafts-folder  . "/anonymous-gmail/[gmail]/drafts")
                  (mu4e-sent-folder  . "/anonymous-gmail/[gmail]/sent mail")
                  (mu4e-refile-folder  . "/anonymous-gmail/[gmail]/all mail")
                  (mu4e-trash-folder  . "/anonymous-gmail/[gmail]/trash")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type . ssl)
                  ))
         ))
  (mu4e t) ;; Run mu4e in the background
  )

  ;; mu4e
  ;; (add-to-list 'load-path "/opt/homebrew/Cellar/mu/1.6.3/share/emacs/site-lisp/mu/mu4e")
  ;; (require 'mu4e)
  ;; (setq
  ;;  mue4e-headers-skip-duplicates  t
  ;;  mu4e-view-show-images t
  ;;  mu4e-view-show-addresses t
  ;;  mu4e-compose-format-flowed nil
  ;;  mu4e-date-format "%y/%m/%d"
  ;;  mu4e-headers-date-format "%Y/%m/%d"
  ;;  mu4e-change-filenames-when-moving t
  ;;  mu4e-attachments-dir "~/Downloads"

  ;;  mu4e-maildir       "~/Mail"   ;; top-level Maildir
  ;;  ;; note that these folders below must start with /
  ;;  ;; the paths are relative to maildir root
  ;;   )

  ;; ;; this setting allows to re-sync and re-index mail
  ;; ;; by pressing U
  ;; (setq mu4e-get-mail-command  "mbsync -a")

  ;; (setq message-send-mail-function   'smtpmail-send-it)
  ;; (setq user-full-name "JU GUIYUAN")
  ;; (setq user-mail-address "lasviceju@gmail.com")

  ;; (setq message-send-mail-function 'smtpmail-send-it
  ;;       ;; smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
  ;;       ;; smtpmail-auth-credentials '(("smtp.gmail.com" 587 "lasviceju@gmail.com" nil))
  ;;       ;; smtpmail-default-smtp-server "smtp.gmail.com"
  ;;       ;; smtpmail-smtp-server "smtp.gmail.com"
  ;;       ;; smtpmail-smtp-service 587
  ;;       starttls-gnutls-program "/opt/homebrew/bin/gnutls-cli"
  ;;       starttls-extra-arguments nil
  ;;       starttls-use-gnutls t)

  ;; (setq mu4e-contexts
  ;;       `(,(make-mu4e-context
  ;;           :name "Yeah"
  ;;           :enter-func (lambda () (mu4e-message "Entering Private context"))
  ;;           :leave-func (lambda () (mu4e-message "Leaving Private context"))
  ;;           :vars '((user-mail-address . "undercloud@yeah.net")
  ;;                   (user-full-name . "Undercloud")
  ;;                   (smtpmail-starttls-credentials . '(("smtp.gmail.com" 994 nil nil)))
  ;;                   (smtpmail-auth-credentials . '(("smtp.gmail.com" 994 "undercloud@yeah.net" nil)))
  ;;                   (smtpmail-smtp-server . "smtp.yeah.net")      
  ;;                   (smtpmail-smtp-service . 994)
  ;;                   (mu4e-refile-folder . "/yeah/Archive")
  ;;                   (mu4e-sent-folder . "/yeah/&XfJT0ZAB-")
  ;;                   (mu4e-drafts-folder . "/yeah/&g0l6P3ux-")
  ;;                   (mu4e-trash-folder . "/yeah/&XfJSIJZk-")
  ;;                   (mu4e-compose-signature .
  ;;                                           (concat
  ;;                                            "Undercloud\n"
  ;;                                            "From nowhere\n"))))
  ;;         ,(make-mu4e-context
  ;;           :name "Outlook"
  ;;           :enter-func (lambda () (mu4e-message "Switch to the Work context"))
  ;;           ;; no leave-func
  ;;           ;; we match based on the maildir of the message
  ;;           ;; this matches maildir /Arkham and its sub-directories
  ;;           :vars '( ( user-mail-address . "lasviceju@outlook.com" )
  ;;                    ( user-full-name . "JU GUIYUAN" )
  ;;                    (smtpmail-starttls-credentials . '(("smtp.office365.com" 589 nil nil)))
  ;;                    (smtpmail-auth-credentials . '(("smtp.office365.com" 589 "lasviceju@outlook.com" nil)))
  ;;                    (smtpmail-smtp-server . "smtp.office365.com")      
  ;;                    (smtpmail-smtp-service . 589)
  ;;                    (mu4e-refile-folder . "/outlook/Archive")
  ;;                    (mu4e-sent-folder   . "/outlook/Sent")
  ;;                    (mu4e-drafts-folder . "/outlook/Drafts")
  ;;                    (mu4e-trash-folder  . "/outlook/Deleted")
  ;;                    ( mu4e-compose-signature  .
  ;;                      (concat
  ;;                       "JU GUIYUAN\n"
  ;;                       ;; "Miskatonic University, Dept. of Occult Sciences\n"))))
  ;;                       "Tokyo, Japan"))))

  ;;         ))

  ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
  ;; guess or ask the correct context, e.g.

  ;; start with the first (default) context;
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  ;; (setq mu4e-context-policy 'pick-first)

  ;; compose with the current context is no context matches;
  ;; default is to ask
  ;; (setq mu4e-compose-context-policy nil)
(use-package alert
  :config
  (if (eq system-type 'darwin)
      (setq
       ;; alert-default-style 'notifier
       alert-default-style 'osx-notifier
       )))

(use-package mu4e-alert
  :straight
  (:type git :host github :repo "iqbalansari/mu4e-alert"
         :fork (:host github :repo "xzz53/mu4e-alert")) ;; Fixed the compatibility with mu4e 1.6.3
  :hook
  (after-init . mu4e-alert-enable-notifications)
  :custom
  (mu4e-alert-email-notification-types '(count))
  :config
  (mu4e-alert-set-default-style 'osx-notifier))

(use-package mu4e-views
  :disabled t
  :after mu4e
  :defer nil
  :bind (:map mu4e-headers-mode-map
	    ("v" . mu4e-views-mu4e-select-view-msg-method) ;; select viewing method
	    ("M-n" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
	    ("M-p" . mu4e-views-cursor-msg-view-window-up) ;; from headers window scroll the email view
        ("f" . mu4e-views-toggle-auto-view-selected-message) ;; toggle opening messages automatically when moving in the headers view
        ("i" . mu4e-views-mu4e-view-as-nonblocked-html) ;; show currently selected email with all remote content
	    )
  :config
  (setq mu4e-views-completion-method 'completing-read) ;; use ivy for completion
  (setq mu4e-views-default-view-method "html") ;; make xwidgets default
  (mu4e-views-mu4e-use-view-msg-method "html") ;; select the default
  (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window) ;; when pressing n and p stay in the current window
  (setq mu4e-views-auto-view-selected-message t)) ;; automatically open messages when moving in the headers view

(use-package simple-modeline
  :disabled t
  :hook (after-init . simple-modeline-mode))

(use-package vs-modeline
  :disabled t
  :straight (vs-modeline :type git
                         :host github
                         :repo "VojtechStep/vs-modeline.el")
  :demand t
  :config
  (vs-modeline-mode))

(use-package smart-mode-line
  :disabled t
  :config
  (sml/setup))

(use-package sky-color-clock
  :disabled
  :straight (:type git :host github :repo "zk-phi/sky-color-clock")
  :config
  (setq sky-color-clock-enable-emoji-icon t)
  (sky-color-clock-initialize 35)
  (push '(:eval (sky-color-clock)) (default-value 'mode-line-format)))

(use-package org-mime
  :hook
  (org-mime-html . (lambda ()
                     (org-mime-change-element-style
                      "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                                    "#E6E1DC" "#232323"))))
  (message-send . org-mime-htmlize)
  :init
  (setq org-mime-export-options '(:section-numbers nil
                                                   :with-author nil
                                                   :with-toc nil)))
;; Colorize hex code
(use-package rainbow-mode
  :config
  (rainbow-mode))

(use-package fireplace)

(use-package snow
  :straight (snow :host github :repo "alphapapa/snow.el"))

(use-package helpful
    :general
    (:keymaps 'override
              :states '(normal insert emacs visual)
              "C-h f" #'helpful-callable
              "C-h v" #'helpful-variable
              "C-h k" #'helpful-key
              ;; "C-c C-d" #'helpful-at-point
              "C-h F" #'helpful-function
              "C-h C" #'helpful-command))

(use-package org-alert
  :custom
  (org-alert-interval 20)
  :config
  (org-alert-enable))

(use-package eradio
  :custom
  (eradio-player '("mpv" "--no-video" "--no-terminal"))
  :init
  ;; https://www.internet-radio.com
  (setq eradio-channels '(("def con - soma fm" . "https://somafm.com/defcon256.pls")          ;; electronica with defcon-speaker bumpers
                          ("Drone Zone - soma fm" . "http://somafm.com/dronezone130.pls")
                          ("Vaporwaves - soma fm" . "http://somafm.com/vaporwaves130.pls")
                          ("Deep Space One - soma fm" . "http://somafm.com/deepspaceone130.pls")
                          ("Smooth Jazz Florida" . "http://us4.internet-radio.com:8266/listen.pls")
                          ("cyberia - lainon"  . "https://lainon.life/radio/cyberia.ogg.m3u") ;; cyberpunk-esque electronica
                          ("cafe - lainon"     . "https://lainon.life/radio/cafe.ogg.m3u"))))  ;; boring ambient, but with lain)

;; (defun my/launcher ()
;;   (interactive)
;;   (with-current-buffer (get-buffer-create "*launcher*")
;;     (let ((frame (make-frame `((auto-raise . t)
;;                                (name . "launcher")
;;                                (minibuffer . only)
;;                                (height . 10)
;;                                (internal-border-width . 20)
;;                                (left . 0.33)
;;                                (left-fringe . 0)
;;                                (line-spacing . 3)
;;                                (menu-bar-lines . 0)
;;                                (right-fringe . 0)
;;                                (tool-bar-lines . 0)
;;                                (top . 48)
;;                                (undecorated . t) ; enable to remove frame border
;;                                ;; (undecorated . nil) ; enable to remove frame border
;;                                (unsplittable . t)
;;                                (vertical-scroll-bars . nil)
;;                                (width . 110)))))
;;       (set-face-background 'internal-border nano-light-subtle frame)
;;       (execute-extended-command nil)
;;       ;; (completing-read "Emacs acronyms: "
;;       ;;                  '(" Emacs: Escape-Meta-Alt-Control-Shift \n"
;;       ;;                    " Emacs: Eight Megabytes And Constantly Swapping "
;;       ;;                    " Emacs: Even a Master of Arts Comes Simpler "
;;       ;;                    " Emacs: Each Manual's Audience is Completely Stupified "
;;       ;;                    " Emacs: Eventually Munches All Computer Storage "
;;       ;;                    " Emacs: Eradication of Memory Accomplished with Complete Simplicity "
;;       ;;                    " Emacs: Easily Maintained with the Assistance of Chemical Solutions "
;;       ;;                    " Emacs: Extended Macros Are Considered Superfluous "
;;       ;;                    " Emacs: Every Mode Accelerates Creation of Software "
;;       ;;                    " Emacs: Elsewhere Maybe All Commands are Simple "
;;       ;;                    " Emacs: Emacs Makes All Computing Simple "
;;       ;;                    " Emacs: Emacs Masquerades As Comfortable Shell "
;;       ;;                    " Emacs: Emacs My Alternative Computer Story "
;;       ;;                    " Emacs: Emacs Made Almost Completely Screwed "
;;       ;;                    " Emacs: Each Mail A Continued Surprise "
;;       ;;                    " Emacs: Eating Memory And Cycle-Sucking "
;;       ;;                    " Emacs: Elvis Masterminds All Computer Software "
;;       ;;                    " Emacs: Emacs Makes A Computer Slow" ))
;;       (delete-frame frame)
;;       (kill-buffer "*launcher*"))))

(use-package elfeed-org
  :init
  (setq rmh-elfeed-org-files (list "~/Space/Notes/Org Roam/elfeed.org"))
  :config
  (elfeed-org))

(use-package elfeed
  :hook
  (elfeed-show-mode . (lambda ()
                        (turn-on-olivetti-mode)
                        (hide-mode-line-mode))))

(use-package ace-link
  :general
  (:keymaps '(Info-mode-map
              help-mode-map
              woman-mode-map
              eww-mode-map
              compilation-mode-map
              helpful-mode-map
              org-mode-map
              elfeed-show-mode-map
              mu4e-view-mode-map)
            :states 'normal
            "f" 'ace-link))

(use-package hledger-mode
:disabled
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :bind (("C-c j" . hledger-run-command)
         :map hledger-mode-map
         ("C-c e" . hledger-jentry)
         ("M-p" . hledger/prev-entry)
         ("M-n" . hledger/next-entry))
  :init
  (add-to-list 'company-backends 'hledger-company)
  (setq hledger-jfile "~/Finance/2021.journal")
  )

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one t)
  )
(use-package focus)

(use-package elisp-demos
  :config
  ;; (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package mlscroll
  :straight (:type git :host github :repo "jdtsmith/mlscroll")
  :config
  (setq mlscroll-shortfun-min-width 11) ;truncate which-func, for default mode-line-format's
  (mlscroll-mode 1))

(use-package pocket-reader
  :bind
  (:map pocket-reader-mode-map
        ()))

(use-package literate-calc-mode)

(use-package annotate)

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package hyperbole
  :disabled
  :bind
  (:map hyperbole-mode-map
        ("S-<return>" . hkey-either)))

(use-package slack
  :demand
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "Programming System Group"
   :default t
   :token (auth-source-pick-first-password
           :host "psg-titech.slack.com"
           :login "lasviceju@gmail.com")
   :cookie (auth-source-pick-first-password
            :host "psg-titech.slack.com"
            :user "lasviceju@gmail.com^cookie")
   :subscribed-channels '(calendar events general random seminar thesis embedded)
   :full-and-display-names t)
  :config
  (slack-start))

(use-package emms
  :disabled
  :config
  (emms-all)
  (emms-default-players))

(use-package chrome
  :disabled
  :straight (:type git :host github :repo "anticomputer/chrome.el"))

(use-package indium
  :disabled)

(use-package kite-mini
  :disabled)

(use-package wooky
  :straight (:type git :host github :repo "katspaugh/wooky.el")
  :bind
  (:map org-mode-map
        ("S-<return>" . my/video-noter))
  :config
  (defun my/eval-in-youtube (exp &optional callback)
    "(callback value)"
    (wooky--call-rpc
     "Runtime.evaluate"
     (list
      :expression
      exp
      :returnByValue t)
     (lambda (data)
       (if callback
           (funcall callback (wooky--on-eval data))
         (wooky--on-eval data)))))

  (defun my/insert-youtube-current-time ()
    (interactive)
    (my/eval-in-youtube "Math.floor(document.getElementById('movie_player').getCurrentTime())"
                        (lambda (val)
                          (org-insert-heading)
                          (insert (my/format-youtube-ts (string-to-number val))))))

  (defun my/format-youtube-ts (second)
    (interactive)
    (let* ((hour (/ second 3600))
           (minute (/ (mod second 3600) 60))
           (second (- second (* hour 3600) (* minute 60))))
      (format "%02d:%02d:%02d" hour minute second)))

  (defun my/youtube-ts-to-second (ts)
    (interactive)
    (let ((splited (-map 'string-to-number (s-split ":" ts))))
      (+ (* (first splited) 3600)
         (* (second splited) 60)
         (third splited))))

  (defun my/jump-to-youtube-time ()
    "ts STRING"
    (interactive)
    (if-let ((thing (first (s-split " " (org-entry-get nil "ITEM"))))
             (ts (string-match "^[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}$" thing))
             (sec (my/youtube-ts-to-second thing)))
        (if (bound-and-true-p wooky-mode)
            (progn (my/eval-in-youtube
                    (format "document.getElementById('movie_player').seekTo(%d)"
                            sec)))
          (progn (shell-command (concat "open -a \"/Applications/Google Chrome.app\" \""
                                 (format "%s&t=%ds"
                                         (org-entry-get (point-min) "ROAM_REFS")
                                         sec)
                                 "\""))
                 (wooky-mode)))
      (message "%s" "Cannot find youtube timestamp")))
  )

(use-package kaolin-themes)

(use-package sicp)

(use-package ctable)

(use-package xwwp-full
  :straight (:type git :host github :repo "BlueFlo0d/xwwp" :files ("*"))
  :general (:states '(normal visual)
            :maps xwidget-webkit-mode-map
              "F"  'xwwp-follow-link
              "f"  'xwwp-ace-toggle)
  :init
  (require 'xwwp-full))
