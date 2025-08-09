;;; init.el --- -*- lexical-binding: t; -*-
(setq user-full-name "Roi DM"
	  user-mail-address "roidm@protonmail.com")

(setq copyright-names-regexp
      (format "%s <%s>" user-full-name user-mail-address))

;; Bootstraps `straight.el'
(setq straight-check-for-modifications nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package '(project :type built-in))
(straight-use-package 'use-package)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;; Emacs
(use-package emacs
  :ensure nil
  :custom
  (column-number-mode t)
  (auto-save-default nil)
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (delete-selection-mode 1)
  (global-auto-revert-non-file-buffers t)
  (history-length 25)
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (make-backup-files nil)
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
  (ring-bell-function 'ignore)
  (split-width-threshold 300)
  (switch-to-buffer-obey-display-actions t)
  (tab-always-indent 'complete)
  (tab-width 4)
  (treesit-font-lock-level 4)
  (truncate-lines t)
  (use-dialog-box nil)
  (electric-pair-mode 1)
  (use-short-answers t)
  (warning-minimum-level :emergency)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (global-display-line-numbers-mode 1)
  (add-to-list 'display-line-numbers-exclude-modes 'which-key-mode)
  :hook
  (prog-mode . display-line-numbers-mode)
  :config
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers)
  ;(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (global-hl-line-mode 1)
  (global-auto-revert-mode 1)
  (indent-tabs-mode -1)
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (winner-mode 1)
  (file-name-shadow-mode 1)
  (set-face-attribute 'default nil :family "JetBrainsMono NF"  :height 125 :weight 'medium)
  (defvar my-font-size-step 10)
  (defun my-increase-font-size ()
    (interactive)
    (set-face-attribute 'default nil :height (+ (face-attribute 'default :height) my-font-size-step))
    (message "Font size: %d" (face-attribute 'default :height)))
  (defun my-decrease-font-size ()
    (interactive)
    (let ((new-size (- (face-attribute 'default :height) my-font-size-step)))
      (when (>= new-size 50)
        (set-face-attribute 'default nil :height new-size)))
    (message "Font size: %d" (face-attribute 'default :height)))
  (global-set-key (kbd "C-+") #'my-increase-font-size)
  (global-set-key (kbd "C--") #'my-decrease-font-size)
  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (modify-coding-system-alist 'file "" 'utf-8))

(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
      (display-buffer-in-side-window) (window-height . 0.25) (side . bottom) (slot . 0))
     ("\\*\\(lsp-help\\)\\*"
      (display-buffer-in-side-window) (window-height . 0.25) (side . bottom) (slot . 0))
     ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
      (display-buffer-in-side-window) (window-height . 0.25) (side . bottom) (slot . 1)))))

;;; Dired
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open")
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mp" "xdg-open" "open")
     (".*" "open" "xdg-open")))
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  ;; Define evil-mode keys for a vim-like experience.
  (evil-define-key 'normal dired-mode-map
    ;; Navigation
    (kbd "h") 'dired-up-directory
    (kbd "l") 'dired-find-file-other-window ; Open in other window is often more useful
    (kbd "j") 'dired-next-line
    (kbd "k") 'dired-previous-line
    (kbd "G") 'dired-goto-file
    (kbd "gg") 'dired-first-line
    (kbd "/") 'dired-goto-root-directory
    (kbd "~") 'dired-home
    (kbd "RET") 'dired-find-file
    (kbd "i") 'dired-maybe-insert-subdir
    ;; Marking
    (kbd "m") 'dired-mark
    (kbd "u") 'dired-unmark
    (kbd "U") 'dired-unmark-all-marks
    (kbd "t") 'dired-toggle-marks
    ;; File Operations
    (kbd "C-n") 'dired-create-file
    (kbd "C-d") 'dired-create-directory
    (kbd "R") 'dired-do-rename
    (kbd "D") 'dired-do-delete
    (kbd "C") 'dired-do-copy
    (kbd "X") 'dired-open-file ; Use dired-open to open externally
    (kbd "M") 'dired-do-chmod
    (kbd "O") 'dired-do-chown))

;; dired-x for additional functionality
(use-package dired-x
  :ensure nil
  :after dired
  :custom (dired-x-hands-off-my-keys nil)
  :config
  ;; Define dired-omit-files to prevent void-variable errors
  (setq dired-omit-files "^\\.[^.]\\|^#\\|^\\.$\\|^\\.\\.$\\|\\.pyc$\\|\\.o$")
  (setq dired-omit-verbose nil))

(use-package diredfl
  :ensure t :straight t
  :after dired
  :hook (dired-mode . diredfl-mode))

(use-package dired-subtree
  :ensure t :straight t
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  (define-key dired-mode-map (kbd "<tab>") #'dired-subtree-toggle)
  (define-key dired-mode-map (kbd "<backtab>") #'dired-subtree-cycle))

(use-package isearch
  :ensure nil
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq search-whitespace-regexp ".*?")
  :bind (("C-s" . isearch-forward)
         ("C-r" . isearch-backward)))

(use-package vc
  :ensure nil
  :defer t
  :bind
  (("C-x v d" . vc-dir)
   ("C-x v =" . vc-diff)
   ("C-x v D" . vc-root-diff)
   ("C-x v v" . vc-next-action))
  :config
  (setq vc-annotate-color-map
        '((20 . "#f5e0dc")
          (40 . "#f2cdcd")
          (60 . "#f5c2e7")
          (80 . "#cba6f7")
          (100 . "#f38ba8")
          (120 . "#eba0ac")
          (140 . "#fab387")
          (160 . "#f9e2af")
          (180 . "#a6e3a1")
          (200 . "#94e2d5")
          (220 . "#89dceb")
          (240 . "#74c7ec")
          (260 . "#89b4fa")
          (280 . "#b4befe"))))

;;; Eldoc
(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-idle-delay 0)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-echo-area-display-truncation-message nil)
  :init
  (global-eldoc-mode))

;;; Org mode
(use-package org :ensure nil :defer t)

(use-package org-modern
  :ensure t :straight t :defer t :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-hide-stars "· "
        org-modern-star '("◉" "○" "◈" "◇" "◆" "▷")
        org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.1
        org-modern-block-name
        '(("src" "»" "«")
          ("example" "»" "«")
          ("quote" "❝" "❞"))
        org-modern-todo-faces
        '(("📥 TODO"      . (:foreground "#f38ba8" :weight bold))
          ("⚡ NEXT"      . (:foreground "#fab387" :weight bold))
          ("⚙️ PROG"      . (:foreground "#8aadf4" :weight bold))
          ("⏳ WAIT"      . (:foreground "#f9e2af" :weight bold))
          ("✅ DONE"      . (:background "#2f3c22" :foreground "#a6e3a1" :weight bold))
          ("❌ CANCEL"    . (:strike-through t :foreground "#b4befe"))
          ("📝 PLAN"      . (:foreground "#89dceb" :weight bold))
          ("🚀 ACTIVE"    . (:foreground "#cba6f7" :weight bold))
          ("⏸️ PAUSED"    . (:foreground "#cdd6f4" :weight bold))
          ("🏆 ACHIEVED"  . (:background "#364a5c" :foreground "#9ece6a" :weight bold :box t))
          ("🗑️ DROPPED"   . (:strike-through t :foreground "#b4befe")))
        org-modern-tag-faces
        `((:foreground ,(face-attribute 'default :foreground)
          :weight bold
          :box (:line-width (1 . -1) :color "#3b4261")))
        org-modern-checkbox
        '((todo . "☐") (done . "☑") (cancel . "☒")
          (priority . "⚑") (on . "◉") (off . "○"))))

(use-package toc-org
  :ensure t :straight t :defer t
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package org-faces
  :ensure nil :after org :defer t
  :config
  (let ((font-family "Firosevka")
        (font-weight 'medium))
    ;; Configura el título del documento
    (set-face-attribute 'org-document-title nil
                        :font font-family
                        :weight font-weight
                        :height 1.3)
    ;; Configura los diferentes niveles de encabezado
    (dolist (face-info '((org-level-1 . 1.2)
                         (org-level-2 . 1.1)
                         (org-level-3 . 1.05)
                         (org-level-4 . 1.0)))
      (set-face-attribute (car face-info) nil
                          :font font-family
                          :weight font-weight
                          :height (cdr face-info)))
    ;; Configura los niveles de encabezado que comparten la misma altura
    (dolist (face '(org-level-5 org-level-6 org-level-7 org-level-8))
      (set-face-attribute face nil
                          :font font-family
                          :weight font-weight
                          :height 1.1))))

(use-package org-bullets
  :ensure t  :straight t  :defer t
  :after org
  :hook (org-mode . org-bullets-mode))
(add-hook 'org-mode-hook 'org-indent-mode)

;;; Wich-key
(use-package which-key
  :ensure nil :defer t
  :hook
  (after-init . which-key-mode)
  :init
  (setq which-key-idle-delay 0.2
        which-key-separator " → "
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 3
        which-key-max-display-columns nil
        which-key-min-display-lines 8
        which-key-allow-imprecise-window-fit nil
        which-key-side-window-slot -10)
  :config
  (put 'which-key-replacement-alist 'initial-value which-key-replacement-alist)
  (which-key-setup-side-window-bottom))

(use-package vertico
  :ensure t :straight t :hook  (after-init . vertico-mode)
  :custom
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle t))

(use-package orderless
  :ensure t :straight t :defer t :after vertico
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t :straight t :hook (after-init . marginalia-mode))

(use-package yasnippet
  :ensure t :straight t :defer t
  :bind ("<f8>" . yas-insert-snippet)
  :config
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "var/snippets/" user-emacs-directory))
  (yas-global-mode 1))

(use-package consult
  :ensure t :straight t :defer t
  :bind (("C-s" . consult-line)
         ("M-o" . consult-outline)
         ("C-M-l" . consult-imenu)
         ("M-b" . consult-bookmark)
         ("C-<tab>" . consult-buffer)
         ("M-r" . consult-recent-file)
         ("M-y" . consult-yank-pop))
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;;; Embark
(use-package embark :ensure t :straight t :defer t)

(use-package embark-consult
  :ensure t :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Treesitter Auto
(use-package treesit-auto
  :ensure t :straight t
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

;;; Markdown mode
(use-package markdown-mode
  :straight t :ensure t :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;;; Corfu
(use-package corfu
  :ensure t :straight t :defer t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (corfu-quit-no-match 'separator)
  (corfu-auto-prefix 1)
  (corfu-quit-no-match t)
  (corfu-scroll-margin 5)
  (corfu-max-width 50)
  (corfu-min-width 50)
  (corfu-popupinfo-delay 0.5)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode t))

(use-package nerd-icons-corfu
  :ensure t :straight t :defer t
  :after (:all corfu))

(use-package eglot
  :ensure nil :straight (:type built-in)
  :defer t
  :hook (
         (js-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (typescript-ts-base-mode . eglot-ensure)
         (css-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (prisma-mode . eglot-ensure)
         (python-base-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (lua-ts-mode . eglot-ensure)
         (web-mode . eglot-ensure))
  :custom
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref t)
  (eglot-autoshutdown t)
  :config
  (dolist (def
           '( ((tsx-ts-mode typescript-ts-base-mode js-ts-mode)
                . ("typescript-language-server" "--stdio"))
             ((python-base-mode) . ("pylsp"))
             ((rust-ts-mode) . ("rust-analyzer"))
             ((go-ts-mode) . ("gopls"))
             ((css-mode) . ("css-languageserver" "--stdio"))
             ((lua-ts-mode) . ("lua-language-server"))
             ((web-mode) . ("vscode-html-language-server" "--stdio"))))
    (add-to-list 'eglot-server-programs def)))

;;; Eldoc box
(use-package eldoc-box :ensure t :straight t :defer t)

;;; Magit
(use-package magit
  :ensure t :straight t :defer t
  :bind ("C-x g" . magit-status)
  :config
  (setopt magit-format-file-function #'magit-format-file-nerd-icons))

;; Evil
(use-package evil
  :ensure t :straight t :defer t
  :hook
  (after-init . evil-mode)
  :init
  (setq evil-want-integration t
		evil-want-keybinding nil
		evil-want-C-u-scroll t
		evil-want-C-u-delete t
		evil-want-Y-yank-to-eol t)
  :config
  (evil-set-undo-system 'undo-tree)
  (setq evil-leader/in-all-states t)
  (setq evil-want-fine-undo t)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))
  (evil-mode 1))

(use-package evil-collection
  :defer t :straight t :ensure t
  :custom
  (evil-collection-want-find-usages-bindings t)
  ;; Hook to initialize `evil-collection' when `evil-mode' is activated.
  :hook
  (evil-mode . evil-collection-init))

(use-package evil-surround
  :ensure t :straight t
  :after evil-collection
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :ensure t :straight t
  :after evil-collection
  :config
  (global-evil-matchit-mode 1))

(use-package undo-tree
  :ensure t :straight t :defer t
  :hook
  (after-init . global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  (undo-limit (* 800000 32))
  (undo-strong-limit (* 12000000 32))
  (undo-outer-limit (* 120000000 32))
  (undo-tree-history-directory-alist
   `(("." . ,(expand-file-name "var/undo/" user-emacs-directory))))
  :config
  (make-directory (expand-file-name "var/undo/" user-emacs-directory) t))

;;; General
(use-package general
  :ensure t :straight t
  :after evil
  :demand t
  :config
  (general-evil-setup t)

  (general-create-definer my-leader-key
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (my-leader-key
	"SPC" '(execute-extended-command :wk "M-x")
	"." '(find-file :wk "find")
	"TAB TAB" '(comment-line :wk "Comment lines")

    ;; AI
    "a"  '(:ignore t :wk "AI")
    "am" '(chatgpt-shell-swap-model :wk "Change model")
    "as" '(chatgpt-shell :wk "Open shell")
    "ar" '(chatgpt-shell-send-to-buffer :wk "send buffer")
    "at" '(chatgpt-shell-send-region :wk "send region")
	;; ──────────────────────── HELP ─────────────────────────────
	"h"  '(:ignore t :which-key "Help")
	"hh" '(help-command :which-key "Help (menu)")
	"hs" '(describe-major-mode :which-key "Describe major mode")
	"hk" '(help-with-timer :which-key "Help timer")
	"hi" '(display-missing-keybindings :which-key "Babel…")
	"hr" '(reload-init-file :which-key "Reload configuration")
	;; ──────────────────────── BUFFER ────────────────────────────
	"b"  '(:ignore t :which-key "Buffers")
	"bb" '(switch-to-buffer :which-key "Switch")
	"bn" '(evil-next-buffer :which-key "Next")
	"bp" '(evil-previous-buffer :which-key "Previous")
	"bk" '(kill-current-buffer :which-key "Kill current buffer")
	"bK" '(kill-some-buffers :which-key "Kill buffers")
	"bc" '(comment-line :which-key "Toggle comment")
	"bl" '(which-key-show :which-key "Which-Key menu")
	;; ──────────────────────── FILE ──────────────────────────────
	"f"  '(:ignore t :which-key "Files")
	"ff" '(consult-find          :which-key "Find file")
	"fg" '(consult-git           :which-key "Git project")
	"fr" '(consult-recent-file   :which-key "Recents")
	"fa" '(org-agenda :which-key "Org agenda")
	"fe" '(find-file-other-window :which-key "Other window")
	"fm" '(magit-mode :which-key "Magit status")
	"fp" '(bookmark-jump :which-key "Bookmarks")
	;; ──────────────────────── GIT / MAGIT ─────────────────────
	"g" '(:ignore t :wk "Magit")
	"gs" '(magit-status :wk "status")
	"gc" '(magit-commit :wk "commit")
	"gC" '(magit-commit-amend :wk "commit amend")
	"gp" '(magit-push-current-to-pushremote :wk "push")
	"gP" '(magit-pull-from-upstream :wk "pull")
	"gb" '(magit-branch :wk "branches")
	"gl" '(magit-log-buffer-file :wk "log current file")
	"gL" '(magit-log-current :wk "log current branch")
	"gd" '(magit-diff-unstaged :wk "diff")
	"gf" '(magit-fetch :wk "fetch")
	"gm" '(magit-merge :wk "merge")
	"gr" '(magit-rebase :wk "rebase")
	;; ──────────────────────── SEARCH ─────────────────────────────
	"s"  '(:ignore t :which-key "Search")
	"ss" '(consult-line          :which-key "Search in buffer")
	"sg" '(consult-ripgrep       :which-key "Search project")
	"sh" '(consult-grep           :which-key "Grepped – advanced")
	;; ──────────────────────── LSP ───────────────────────────────
	"l"  '(:ignore t :which-key "Eglot")
	"ld" '(eglot-find-definition   :which-key "Go to def.")
	"lr" '(eglot-rename             :which-key "Rename")
	"li" '(eglot-code-actions       :which-key "Code actions")
	"lc" '(eglot-format-buffer      :which-key "Format")
	"ll" '(eglot-diagnostics-mode   :which-key "Toggle diag.")
	"lt" '(eglot-shutdown          :which-key "Shutdown")
	;; ──────────────────────── ORG MODE ─────────────────────────
	"o"  '(:ignore t :which-key "Org")
	"oa" '(org-agenda :which-key "Agenda")
	"ot" '(org-tags-view :which-key "Tags view")
	"od" '(org-deadline :which-key "Set deadline")
	"ox" '(org-export-dispatch :which-key "Export")
	"oj" '(org-journal-custom-toggle :which-key "Journal")
	"or" '(org-refile :which-key "Refile")
	"oi" '(org-insert-link :which-key "Link")
	"oh" '(org-habit :which-key "Habit")
	"og" '(org-gmail-threads :which-key "Gmail threads")
	;; ──────────────────────── EDIT / CODING ─────────────────────
	"e"  '(:ignore t :which-key "Edit/Code")
	"ec" '(compile :which-key "Compile T")
	"er" '(eval-region :which-key "Eval region")
	"eb" '(eval-buffer :which-key "Eval buffer")
	"et" '(comment-or-uncomment-region :which-key "Toggle comment")
	"ex" '(remove-text-properties :which-key "Clean buffer")
	"el" '(enable-lisp-mode :which-key "Lisp")
	"eL" '(escape-newline :which-key "No wrap")
	;; ──────────────────────── MISC ──────────────────────────────
	"w"  '(:ignore t :which-key "Workspace/Window")
	"wd" '(workspace-previous :which-key "Previous")
	"wf" '(workspace-switch-to :which-key "Switch")
	"wl" '(buffer-menu :which-key "Buffer list")
	"wr" '(rename-buffer :which-key "Rename")
	"wq" '(save-buffers-kill-terminal :which-key "Quit")
    "<left>" '(evil-window-left :wk "W left")
    "<down>" '(evil-window-down :wk "W down")
    "<up>" '(evil-window-up :wk "W up")
    "<right>" '(evil-window-right :wk "W right")

	"t"  '(:ignore t :which-key "Theme/UI")
	"tc" '(consult-theme :which-key "Check themes")
	"ta" '(doom-solarized-dark :which-key "Solarized dark")
	"tb" '(doom-one-bright :which-key "One bright")
	"tm" '(doom-moonlight-dark :which-key "Moonlight dark")
	"th" '(doom-themes-toggle :which-key "Toggle theme")

	"i"  '(:ignore t :which-key "Info/Settings")
	"if" '(customize-group :which-key "Custom")
	"ii" '(describe-key :which-key "Describe key")
	"ic" '(describe-function :which-key "Describe function")
	"is" '(describe-variable :which-key "Describe variable")
	"ig" '(god-mode-toggle :which-key "God mode")

	"q"  '(:ignore t :wk "quit")
	"qq" '(save-buffers-kill-terminal :wk "quit")
	"qr" '(restart-emacs)))

;;; Rainbow
(use-package rainbow-mode
  :defer t :straight t :ensure t
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :defer t :straight t :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;; dotenv
(use-package dotenv-mode :defer t :straight t :ensure t)

;;; Pulsar
(use-package pulsar
  :defer t :straight t :ensure t
  :hook
  (after-init . pulsar-global-mode)
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.025)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'evil-ex-lazy-highlight)
  (add-to-list 'pulsar-pulse-functions 'evil-scroll-down)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)
  (add-to-list 'pulsar-pulse-functions 'evil-yank)
  (add-to-list 'pulsar-pulse-functions 'evil-yank-line)
  (add-to-list 'pulsar-pulse-functions 'evil-delete)
  (add-to-list 'pulsar-pulse-functions 'evil-delete-line)
  (add-to-list 'pulsar-pulse-functions 'evil-jump-item)
  (add-to-list 'pulsar-pulse-functions 'diff-hl-next-hunk)
  (add-to-list 'pulsar-pulse-functions 'diff-hl-previous-hunk))

;;: dashboard , themes and modeline
(use-package dashboard
  :defer t :straight t :ensure t
  :bind
  ("<f5>" . dashboard-open)
  :init
  (defun my-dashboard-initial-buffer ()
	"Devuelve el buffer del dashboard, creándolo si es necesario."
	(require 'dashboard)
	(dashboard-insert-startupify-lists)
	(get-buffer-create "*dashboard*"))
  (setq initial-buffer-choice #'my-dashboard-initial-buffer)
  :config
  ;;(setq dashboard-startup-banner '1)
  (setq dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5))
        dashboard-center-content t
        dashboard-show-shortcuts nil))
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (get-buffer "*scratch*")
              (with-current-buffer "*scratch*"
                (read-only-mode -1)
                (lisp-interaction-mode)
                (setq buffer-undo-list nil)))))

(defun my-kill-mode-line-in-dashboard ()
  (when (string= (buffer-name) "*dashboard*")
	(setq mode-line-format nil)))
(add-hook 'window-configuration-change-hook #'my-kill-mode-line-in-dashboard)

(defun my-apply-font-to-frame (frame)
  "Set the default font for FRAME."
  (with-selected-frame frame
    (set-face-attribute 'default nil
                        :family "JetBrainsMono NF"
                        :height 126               ; 12.5 pt (≈ 12 px con DPI = 96)
                        :weight 'SemiBold)))

(my-apply-font-to-frame (selected-frame))
(add-hook 'after-make-frame-functions #'my-apply-font-to-frame)
(defvar my--default-font-size 125
  "Default font height in units of 1/10 pt.")

(defun my-apply-font-to-frame (frame)
  (with-selected-frame frame
    (set-face-attribute 'default nil
                        :family "JetBrainsMono NF"
                        :height my--default-font-size
                        :weight 'SemiBold)))
(when (display-graphic-p)
  (set-face-attribute 'default nil :height my--default-font-size))

(use-package doom-modeline
  :ensure t :straight t :defer t
  :custom
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-buffer-name t)
  (doom-modeline-vcs-max-length 25)
  :config
  (setq display-time-format "%a%e %b, %H:%M")
  (display-time-mode 1)
  (setq doom-modeline-icon t)
  :hook
  (after-init . doom-modeline-mode))

(use-package ef-themes :straight t)

(use-package doom-themes
  :ensure t :straight t
  :init
  (add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
  :config
  (setq doom-themes-enable-bold t
		doom-themes-enable-italic t)
  (load-theme 'doom-catppuccin t)
  (doom-themes-neotree-config))

;;; Neotree
(use-package neotree
  :ensure t :straight t :defer t
  :bind ("<f6>" . neotree-toggle)
  :custom
  (neo-show-hidden-files t)
  (neo-vc-integration '(face char))
  :defer t
  :config
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 35)
   (setq neo-theme 'nerd-icons))

;;; Nerd Icons
(use-package nerd-icons :ensure t :straight t :defer t)

(use-package nerd-icons-dired
  :ensure t :straight t :defer t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure t :straight t
  :after (:all nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;;; Pinentry
(require 'epa)
(require 'auth-source)
(setq auth-sources '("~/.authinfo.gpg"))
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(use-package pinentry
  :straight t :ensure t
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))
;;(setq auth-source-debug t)

(use-package pdf-tools
  :ensure t :straight t :defer t
  :commands (pdf-loader-install)
  :mode "\\.pdf\\'"
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page)
              ("C-=" . pdf-view-enlarge)
              ("C--" . pdf-view-shrink))
  :init (pdf-loader-install)
  :config (add-to-list 'revert-without-query ".pdf"))

;; Transient
(use-package transient
  :defer t
  :ensure nil
  :custom
  (transient-history-file
   (expand-file-name "var/transient/transient-history"
                     user-emacs-directory))
  (transient-delete-after-exit t)
  (transient-menu-split-change-keys '("q" ""
                                      "C-g" ""))
  :config
  (make-directory (file-name-directory transient-history-file) t))

;; ChatGPT
(use-package chatgpt-shell
  :straight t :defer t
  :commands (chatgpt-shell)
  :init
  (require 'auth-source)
  (require 'subr-x)
  (defun em-chatgpt-get-api-key ()
    "Retrieves the OpenAI key stored in `authinfo.gpg`."
    (let ((entry (car (auth-source-search :host "openai.com"
                                          :user "OPENAI_KEY"
                                          :require '(:user :secret)))))
      (when-let ((secret (plist-get entry :secret)))
        (if (functionp secret)
            (funcall secret)
          secret))))

  (defun em-chatgpt-get-gemini-api-key ()
    "Retrieves the Gemini key stored in `authinfo.gpg`."
    (let ((entry (car (auth-source-search :host "api.gemini.google.com"
                                          :user "oauth2-token"
                                          :require '(:user :secret)))))
      (when-let ((secret (plist-get entry :secret)))
        (if (functionp secret)
            (funcall secret)
          secret))))

  :config
  (setq chatgpt-shell-model "gpt-4.1-mini")
  (setq chatgpt-shell-openai-key (em-chatgpt-get-api-key))
  (setq chatgpt-shell-google-key (em-chatgpt-get-gemini-api-key)))

(define-minor-mode em-chatgpt-mode
  "Minor mode to enhance the chatgpt-shell experience."
  :lighter " EmGPT"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-s") #'em-chatgpt-send-prompt)
            (define-key map (kbd "C-c C-k") #'em-chatgpt-clear-buffer)
            map))

(defun em-chatgpt-clear-buffer ()
  "Clears the current chatgpt-shell buffer."
  (interactive)
  (when (derived-mode-p 'chatgpt-shell-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (message "chatgpt-shell buffer cleared"))))

(defun em-chatgpt-send-prompt (prompt)
  "Sends PROMPT to the current chatgpt-shell session."
  (interactive "sPrompt for ChatGPT: ")
  (unless (derived-mode-p 'chatgpt-shell-mode)
    (user-error "You are not in a chatgpt-shell buffer"))
  (chatgpt-shell-send-message prompt))

(global-set-key (kbd "C-c g") #'chatgpt-shell)

(use-package vterm
  :ensure t :straight t :defer t
  :bind ("M-<f7>" . vterm-toggle)
  :config
  (setq shell-file-name "/bin/zsh"
        vterm-max-scrollback 5000))

(use-package vterm-toggle
  :straight t
  :after vterm
  :config
  (evil-define-key 'normal vterm-mode-map (kbd "<escape>") 'vterm--self-insert)
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.4))))

;;; Helper to install fonts, treesitter parsers...
(defun my-first-install ()
  "Install tree-sitter grammars ..."
  (interactive)
  (switch-to-buffer "*Messages*")
  (message ">>> All required packages installed.")
  (message ">>> Configuring Emacs...")
  (message ">>> Configuring Tree Sitter parsers...")
  (require 'treesit-auto)
  (treesit-auto-install-all)
  (message ">>> Configuring Nerd Fonts...")
  (require 'nerd-icons)
  (nerd-icons-install-fonts)
  (message ">>> Emacs installed! :)")
  (read-key)
  (kill-emacs))

(provide 'init)

;; init.el ends here
