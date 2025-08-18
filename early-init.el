;; -*- lexical-binding: t -*-

;;  Performance Tuning

;; Constants for GC thresholds
(defconst my/gc-threshold-normal (* 16 1024 1024) ; 16 MiB
  "GC threshold used during normal operation.")
(defconst my/gc-threshold-high most-positive-fixnum
  "GC threshold used while loading the rest of Emacs.")

;; Set high GC during startup
(setq gc-cons-threshold my/gc-threshold-high
      gc-cons-percentage 0.6)

;; Functions to adjust GC
(defun my/gc-set-high ()
  "Raise the GC threshold for heavy operations."
  (setq gc-cons-threshold my/gc-threshold-high
        gc-cons-percentage 0.6))

(defun my/gc-set-normal ()
  "Restore the GC threshold to its normal value."
  (setq gc-cons-threshold my/gc-threshold-normal
        gc-cons-percentage 0.1))

;; Restore GC after startup
(add-hook 'emacs-startup-hook #'my/gc-set-normal)

;; Keep GC light during minibuffer use
(add-hook 'minibuffer-setup-hook #'my/gc-set-high)
(add-hook 'minibuffer-exit-hook
          (lambda () (run-at-time 1 nil #'my/gc-set-normal)))

;; UI Settings

;; Disable GUI elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq inhibit-startup-message t
      initial-scratch-message ""
      use-dialog-box nil)

;; Startup Optimizations

;; Save original values
(defvar my/file-name-handler-alist-original file-name-handler-alist)
(defvar my/vc-handled-backends-original vc-handled-backends)

;; Disable heavy features during startup
(setq file-name-handler-alist nil
      vc-handled-backends nil)

;; Restore them after init
(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist my/file-name-handler-alist-original
                  vc-handled-backends my/vc-handled-backends-original)))

;; Package Management

;; Disable built-in package manager
(setq package-enable-at-startup nil)
(setq byte-compile-warnings '(not obsolete))

;; Native compilation settings
(when (featurep 'native-compile)
  (setq native-comp-deferred-compilation t)
  (unless init-file-debug
    (setq native-comp-async-report-warnings-errors nil)))

;; Bootstrap straight.el
(setq straight-check-for-modifications nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Load essential packages
(straight-use-package '(project :type built-in))
(straight-use-package 'use-package)

;; History and Files

;; Setup history files directory
(defun my/setup-history-files ()
  "Ensure history directories exist and set their locations."
  (let ((history-dir "~/.config/emacs/var/history/"))
    (unless (file-directory-p history-dir)
      (make-directory history-dir t))
    (setq savehist-file (expand-file-name "savehist" history-dir)
          recentf-save-file (expand-file-name "recentf" history-dir)
          save-place-file (expand-file-name "places" history-dir))))

(my/setup-history-files)

;; Setup auto-save and backup locations
(let* ((var-dir (expand-file-name "var/" user-emacs-directory))
       (auto-save-list-dir (concat var-dir "auto-save-list/"))
       (auto-saves-dir (concat var-dir "auto-saves/"))
       (backups-dir (concat var-dir "backups/")))
  (mapc (lambda (dir) (make-directory dir t))
        (list auto-save-list-dir auto-saves-dir backups-dir))
  (setq auto-save-list-file-prefix (concat auto-save-list-dir "saves-")
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        backup-directory-alist `(("." . ,backups-dir))))

(message "Early init completed")
