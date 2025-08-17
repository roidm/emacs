;; -*- lexical-binding: t; -*-

(defconst my/gc-threshold-normal (* 16 1024 1024)  ; 16 MiB
  "GC threshold used during normal operation.")

(defconst my/gc-threshold-high  most-positive-fixnum
  "GC threshold used while loading the rest of Emacs.")

(defconst my/gc-percentage-original gc-cons-percentage
  "The original gc‑cons‑percentage value.")

;; 1. Raise GC during start‑up
(setq gc-cons-threshold  my/gc-threshold-high
      gc-cons-percentage 0.6)

(defun my/gc-set-high ()
  "Raise the GC threshold for heavy operations."
  (setq gc-cons-threshold  my/gc-threshold-high
        gc-cons-percentage 0.6))

(defun my/gc-set-normal ()
  "Restore the GC threshold to its normal value."
  (setq gc-cons-threshold  my/gc-threshold-normal
        gc-cons-percentage my/gc-percentage-original))

;; 2. Disable file-name handlers and VC for the start‑up period
(defvar my/file-name-handler-alist-original file-name-handler-alist)
(defvar my/vc-handled-backends-original      vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

;; 3. Restore everything once the system has started
(add-hook 'emacs-startup-hook #'my/gc-set-normal)

;; 4. Keep GC light during minibuffer use
(add-hook 'minibuffer-setup-hook #'my/gc-set-high)
(add-hook 'minibuffer-exit-hook
          (lambda () (run-at-time 1 nil #'my/gc-set-normal)))

;; 5. Occasionally run GC when Emacs is idle
(run-with-idle-timer 5 t
                     (lambda ()
                       (garbage-collect)
                       (my/gc-set-normal)))

(message "Smart GC active")

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq inhibit-startup-message t
	  initial-scratch-message ""
	  use-dialog-box nil)

(defun my-safe-require (feature)
  "Try to FEATURE and ignore errors if not found."
  (condition-case nil
      (require feature)
    (file-error nil)))

(setq package-enable-at-startup nil) ;; Disables the default package manager.

(setq byte-compile-warnings '(not obsolete))

(when (featurep 'native-compile)
  (setq native-comp-deferred-compilation t
        straight-disable-native-compile t)
  (setq native-comp-async-report-warnings-errors init-file-debug
        native-comp-warning-on-missing-source init-file-debug))

(defun my-setup-history-files ()
  (let ((history-dir "~/.config/emacs/var/history/"))
    (unless (file-directory-p history-dir)
      (make-directory history-dir t))
    (setq savehist-file (expand-file-name "savehist" history-dir)
          recentf-save-file (expand-file-name "recentf" history-dir)
          save-place-file (expand-file-name "places" history-dir))))
(my-setup-history-files)

(let* ((var-dir (expand-file-name "var/" user-emacs-directory))
       (auto-save-list-dir (concat var-dir "auto-save-list/"))
       (auto-saves-dir (concat var-dir "auto-saves/"))
       (backups-dir (concat var-dir "backups/")))

  (mapc (lambda (dir) (make-directory dir t))
        (list auto-save-list-dir auto-saves-dir backups-dir))
  (setq auto-save-list-file-prefix (concat auto-save-list-dir "saves-"))
  (setq auto-save-file-name-transforms
        `((".*" ,auto-saves-dir t)))
  (setq backup-directory-alist
        `(("." . ,backups-dir))))
