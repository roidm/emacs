;; -*- lexical-binding: t; -*-

;; (defmacro k-time (&rest body)
  ;; "Measure and return the time it takes evaluating BODY."
  ;; `(let ((time (current-time)))
     ;; ,@body
     ;; (float-time (time-since time))))

;; Set garbage collection threshold to 1GB.
;; (setq gc-cons-threshold #x40000000)

;; When idle for 15sec run the GC no matter what.
;; (defvar k-gc-timer
  ;; (run-with-idle-timer 15 t
                       ;; (lambda ()
                         ;; (message "Garbage Collector has run for %.06fsec"
                                  ;; (k-time (garbage-collect))))))

;;; Smart GC settings

;; Default values
(defvar gc-threshold-normal (* 16 1024 1024)) ;; 16MB
(defvar gc-threshold-high   most-positive-fixnum)
(defvar gc-percentage-original gc-cons-percentage)

;; Raise threshold during startup for faster load
(setq gc-cons-threshold gc-threshold-high
      gc-cons-percentage 0.6)

;; Functions to adjust GC
(defun gc-set-high ()
  "Set GC threshold to high for heavy operations."
  (setq gc-cons-threshold gc-threshold-high
        gc-cons-percentage 0.6))

(defun gc-set-normal ()
  "Restore GC threshold to normal."
  (setq gc-cons-threshold gc-threshold-normal
        gc-cons-percentage gc-percentage-original))

;; Restore normal GC after startup
(add-hook 'emacs-startup-hook #'gc-set-normal)

;; Temporarily raise GC when using minibuffer
(add-hook 'minibuffer-setup-hook #'gc-set-high)
(add-hook 'minibuffer-exit-hook
          (lambda () (run-at-time 1 nil #'gc-set-normal)))

;; Lower GC when idle to free memory
(run-with-idle-timer 5 t
                     (lambda ()
                       (garbage-collect)
                       (gc-set-normal)))

(message "Smart GC active")

;; Funciones de utilidad
(defun my-safe-require (feature)
  "Try to FEATURE and ignore errors if not found."
  (condition-case nil
      (require feature)
    (file-error nil)))

(setq package-enable-at-startup nil) ;; Disables the default package manager.

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
