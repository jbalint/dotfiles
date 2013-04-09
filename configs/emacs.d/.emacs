; Put this in the main .emacs
; (load "~/.emacs.d/.emacs")

(require 'font-lock)
(require 'cc-mode)
(setq global-font-lock-mode t
      iswitchb-default-method 'samewindow
      font-lock-maximum-decoration t
      indent-tabs-mode nil
	  gdb-many-windows 1
	  gdb-use-separate-io-buffer 1)

(add-to-list 'load-path "~/.emacs.d/color-theme")
(add-to-list 'load-path "~/.emacs.d/org-mode/lisp")

; MySQL C conventions from
; http://forge.mysql.com/wiki/MySQL_Internals_Coding_Guidelines
(c-add-style "MySQL"
	     '("K&R"
	       (c-basic-offset . 2)
	       (c-comment-only-line-offset . 0)
	       (c-offsets-alist . ((statement-block-intro . +)
				   (knr-argdecl-intro . 0)
				   (substatement-open . 0)
				   (label . -)
				   (statement-cont . +)
				   (arglist-intro . c-lineup-arglist-intro-after-paren)
				   (arglist-close . c-lineup-arglist)
				   ))
	       ))

(defun mysql-c-mode ()
  (c-mode)
  (c-set-style "MySQL")
  (setq tab-width 8)
  (setq indent-tabs-mode nil)
  (setq comment-column 48))

; match C:\Work\odbc\odbc51_wk etc on Windows and ~/Desktop/Work/odbc on Unix
(setq auto-mode-alist (cons '("/Work/odbc" . mysql-c-mode) auto-mode-alist)
      backup-directory-alist (cons '("." . "~/.emacs.d/backup") nil)
      ;;      default-tab-width 4
      display-time-day-and-date t
      inhibit-startup-message t
      )

(column-number-mode 1)
(iswitchb-mode 1)
(show-paren-mode 1)
(transient-mark-mode 1)
(display-time)

(load "~/.emacs.d/gud-yt.el")
(load (concat "~/.emacs.d/host-" system-name) 1)

(setq gud-jdb-use-classpath nil)
; gud-jdb-classpath - ...?
; gud-jdb-sourcepath - ...X
; gud-jdb-directories - :X
