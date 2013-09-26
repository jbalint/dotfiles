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

(setq default-tab-width 4)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d/lisp")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get 'sync)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;;;;;;;;;;;;;; '(org-agenda-files (quote ("~/Dropbox/important/org/essentia/parent_child_association_tool.org" "/home/jbalint/Dropbox/important/org/GTD.org" "/home/jbalint/Dropbox/important/org/aspen.org" "/home/jbalint/Dropbox/important/org/essentia.org" "/home/jbalint/Dropbox/important/org/mobile.org" "/home/jbalint/Dropbox/important/org/money.org" "/home/jbalint/Dropbox/important/org/music.org" "/home/jbalint/Dropbox/important/org/notes.org" "/home/jbalint/Dropbox/important/org/oracle.org" "/home/jbalint/Dropbox/important/org/personal.org" "/home/jbalint/Dropbox/important/org/oracle_work_log/OWL_2013_08_12.org" "/home/jbalint/Dropbox/important/org/oracle_work_log/OWL_2013_08_19.org" "/home/jbalint/Dropbox/important/org/oracle_work_log/OWL_2013_08_26.org" "/home/jbalint/Dropbox/important/org/oracle_work_log/OWL_2013_09_02.org")))
 '(tool-bar-mode nil)
 '(menu-bar-mode nil)
 '(safe-local-variable-values (quote ((org-log-done . t) (eval load-theme (quote tango-dark)) (eval load-theme "wombat")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Org-mode customizations
(setq org-log-done t)
(setq org-agent-file-regexp "*.org")
(setq org-directory "~/Dropbox/important/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (list org-directory (concat org-directory "/oracle_work_log")))

(setq org-mobile-directory (concat org-directory "/MobileOrg"))
(setq org-mobile-inbox-for-pull (concat org-directory "/mobile.org"))

(add-hook 'org-mode-hook (lambda () (local-set-key "\C-ca" 'org-agenda)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t))) ; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-ditaa.html
))

(setq org-todo-keywords
	  '((sequence "TODO" "|" "DONE")
		(sequence "|" "WAITING")
		(sequence "|" "CANCELED")))

; http://orgmode.org/manual/Code-evaluation-security.html
(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "ditaa")))  ; don't ask for ditaa
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
(setq org-src-fontify-natively t)

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

(set-default-font "Fixed Medium Semi-Condensed 10")

; use tab-width 8 for Oasis code
(add-hook 'find-file-hook
		  (lambda () (if (or (string-match "com/oasis.*\.java" (buffer-file-name))
							 (string-match "agora.*\.java" (buffer-file-name)))
						 (setq tab-width 8))))
(put 'erase-buffer 'disabled nil)

;; BBDB (config from link in .wl)
(require 'bbdb)
(bbdb-initialize)
(setq 
 bbdb-offer-save 1                        ;; 1 means save-without-asking

 
 bbdb-use-pop-up t                        ;; allow popups for addresses
 bbdb-electric-p t                        ;; be disposable with SPC
 bbdb-popup-target-lines  1               ;; very small
 
 bbdb-dwim-net-address-allow-redundancy t ;; always use full name
 bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs

 bbdb-always-add-address t                ;; add new addresses to existing...
 ;; ...contacts automatically
 bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx

 bbdb-completion-type nil                 ;; complete on anything

 bbdb-complete-name-allow-cycling t       ;; cycle through matches
 ;; this only works partially

 bbbd-message-caching-enabled t           ;; be fast
 bbdb-use-alternate-names t               ;; use AKA


 bbdb-elided-display t                    ;; single-line addresses

 ;; auto-create addresses from mail
 bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook   
 bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
 ;; NOTE: there can be only one entry per header (such as To, From)
 ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

 '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter"))
)

(if (not window-system)
	(progn
	  (require 'xclip)
	  (turn-on-xclip)))
