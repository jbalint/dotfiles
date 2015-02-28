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
(add-to-list 'load-path "~/.emacs.d/lisp/ecb")
(require 'ecb nil t)

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
 '(ansi-color-names-vector
   ["black" "red" "green" "yellow" "blue" "magenta" "cyan" "yellow"])
 '(background-color nil)
 '(background-mode dark)
 '(browse-url-browser-function 'browse-url-xdg-open)
 '(cursor-color nil)
 '(custom-safe-themes
   (quote
	("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(ecb-options-version "2.40")
 '(foreground-color nil)
 '(menu-bar-mode nil)
 '(notmuch-saved-searches
   (quote
	((:name "i2 - IMPORTANT inbox" :query "tab:i2" :key "i")
	 (:name "inbox" :query "tag:inbox")
	 (:name "unread" :query "tag:unread" :key "u")
	 (:name "flagged" :query "tag:flagged" :key "f")
	 (:name "sent" :query "tag:sent" :key "t")
	 (:name "drafts" :query "tag:draft" :key "d")
	 (:name "all mail" :query "*" :key "a"))))
 '(org-agenda-files nil)
 '(org-modules
   (quote
	(org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m org-wl)))
 '(safe-local-variable-values
   (quote
	((tags-table-list quote
					  ("/home/jbalint/sw/fabric-core-trunk/TAGS"))
	 (org-log-done . t)
	 (eval load-theme
		   (quote tango-dark))
	 (eval load-theme "wombat"))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "stbeehive.oracle.com")
 '(smtpmail-smtp-service 465)
 '(smtpmail-stream-type (quote ssl)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XSB and Flora-2 configurations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/sw/xsb-src/XSB/etc")
(autoload 'xsb-mode "prolog" "Major mode for editing XSB programs." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(setq prolog-program-name "~/sw/xsb-src/XSB/bin/xsb")
(setq auto-mode-alist (cons '("\\.P$" . xsb-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.fl[rih]$" . flora-mode) auto-mode-alist))
(autoload 'flora-mode "flora" "Major mode for editing Flora-2 programs." t)
(setq flora-program-name "~/sw/flora2bundle-0.99.5/flora2/runflora")
;(setq flora-program-name "~/sw/flora-src/flora2/runflora")

;;;;;;;;;;;;;;;;;;;;;;;
;; Window navigation ;;
;;;;;;;;;;;;;;;;;;;;;;;
(require 'windmove)
(add-hook 'window-configuration-change-hook
		  (lambda ()
			(if (> 3 (count-windows))
				(global-set-key (kbd "C-x o") 'other-window)
			  (global-unset-key (kbd "C-x o"))
			  (global-set-key (kbd "C-x o h") 'windmove-left)
			  (global-set-key (kbd "C-x o k") 'windmove-up)
			  (global-set-key (kbd "C-x o l") 'windmove-right)
			  (global-set-key (kbd "C-x o j") 'windmove-down))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode customizations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-log-done t)
(setq org-agenda-file-regexp "\\`[^.].*\\.org\\'")
(setq org-directory "~/Dropbox/important/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files
	  (list org-directory
			(concat org-directory "/essentia")
			(concat org-directory "/notes")
			(concat org-directory "/oracle")
			(concat org-directory "/oracle_work_log")))

(setq org-mobile-directory (concat org-directory "/MobileOrg"))
(setq org-mobile-inbox-for-pull (concat org-directory "/mobile.org"))

(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

(add-hook 'org-mode-hook
		  (lambda ()
			(local-set-key "\C-ca" 'org-agenda)
			(local-unset-key "\C-c\C-o")
			(local-set-key "\C-c\C-o"
						   (lambda () (interactive)
							 (unless (oracle-elem-open)
							   (org-open-at-point))))
			(require 'ob-plantuml)
			(flyspell-mode t)

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
  (not (or (string= lang "ditaa")
		   (string= lang "plantuml"))))  ; don't ask for ditaa
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BBDB (config from link in .wl) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;
;; Oracle crap ;;
;;;;;;;;;;;;;;;;;
(defun oracle-elem-id (prefix)
  "Get element id at from element link point given prefix, e.g WL #1234, HAM-567"
  (save-excursion
	(unless (string-equal
			 (downcase prefix)
			 (downcase (buffer-substring (point) (min (buffer-size) (+ (string-width prefix) (point))))))
	  ;; these are done sequentially in hopes that we won't get BEFORE the elem link
	  (skip-chars-backward "0123456789")
	  (skip-chars-backward " #-")
	  (skip-chars-backward (concat (downcase prefix) (upcase prefix))))
	(if (looking-at (concat " *" prefix "\\( *#\\|-\\)\\([0-9]+\\)\\b"))
		(match-string 2))))

(defun oracle-mysql-bug-link (id)
  (if (> (string-to-number id) 500000)
	  (concat "http://clustra.no.oracle.com/orabugs/bug.php?id=" id)
	(concat "http://bugs.mysql.com/" id)))

(setq oracle-elem-link-urls-alist
	  '(("rb" . "http://rb.no.oracle.com/rb/r/")
		("wl" . "http://wl.no.oracle.com/?tid=")
		;; ("ham" . "http://tyr41.no.oracle.com:48080/jira/browse/HAM-")
		("ham" . "https://etools-jira.no.oracle.com:48443/jira/browse/HAM-")
		("bug" . oracle-mysql-bug-link)))

(defun oracle-elem-open ()
  (interactive)
  ;; find an element id
  (let ((link-data
		 (catch 'loop
		   (dolist (elem-link-url oracle-elem-link-urls-alist)
			 (let ((e (oracle-elem-id (car elem-link-url))))
			   (if e (throw 'loop (list e (cdr elem-link-url)))))))))
	;; open it
	(when link-data
	  (let ((id (car link-data))
			(url-prefix-or-resolver (car (cdr link-data))))
		(browse-url
		 (if (stringp url-prefix-or-resolver)
			 (concat url-prefix-or-resolver id)
		   (funcall url-prefix-or-resolver id)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: this requires existence of xclip binary
(if (not window-system)
	(progn
	  (require 'xclip)
	  (turn-on-xclip)))

; use tab-width 8 for Oasis, Agora code
(add-hook 'find-file-hook
		  (lambda () (if (or (string-match "com/oasis.*\.java" (buffer-file-name))
							 (string-match "agora.*\.java" (buffer-file-name)))
						 (setq tab-width 8))))
(put 'erase-buffer 'disabled nil)

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;(set-default-font "Fixed Medium Semi-Condensed 10")
(set-default-font "Inconsolata:pixelsize=14:antialias=true:autohint=true")

(add-hook 'c-mode-common-hook
		  (lambda () (local-unset-key (kbd "C-c C-l"))))

(require 'w3m-load)
(put 'set-goal-column 'disabled nil)

(setq custom-theme-load-path '("/home/jbalint/sw/emacs-sw/emacs-color-theme-solarized"))
(load-theme 'solarized-dark t)
(put 'narrow-to-region 'disabled nil)

(setq cedet-java-jdk-root "~/sw/jdk8")
(setq semanticdb-javap-classpath '("~/sw/jdk8/jre/lib/rt.jar"))

(require 'reposition)
(defun recenter-scroll-offset (&optional offset)
  "Scroll the current window by recentering `offset' number of lines.

A prefix argument can be used to scroll backwards or more than one."
  (interactive "p")
  (let* ((offset (if offset offset -1))
		 (current-line (repos-count-screen-lines (window-start) (point))))
	(recenter-top-bottom (- current-line offset))))

(global-set-key (kbd "C-x n") 'recenter-scroll-offset)
(global-set-key (kbd "C-x p") (lambda () (interactive) (recenter-scroll-offset -1)))
(put 'upcase-region 'disabled nil)

(setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar")

(global-unset-key (kbd "<f1>"))
(global-set-key (kbd "<f1>") 'other-frame)

;;;; need this here I guess
;;;(defun org-defvaralias (new-alias base-variable &optional docstring)
;;;  "Compatibility function for defvaralias.
;;;Don't do the aliasing when `defvaralias' is not bound."
;;;  (declare ((indent 1)))
;;;  (when (fboundp 'defvaralias)
;;;    (defvaralias new-alias base-variable docstring)))
;;;(put 'org-defvaralias 'lisp-indent-function 1)

(fset 'yes-or-no-p 'y-or-n-p)


(setq org-agenda-custom-commands 
      '(("c" "Desk Work" tags-todo "computer" ;; (1) (2) (3) (4)
         ((org-agenda-files '("~/org/widgets.org" "~/org/clients.org")) ;; (5)
          (org-agenda-sorting-strategy '(priority-up effort-down))) ;; (5) cont.
         ("~/computer.html")) ;; (6)
        ;; ...other commands here
		("A" "Wedding - need addr" tags-todo "ADDR")
		("x" "GTD Projects" tags-todo "project")
		("y" nil ((tags "project")
				  ))
		 
        ))

(defun add-oracle-elem-open-binding ()
  "Add the C-c C-o binding to open an Oracle element or fall back to opening a URL"
  (local-set-key "\C-c\C-o"
				 (lambda () (interactive)
				   (unless (oracle-elem-open)
					 (if (string= "No URL at point" (w3m-external-view-this-url))
						 (browse-url-at-point))))))

(add-hook 'wl-message-redisplay-hook 'add-oracle-elem-open-binding)

(add-hook 'wl-summary-mode-hook 'add-oracle-elem-open-binding)

(add-hook 'wl-mail-setup-hook
		  (lambda () (interactive)
			(add-oracle-elem-open-binding)
			(flyspell-mode t)))

;;;;;;;;;;;;
;; ledger ;;
;;;;;;;;;;;;
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

;;;;;;;;;;;;;
;; notmuch ;;
;;;;;;;;;;;;;
(autoload 'notmuch "notmuch" "notmuch mail" t)
(require 'notmuch)
(setq notmuch-show-relative-dates nil)
(define-key notmuch-show-mode-map "d"
  (lambda ()
	(interactive)
	(notmuch-show-tag '("+deleted"))))
(define-key notmuch-search-mode-map "d"
  (lambda ()
	(interactive)
	(notmuch-search-tag '("+deleted"))))

;;;;;;;;;;;;;;;;;
;; ESS (for R) ;;
;;;;;;;;;;;;;;;;;
(require 'ess-site)
