;; Install Cask/Pallet packages first

;;(require 'cask "/home/jbalint/sw/emacs-sw/cask/cask.el")
(require 'cask "/home/jbalint/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;;;(package-initialize)

(require 'font-lock)
(require 'cc-mode)
(setq-default indent-tabs-mode nil)
(setq global-font-lock-mode t
      font-lock-maximum-decoration t
	  gdb-many-windows 1
	  gdb-use-separate-io-buffer 1)

(setq c-default-style "linux"
	  c-basic-offset 4)

(ido-mode t)

;;(add-to-list 'auto-save-file-name-transforms '("\\(.*\\)" "~/tmp/emacs/\\1" t))
(setq auto-save-file-name-transforms `((".*" "~/tmp/emacs" t)))

(setq backup-directory-alist (cons '("." . "~/tmp/emacs") nil)
      ;;      default-tab-width 4
      display-time-day-and-date t
      inhibit-startup-message t
      )

(column-number-mode 1)
(show-paren-mode 1)
(transient-mark-mode 1)
(display-time)

(setq fill-column 119)

(load "~/.emacs.d/gud-yt.el")
(load (concat "~/.emacs.d/host-" system-name) 1)

(setq gud-jdb-use-classpath nil)
; gud-jdb-classpath - ...?
; gud-jdb-sourcepath - ...X
; gud-jdb-directories - :X

(setq default-tab-width 4)

;;;;;(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/sw/emacs-gargoyle")
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/ecb")
(add-to-list 'load-path "~/aur/ledger-git/src/ledger/lisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(require 'ecb nil t)

;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;;;(el-get 'sync)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "red" "green" "yellow" "blue" "magenta" "cyan" "yellow"])
 '(background-color nil)
 '(background-mode dark)
 '(browse-url-browser-function (quote browse-url-xdg-open))
 '(cursor-color nil)
 '(custom-safe-themes
   (quote
	("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(default-input-method (quote kannada-jessscript))
 '(ecb-options-version "2.40")
 '(foreground-color nil)
 '(menu-bar-mode nil)
 '(message-citation-line-function (quote message-insert-formatted-citation-line))
 '(notmuch-saved-searches
   (quote
	((:name "followup" :query "folder:followup")
	 (:name "inboxtwo" :query "tab:i2")
	 (:name "inbox" :query "tag:inbox")
	 (:name "unread" :query "tag:unread" :key "u")
	 (:name "flagged" :query "tag:flagged" :key "f")
	 (:name "sent" :query "tag:sent" :key "t")
	 (:name "drafts" :query "tag:draft" :key "d")
	 (:name "all mail" :query "*" :key "a")
	 (:name "folder:INBOX" :query "folder:INBOX"))))
 '(org-capture-templates (quote (("" "hi" entry (file "~/org/notes.org") ""))))
 '(org-modules
   (quote
	(org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m org-wl)))
 '(safe-local-variable-values
   (quote
	((eval progn
		   (require
			(quote color-theme))
		   (color-theme-initialize)
		   (color-theme-aalto-light))
	 (tags-table-list quote
					  ("/home/jbalint/sw/fabric-core-trunk/TAGS"))
	 (org-log-done . t)
	 (eval load-theme
		   (quote tango-dark))
	 (eval load-theme "wombat"))))
 '(scroll-bar-mode nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "stbeehive.oracle.com")
 '(smtpmail-smtp-service 465)
 '(smtpmail-stream-type (quote ssl))
 '(tool-bar-mode nil)
 '(user-full-name "Jess Balint")
 '(user-mail-address "jess.balint@oracle.com"))
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
(setq flora-program-name "~/sw/flora-src/flora2/runflora")

;;;;;;;;;;;;;;;;;;;;;;;
;; Window navigation ;;
;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-p") 'ace-window)
(require 'windmove)
;; (add-hook 'window-configuration-change-hook
;; 		  (lambda ()
;; 			(if (> 3 (count-windows))
;; 				(global-set-key (kbd "C-x o") 'other-window)
;; 			  (global-unset-key (kbd "C-x o"))
;; 			  (global-set-key (kbd "C-x o h") 'windmove-left)
;; 			  (global-set-key (kbd "C-x o k") 'windmove-up)
;; 			  (global-set-key (kbd "C-x o l") 'windmove-right)
;; 			  (global-set-key (kbd "C-x o j") 'windmove-down))))


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
			(auto-fill-mode t)

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


(defun my-display-buffer-in-side-window (a b)
  (message "DISPLAYING ORG AGENDA WINDOW PROPERLY")
  (display-buffer-in-side-window a b))

(progn
  (setq display-buffer-alist nil)
  (add-to-list 'display-buffer-alist
			   (cons (rx "*Org Agenda")
					 (cons 'display-buffer-in-side-window
						   '((side . right)
							 (window-width 0.5))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BBDB (config from link in .wl) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'bbdb-loaddefs)
(require 'bbdb)
(bbdb-initialize 'gnus 'message)
(bbdb-mua-auto-update-init 'gnus 'message)
(add-to-list 'bbdb-mua-mode-alist '(notmuch notmuch-message-mode))

(require 'bbdb)
(bbdb-initialize 'message)
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
	  ;; including "-" due to "3-" for SR # prefix
	  (skip-chars-backward "0123456789-")
	  (skip-chars-backward " #-")
	  (skip-chars-backward (concat (downcase prefix) (upcase prefix))))
	;; include optional "3-" for SR # prefix
	(if (looking-at (concat " *" prefix "\\( *#\\|-\\)\\(3-\\)?\\([0-9]+\\)\\b"))
		(concat (match-string 2) (match-string 3)))))

(defun oracle-mysql-bug-link (id)
  (if (> (string-to-number id) 500000)
	  (concat "http://clustra.no.oracle.com/orabugs/bug.php?id=" id)
	(concat "http://bugs.mysql.com/" id)))

(setq oracle-elem-link-urls-alist
	  '(("rb" . "http://rb.no.oracle.com/rb/r/")
		("wl" . "http://wl.no.oracle.com/?tid=")
		;; ("ham" . "http://tyr41.no.oracle.com:48080/jira/browse/HAM-")
		("ham" . "https://etools-jira.no.oracle.com:48443/jira/browse/HAM-")
		("bug" . oracle-mysql-bug-link)
		("mysqlconnj" . "https://jira.oraclecorp.com/jira/browse/MYSQLCONNJ-")
		("myc" . "https://jira.oraclecorp.com/jira/browse/MYC-")
		("my" . "https://jira.oraclecorp.com/jira/browse/MY-")
		("myp" . "https://jira.oraclecorp.com/jira/browse/MYP-")
		;; ("sr" . "https://mosemp.us.oracle.com/mosspui/src/sr/viewer/index.html#/")
        ("sr" . "https://mosemp.us.oracle.com/mosspui/src/sr/viewer/index.html#/")))
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
		(message "Oracle opening %s" id)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MySQL Connector/J code style settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((file-name (if (buffer-file-name) (buffer-file-name) "")))
  file-name)
(add-hook 'find-file-hook
		  (lambda nil
			(let ((file-name (if (buffer-file-name) (buffer-file-name) "")))
			  (unless (not (string-match "/cj" file-name))
				(auto-fill-mode 1)
				(setq indent-tabs-mode nil
					  fill-column 160)
				;; c.f. `c-offsets-alist'
				(c-set-offset 'arglist-cont '++)
				(c-set-offset 'arglist-cont-nonempty '++)
				(c-set-offset 'arglist-intro '++)
				(c-set-offset 'func-decl-cont '++)
				(c-set-offset 'inher-cont '++)
				(c-set-offset 'member-init-cont '++)
				(c-set-offset 'statement-cont '++)
				(c-set-offset 'case-label '+)))))
;; lets `fill-paragraph' work nicely with source code
(require 'cc-mode)
(require 'fillcode)
(add-hook 'java-mode-hook
		  (lambda nil
			(let ((file-name (if (buffer-file-name) (buffer-file-name) "")))
			  (when (string-match "/cj.*\.java" file-name)
				(flycheck-mode)
				(require 'fillcode)
				(fillcode-mode 1)))))
;; tweak this a little bit, order MATTERS
;; check default value for explanations
(setq fillcode-fill-points
  (list
   "<<[^<]\\|>>[^>]"
   "&&[^&]\\|||[^|]"
   (concat "[<>!=]=[^=]\\|\\s-<\\s-\\|\\s->\\s-")
   (concat "/[^=]\\|\\s-\\*\\s-\\|\\s--\\s-\\|\\s-\\+[^+=]")
   "[|~^][^&|=]"

   ";[^;]"
   ",[^,]"

   "[([][^]})({[]"
   "\\s-{[^({[]"
   ))

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;(set-default-font "Fixed Medium Semi-Condensed 10")
(set-default-font "Inconsolata:pixelsize=14:antialias=true:autohint=true")

(add-hook 'c-mode-common-hook
		  (lambda () (local-unset-key (kbd "C-c C-l"))))

(require 'w3m-load)
(put 'set-goal-column 'disabled nil)

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
         (;;(org-agenda-files '("~/org/widgets.org" "~/org/clients.org")) ;; (5)
          (org-agenda-sorting-strategy '(priority-up effort-down))) ;; (5) cont.
         ("~/computer.html")) ;; (6)
        ;; ...other commands here
		("A" "Wedding - need addr" tags-todo "ADDR")
		("x" "GTD Projects" tags-todo "project")
		("y" nil ((tags "project")
				  ))
		 
        ))

;; TODO make this nicer
;; TODO need `shr-browse-url' here? (when in notmuch HTML emails)
(defun add-oracle-elem-open-binding ()
  (interactive)
  "Add the C-c C-o binding to open an Oracle element or fall back to opening a URL"
  (local-set-key "\C-c\C-o"
				 (lambda () (interactive)
				   ;; try org link FIRST
				   (condition-case nil ;; could check the `major-mode' here
					   (org-open-at-point)
					 ;; fallback if `org-open-at-point' fails
					 (error (unless (oracle-elem-open)
							  (if (string= "No URL at point" (w3m-external-view-this-url))
								  (browse-url-at-point))))))))

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
(add-hook 'notmuch-search-hook 'add-oracle-elem-open-binding)
(add-hook 'notmuch-show-hook
		  (lambda () (interactive)
			(add-oracle-elem-open-binding)
			(flyspell-mode t)))
(add-hook 'notmuch-message-mode-hook
		  (lambda () (interactive)
			(add-oracle-elem-open-binding)
			(flyspell-mode t)))

(defun mail-to-cj ()
  "Draft a new message to C/J folks."
  (interactive)
  (notmuch-mua-mail "Alexander Soklakov <alexander.soklakov@oracle.com>, Filipe Silva <filipe.silva@oracle.com>"
					""
					'(("Cc" . "JDBC Reviewers <MYSQL-CONNECTORS-JAVA_GRP@oracle.com>"))))

;;;;;;;;;;;;;;;;;
;; ESS (for R) ;;
;;;;;;;;;;;;;;;;;
(require 'ess-site)

;;;;;;;;;;;;;;;;;;;;;
;; Solarized theme ;;
;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/sellout/emacs-color-theme-solarized
;; https://github.com/sellout/emacs-color-theme-solarized/issues/142
;;(setq custom-theme-load-path '("/home/jbalint/sw/emacs-sw/emacs-color-theme-solarized"))
(set-terminal-parameter nil 'background-mode 'dark)
;;(load-theme 'solarized t)

;;;;;;;;;;;;;
;; Haskell ;;
;;;;;;;;;;;;;
(load "haskell-mode-autoloads")
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(defun haskell-eval ()
  (interactive)
  (let ((sym (haskell-ident-at-point)))
	(message "%s" (inferior-haskell-get-result sym))))

(defun haskell-find ()
  "TODO needs a better name"
  (interactive)
  (let ((sym (haskell-ident-at-point)))
	(message "%s" (haskell-process-hoogle-ident sym))))

(add-hook 'haskell-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-c C-c") 'haskell-eval)
			(local-set-key (kbd "C-c C-f") 'haskell-find)
			(local-set-key (kbd "C-c RET")
						   (lambda () (interactive)
							 (inferior-haskell-get-result "main")))))

;;;;;;;;;;;;;;
;; Markdown ;;
;;;;;;;;;;;;;;
(autoload 'markdown-mode "markdown-mode" "Markdown mode" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;
;; Protocol Buffers ;;
;;;;;;;;;;;;;;;;;;;;;;
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))

;;;;;;;;;;;
;; OCaml ;;
;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?\\'" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
;(autoload 'ocamldebug "ocamldebug" "Run the Caml debugger" t)

;;;;;;;;;;;;;;;;;;;
;; Flycheck Java ;;
;;;;;;;;;;;;;;;;;;;
;; using my branch because the other one doesn't work with project settings
;; requires package "dash" and "s"
(add-to-list 'load-path "/home/jbalint/sw/emacs-sw/flycheck-java")
(setq flycheck-java-ecj-jar-path "/home/jbalint/sw/java-sw/ecj-4.4.2.jar")
;; have to do this after init....
(add-hook 'after-init-hook (lambda ()
							 (require 'flycheck)
							 (require 'flycheck-java)))

;;;;;;;;;;;
;; Idris ;;
;;;;;;;;;;;
(setq idris-interpreter-path "/home/jbalint/sw/idris/.cabal-sandbox/bin/idris")
(setq idris-repl-prompt-style 'long) ;; to prevent lambda chars on terminal, c.f. idris-repl.el

;;;;;;;;;;;;
;; Erlang ;;
;;;;;;;;;;;;
(add-hook 'erlang-mode-hook
		  (lambda () (setq indent-tabs-mode nil)))

;;;;;;;;;;;;;;;;
;; Projectile ;;
;;;;;;;;;;;;;;;;
;; you must set helm-projectile-fuzzy-match to nil before loading helm-projectile
(setq helm-projectile-fuzzy-match nil)
(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
;; Erlang/rebar3
(add-to-list 'projectile-globally-ignored-directories "_build")
;; Java
(add-to-list 'projectile-globally-ignored-directories "build")

;;;;;;;;;;;;;;;;;;
;; PDF Renaming ;;
;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/home/jbalint/sw/emacs-sw/pdf_renaming")
(require 'pdf-renaming-mode)

;;;;;;;;;;;
;; Scala ;;
;;;;;;;;;;;
;; (use-package ensime
;; 			 :commands ensime ensime-mode)
;; (add-hook 'scala-mode-hook 'ensime-mode)

;;;;;;;;;;;;
;; SPARQL ;;
;;;;;;;;;;;;

;;(sparql-set-base-url "http://admin:admin@localhost:5820/jora1/query")
;;(sparql-set-format "text/csv")

;;;;;;;;
;; N3 ;;
;;;;;;;;
(require 'n3-mode) ;; locally in .emacs.d/lisp/
(add-to-list 'auto-mode-alist '("\\.ttl\\'" . n3-mode))
(add-to-list 'auto-mode-alist '("\\.n3\\'" . n3-mode))

;;;;;;;;;;;;;
;; Kannada ;;
;;;;;;;;;;;;;
;; this is a real hack. lots of stuff copied from indian.el. not sure how it's supposed to work
(progn
  (require 'quail)
  (require 'indian)
  (require 'ind-util)
										;(quail-activate)
  ;; this is my modified version of the kannada input method. The "e" vowel is mapped wrong to z and w. I guess this base-table has them backwards in whatever file `indian-knd-base-table' is defined in. i swapped them here and made my own version and it works

  ;; o's are switched too
  (setq my-indian-knd-base-table
		'(
		  (;; VOWELS
		   (?ಅ nil) (?ಆ ?ಾ) (?ಇ ?ಿ) (?ಈ ?ೀ) (?ಉ ?ು) (?ಊ ?ೂ)
		   (?ಋ ?ೃ) (?ಌ nil) nil (?ಎ ?ೆ) (?ಏ ?ೇ) (?ಐ ?ೈ)
		   nil (?ಒ ?ೊ) (?ಓ ?ೋ) (?ಔ ?ೌ) (?ೠ ?ೄ) (?ೡ nil))
		  (;; CONSONANTS
		   ?ಕ ?ಖ ?ಗ ?ಘ ?ಙ                  ;; GUTTRULS
			  ?ಚ ?ಛ ?ಜ ?ಝ ?ಞ                  ;; PALATALS
			  ?ಟ ?ಠ ?ಡ ?ಢ ?ಣ                  ;; CEREBRALS
			  ?ತ ?ಥ ?ದ ?ಧ ?ನ nil              ;; DENTALS
			  ?ಪ ?ಫ ?ಬ ?ಭ ?ಮ                  ;; LABIALS
			  ?ಯ ?ರ ?ಱ ?ಲ ?ಳ nil ?ವ          ;; SEMIVOWELS
			  ?ಶ ?ಷ ?ಸ ?ಹ                    ;; SIBILANTS
			  nil nil nil nil nil nil ?ೞ nil      ;; NUKTAS
			  "ಜ್ಞ" "ಕ್ಷ")
		  (;; Misc Symbols
		   nil ?ಂ ?ಃ nil ?್ nil nil)
		  (;; Digits
		   ?೦ ?೧ ?೨ ?೩ ?೪ ?೫ ?೬ ?೭ ?೮ ?೯)
		  (;; Inscript-extra (4)  (#, $, ^, *, ])
		   "್ರ" "ರ್" "ತ್ರ" "ಶ್ರ" nil)))



  (quail-define-package "kannada-jessscript" "Kannada" "KndJ" t "Kannada keyboard Inscript")

  (defun quail-define-inscript-package (char-tables key-tables pkgname lang
													title docstring)
	(funcall 'quail-define-package pkgname lang title nil docstring
			 nil nil nil t nil nil nil nil)
	(let (char-table key-table char key)
	  (while (and char-tables key-tables)
		(setq char-table  (car char-tables)
			  char-tables (cdr char-tables)
			  key-table   (car key-tables)
			  key-tables  (cdr key-tables))
		(while (and char-table key-table)
		  (setq char       (car char-table)
				char-table (cdr char-table)
				key        (car key-table)
				key-table  (cdr key-table))
		  (if (and (consp char) (consp key))
			  (setq char-table (append char char-table)
					key-table  (append key  key-table))
			(if (and key char)
				(quail-defrule
				 (if (characterp key) (char-to-string key) key)
				 (if (stringp char)   (vector char) char))))))))

  (defvar inscript-dev-keytable
	'(
	  (;; VOWELS  (18)
	   (?D nil) (?E ?e) (?F ?f) (?R ?r) (?G ?g) (?T ?t)
	   (?+ ?=) ("F]" "f]") (?! ?@) (?Z ?z) (?S ?s) (?W ?w)
	   (?| ?\\) (?~ ?`) (?A ?a) (?Q ?q) ("+]" "=]") ("R]" "r]"))
	  (;; CONSONANTS (42)
	   ?k ?K ?i ?I ?U                ;; GRUTTALS
		  ?\; ?: ?p ?P ?}               ;; PALATALS
		  ?' ?\" ?\[ ?{ ?C              ;; CEREBRALS
		  ?l ?L ?o ?O ?v ?V             ;; DENTALS
		  ?h ?H ?y ?Y ?c                ;; LABIALS
		  ?/ ?j ?J ?n ?N "N]" ?b        ;; SEMIVOWELS
		  ?M ?< ?m ?u                   ;; SIBILANTS
		  "k]" "K]" "i]" "p]" "[]" "{]" "H]" "/]" ;; NUKTAS
		  ?% ?&)
	  (;; Misc Symbols (7)
	   ?X ?x ?_ ">]" ?d "X]" ?>)
	  (;; Digits
	   ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
	  (;; Inscripts
	   ?# ?$ ?^ ?* ?\])))

  (quail-define-inscript-package
   my-indian-knd-base-table inscript-dev-keytable
   "kannada-jessscript" "Kannada" "KndJ"
   "Kannada keyboard Inscript."))

;; Misc ;;

(defun notabs ()
  "Change the current buffer to use spaces instead of tabs"
  (interactive)
  (setq indent-tabs-mode nil))

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))
