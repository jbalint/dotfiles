;; Install Cask/Pallet packages first
(require 'cask "/home/jbalint/sw/emacs-sw/cask/cask.el")
(cask-initialize)

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

(add-to-list 'warning-suppress-types '(undo discard-info))

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
;; https://emacs.stackexchange.com/questions/3824/what-piece-of-code-in-emacs-makes-line-number-mode-print-as-line-number-i
(setq line-number-display-limit-width 2000000)

;;;;;(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/sw/emacs-gargoyle")
;;(add-to-list 'load-path "~/sw/emacs-sw/ledger-mode")
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/ecb")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path (concat (getenv "BS_HOME") "/percy"))
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
 '(browse-url-browser-function 'browse-url-xdg-open)
 '(custom-safe-themes
   '("00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "37c8c2817010e59734fe1f9302a7e6a2b5e8cc648cf6a6cc8b85f3bf17fececf" "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))
 '(default-input-method 'kannada-jessscript)
 '(ecb-options-version "2.40")
 '(geiser-chicken-binary "chicken-csi")
 '(menu-bar-mode nil)
 '(message-citation-line-function 'message-insert-formatted-citation-line)
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox -tag:save -tag:calcite -tag:save30days -tag:mailinglists -tag:spark -to:morganbecker@hotmail.com date:2w.." :key "i" :sort-order newest-first)
     (:name "unread" :query "tag:unread" :key "u" :sort-order newest-first)
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a" :sort-order newest-first)))
 '(org-capture-templates '(("" "hi" entry (file "~/org/notes.org") "")))
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m org-wl))
 '(package-selected-packages
   '(eglot-hierarchy gptel password-store paredit tide git-link shen-mode geiser-racket lsp-java geiser-chicken polymode vimrc-mode helm-flycheck flycheck-vdm eglot go-mode sqlformat dumb-jump bazel terraform-mode slime lsp-mode bazel-mode monokai-theme cargo yaml-mode typescript-mode company-racer racer graphql-mode racket-mode haskell-mode cmake-mode calfw ggtags wanderlust w3m sparql-mode rudel pallet markdown-mode magit lua-mode lispy ledger-mode idris-mode helm-projectile helm-ag groovy-mode flymake-easy flycheck-haskell find-file-in-project ess ensime emacs-eclim edts e2wm cider bbdb lsp-mode yasnippet lsp-treemacs helm-lsp projectile hydra flycheck company avy which-key helm-xref dap-mode))
 '(package-vc-selected-packages
   '((eglot-hierarchy :vc-backend Git :url "https://github.com/dolmens/eglot-hierarchy")))
 '(safe-local-variable-values '((org-log-done . t)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(user-full-name "Jess Balint")
 '(user-mail-address "jbalint@gmail.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;
;; Helm ;;
;;;;;;;;;;
(helm-mode 1)
;; List of times to show in helm-world-time
(setq display-time-world-list '(("America/Chicago" "Madison")
                                ("Europe/Berlin" "Heidelberg")
                                ("America/New_York" "NYC")
                                ("Europe/Moscow" "Moscow")
                                ("Pacific/Honolulu" "Hawaii")
                                ("UTC" "UTC")
                                ))
(global-set-key (kbd "C-x C-f") 'helm-find-files)

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
(setq org-clock-into-drawer nil) ; [SYS-107]
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
			(require 'ob-plantuml)
			(flyspell-mode t)
			(auto-fill-mode t)

			(org-babel-do-load-languages
			 'org-babel-load-languages
			 '((ditaa . t) ; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-ditaa.html
               (dot . t)))
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

(add-hook 'c-mode-common-hook
		  (lambda () (local-unset-key (kbd "C-c C-l"))))

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

;;;;;;;;;;;;;;;
;; bookmarks ;;
;;;;;;;;;;;;;;;
(setq bookmark-save-flag 0)
(setq bookmark-default-file "~/tmp/bookmarks")

;;;;;;;;;;;;
;; ledger ;;
;;;;;;;;;;;;
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
;; TODO : ledger mode doesn't define hooks?
(add-hook 'ledger-mode-hook
          (lambda () (interactive)
            ;; this is against convention? I thought it would be nil instead of -1
	    (electric-indent-local-mode -1)
            (setq-local tab-always-indent 'complete)
            (setq-local completion-cycle-threshold t)
            (setq-local ledger-complete-in-steps t)))

;;;;;;;;;;;;;;;;;
;; ESS (for R) ;;
;;;;;;;;;;;;;;;;;
;;(require 'ess-site)

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
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
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

;;(sparql-set-base-url "http://localhost:5820/jora1/query")
;;(sparql-set-format "text/csv")
(add-to-list 'auto-mode-alist '("\\.sms\\'" . sparql-mode))
(add-to-list 'auto-mode-alist '("\\.rq\\'" . sparql-mode))
(add-to-list 'auto-mode-alist '("\\.sms2\\'" . sparql-mode))

;;;;;;;;
;; N3 ;;
;;;;;;;;
(require 'n3-mode) ;; locally in .emacs.d/lisp/
(add-to-list 'auto-mode-alist '("\\.ttl\\'" . n3-mode))
(add-to-list 'auto-mode-alist '("\\.n3\\'" . n3-mode))

;;;;;;;;;;;
;; Slime ;;
;;;;;;;;;;;
(setq inferior-lisp-program "/usr/bin/sbcl")
;;(require 'slime-autoloads)

;;;;;;;;;;;;;;;
;; Bookmarks ;;
;;;;;;;;;;;;;;;
(setq bookmark-default-file "~/.emacs.bmk")

;;;;;;;;;;
;; Rust ;;
;;;;;;;;;;

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

;;;;;;;;;;;
;; Dired ;;
;;;;;;;;;;;
;; https://www.emacswiki.org/emacs/OperatingOnFilesInDired
(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))
(add-hook 'dired-mode-hook
  (lambda ()
    (local-unset-key (kbd "C-c o"))
    (define-key dired-mode-map (kbd "C-c o") 'dired-open-file)))

;;;;;;;;;;;;
;; Octave ;;
;;;;;;;;;;;;
(autoload 'octave-mode "octave-mod" nil)
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;;;;;;;;;
;; C++ ;;
;;;;;;;;;
;; https://ddavis.io/posts/eglot-cpp-ide/
(defun jb/projectile-proj-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(with-eval-after-load 'project
  (add-to-list 'project-find-functions
               'jb/projectile-proj-find-function))

;;;;;;;;;;
;; PDDL ;;
;;;;;;;;;;
(let ((downloaded-pddl-file "/home/jbalint/sw/emacs-sw/pddl-mode.el"))
  (cond ((file-exists-p downloaded-pddl-file)
         (progn (load-file downloaded-pddl-file)
                (add-to-list 'auto-mode-alist '("\\.PDDL$" . PDDL-mode))))))

;;;;;;;;;;;
;; gptel ;;
;;;;;;;;;;;
(setq
 gptel-model "claude-3-sonnet-20240229"
 gptel-backend
 (gptel-make-anthropic "Claude"
   :stream t
   :key (lambda ()
          (password-store-get "Insight/N88-726/gptel_api_key"))))

(setq
 gptel-model "gpt-4"
 gptel-backend
 (gptel-make-openai "ChatGPT"
   :stream t
   :models '("gpt-4-turbo")
   :key (lambda ()
          (password-store-get "Insight/N88-730/gptel_api_key"))))

;;;;;;;;;;;;;
;; notmuch ;;
;;;;;;;;;;;;;
(add-hook 'notmuch-search-hook
          (lambda () (define-key notmuch-search-mode-map "`" 'notmuch-search-apply-tag-macro)))

(setq notmuch-search-tag-macro-alist
      (list
       '("s" ("+save"))
       '("t" ("+save30days"))
       '("d" ("+deleted"))
       '("v" ("+calcite"))))

(defun notmuch-search-apply-tag-macro (key)
  (interactive "k")
  (let ((macro (assoc key notmuch-search-tag-macro-alist)))
    (apply 'notmuch-search-tag (cdr macro))))

(autoload 'notmuch "notmuch" "notmuch mail" t)

;;;;;;;;;;
;; TLA+ ;;
;;;;;;;;;;
(add-to-list 'load-path "~/sw/emacs-sw/tla-tools")

(require 'tla-tools)
;;(require 'tla-pcal-mode)

;;;;;;;;;;;;;
;; Paredit ;;
;;;;;;;;;;;;;
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)

;;;;;;;;;;;
;; Shell ;;
;;;;;;;;;;;
(defun shell-clear-and-run ()
  "Clear the shell buffer and rerun the last command"
  (interactive)
  (erase-buffer)
  (comint-previous-input 0)
  (comint-send-input))

(add-hook 'shell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x r e") 'shell-clear-and-run)))

;;;;;;;;;;;
;; Tide/Javascript ;;
;;;;;;;;;;;
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; if you use typescript-mode
(add-hook 'typescript-mode-hook #'setup-tide-mode)
;; if you use treesitter based typescript-ts-mode (emacs 29+)
(add-hook 'typescript-ts-mode-hook #'setup-tide-mode)

;;;;;;;;;;
;; Misc ;;
;;;;;;;;;;

;; use my normal colors in shell mode
;; c.f. https://stackoverflow.com/questions/25819034/colors-in-emacs-shell-prompt
(set-face-attribute 'comint-highlight-prompt nil :inherit nil)

(defun notabs ()
  "Change the current buffer to use spaces instead of tabs"
  (interactive)
  (setq indent-tabs-mode nil))

(put 'downcase-region 'disabled nil)

(setq sqlformat-command 'pgformatter)
(put 'erase-buffer 'disabled nil)

(load-theme 'modus-vivendi t)
