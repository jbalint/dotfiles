;;; spark-mode.el --- Spark mode, and its idiosyncratic commands

;; Modified from lisp-mode.el for emacs-21.1.  Tyson 8/15/03
;; $Revision: 1.5 $

;; Copyright (C) 1985, 1986, 1999, 2000, 2001 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: spark, languages

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The base major mode for editing Spark code (used also for Emacs Spark).
;; This mode is documented in the Emacs manual.

;;; Language structures
;;;
;;; Statements
;;;   import: names*
;;;   importall: module_name
;;;   export: symbol
;;;   {defpredicate (symbol var*) ...}   (Only args are   defpredicate: string)
;;;   {defaction (symbol var*)}
;;;   {deffunction (symbol var*)}
;;;   {defsymbol symbol}
;;;   {defpredicate string cue: [...] precondition: pred body: task <annotations>}
;;;   pred
;;; Expressions
;;;   (and predterm*)
;;;   (or predterm*)
;;;   (not predterm)
;;;   (exists [vars] predterm)
;;; Closures
;;;   {fun [var*] term}
;;;   {task [var*] task}
;;;   {pred [var*] pred}
;;; These are in tasks
;;;    [succeed: ]  (and [])
;;;    [fail: termexpr]
;;;    [finish:]             (only a task in repeat, forall, while)
;;;   [achieve: pred]                       (only at top level of defprocedure)
;;;   [conclude: pred]
;;;   [context: pred]
;;;   [retract: pred]
;;;   [retractall: [vars] pred]
;;;    [do: action]
;;;    [body: task]
;;;    [contiguous: task*]
;;;    [execute: taskclosure term*]
;;;    [parallel: task*]
;;;    [repeat: [vars] task]
;;;    [seq: task*]
;;;    [try: task*]
;;;    [select: (pred task)*]
;;;    [wait:   (pred task)*]
;;;    [while: [vars] pred task]
;;;    [forall: [vars] pred task]
;;; These are followed by predicates
;;;   precondition: pred                    (only at top level of defprocedure)
;;;   property: pred                        (only at top level of defprocedure)
;;;   newfact: pred                         (only at top level of defprocedure)
;;; These are special
;;;    cue:  <either [do:..], [newfact:..], [achieve..]>
;;;                                        (only at top level of defprocedure)
;;;    doc: string                         (only at top level of defprocedure)
;;;    determined: special-string          (only at top level of defprocedure)
;;;    comment: string        (only at top level of defprocedure or in a task)
;;;    label: symbol                                          (only in a task)

;;; Code:
;;; SPARK files are in spark-mode
(setq auto-mode-alist (cons '("\\.spark$" . spark-mode) auto-mode-alist))

;;; We use a variable from this library, so load the library now.
(require 'font-lock)

(defvar spark-mode-abbrev-table nil)

(defvar emacs-spark-mode-syntax-table
  (let ((table (make-syntax-table)))
    (let ((i 0))
      ;; Low number ASCII characters are non-alphanumeric
      (while (< i ?0)
	(modify-syntax-entry i "_   " table)
	(setq i (1+ i)))
      ;; Skip the integers
      (setq i (1+ ?9))
      ;; The rest to A are non-alphanumeric
      (while (< i ?A)
	(modify-syntax-entry i "_   " table)
	(setq i (1+ i)))
      ;; Those past Z, below a, are non-alphanumeric
      (setq i (1+ ?Z))
      (while (< i ?a)
	(modify-syntax-entry i "_   " table)
	(setq i (1+ i)))
      (setq i (1+ ?z))
      ;; Those past z, below 128, are non-alphanumeric
      (while (< i 128)
	(modify-syntax-entry i "_   " table)
	(setq i (1+ i)))
      ;; A space, tab, form feed are white space
      (modify-syntax-entry ?  "    " table)
      (modify-syntax-entry ?\t "    " table)
      (modify-syntax-entry ?\f "    " table)
      ;; Newline, CR are newlines
      (modify-syntax-entry ?\n ">   " table)
      ;; Give CR the same syntax as newline, for selective-display.
      (modify-syntax-entry ?\^m ">   " table)
      ;; A hash symbol is a comment-to-eol character
      (modify-syntax-entry ?\# "<   " table)
      ;; Back and forward ticks, comma, and hash are singlequotes
      ;;(modify-syntax-entry ?` "'   " table)
      ;;(modify-syntax-entry ?' "'   " table)
      ;;(modify-syntax-entry ?, "'   " table)
      ;; Used to be singlequote; changed for flonums.
      (modify-syntax-entry ?. "_   " table)
      ;;(modify-syntax-entry ?# "'   " table)
      ;; double quote is double quote
      (modify-syntax-entry ?\" "\"    " table)
      ;; backslash is backslash;   WMT - not sure about this?
      (modify-syntax-entry ?\\ "\\   " table)
      ;; Open paren is like open paren and matches close paren
      (modify-syntax-entry ?\( "()  " table)
      ;; Close paren is like close paren and matches open paren
      (modify-syntax-entry ?\) ")(  " table)
      ;; Open bracket is like open paren and matches close bracket
      (modify-syntax-entry ?\[ "(]  " table)
      ;; Close bracket is like close paren and matches open bracket
      (modify-syntax-entry ?\] ")[  " table)
      ;; Open brace is like open paren and matches close brace
      (modify-syntax-entry ?\{ "(}  " table)
      ;; Close brace is like close paren and matches open brace
      (modify-syntax-entry ?\} "){  " table))
    table))

;;; Hmmmm... not sure about these changes... WMT
(defvar spark-mode-syntax-table
  (let ((table (copy-syntax-table emacs-spark-mode-syntax-table)))
    ;;(modify-syntax-entry ?\[ "_   " table)
    ;;(modify-syntax-entry ?\] "_   " table)
    ;;(modify-syntax-entry ?\{ "_   " table)
    ;;(modify-syntax-entry ?\} "_   " table)
    ;; (modify-syntax-entry ?# "' 14bn" table)
    ;; (modify-syntax-entry ?| "\" 23b" table)
    table))

(define-abbrev-table 'spark-mode-abbrev-table ())

(defvar spark-imenu-generic-expression
  (list
   (list (purecopy "Predicate")
	 ;; Any number of whitespace(?) characters at the start of a line, followed by
	 ;;  defpredicate or deftask
	 ;; followed by at least one whitespace(?) character folowed by
	 ;; an open parenthesis followed by
	 ;;  a word constituent character, 
	 ;; followed by a set of either another word starting constiutent or a word character
	 (purecopy "^\\s-*{defpredicate\\s-+(\\(\\sw\\(\\sw\\|\\s_\\)+\\)") 1)  ;)
   ;; Task
   (list (purecopy "Task")
	 (purecopy "^\\s-*{deftask\\s-+(\\(\\sw\\(\\sw\\|\\s_\\)+\\)") 1)
   ;; Action
   (list (purecopy "Action")
	 (purecopy "^\\s-*{defaction\\s-+(\\(\\sw\\(\\sw\\|\\s_\\)+\\)") 1)
   ;; Function
   (list (purecopy "Function")
	 (purecopy "^\\s-*{deffunction\\s-+(\\(\\sw\\(\\sw\\|\\s_\\)+\\)") 1)
   ;; Symbol
   (list (purecopy "Symbol")
	 (purecopy "^\\s-*{defsymbol\\s-+\\(\\sw\\(\\sw\\|\\s_\\)+\\)") 1) ;; ?? WMT
   ;; Procedure
   (list nil
	 ;; NIL means these are at the top level of the menu
	 (purecopy "^\\s-*{procedure\\s-+\\\"\\([^\\\"]*\\)\\\"") 1)
   )

  "Imenu generic expression for Spark mode.  See `imenu-generic-expression'.")

(defun spark-mode-variables (spark-syntax)
  (cond (spark-syntax
	  (set-syntax-table spark-mode-syntax-table)))
  (setq local-abbrev-table spark-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat page-delimiter "\\|$" )) ;WMT Not sure what to do here
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'spark-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because spark-fill-paragraph should do the job.
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'spark-mode-auto-fill)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'spark-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'spark-indent-region)
  (make-local-variable 'parse-sexp-ignore-comments)  ;; WMT ???
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp "####* \\|(\\|{")	; WMT Not sure here.  Maybe also allow {
  (make-local-variable 'outline-level)
  (setq outline-level 'spark-outline-level)
  (make-local-variable 'comment-start)
  (setq comment-start "#")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)#+ *")  ;;WMT Do backslashes matter?
  (make-local-variable 'comment-add)
  (setq comment-add 1)			;default to `##' in comment-region ;WMT??
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'spark-comment-indent)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression spark-imenu-generic-expression)
  (make-local-variable 'multibyte-syntax-as-symbol)
  (setq multibyte-syntax-as-symbol t)
  (setq font-lock-defaults
    '(
;      (spark-font-lock-keywords
;       spark-font-lock-keywords-1 
;       spark-font-lock-keywords-2)
      spark-font-lock-keywords-2
	  nil nil ((?\_ . "w")) 
	  beginning-of-defun		;WMT ???
	  (font-lock-mark-block-function . mark-defun)))
;  (setq font-lock-defaults '((perl-font-lock-keywords
;			      perl-font-lock-keywords-1
;			      perl-font-lock-keywords-2)
;			     nil nil ((?\_ . "w")) nil
;			     (font-lock-syntactic-keywords
;			      . perl-font-lock-syntactic-keywords)
;			     (font-lock-syntactic-face-function
;			      . perl-font-lock-syntactic-face-function)
;			     (parse-sexp-lookup-properties . t)))
  )

(defun spark-outline-level ()
  "Spark mode `outline-level' function."
  (if (or (looking-at "(") (looking-at "{"))
      1000
    (looking-at outline-regexp)
    (- (match-end 0) (match-beginning 0))))


(defvar spark-mode-shared-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'spark-indent-line)
    (define-key map "\e\C-q" 'indent-sexp) ; WMT ???  Different function name??
    (define-key map "\177" 'backward-delete-char-untabify)
    ;; This gets in the way when viewing a Spark file in view-mode.  As
    ;; long as [backspace] is mapped into DEL via the
    ;; function-key-map, this should remain disabled!!
    ;;;(define-key map [backspace] 'backward-delete-char-untabify)
    map)
  "Keymap for commands shared by all sorts of Spark modes.")

(defvar emacs-spark-mode-map ()
  "Keymap for Emacs Spark mode.
All commands in `spark-mode-shared-map' are inherited by this map.")

(if emacs-spark-mode-map
    ()
  (let ((map (make-sparse-keymap "Emacs-Spark")))
    (setq emacs-spark-mode-map (make-sparse-keymap))
    (set-keymap-parent emacs-spark-mode-map spark-mode-shared-map)
    ;;(define-key emacs-spark-mode-map "\e\t" 'spark-complete-symbol)
    ;; (define-key emacs-spark-mode-map "\e\C-x" 'eval-defun)
    (define-key emacs-spark-mode-map [menu-bar] (make-sparse-keymap))
    (define-key emacs-spark-mode-map [menu-bar emacs-spark]
      (cons "Emacs-Spark" map))
    ;;(define-key map [edebug-defun]
    ;;  '("Instrument Function for Debugging" . edebug-defun))
    ;;(define-key map [byte-recompile]
    ;;  '("Byte-recompile Directory..." . byte-recompile-directory))
    ;;(define-key map [emacs-byte-compile-and-load]
    ;;  '("Byte-compile And Load" . emacs-spark-byte-compile-and-load))
    ;;(define-key map [byte-compile]
    ;;  '("Byte-compile This File" . emacs-spark-byte-compile))
    (define-key map [separator-eval] '("--")) ;WMT ???
    ;;(define-key map [eval-buffer] '("Evaluate Buffer" . eval-current-buffer))
    ;;(define-key map [eval-region] '("Evaluate Region" . eval-region))
    ;;(define-key map [eval-sexp] '("Evaluate Last S-expression" . eval-last-sexp))
    (define-key map [separator-format] '("--")) ;WMT ???
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . spark-indent-line))
    ;;(put 'eval-region 'menu-enable 'mark-active)
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)))

;;(defun emacs-spark-byte-compile ()
;;  "Byte compile the file containing the current buffer."
;;  (interactive)
;;  (if buffer-file-name
;;      (byte-compile-file buffer-file-name)
;;    (error "The buffer must be saved in a file first")))

;;(defun emacs-spark-byte-compile-and-load ()
;;  "Byte-compile the current file (if it has changed), then load compiled code."
;;  (interactive)
;;  (or buffer-file-name
;;      (error "The buffer must be saved in a file first"))
;;  (require 'bytecomp)
;;  ;; Recompile if file or buffer has changed since last compilation.
;;  (if (and (buffer-modified-p)
;;	   (y-or-n-p (format "Save buffer %s first? " (buffer-name))))
;;      (save-buffer))
;;  (let ((compiled-file-name (byte-compile-dest-file buffer-file-name)))
;;    (if (file-newer-than-file-p compiled-file-name buffer-file-name)
;;	(load-file compiled-file-name)
;;      (byte-compile-file buffer-file-name t))))

(defcustom emacs-spark-mode-hook nil
  "Hook run when entering Emacs Spark mode."
  :options '(turn-on-eldoc-mode imenu-add-menubar-index checkdoc-minor-mode)
  :type 'hook
  :group 'spark)

(defcustom spark-mode-hook nil
  "Hook run when entering Spark mode."
  :options '(imenu-add-menubar-index)
  :type 'hook
  :group 'spark)

(defcustom spark-interaction-mode-hook nil
  "Hook run when entering Spark Interaction mode."
  :options '(turn-on-eldoc-mode)
  :type 'hook
  :group 'spark)

(define-derived-mode emacs-spark-mode nil "Emacs-Spark"
  "Major mode for editing Spark code to run in Emacs.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Hashes start comments.
\\{emacs-spark-mode-map}
Entry to this mode calls the value of `emacs-spark-mode-hook'
if that value is non-nil."
  (spark-mode-variables nil)
  (setq imenu-case-fold-search nil))

(defvar spark-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map spark-mode-shared-map)
    ;; (define-key map "\e\C-x" 'spark-eval-defun)
    ;; (define-key map "\C-c\C-z" 'run-spark)
    map)
  "Keymap for ordinary Spark mode.
All commands in `spark-mode-shared-map' are inherited by this map.")

(define-derived-mode spark-mode nil "Spark"
  "Major mode for editing Spark code for Sparks other than GNU Emacs Spark.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Hashes start comments.
\\{spark-mode-map}
Note that `run-spark' may be used either to start an inferior Spark job
or to switch back to an existing one.

Entry to this mode calls the value of `spark-mode-hook'
if that value is non-nil."
  (spark-mode-variables t)
  (make-local-variable 'font-lock-keywords-case-fold-search)
  (setq font-lock-keywords-case-fold-search t)
  (setq imenu-case-fold-search t))

;; This will do unless inf-spark.el is loaded.
;;(defun spark-eval-defun (&optional and-go)
;;  "Send the current defun to the Spark process made by \\[run-spark]."
;;  (interactive)
;;  (error "Process spark does not exist"))

;;(defvar spark-interaction-mode-map
;;  (let ((map (make-sparse-keymap)))
;;    (set-keymap-parent map spark-mode-shared-map)
;;    (define-key map "\e\C-x" 'eval-defun)
;;    (define-key map "\e\t" 'spark-complete-symbol)
;;    (define-key map "\n" 'eval-print-last-sexp)
;;    map)
;;  "Keymap for Spark Interaction mode.
;;All commands in `spark-mode-shared-map' are inherited by this map.")

;;(define-derived-mode spark-interaction-mode emacs-spark-mode "Spark Interaction"
;;  "Major mode for typing and evaluating Spark forms.
;;Like Spark mode except that \\[eval-print-last-sexp] evals the Spark expression
;;before point, and prints its value into the buffer, advancing point.
;;Note that printing is controled by `eval-expression-print-length'
;;and `eval-expression-print-level'.
;;
;;Commands:
;;Delete converts tabs to spaces as it moves back.
;;Paragraphs are separated only by blank lines.
;;Semicolons start comments.
;;\\{spark-interaction-mode-map}
;;Entry to this mode calls the value of `spark-interaction-mode-hook'
;;if that value is non-nil.")

;;(defun eval-print-last-sexp ()
;;  "Evaluate sexp before point; print value into current buffer.
;;
;;Note that printing the result is controlled by the variables
;;`eval-expression-print-length' and `eval-expression-print-level',
;;which see."
;;  (interactive)
;;  (let ((standard-output (current-buffer)))
;;    (terpri)
;;    (eval-last-sexp t)
;;    (terpri)))


(defun last-sexp-setup-props (beg end value alt1 alt2)
  "Set up text properties for the output of `eval-last-sexp-1'.
BEG and END are the start and end of the output in current-buffer.
VALUE is the Spark value printed, ALT1 and ALT2 are strings for the 
alternative printed representations that can be displayed."
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'last-sexp-toggle-display)
    (define-key map [down-mouse-2] 'mouse-set-point)
    (define-key map [mouse-2] 'last-sexp-toggle-display)
    (add-text-properties
     beg end 
     `(printed-value (,value ,alt1 ,alt2)
		     mouse-face highlight 
		     keymap ,map
		     help-echo "RET, mouse-2: toggle abbreviated display"
		     rear-nonsticky (mouse-face keymap help-echo
						printed-value)))))


(defun last-sexp-toggle-display ()
  "Toggle between abbreviated and unabbreviated printed representations."
  (interactive)
  (let ((value (get-text-property (point) 'printed-value)))
    (when value
      (let ((beg (previous-single-property-change (point) 'printed-value))
	    (end (next-single-char-property-change (point) 'printed-value))
	    (standard-output (current-buffer))
	    (point (point)))
	(delete-region beg end)
	(insert (nth 1 value))
	(last-sexp-setup-props beg (point) 
			       (nth 0 value)
			       (nth 2 value)
			       (nth 1 value))
	(goto-char (min (point-max) point))))))


;;(defun eval-last-sexp-1 (eval-last-sexp-arg-internal)
;;  "Evaluate sexp before point; print value in minibuffer.
;;With argument, print output into current buffer."
;;  (let ((standard-output (if eval-last-sexp-arg-internal (current-buffer) t)))
;;    (let ((value
;;	   (eval (let ((stab (syntax-table))
;;		       (opoint (point))
;;		       ignore-quotes
;;		       expr)
;;		   (unwind-protect
;;		       (save-excursion
;;			 (set-syntax-table emacs-spark-mode-syntax-table)
;;			 ;; If this sexp appears to be enclosed in `...'
;;			 ;; then ignore the surrounding quotes.
;;			 (setq ignore-quotes
;;			       (or (eq (following-char) ?\')
;;				   (eq (preceding-char) ?\')))
;;			 (forward-sexp -1)
;;			 ;; If we were after `?\e' (or similar case),
;;			 ;; use the whole thing, not just the `e'.
;;			 (when (eq (preceding-char) ?\\)
;;			   (forward-char -1)
;;			   (when (eq (preceding-char) ??)
;;			     (forward-char -1)))
;;
;;			 ;; Skip over `#N='s.
;;			 (when (eq (preceding-char) ?=)
;;			   (let (labeled-p)
;;			     (save-excursion
;;			       (skip-chars-backward "0-9#=")
;;			       (setq labeled-p (looking-at "\\(#[0-9]+=\\)+")))
;;			     (when labeled-p
;;			       (forward-sexp -1))))
;;
;;			 (save-restriction
;;			   ;; vladimir@cs.ualberta.ca 30-Jul-1997: skip ` in
;;			   ;; `variable' so that the value is returned, not the
;;			   ;; name
;;			   (if (and ignore-quotes
;;				    (eq (following-char) ?`))
;;			       (forward-char))
;;			   (narrow-to-region (point-min) opoint)
;;			   (setq expr (read (current-buffer)))
;;			   ;; If it's an (interactive ...) form, it's more
;;			   ;; useful to show how an interactive call would
;;			   ;; use it.
;;			   (and (consp expr)
;;				(eq (car expr) 'interactive)
;;				(setq expr
;;				      (list 'call-interactively
;;					    (list 'quote
;;						  (list 'lambda
;;							'(&rest args)
;;							expr
;;							'args)))))
;;			   expr))
;;		     (set-syntax-table stab))))))
;;      (let ((unabbreviated (let ((print-length nil) (print-level nil))
;;			     (prin1-to-string value)))
;;	    (print-length eval-expression-print-length)
;;	    (print-level eval-expression-print-level)
;;	    (beg (point))
;;	    end)
;;	(prin1 value)
;;	(setq end (point))
;;	(when (and (bufferp standard-output)
;;		   (or (not (null print-length))
;;		       (not (null print-level)))
;;		   (not (string= unabbreviated
;;				 (buffer-substring-no-properties beg end))))
;;	  (last-sexp-setup-props beg end value 
;;				 unabbreviated
;;				 (buffer-substring-no-properties beg end))
;;	  )))))


;;(defun eval-last-sexp (eval-last-sexp-arg-internal)
;;  "Evaluate sexp before point; print value in minibuffer.
;;Interactively, with prefix argument, print output into current buffer."
;;  (interactive "P")
;;  (if (null eval-expression-debug-on-error)
;;      (eval-last-sexp-1 eval-last-sexp-arg-internal)
;;    (let ((old-value (make-symbol "t")) new-value value)
;;      (let ((debug-on-error old-value))
;;	(setq value (eval-last-sexp-1 eval-last-sexp-arg-internal))
;;	(setq new-value debug-on-error))
;;      (unless (eq old-value new-value)
;;	(setq debug-on-error new-value))
;;      value)))

;;(defun eval-defun-1 (form)
;;  "Change defvar into defconst within FORM.
;;Likewise for other constructs as necessary."
;  ;; The code in edebug-defun should be consistent with this, but not
;;  ;; the same, since this gets a macroexpended form.
;;  (cond ((and (eq (car form) 'defvar)
;;	      (cdr-safe (cdr-safe form)))
;;	 ;; Force variable to be bound.
;;	 (cons 'defconst (cdr form)))
;;	;; `defcustom' is now macroexpanded to
;;	;; `custom-declare-variable' with a quoted value arg.
;;	((and (eq (car form) 'custom-declare-variable)
;;	      (default-boundp (eval (nth 1 form))))
;;	 ;; Force variable to be bound.
;;	 (set-default (eval (nth 1 form)) (eval (nth 1 (nth 2 form))))
;;	 form)
;;	((eq (car form) 'progn)
;;	 (cons 'progn (mapcar 'eval-defun-1 (cdr form))))
;;	(t form)))

;;(defun eval-defun-2 ()
;;  "Evaluate defun that point is in or before.
;;The value is displayed in the minibuffer.
;;If the current defun is actually a call to `defvar',
;;then reset the variable using the initial value expression
;;even if the variable already has some other value.
;;\(Normally `defvar' does not change the variable's value
;;if it already has a value.\)
;;
;;With argument, insert value in current buffer after the defun.
;;Return the result of evaluation."
;;  (interactive "P")
;;  (let ((debug-on-error eval-expression-debug-on-error)
;;	(print-length eval-expression-print-length)
;;	(print-level eval-expression-print-level))
;;    (save-excursion
;;      ;; Arrange for eval-region to "read" the (possibly) altered form.
;;      ;; eval-region handles recording which file defines a function or
;;      ;; variable.  Re-written using `apply' to avoid capturing
;;      ;; variables like `end'.
;;      (apply
;;       #'eval-region
;;       (let ((standard-output t)
;; 	     beg end form)
;; 	 ;; Read the form from the buffer, and record where it ends.
;; 	 (save-excursion
;; 	   (end-of-defun)
;; 	   (beginning-of-defun)
;; 	   (setq beg (point))
;; 	   (setq form (read (current-buffer)))
;; 	   (setq end (point)))
;; 	 ;; Alter the form if necessary, changing defvar into defconst, etc.
;; 	 (setq form (eval-defun-1 (macroexpand form)))
;; 	 (list beg end standard-output
;; 	       `(lambda (ignore)
;; 		 ;; Skipping to the end of the specified region
;; 		 ;; will make eval-region return.
;; 		 (goto-char ,end)
;; 		 ',form))))))
;;   ;; The result of evaluation has been put onto VALUES.  So return it.
;;   (car values))

;; (defun eval-defun (edebug-it)
;;   "Evaluate the top-level form containing point, or after point.

;; If the current defun is actually a call to `defvar', then reset the
;; variable using its initial value expression even if the variable
;; already has some other value.  (Normally `defvar' does not change the
;; variable's value if it already has a value.)

;; With a prefix argument, instrument the code for Edebug.

;; If acting on a `defun' for FUNCTION, and the function was
;; instrumented, `Edebug: FUNCTION' is printed in the minibuffer.  If not
;; instrumented, just FUNCTION is printed.

;; If not acting on a `defun', the result of evaluation is displayed in
;; the minibuffer.  This display is controlled by the variables
;; `eval-expression-print-length' and `eval-expression-print-level',
;; which see."
;;   (interactive "P")
;;   (cond (edebug-it
;; 	 (require 'edebug)
;; 	 (eval-defun (not edebug-all-defs)))
;; 	(t
;; 	 (if (null eval-expression-debug-on-error)
;; 	     (eval-defun-2)
;; 	   (let ((old-value (make-symbol "t")) new-value value)
;; 	     (let ((debug-on-error old-value))
;; 	       (setq value (eval-defun-2))
;; 	       (setq new-value debug-on-error))
;; 	     (unless (eq old-value new-value)
;; 	       (setq debug-on-error new-value))
;; 	     value)))))


(defun spark-comment-indent ()
  ;; 3 comment characters result in no change
  (if (looking-at "\\s<\\s<\\s<")
      (current-column)
    ;; Two comment characters indent like an expression
    (if (looking-at "\\s<\\s<")
	(let ((tem (or (calculate-spark-indent) (current-column))))
	  (if (listp tem) (car tem) tem))
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
	   comment-column))))

(defun spark-mode-auto-fill ()
  (if (> (current-column) (current-fill-column))
      (if (save-excursion
	    (nth 4 (parse-partial-sexp (save-excursion ;; WMT ?? parse...
					 (beginning-of-defun)
					 (point))
				       (point))))
	  (do-auto-fill)
	(let ((comment-start nil) (comment-start-skip nil))
	  (do-auto-fill)))))

(defvar spark-indent-offset nil
  "If non-nil, indent second line of expressions that many more columns.")
(defvar spark-indent-function 'spark-indent-function)

(defun spark-indent-line (&optional whole-exp)
  "Indent current line as Spark code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (interactive "P")
  (let ((indent (calculate-spark-indent)) shift-amt beg end
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (or (null indent) (looking-at "\\s<\\s<\\s<"))
	;; Don't alter indentation of a ;;; comment line
	;; or a line that starts in a string.
	(goto-char (- (point-max) pos))
      (if (and (looking-at "\\s<") (not (looking-at "\\s<\\s<")))
	  ;; Single-semicolon comment lines should be indented
	  ;; as comment lines, not as code.
	  (progn (indent-for-comment) (forward-char -1))
	(if (listp indent) (setq indent (car indent)))
	(setq shift-amt (- indent (current-column)))
	(if (zerop shift-amt)
	    nil
	  (delete-region beg (point))
	  (indent-to indent)))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos)))
      ;; If desired, shift remaining lines of expression the same amount.
      (and whole-exp (not (zerop shift-amt))
	   (save-excursion
	     (goto-char beg)
	     (forward-sexp 1)
	     (setq end (point))
	     (goto-char beg)
	     (forward-line 1)
	     (setq beg (point))
	     (> end beg))
	   (indent-code-rigidly beg end shift-amt)))))

(defvar calculate-spark-indent-last-sexp)

(defun calculate-spark-indent (&optional parse-start)
  "Return appropriate indentation for current line as Spark code.
In usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.

The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).
This means that following lines at the same level of indentation
should not necessarily be indented the same as this line.
Then COLUMN is the column to indent to, and CONTAINING-SEXP-START
is the buffer position of the start of the containing expression."
  (save-excursion
    (beginning-of-line)
    ;;
    (let ((indent-point (point))
	  keyword-p ; T if first object on line is keyword
	  list-p ; T if first object on line is open paren (bracket)
          state paren-depth
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-spark-indent-last-sexp containing-sexp)
      ;; WMT -- I want to know if we are looking at a keyword or a list
      (when 
	  ;; Get to the first non-blank position
	  (re-search-forward "\\s-*" (line-end-position nil) t)
	(or (setq keyword-p (looking-at "\\sw+:"))
	    (setq list-p (looking-at "\\s(")))
	)
      (if parse-start
          (goto-char parse-start)
          (beginning-of-defun))
      ;; Find outermost containing sexp
      (while (< (point) indent-point)
        (setq state (parse-partial-sexp (point) indent-point 0)))
      ;; Find innermost containing sexp
      (while (and retry
		  state
                  (> (setq paren-depth (elt state 0)) 0))
        (setq retry nil)
        (setq calculate-spark-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-spark-indent-last-sexp
		 (> calculate-spark-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-spark-indent-last-sexp
					    indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-spark-indent-last-sexp)
	    ;; indent-point immediately follows open paren.
	    ;; Don't call hook.
            (setq desired-indent (current-column))
	  ;; Find the start of first element of containing sexp.
	  (parse-partial-sexp (point) calculate-spark-indent-last-sexp 0 t)
	  (cond ((looking-at "\\s(")
		 ;; First element of containing sexp is a list.
		 ;; Indent under that list.
		 )
		((and keyword-p (looking-at "\\sw+:"))
		 ;; First element of containing sexp is a keyword.
		 ;; And we're looking at a keyword.   Indent at the toplevel bracket
		 )
		((> (save-excursion (forward-line 1) (point))
		    calculate-spark-indent-last-sexp)
		 ;; This is the first line to start within the containing sexp.
		 ;; It's almost certainly a function call.
		 (if (= (point) calculate-spark-indent-last-sexp)
		     ;; Containing sexp has nothing before this line
		     ;; except the first element.  Indent under that element.
		     nil
		   ;; Skip the first element, find start of second (the first
		   ;; argument of the function call) and indent under.
		   (progn (forward-sexp 1)
			  (parse-partial-sexp (point)
					      calculate-spark-indent-last-sexp
					      0 t)))
		 (backward-prefix-chars))
		(t
		 ;; Indent beneath first sexp on same line as
		 ;; `calculate-spark-indent-last-sexp'.  Again, it's
		 ;; almost certainly a function call.
		 (goto-char calculate-spark-indent-last-sexp)
		 ;; WMT  See if this is a list when we are indenting a list
		 (setq list-p (and list-p (looking-at "\\s(")))
		 (beginning-of-line)
		 (parse-partial-sexp (point) calculate-spark-indent-last-sexp
				     0 t)
		 (backward-prefix-chars)
		 (when (and list-p (looking-at "\\sw+:"))
		   (re-search-forward "\\s(" (line-end-position) t)
		   (forward-char -1))
		 ))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by spark-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
	       nil)
              ((and (integerp spark-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) spark-indent-offset))
              (desired-indent)
              ((and (boundp 'spark-indent-function)
                    spark-indent-function
                    (not retry))
               (or (funcall spark-indent-function indent-point state)
                   normal-indent))
              (t
               normal-indent))))))

(defun spark-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-spark-indent-last-sexp 0 t) ; WMT ???
    ;; WMT - we need to see if we are looking at a keyword and indent per parent.
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-spark-indent-last-sexp))
              (progn (goto-char calculate-spark-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
					 calculate-spark-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-spark-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (or (get (intern-soft function) 'spark-indent-function)
			 (get (intern-soft function) 'spark-indent-hook)))
	(cond ((or (eq method 'defun)	; WMT ????  The function is checked for a hook or function.
					;  If it has one and it is 'defun'
					;  Then go through this stuff.
					;  We may not want that flexibility
		   (and (null method)
			(> (length function) 3)
			;; WMT???  We may not want this flexibility either.
			(string-match "\\`def" function)))
	       (spark-indent-defform state indent-point)) ; WMT ??? Defform??
	      ((integerp method)	; How much to indent.
	       (spark-indent-specform method state
				     indent-point normal-indent))
	      (method			; Otherwise a function
		(funcall method state indent-point)))))))

(defvar spark-body-indent 2
  "Number of columns to indent the second line of a `(def...)' form.")

(defun spark-indent-specform (count state indent-point normal-indent)
  (let ((containing-form-start (elt state 1))
        (i count)
        body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  spark-indent-function guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ spark-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
                (condition-case ()
                    (progn
                      (setq count (1- count))
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))
                  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (if (> count 0)
        ;; A distinguished form.  If it is the first or second form use double
        ;; spark-body-indent, else normal indent.  With spark-body-indent bound
        ;; to 2 (the default), this just happens to work the same with if as
        ;; the older code, but it makes unwind-protect, condition-case,
        ;; with-output-to-temp-buffer, et. al. much more tasteful.  The older,
        ;; less hacked, behavior can be obtained by replacing below with
        ;; (list normal-indent containing-form-start).
        (if (<= (- i count) 1)
            (list (+ containing-form-column (* 2 spark-body-indent))
                  containing-form-start)
            (list normal-indent containing-form-start))
      ;; A non-distinguished form.  Use body-indent if there are no
      ;; distinguished forms and this is the first undistinguished form,
      ;; or if this is the first undistinguished form and the preceding
      ;; distinguished form has indentation at least as great as body-indent.
      (if (or (and (= i 0) (= count 0))
              (and (= count 0) (<= body-indent normal-indent)))
          body-indent
          normal-indent))))

(defun spark-indent-defform (state indent-point)
  (goto-char (car (cdr state)))
  (forward-line 1)
  (if (> (point) (car (cdr (cdr state))))
      (progn
	(goto-char (car (cdr state)))
	(+ spark-body-indent (current-column)))))


;; (put 'progn 'spark-indent-function 0), say, causes progn to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

;; (put 'lambda 'spark-indent-function 'defun)
;; (put 'autoload 'spark-indent-function 'defun)
;; (put 'progn 'spark-indent-function 0)
;; (put 'prog1 'spark-indent-function 1)
;; (put 'prog2 'spark-indent-function 2)
;; (put 'save-excursion 'spark-indent-function 0)
;; (put 'save-window-excursion 'spark-indent-function 0)
;; (put 'save-selected-window 'spark-indent-function 0)
;; (put 'save-restriction 'spark-indent-function 0)
;; (put 'save-match-data 'spark-indent-function 0)
;; (put 'save-current-buffer 'spark-indent-function 0)
;; (put 'with-current-buffer 'spark-indent-function 1)
;; (put 'combine-after-change-calls 'spark-indent-function 0)
;; (put 'with-output-to-string 'spark-indent-function 0)
;; (put 'with-temp-file 'spark-indent-function 1)
;; (put 'with-temp-buffer 'spark-indent-function 0)
;; (put 'with-temp-message 'spark-indent-function 1)
;; (put 'with-syntax-table 'spark-indent-function 1)
;; (put 'let 'spark-indent-function 1)
;; (put 'let* 'spark-indent-function 1)
;; (put 'while 'spark-indent-function 1)
;; (put 'if 'spark-indent-function 2)
;; (put 'catch 'spark-indent-function 1)
;; (put 'condition-case 'spark-indent-function 2)
;; (put 'unwind-protect 'spark-indent-function 1)
;; (put 'with-output-to-temp-buffer 'spark-indent-function 1)
;; (put 'eval-after-load 'spark-indent-function 1)
;; (put 'dolist 'spark-indent-function 1)
;; (put 'dotimes 'spark-indent-function 1)
;; (put 'when 'spark-indent-function 1)
;; (put 'unless 'spark-indent-function 1)

;;; Language forms in SPARKL
;;; defpredicate (handled above)
;;; deftask (handled above)
;;; deffunction (handled above)
;;; defsymbol (handled above)
;;; defaction (handled above)
;;; procedure (handled above)
;;;   cue:, precondition:, body:
;;;   forall: , while:, repeat:
(put 'forall: 'spark-indent-function 1)
(put 'while: 'spark-indent-function 1)
(put 'repeat: 'spark-indent-function 1)
;;; and, or  (n-ary)
(put 'and 'spark-indent-function 0)
(put 'or 'spark-indent-function 0)
;;; not
;;; exists
(put 'exists 'spark-indent-function 1)
;;; For all these task keywords, put the indent as 0
;;;   So no matter which one comes after [, all the rest line up correctly.
(put 'succeed: 'spark-indent-function 0)
(put 'fail: 'spark-indent-function 0)
(put 'finish: 'spark-indent-function 0)
(put 'achieve: 'spark-indent-function 0)
(put 'conclude: 'spark-indent-function 0)
(put 'context: 'spark-indent-function 0)
(put 'retract: 'spark-indent-function 0)
(put 'retractall: 'spark-indent-function 0)
(put 'do: 'spark-indent-function 0)
(put 'body: 'spark-indent-function 0)
(put 'contiguous: 'spark-indent-function 0)
(put 'execute: 'spark-indent-function 0)
(put 'parallel: 'spark-indent-function 0)
(put 'repeat: 'spark-indent-function 0)
(put 'seq: 'spark-indent-function 0)
(put 'try: 'spark-indent-function 0)
(put 'select: 'spark-indent-function 0)
(put 'wait: 'spark-indent-function 0)
(put 'while: 'spark-indent-function 0)
(put 'forall: 'spark-indent-function 0)
;;; Closures
(put 'pred 'spark-indent-function 1)
(put 'task 'spark-indent-function 1)
(put 'fun 'spark-indent-function 1)


(defun indent-sexp (&optional endpos)
  "Indent each line of the list starting just after point.
If optional arg ENDPOS is given, indent each line, stopping when
ENDPOS is encountered."
  (interactive)
  (let ((indent-stack (list nil))
	(next-depth 0)
	;; If ENDPOS is non-nil, use nil as STARTING-POINT
	;; so that calculate-spark-indent will find the beginning of
	;; the defun we are in.
	;; If ENDPOS is nil, it is safe not to scan before point
	;; since every line we indent is more deeply nested than point is.
	(starting-point (if endpos nil (point)))
	(last-point (point))
	last-depth bol outer-loop-done inner-loop-done state this-indent)
    (or endpos
	;; Get error now if we don't have a complete sexp after point.
	(save-excursion (forward-sexp 1)))
    (save-excursion
      (setq outer-loop-done nil)
      (while (if endpos (< (point) endpos)
	       (not outer-loop-done))
	(setq last-depth next-depth
	      inner-loop-done nil)
	;; Parse this line so we can learn the state
	;; to indent the next line.
	;; This inner loop goes through only once
	;; unless a line ends inside a string.
	(while (and (not inner-loop-done)
		    (not (setq outer-loop-done (eobp))))
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq next-depth (car state))
	  ;; If the line contains a comment other than the sort
	  ;; that is indented like code,
	  ;; indent it now with indent-for-comment.
	  ;; Comments indented like code are right already.
	  ;; In any case clear the in-comment flag in the state
	  ;; because parse-partial-sexp never sees the newlines.
	  (if (car (nthcdr 4 state))
	      (progn (indent-for-comment)
		     (end-of-line)
		     (setcar (nthcdr 4 state) nil)))
	  ;; If this line ends inside a string,
	  ;; go straight to next line, remaining within the inner loop,
	  ;; and turn off the \-flag.
	  (if (car (nthcdr 3 state))
	      (progn
		(forward-line 1)
		(setcar (nthcdr 5 state) nil))
	    (setq inner-loop-done t)))
	(and endpos
	     (<= next-depth 0)
	     (progn
	       (setq indent-stack (nconc indent-stack
					 (make-list (- next-depth) nil))
		     last-depth (- last-depth next-depth)
		     next-depth 0)))
	(or outer-loop-done endpos
	    (setq outer-loop-done (<= next-depth 0)))
	(if outer-loop-done
	    (forward-line 1)
	  (while (> last-depth next-depth)
	    (setq indent-stack (cdr indent-stack)
		  last-depth (1- last-depth)))
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  last-depth (1+ last-depth)))
	  ;; Now go to the next line and indent it according
	  ;; to what we learned from parsing the previous one.
	  (forward-line 1)
	  (setq bol (point))
	  (skip-chars-forward " \t")
	  ;; But not if the line is blank, or just a comment
	  ;; (except for double-semi comments; indent them as usual).
	  (if (or (eobp) (looking-at "\\s<\\|\n"))
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		(setq this-indent (car indent-stack))
	      (let ((val (calculate-spark-indent
			  (if (car indent-stack) (- (car indent-stack))
			    starting-point))))
		(if (null val)
		    (setq this-indent val)
		  (if (integerp val)
		      (setcar indent-stack
			      (setq this-indent val))
		    (setcar indent-stack (- (car (cdr val))))
		    (setq this-indent (car val))))))
	    (if (and this-indent (/= (current-column) this-indent))
		(progn (delete-region bol (point))
		       (indent-to this-indent)))))
	(or outer-loop-done
	    (setq outer-loop-done (= (point) last-point))
	    (setq last-point (point)))))))

(defun spark-indent-region (start end)
  "Indent every line whose first char is between START and END inclusive."
  (save-excursion
    (let ((endmark (copy-marker end)))
      (goto-char start)
      (and (bolp) (not (eolp))
	   (spark-indent-line))
      (indent-sexp endmark)
      (set-marker endmark nil))))

;;;; Spark paragraph filling commands.

(defun spark-fill-paragraph (&optional justify)
  "Like \\[fill-paragraph], but handle Spark comments.
If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's indentation
and initial comment characters."
  (interactive "P")
  (let (
	;; Non-nil if the current line contains a comment.
	has-comment

	;; Non-nil if the current line contains code and a comment.
	has-code-and-comment

	;; If has-comment, the appropriate fill-prefix for the comment.
	comment-fill-prefix
	)

    ;; Figure out what kind of comment we are looking at.
    (save-excursion
      (beginning-of-line)
      (cond

       ;; A line with nothing but a comment on it?
       ((looking-at "[ \t]*#[# \t]*")
	(setq has-comment t
	      comment-fill-prefix (buffer-substring (match-beginning 0)
						    (match-end 0))))

       ;; A line with some code, followed by a comment?  Remember that the
       ;; semi which starts the comment shouldn't be part of a string or
       ;; character.
       ((condition-case nil
	    (save-restriction
	      (narrow-to-region (point-min)
				(save-excursion (end-of-line) (point)))
	      (while (not (looking-at "#\\|$"))
		(skip-chars-forward "^#\n\"\\\\?")
		(cond
		 ((eq (char-after (point)) ?\\) (forward-char 2))
		 ((memq (char-after (point)) '(?\" ??)) (forward-sexp 1))))
	      (looking-at "#+[\t ]*"))
	  (error nil))
	(setq has-comment t has-code-and-comment t)
	(setq comment-fill-prefix
	      (concat (make-string (/ (current-column) 8) ?\t)
		      (make-string (% (current-column) 8) ?\ )
		      (buffer-substring (match-beginning 0) (match-end 0)))))))

    (if (not has-comment)
        ;; `paragraph-start' is set here (not in the buffer-local
        ;; variable so that `forward-paragraph' et al work as
        ;; expected) so that filling (doc) strings works sensibly.
        ;; Adding the opening paren to avoid the following sexp being
        ;; filled means that sexps generally aren't filled as normal
        ;; text, which is probably sensible.  The `#'  and "{" stops the
        ;; filled para at following comment lines
	(let ((paragraph-start (concat paragraph-start
                                       "\\|\\s-*[{[\(#\"]")))
          (fill-paragraph justify))

      ;; Narrow to include only the comment, and then fill the region.
      (save-excursion
	(save-restriction
	  (beginning-of-line)
	  (narrow-to-region
	   ;; Find the first line we should include in the region to fill.
	   (save-excursion
	     (while (and (zerop (forward-line -1))
			 (looking-at "^[ \t]*#")))
	     ;; We may have gone too far.  Go forward again.
	     (or (looking-at ".*#")
		 (forward-line 1))
	     (point))
	   ;; Find the beginning of the first line past the region to fill.
	   (save-excursion
	     (while (progn (forward-line 1)
			   (looking-at "^[ \t]*#")))
	     (point)))

	  ;; Lines with only semicolons on them can be paragraph boundaries.
	  (let* ((paragraph-start (concat paragraph-start "\\|[ \t#]*$"))
		 (paragraph-separate (concat paragraph-start "\\|[ \t#]*$"))
		 (paragraph-ignore-fill-prefix nil)
		 (fill-prefix comment-fill-prefix)
		 (after-line (if has-code-and-comment
				 (save-excursion
				   (forward-line 1) (point))))
		 (end (progn
			(forward-paragraph)
			(or (bolp) (newline 1))
			(point)))
		 ;; If this comment starts on a line with code,
		 ;; include that like in the filling.
		 (beg (progn (backward-paragraph)
			     (if (eq (point) after-line)
				 (forward-line -1))
			     (point))))
	    (fill-region-as-paragraph beg end
				      justify nil
				      (save-excursion
					(goto-char beg)
					(if (looking-at fill-prefix)
					    nil
					  (re-search-forward comment-start-skip)
					  (point))))))))
    t))

(defun indent-code-rigidly (start end arg &optional nochange-regexp)
  "Indent all lines of code, starting in the region, sideways by ARG columns.
Does not affect lines starting inside comments or strings, assuming that
the start of the region is not inside them.

Called from a program, takes args START, END, COLUMNS and NOCHANGE-REGEXP.
The last is a regexp which, if matched at the beginning of a line,
means don't indent that line."
  (interactive "r\np")
  (let (state)
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp)
	  (setq state (parse-partial-sexp (point)
					  (progn
					    (forward-line 1) (point))
					  nil nil state)))
      (while (< (point) end)
	(or (car (nthcdr 3 state))
	    (and nochange-regexp
		 (looking-at nochange-regexp))
	    ;; If line does not start in string, indent it
	    (let ((indent (current-indentation)))
	      (delete-region (point) (progn (skip-chars-forward " \t") (point)))
	      (or (eolp)
		  (indent-to (max 0 (+ indent arg)) 0))))
	(setq state (parse-partial-sexp (point)
					(progn
					  (forward-line 1) (point))
					nil nil state))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Faces
;;; font-lock-keyword-face
;;; font-lock-predicate-name-face
;;; font-lock-action-name-face
;;; font-lock-function-name-face
;;; font-lock-procedure-name-face
;;; font-lock-symbol-face
;;; font-lock-variable2-name-face  (for $$ variables)
;;; font-lock-variable-name-face
;;; font-lock-doc-face
;;; font-lock-comment-face
;;; font-lock-fun-closure-face
;;; font-lock-pred-closure-face
;;; font-lock-task-closure-face
;;; Some of these are already defined, but we'll define the others in terms of common ones.
;;(set-face-foreground font-lock-variable-name-face "white")
(make-face-bold 'font-lock-variable-name-face) ;Boldify $variables (default color)
(copy-face 'font-lock-variable-name-face 'font-lock-variable2-name-face)
(copy-face 'default 'font-lock-variable6-name-face)
(make-face-bold  'font-lock-variable6-name-face)
;;(make-face-bold 'font-lock-variable2-name-face)
(set-face-underline-p 'font-lock-variable2-name-face 'unspecified) ; Underline $$variables
(defvar font-lock-predicate-name-face font-lock-function-name-face)
(defvar font-lock-action-name-face font-lock-function-name-face)
(defvar font-lock-procedure-name-face font-lock-function-name-face)
(defvar font-lock-doc-face font-lock-doc-string-face)
(defvar font-lock-string-face font-lock-doc-string-face)
(defvar font-lock-fun-closure-face font-lock-function-name-face)
(defvar font-lock-pred-closure-face font-lock-function-name-face)
(defvar font-lock-task-closure-face font-lock-function-name-face)

(defconst spark-font-lock-keywords-1
    ;; Import
    '(("\\<\\(import\\(\\|all\\)\\|export\\):\\>"
       (1 font-lock-keyword-face))
      )
  "Supdued level highlighting")


(defconst spark-font-lock-keywords-2
    (append spark-font-lock-keywords-1
	    (list
	     (cons
	     (concat "\\<\\("
		      "achieve\\|body\\|c\\(o\\(n\\(clude\\|t\\(ext\\|iguous\\)\\)\\)\\|\\ue\\)\\|"
		      "d\\(etermined\\|o\\)\\|execute\\|f\\(ail\\|inish\\|orall\\)\\|label\\|"
		      "newfact\\|p\\(arallel\\|r\\(econdition\\|operty\\)\\)\\|re\\(peat\\|etract\\(\\|all\\)\\)\\|"
		      "s\\(e\\(lect\\|q\\)\\|ucceed\\)\\|try\\|w\\(ait\\|hile\\)"
		      "\\):")
	     'font-lock-keyword-face)
	     '("{defpredicate\\s-+(\\(\\sw+\\)\\>" 1 font-lock-predicate-name-face t)
	     '("{defaction\\s-+(\\(\\sw+\\)\\>" 1 font-lock-action-name-face t)
	     '("{deffunction\\s-+(\\(\\sw+\\)\\>" 1 font-lock-function-name-face t)
	     '("{defsymbol\\s-+\\(\\sw+\\)\\>" 1 font-lock-symbol-face t)
	     '("{defprocedure\\s-+\\(\\\"[^\\\"]\\\"\\)\\>" 1 font-lock-procedure-name-face t)
	     ;; I had these as different faces, but the different face didn't work
	     ;;'("[^\\$]\\(\\$\\sw+\\)\\>" 1 font-lock-variable-name-face)
	     ;;'("\\(\\$\\$+\\sw+\\)\\>" 1 font-lock-variable2-name-face)
	     '("\\(\\$+\\sw+\\)\\>" 1 font-lock-variable-name-face)
	     '("\\<doc:\\s-+\\(\\\"[^\\\"]\\\"\\)\\>" 1 font-lock-doc-face t)
	     '("\\<comment:\\s-+\\(\\\"[^\\\"]\\\"\\)\\>" 1 font-lock-comment-face t)
	     ;; Closures
	     '("{\\(fun\\)\\>" 1 font-lock-fun-closure-face)
	     '("{\\(pred\\)\\>" 1 font-lock-pred-closure-face)
	     '("{\\(task\\)\\>" 1 font-lock-task-closure-face)
	     ))
  "Gaudy level highlighting for SPARK mode.")
  
  
(defvar spark-font-lock-keywords spark-font-lock-keywords-1
    "Default expressions to highlight in SPARK mode.")
  
  


(provide 'spark-mode)

;;; spark-mode.el ends here
