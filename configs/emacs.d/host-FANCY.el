(load-file "D:/SW/inst/cedet-1.0pre4/common/cedet.el")

;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
;; Select one of the following:

;; * This enables the database and idle reparse engines
;;(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as the nascent intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;; (semantic-load-enable-guady-code-helpers)

;; * This turns on which-func support (Plus all other code helpers)
;; (semantic-load-enable-excessive-code-helpers)

;; This turns on modes that aid in grammar writing and semantic tool
;; development.  It does not enable any other features such as code
;; helpers above.
;; (semantic-load-enable-semantic-debugging-helpers)

(add-to-list 'load-path "D:/SW/inst/ecb-2.32")
(require 'cc-mode)
;
(require 'ecb)

(require 'ecb-autoloads)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.32")
 '(ecb-tip-of-the-day nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(setq compilation-window-height 8
	  compilation-ask-about-save nil)

(setq compilation-finish-function
      (lambda (buf str)

        (if (string-match "exited abnormally" str)

            ;;there were errors
            (message "compilation errors, press C-x ` to visit")

          ;;no errors, make the compilation window go away in 0.5 seconds
          (run-at-time 0.5 nil 'delete-windows-on buf)
          (message "NO COMPILATION ERRORS!"))))

;(c-toggle-hungry-state)

(c-set-offset 'substatement-open 0)

(global-set-key [(f9)] 'compile)

(setq compilation-read-command nil)
; make this default for windows (and other compile stuff)
(setq compile-command "nmake")

;;(require 'info)
(add-to-list 'Info-default-directory-list "D:/SW/inst/ecb-2.32/info-help")


(setq sql-mysql-options '("-C" "-t" "-f" "-n"))
(setq sql-mysql-program "D:/SW/inst/mysql-5.1.35-win32/bin/mysql")