;;; -*- lexical-binding: t -*-
;;; hledger-mode.el -- A mode for writing journal entries for hledger

;;; Copyright (C) 2015-2016 Narendra Joshi 

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; URL: 
;; Version: 0.1
;; Keywords: hledger
;; Package-Requires: ((json "1.4") (popup "0.5.3"))

;;; Commentary:
;;
;; This is a major mode writing hledger journal files. You must have
;; hledger installed to be able to create the reports: balancesheet,
;; income statement, etc.
;;

;;; Code;

(require 'json)

(require 'hledger-core)
(require 'hledger-helpers)
(require 'hledger-reports)
(require 'hledger-mail)
(require 'hledger-webservice)

(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

(defgroup hledger nil
  "Customization group hledger-mode.")

(defcustom hledger-mode-hook nil
  "Normal hook for entering hledger-mode."
  :type 'hook
  :group 'hledger)

(defvar hledger-accounts-cache nil
  "List of accounts cached for ac and company modes.")

(defvar ac-source-hledger-source
  `((init . hledger-get-accounts)
    (candidates . hledger-accounts-cache))
  "A source for completing account names in a hledger buffer.")

(defun company-hledger (command &optional arg &rest ignored)
  "Company backend for completion of existing account names."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'company-hledger))
    (`prefix (and (eq major-mode 'hledger-mode)
                  (company-grab-symbol)))
    (`candidates
     (remove-if-not
      (lambda (c)
        (string-prefix-p arg c))
      hledger-accounts-cache))))

(defvar hledger-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c i") 'hledger-append-clipboard-to-journal)
    (define-key map (kbd "RET")  'hledger-ret-command)
    (define-key map (kbd "<backtab>") 'hledger-backtab-command)
    map))

(defvar hledger-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'hledger-kill-reporting-window)
    (define-key map (kbd "h") 'hledger-show-view-mode-help)
    (define-key map (kbd "w") 'hledger-copy-to-clipboard)
    (define-key map (kbd "<") 'hledger-prev-report)
    (define-key map (kbd ">") 'hledger-next-report)
    (define-key map (kbd ".") 'hledger-present-report)
    (define-key map (kbd "d") (hledger-as-command hledger-daily "daily"))
    (define-key map (kbd "m") (hledger-as-command hledger-monthly "monthly"))
    (define-key map (kbd "o") (hledger-as-command hledger-overall "overall"))
    (define-key map (kbd "b") (hledger-as-command balancesheet "balancesheet"))
    (define-key map (kbd "i") (hledger-as-command incomestatement "incomestatement"))
    map))

(defconst hledger-font-lock-keywords-1
  (list
   `(,hledger-account-regex . font-lock-variable-name-face)
   `(,hledger-date-regex . font-lock-string-face)
   `(,hledger-amount-regex . font-lock-constant-face))
  "Minimal highlighting expressions for hledger mode")

(defvar hledger-font-lock-defaults '(hledger-font-lock-keywords-1)
  "Default highlighting expressions for hledger mode")

(defvar hledger-mode-syntax-table (let ((st (make-syntax-table)))
                                    (modify-syntax-entry ?: "_" st)
                                    (modify-syntax-entry ?\; "<" st)
                                    (modify-syntax-entry ?\n ">" st)
                                    st)
  "Syntax table for hledger mode.")

(defun hledger-mode-init ()
  "Function that does initial setup in the major-mode function."
  (use-local-map hledger-mode-map)
  (setq-local font-lock-defaults hledger-font-lock-defaults)
  (setq-local indent-line-function 'hledger-indent-line)
  (setq-local indent-region-function 'hledger-indent-region-function)
  (setq-local ac-sources '(ac-source-hledger-source))
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (electric-indent-local-mode -1)
  (setq hledger-accounts-cache (hledger-get-accounts)))

;;;###autoload
(define-derived-mode hledger-mode prog-mode "HLedger" ()
  "Major mode for editing journal files."
  :syntax-table hledger-mode-syntax-table
  (interactive)
  (hledger-mode-init))

;;;###autoload
(define-derived-mode hledger-view-mode special-mode "HLedger View" ()
  "Major mode for viewing hledger reports. I have a separate major mode
so that the key bindings are not shared between buffers that are used for
viewing reports and the journal file. I require the same kind of syntax
highlighting in both kinds of buffers."
  :syntax-table hledger-mode-syntax-table
  (interactive)
  (setq-local font-lock-defaults hledger-font-lock-defaults)
  ;; Highlight current line for better readability
  (hl-line-mode 1)
  (use-local-map hledger-view-mode-map))

(provide 'hledger-mode)

