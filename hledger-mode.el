;;; hledger-mode.el --- A mode for writing journal entries for hledger.

;; Copyright (C) 2015-2016 Narendra Joshi
;; Author: Narendra Joshi <narendraj9@gmail.com>
;; URL: https://github.com/narendraj9/hledger-mode.git
;; Version: 0.1
;; Keywords: data
;; Package-Requires: ((emacs "24.4") (popup "0.5.3") (async "1.9") (htmlize "1.47"))

;;; Commentary:
;;
;; A major mode for writing hledger journal files.  It generates some
;; useful reports along with some financial ratios that can help you
;; keep a check on your financial health.  This is an attempt to
;; organize personal finances for Emacs users.  If you don't like this,
;; try `ledger-mode'.
;;
;; Note: You must have hledger installed to be able to create the
;;       reports: overall report, daily report, balancesheet, income
;;       statement, etc.
;;

;;; Code:

(require 'hledger-defuns)
(require 'hledger-core)
(require 'hledger-navigate)
(require 'hledger-reports)
(require 'hledger-mail)
(require 'hledger-webservice)

(defgroup hledger nil
  "Customization group hledger-mode."
  :group 'data)

(defcustom hledger-mode-hook nil
  "Normal hook for entering ‘hledger-mode’."
  :type 'hook
  :group 'hledger)

(defcustom hledger-date-face font-lock-string-face
  "Face for date."
  :type 'face
  :group 'hledger)

(defcustom hledger-amount-face font-lock-constant-face
  "Face for date."
  :type 'face
  :group 'hledger)

(defcustom hledger-account-face font-lock-variable-name-face
  "Face for date."
  :type 'face
  :group 'hledger)

(defcustom hledger-description-face nil
  "Face for description text."
  :type 'face
  :group 'hledger)

(defvar hledger-accounts-cache nil
  "List of accounts cached for ac and company modes.")

(defvar hledger-ac-source
  `((init . hledger-get-accounts)
    (candidates . hledger-accounts-cache))
  "A source for completing account names in a hledger buffer.")

;;;###autoload
(defun hledger-company (command &optional arg &rest ignored)
  "Company backend for ‘hledger-mode’.
COMMAND, ARG and IGNORED the regular meanings."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'hledger-company))
    (`prefix (and (eq major-mode 'hledger-mode)
                  (company-grab-symbol)))
    (`candidates
     (delq nil
           (mapcar (lambda (c)
                     (and (string-prefix-p arg c) c))
                   hledger-accounts-cache)))))

;;;###autoload
(defvar hledger-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-i") 'hledger-append-clipboard-to-journal)
    (define-key map (kbd "C-c C-d") 'hledger-reschedule)
    (define-key map (kbd "C-c C-b") 'hledger-edit-amount)
    (define-key map (kbd "C-c C-p") 'hledger-backward-entry)
    (define-key map (kbd "C-c C-n") 'hledger-next-or-new-entry)
    (define-key map (kbd "RET")  'hledger-ret-command)
    (define-key map (kbd "<backtab>") 'hledger-backtab-command)
    map))

(defvar hledger-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-q") 'hledger-kill-reporting-window)
    (define-key map (kbd "e") 'hledger-jentry)
    (define-key map (kbd "g") 'hledger-redo)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "h") 'hledger-show-view-mode-help)
    (define-key map (kbd "w") 'hledger-copy-to-clipboard)
    (define-key map (kbd "j") 'hledger-run-command)
    (define-key map (kbd "t") 'hledger-report-ending-today)
    (define-key map (kbd "w") 'hledger-widen-results-for-register)
    (define-key map (kbd "<") 'hledger-prev-report)
    (define-key map (kbd ">") 'hledger-next-report)
    (define-key map (kbd ".") 'hledger-present-report)
    (define-key map (kbd "o") (hledger-as-command hledger-overall-report*
                                                  "overall"))
    (define-key map (kbd "i") (hledger-as-command hledger-incomestatement*
                                                  "incomestatement"))
    (define-key map (kbd "d") (hledger-as-command hledger-daily-report*
                                                  "daily"))
    (define-key map (kbd "b") (hledger-as-command hledger-balancesheet*
                                                  "balancesheet"))
    (define-key map (kbd "<tab>") 'hledger-expand-account)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "%") 'hledger-display-percentages)
    map))

(defconst hledger-font-lock-keywords-1
  (list
   `(,hledger-account-regex . hledger-account-face)
   `(,hledger-date-regex . hledger-date-face)
   `(,hledger-amount-regex . hledger-amount-face))
  "Minimal highlighting expressions for hledger mode.")

(defvar hledger-font-lock-defaults '(hledger-font-lock-keywords-1)
  "Default highlighting expressions for hledger mode.")

(defvar hledger-mode-syntax-table (let ((st (make-syntax-table)))
                                    (modify-syntax-entry ?: "_" st)
                                    (modify-syntax-entry ?\; "<" st)
                                    (modify-syntax-entry ?\n ">" st)
                                    st)
  "Syntax table for hledger mode.")

(defun hledger-mode-init ()
  "Function that does initial setup in the \"major-mode\" function."
  (setq font-lock-defaults hledger-font-lock-defaults)
  (setq-local indent-line-function 'hledger-indent-line)
  (setq-local indent-region-function 'hledger-indent-region-function)
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq require-final-newline t)
  (electric-indent-local-mode -1)
  ;; Make an overlay for current entry if enabled
  (when hledger-enable-current-overlay
    (add-hook 'post-command-hook 'hledger-update-current-entry-overlay))
  ;; How can make this execute lazily?
  (setq hledger-accounts-cache (hledger-get-accounts)))

;;;###autoload
(define-derived-mode hledger-mode fundamental-mode "HLedger" ()
  "Major mode for editing journal files."
  :syntax-table hledger-mode-syntax-table
  (hledger-mode-init)
  (hledger-init-thing-at-point))

;;;###autoload
(define-derived-mode hledger-view-mode special-mode "HLedger View" ()
  "Major mode for viewing hledger reports. I have a separate major mode
so that the key bindings are not shared between buffers that are used for
viewing reports and the journal file. I require the same kind of syntax
highlighting in both kinds of buffers."
  :syntax-table hledger-mode-syntax-table
  (setq font-lock-defaults hledger-font-lock-defaults)
  ;; Populate accounts cache if not already.
  (or hledger-accounts-cache
      (setq hledger-accounts-cache (hledger-get-accounts)))
  ;; Setting up font-lock for partial account names.  This is only to
  ;; make sure they have the right face in a tree-type report. Why?
  ;; Why not!?
  (let* ((account-words (apply 'append
                               (mapcar (lambda (s)
                                         (split-string s ":" t))
                                       hledger-accounts-cache)))
         (font-lock-acc-string (concat "\\<\\("
                                       (mapconcat 'identity
                                                  (delete-dups account-words)
                                                  "\\|")
                                       "\\)\\>")))
    ;; Do this only in view mode
    (font-lock-add-keywords 'hledger-view-mode
                            `((,font-lock-acc-string . hledger-account-face)
                              (":" . hledger-account-face))))
  ;; Avoid wrapping lines in reports
  (setq truncate-lines t)
  (hledger-init-thing-at-point))

(provide 'hledger-mode)

;;; hledger-mode.el ends here
