# hledger-mode

[![MELPA](https://melpa.org/packages/hledger-mode-badge.svg)](https://melpa.org/#/hledger-mode)

An Emacs major mode for writing [hledger](https://hledger.org/) journals and
generating useful accounting reports.

![Sample Reports](_assets/new_demo.gif?raw=true "Reports")

## Installation

The external `hledger` program should be installed first, or most of the report
features won't work.

This package is available on [MELPA](http://melpa.org/):

    M-x package-install hledger-mode

If you are not installing from Melpa, to make `hledger-mode` available
to your Emacs add it to `load-path`.

    (add-to-list 'load-path "/path/to/hledger-mode/dir/")

## Setup

```elisp
;;; Basic configuration
(require 'hledger-mode)

;; To open files with .journal extension in hledger-mode
(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

;; Provide the path to you journal file.
;; The default location is too opinionated.
(setq hledger-jfile "/path/to/your/journal-file.journal")

;;; Auto-completion for account names
;; For company-mode users,
(add-to-list 'company-backends 'hledger-company)

;; For auto-complete users,
(add-to-list 'ac-modes 'hledger-mode)
(add-hook 'hledger-mode-hook
    (lambda ()
        (setq-local ac-sources '(hledger-ac-source))))

;; For easily adjusting dates.
(define-key hledger-mode-map (kbd "<kp-add>") 'hledger-increment-entry-date)
(define-key hledger-mode-map (kbd "<kp-subtract>") 'hledger-decrement-entry-date)
```

## Configuration

For configuring various parameters, e.g. the accounts used for
computing ratios in the overall report, `M-x customize-group` and
customize the `hledger` group. For example, the Emergency Fund Ratio
is computed with expenses incurred in accounts listed in the variable
`hledger-ratios-essential-expense-accounts`.

## Usage

I recommend the following key bindings:

```elisp

;; Personal Accounting
(global-set-key (kbd "C-c e") 'hledger-jentry)
(global-set-key (kbd "C-c j") 'hledger-run-command)

```

Once you are in a report buffer after executing a command with
`hledger-run-command`, press `h` to see the list of reports that you
can have a look at. Press `s` in the overall report to see the meaning
of the personal finance ratios for your report.

To enable email reporting, you would need to setup your email api
credentials. You can set those up with `M-x customize-group hledger`.

Once you have done so, you can enable monthly email reporting on
`hledger-reporting-day` with the following in your `init.el`:

```elisp

(hledger-enable-reporting)

```

Here is my configuration with `use-package' declarations:

``` elisp
(use-package hledger-mode
  :pin manual
  :after htmlize
  :load-path "packages/rest/hledger-mode/"
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :commands hledger-enable-reporting
  :preface
  (defun hledger/next-entry ()
    "Move to next entry and pulse."
    (interactive)
    (hledger-next-or-new-entry)
    (hledger-pulse-momentary-current-entry))

  (defface hledger-warning-face
    '((((background dark))
       :background "Red" :foreground "White")
      (((background light))
       :background "Red" :foreground "White")
      (t :inverse-video t))
    "Face for warning"
    :group 'hledger)

  (defun hledger/prev-entry ()
    "Move to last entry and pulse."
    (interactive)
    (hledger-backward-entry)
    (hledger-pulse-momentary-current-entry))

  :bind (("C-c j" . hledger-run-command)
         :map hledger-mode-map
         ("C-c e" . hledger-jentry)
         ("M-p" . hledger/prev-entry)
         ("M-n" . hledger/next-entry))
  :init
  (setq hledger-jfile
        (expand-file-name "~/miscellany/personal/finance/accounting.journal")
        hledger-email-secrets-file (expand-file-name "secrets.el"
                                                     emacs-assets-directory))
  ;; Expanded account balances in the overall monthly report are
  ;; mostly noise for me and do not convey any meaningful information.
  (setq hledger-show-expanded-report nil)

  (when (boundp 'my-hledger-service-fetch-url)
    (setq hledger-service-fetch-url
          my-hledger-service-fetch-url))

  :config
  (add-hook 'hledger-view-mode-hook #'hl-line-mode)
  (add-hook 'hledger-view-mode-hook #'center-text-for-reading)

  (add-hook 'hledger-view-mode-hook
            (lambda ()
              (run-with-timer 1
                              nil
                              (lambda ()
                                (when (equal hledger-last-run-command
                                             "balancesheet")
                                  ;; highlight frequently changing accounts
                                  (highlight-regexp "^.*\\(savings\\|cash\\).*$")
                                  (highlight-regexp "^.*credit-card.*$"
                                                    'hledger-warning-face))))))

  (add-hook 'hledger-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'hledger-company))))

(use-package hledger-input
  :pin manual
  :load-path "packages/rest/hledger-mode/"
  :bind (("C-c e" . hledger-capture)
         :map hledger-input-mode-map
         ("C-c C-b" . popup-balance-at-point))
  :preface
  (defun popup-balance-at-point ()
    "Show balance for account at point in a popup."
    (interactive)
    (if-let ((account (thing-at-point 'hledger-account)))
        (message (hledger-shell-command-to-string (format " balance -N %s "
                                                          account)))
      (message "No account at point")))

  :config
  (setq hledger-input-buffer-height 20)
  (add-hook 'hledger-input-post-commit-hook #'hledger-show-new-balances)
  (add-hook 'hledger-input-mode-hook #'auto-fill-mode)
  (add-hook 'hledger-input-mode-hook
            (lambda ()
              (make-local-variable 'company-idle-delay)
              (setq-local company-idle-delay 0.1))))
```

## Auxiliary tools

You are welcome to use the web application hosted
at [vicarie.in](https://services.vicarie.in) for logging data while
you are away from computer. You can use the command
`hledger-fetch-entries` later on to get those entries into your
journal file.

If you want real-time checking of your journal, you might want to
install
[flycheck-hledger](https://github.com/DamienCassou/flycheck-hledger/):

![flycheck-hledger](_assets/flycheck-hledger.png?raw=true "flycheck-hledger")

## Testing

`hledger-mode` contains automated tests. You may run them by executing
`cask exec ert-runner`.

You may need to install `cask` locally if you do not already have it
installed, and run `cask install` to install any dependencies that you
may be missing.

## Contributing

This project is new and improving. Please feel free to contribute to
it. You might start with writing a document on contributing to the
project or by refactoring it a bit [See `hledger-reports.el`. It's a
mess.].

Cheers!
