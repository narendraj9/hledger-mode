# Hledger-mode

[![MELPA](https://melpa.org/packages/hledger-mode-badge.svg)](https://melpa.org/#/hledger-mode)

A mode for writing hledger journals with a set of useful reports.

## Installation

It's available on [MELPA](http://melpa.org/):

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

```

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

## Auxiliary tools

You are welcome to use the web application hosted
at [vicarie.in](https://services.vicarie.in) for logging data while
you are away from computer. You can use the command
`hledger-fetch-entries` later on to get those entries into your
journal file.


## Contributing

This project is new and improving. Please feel free to contribute to
it. You might start with writing a document on contributing to the
project or by refactoring it a bit [See `hledger-reports.el`. It's a
mess.].

Cheers!
