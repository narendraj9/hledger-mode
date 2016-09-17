# hledger-mode


A mode for writing hledger journals with a set of useful reports.

## Installation

To make `hledger-mode` available to your Emacs add it `load-path`:

```elisp 

(add-to-list 'load-path "/path/to/hledger-mode/dir/")
(require 'hledger-mode)

;; Provide the path to you journal file. 
;; The default location is too opinionated.
(setq hledger-jfile "/path/to/your/journal-file.journal")

```

## Usage

I recommend that the following setup:

```elisp

;; Personal Accounting
(global-set-key (kbd "C-c e") 'hledger-jentry)
(global-set-key (kbd "C-c j") 'hledger-run-command)

```

Once you are in a report buffer after executing a command with
`hledger-run-command`, press `h` to see the list of reports that you
can have a look at.

To enable email reporting, you would need to setup your email api credentials. You can set those up with `M-x custize-group hledger`. Once you have done so, you can enable monthly email reporting on `hledger-reporting-day` with the following:

```elisp

(hledger-enable-reporting)

```








