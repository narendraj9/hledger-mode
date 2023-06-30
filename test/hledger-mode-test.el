(require 'hledger-core)

(defun act-name-first-match (nme)
  (string-match hledger-account-regex nme)
  (match-string 0 nme))

(defun first-amount-match (nme)
  (string-match (hledger-amount-regex) nme)
  (match-string 0 nme))

(ert-deftest ert-test-correct-account-no-spaces ()
  "Account name regex matches account name with no spaces"
  (should (equal (act-name-first-match "Revenues:Income") "Revenues:Income")))

(ert-deftest ert-test-account-name-with-space ()
  "Account name regex matches account name with a space"
  (should (equal (act-name-first-match "Revenues:Consulting Income") "Revenues:Consulting Income")))

(ert-deftest ert-test-malformed-account-is-matched-fully ()
  "Account name regex match does include amount when not correctly separated"
  (should (equal (act-name-first-match "Revenues:Consulting Income $42.00") "Revenues:Consulting Income $42.00")))

(ert-deftest ert-test-account-name-doesnt-match-if-starting-whitelist-not-matched ()
  "Account name regex match doesn't match if it does not start with a whitelisted word"
  (should (equal (string-match hledger-account-regex "BogusType:Banking") nil)))

(ert-deftest ert-test-account-name-matches-with-digit ()
  "Account name regex matches account name containing digit"
  (should (equal (act-name-first-match "Revenues:Consulting Income 9") "Revenues:Consulting Income 9")))

(ert-deftest ert-test-account-name-nested ()
  "Account name may be nested more than one level"
  (should (equal (act-name-first-match "Revenues:Consulting Income:Haskell") "Revenues:Consulting Income:Haskell")))

(ert-deftest ert-test-account-name-with-non-ascii-and-punctuation ()
  "Account name regex matches account name with non-ASCII characters and punctuation"
  (should (equal (act-name-first-match "Revenues:Consulting Income:Kö Pte. Ltd.") "Revenues:Consulting Income:Kö Pte. Ltd.")))

(ert-deftest ert-test-account-name-doesnt-match-forbidden-characters ()
  "Account name regex match stops at forbidden characters"
  (let ((m (string-match hledger-account-regex "Revenues:Consulting Income:Company;Co")))
    (should (= (match-beginning 0) 0))
    (should (= (match-end 0) 34))))


(setq hledger-currency-string "$")

(ert-deftest ert-test-amount-with-different-currency-string ()
  "Test matching an amount after changing the currency string to dollars"
  (should (equal (first-amount-match "$400.00") "$400.00")))

(ert-deftest ert-test-amount-with-comma ()
  "Test amount matching containing a comma"
  (should (equal (first-amount-match "$4,000.00") "$4,000.00")))

(defconst hledger--ert-test-input "alias save:those spaces = expenses:payee with spaces

2023-06-26 Payee | Description: Details  ; comment
  assets:bank:savings account  -INR 100  ; tag:value
  expenses:payee with spaces  INR 100")

(ert-deftest ert-hledger-account-bounds-is-correct ()
  (with-temp-buffer
    (insert hledger--ert-test-input)
    (goto-char 0)
    ;; in the middle of an account with spaces
    (save-excursion
      (search-forward "savings account")
      (let* ((bounds (hledger-bounds-of-account-at-point))
             (text (buffer-substring (car bounds) (cdr bounds))))
        (should (string= text "assets:bank:savings account"))))
    ;; in an amount
    (save-excursion
      (search-forward "-INR")
      (let ((bounds (hledger-bounds-of-account-at-point)))
        (should (null bounds))))
    ;; in the description line
    (save-excursion
      (search-forward "Payee")
      (let ((bounds (hledger-bounds-of-account-at-point)))
        (should (null bounds))))
    ;; in an alias value
    (save-excursion
      (search-forward "with spaces")
      (let* ((bounds (hledger-bounds-of-account-at-point))
             (text (buffer-substring (car bounds) (cdr bounds))))
        (should (string= text "expenses:payee with spaces"))))))

(ert-deftest ert-hledger-fontification-is-correct ()
  ;; Patterned after `groovy-mode': https://emacs.stackexchange.com/a/46902/19248
  (with-temp-buffer
    (insert hledger--ert-test-input)
    (goto-char 0)
    (delay-mode-hooks (hledger-mode))
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings
        (font-lock-fontify-buffer)))
    (should
     (equal-including-properties
      (buffer-string)
      #("alias save:those spaces = expenses:payee with spaces\n\n2023-06-26 Payee | Description: Details  ; comment\n  assets:bank:savings account  -INR 100  ; tag:value\n  expenses:payee with spaces  INR 100" 6 10
        (face font-lock-variable-name-face)
        10 11
        (face hledger-account-colon-face)
        11 23
        (face font-lock-variable-name-face)
        26 34
        (face font-lock-variable-name-face)
        34 35
        (face hledger-account-colon-face)
        35 52
        (face font-lock-variable-name-face)
        54 64
        (face hledger-date-face)
        95 97
        (face font-lock-comment-delimiter-face)
        97 105
        (face font-lock-comment-face)
        107 113
        (face font-lock-variable-name-face)
        113 114
        (face hledger-account-colon-face)
        114 118
        (face font-lock-variable-name-face)
        118 119
        (face hledger-account-colon-face)
        119 134
        (face font-lock-variable-name-face)
        137 144
        (face font-lock-constant-face)
        146 148
        (face font-lock-comment-delimiter-face)
        148 151
        (face font-lock-comment-face)
        151 152
        (face font-lock-comment-face)
        152 158
        (face font-lock-comment-face)
        160 168
        (face font-lock-variable-name-face)
        168 169
        (face hledger-account-colon-face)
        169 186
        (face font-lock-variable-name-face)
        188 195
        (face font-lock-constant-face))))))
