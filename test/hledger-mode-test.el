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

(ert-deftest ert-hledger-account-bounds-is-correct ()
  (with-temp-buffer
    (insert "alias save those spaces = expenses:payee with spaces

2023-06-26 Payee | Description  ; comment
  assets:bank:savings account  -INR 100
  expenses:payee with spaces  INR 100

2023-06-27 Description
  assets:bank:savings account  -$50
  expenses:personal  $50")
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
    ;; in a different amount
    (save-excursion
      (search-forward "$")
      (let ((bounds (hledger-bounds-of-account-at-point)))
        (should (null bounds))))
    (save-excursion
      (search-forward "-$5")
      (let ((bounds (hledger-bounds-of-account-at-point)))
        (should (null bounds))))
    (save-excursion
      (search-forward " $5")
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

(ert-deftest hledger-date-manipulation-works ()
  (with-temp-buffer
    (insert "2023-07-08")
    (backward-char)
    (hledger-add-days-to-entry-date 1)
    (should (string= (buffer-substring-no-properties (point-min) (point-max)) "2023-07-09"))
    (hledger-add-days-to-entry-date -2)
    (should (string= (buffer-substring-no-properties (point-min) (point-max)) "2023-07-07"))))
