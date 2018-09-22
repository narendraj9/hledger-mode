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

(ert-deftest ert-test-account-name-doesnt-include-amount ()
  "Account name regex match does not include amount"
  (should (equal (act-name-first-match "Revenues:Consulting Income $42.00") "Revenues:Consulting Income")))

(ert-deftest ert-test-account-name-doesnt-match-if-starting-whitelist-not-matched ()
  "Account name regex match doesn't match if it does not start with a whitelisted word"
  (should (equal (string-match hledger-account-regex "BogusType:Banking") nil)))

(ert-deftest ert-test-account-name-matches-with-digit ()
  "Account name regex matches account name containing digit"
  (should (equal (act-name-first-match "Revenues:Consulting Income 9") "Revenues:Consulting Income 9")))

(ert-deftest ert-test-account-name-nested ()
  "Account name may be nested more than one level"
  (should (equal (act-name-first-match "Revenues:Consulting Income:Haskell") "Revenues:Consulting Income:Haskell")))


(setq hledger-currency-string "$")

(ert-deftest ert-test-amount-with-different-currency-string ()
  "Test matching an amount after changing the currency string to dollars"
  (should (equal (first-amount-match "$400.00") "$400.00")))

(ert-deftest ert-test-amount-with-comma ()
  "Test amount matching containing a comma"
  (should (equal (first-amount-match "$4,000.00") "$4,000.00")))
