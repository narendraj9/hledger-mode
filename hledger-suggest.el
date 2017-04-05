;;; hledger-suggest.el --- Providing useful suggestions for new journal entries  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;;
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file would contain my attempts to make entries journal entries
;; as easy as possible for me.  That means using the data that I
;; already have to be as quick as possible in generating future
;; entries.  This would make sure that I don't skip writing entries as
;; I move through various stages in my life.  Whatever, let's see what
;; this turns into.
;;

;;; Code:

; (require 'emlib) ;; not needed yet

(defvar hledger-suggest-model nil
  "Model we will train or read from disk for providing suggestions.")

(defun hledger-suggest (what)
  "Provide suggestion for an entry at the moment.
Argument WHAT is the type of thing we want a suggestion for.")


(provide 'hledger-suggest)
;;; hledger-suggest.el ends here
