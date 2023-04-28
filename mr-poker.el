;;; mr-poker.el --- A package to practice memorizing poker cards -*- lexical-binding: t; -*-

;; Author: Jason Tian <hi@jsntn.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: games
;; URL: https://github.com/jsntn/mr-poker.el

;;; Commentary:

;;; Code:


(defun mr-poker-display-instructions ()
  "Display instructions for using the Mr Poker."
  (interactive)
  (with-current-buffer (get-buffer-create "*Mr Poker Instructions*")
    (erase-buffer)
    (insert "Mr Poker Instructions:\n\n")
    (insert "mr-poker-shuffle-and-display\n")
    (insert "Shuffle the deck of cards and displays a specified number of cards in a new *Shuffled Cards* buffer.\n\n")
    (insert "mr-poker-recall\n")
    (insert "Recall the shuffled cards displayed in the *Shuffled Cards* buffer.\nWith universal argument (C-u), recall in reverse order.\n\n")
    (insert "To recall a card, enter its abbreviation in the following format:\n\n")
    (insert "Suit (S, H, D, C) + Value (A, 2-9, T, J, Q, K)\n\n")
    (insert "For example:\n")
    (insert "- SA for Spades Ace\n")
    (insert "- H8 for Hearts 8\n")
    (insert "- DT for Diamonds 10  --> T for 10\n")
    (insert "- CQ for Clubs Queen\n\n")
    (insert "mr-poker-recall-resume\n")
    (insert "Resume the mr-poker-recall function from the last index processed.\nTo resume the reverse order recall, use universal argument (C-u).")
    (display-buffer (current-buffer))
    (message "Mr Poker Instructions displayed.")))

(defun mr-poker-def-cards ()
  "Define all the poker cards and return a list of their values."
  (let ((values '("Ace" "2" "3" "4" "5" "6" "7" "8" "9" "10" "Jack" "Queen" "King"))
	(suits '("Spades" "Hearts" "Diamonds" "Clubs"))
	(cards '()))
    (dolist (suit suits)
      (dolist (value values)
	(setq cards (cons (concat suit " " value) cards))))
    cards))

(defun mr-poker-shuffle (list)
  "Shuffle the given list using the Fisher-Yates shuffle algorithm
and return the shuffled list."
  (let ((n (length list)))
    (while (> n 1)
      (setq n (1- n))
      (let ((i (random n)))
	(let ((tmp (nth i list)))
	  (setcar (nthcdr i list) (car (nthcdr n list)))
	  (setcar (nthcdr n list) tmp)))))
  list)

(defun mr-poker-shuffle-and-display ()
  "Shuffle the deck of cards and display a specified number of cards in a new buffer."
  (interactive)
  (let* ((num-cards (read-number "How many cards do you want to shuffle and display? "))
	 (cards (mr-poker-def-cards))
	 (shuffled-cards (mr-poker-shuffle cards)))
    (setq shuffled-cards (seq-take shuffled-cards num-cards))
    (with-current-buffer (get-buffer-create "*Shuffled Cards*")
      (erase-buffer)
      (dolist (card shuffled-cards)
	(insert (concat card "\n")))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun mr-poker-abbrev-to-full (abbrev)
  "Convert a card abbreviation to its full name.
Recognize abbreviations like 'C6'."
  (let ((suits '(("S" . "Spades")
             ("H" . "Hearts")
         ("D" . "Diamonds")
         ("C" . "Clubs")))
        (values '(("A" . "Ace")
         ("2" . "2")
         ("3" . "3")
         ("4" . "4")
         ("5" . "5")
         ("6" . "6")
         ("7" . "7")
         ("8" . "8")
         ("9" . "9")
         ("T" . "10")
         ("J" . "Jack")
         ("Q" . "Queen")
         ("K" . "King"))))
    (if (member abbrev suits)
        abbrev
      (let ((suit-abbrev (substring abbrev 0 1))
        (value-abbrev (substring abbrev 1 2)))
    (concat (cdr (assoc suit-abbrev suits)) " "
        (cdr (assoc value-abbrev values)))))))

(defvar mr-poker-last-index nil
  "The last index processed by the mr-poker-recall function.")

(defun mr-poker-recall (&optional resume)
  "Recall the shuffled cards displayed in the *Shuffled Cards* buffer.
Recognize the abbreviation in upper or lower case, and prompt the user to recall
each card one by one to test if they are in the correct order."
  (interactive)
  (with-current-buffer "*Shuffled Cards*"
    (let ((cards (split-string (buffer-string) "\n" t))
	  (index (if (and resume mr-poker-last-index)
		     mr-poker-last-index
		   0)))
      (if current-prefix-arg  ; Check for C-u
	  (setq cards (reverse cards))) ; If present, reverse cards list
      (while (< index (length cards))
	(let ((user-input
	       (read-string
		(format "Recall card %d (abbreviation): "
			(if current-prefix-arg
			    (- (length cards) index)
			  (1+ index))))))
	  (setq user-input (upcase user-input)) ; Convert to uppercase
	  (let ((full-name (mr-poker-abbrev-to-full user-input)))
	    (while (not (equal full-name (nth index cards)))
	      (message "Incorrect. Try again.")
	      (setq user-input
		    (read-string
		     (format "Recall card %d (abbreviation): "
			     (if current-prefix-arg
				 (- (length cards) index)
			       (1+ index)))))
	      (setq user-input (upcase user-input))
	      (setq full-name (mr-poker-abbrev-to-full user-input)))
	    (message "Correct! Card %s is %s."
		     (if current-prefix-arg
			 (- (length cards) index)
		       (1+ index))
		     full-name)))
	(setq index (1+ index))
	(setq mr-poker-last-index index)))))

(defun mr-poker-recall-resume ()
  "Resume the mr-poker-recall function from the last index processed."
  (interactive)
  (if mr-poker-last-index
      (progn
        (message "Resuming from index %d." mr-poker-last-index)
        (mr-poker-recall t))
    (message "No last index recorded. Please start mr-poker-recall first.")))


(provide 'mr-poker)
;;; mr-poker.el ends here
