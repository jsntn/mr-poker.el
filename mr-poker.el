;;; mr-poker.el --- A package to practice memorizing poker cards -*- lexical-binding: t; -*-

;; Author: Jason Tian <hi@jsntn.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: games
;; URL: https://github.com/jsntn/mr-poker

;;; Commentary:

;;; Code:


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

(defun mr-poker-shuffle-cards (num-cards)
  "Shuffle the deck of cards and return a list of the specified number of cards."
  (let* ((cards (mr-poker-def-cards))
	 (shuffled-cards (mr-poker-shuffle cards)))
    (seq-take shuffled-cards num-cards)))

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
         ("10" . "10")
         ("J" . "Jack")
         ("Q" . "Queen")
         ("K" . "King"))))
    (if (member abbrev suits)
        abbrev
      (let ((suit-abbrev (substring abbrev 0 1))
        (value-abbrev (substring abbrev 1 2)))
    (concat (cdr (assoc suit-abbrev suits)) " "
        (cdr (assoc value-abbrev values)))))))

(defun mr-poker-recall ()
  "Recall the shuffled cards displayed in the *Shuffled Cards* buffer.
Recognize the abbreviation in upper or lower case, and prompt the user to recall
each card one by one to test if they are in the correct order."
  (interactive)
  (with-current-buffer "*Shuffled Cards*"
    (let ((cards (split-string (buffer-string) "\n" t))
	  (index 0))
      (while (< index (length cards))
	(let ((user-input
	       (read-string
		(format "Recall card %d (abbreviation): "
			(1+ index)))))
	  (setq user-input (upcase user-input)) ; Convert to uppercase
	  (let ((full-name (mr-poker-abbrev-to-full user-input)))
	    (while (not (equal full-name (nth index cards)))
	      (message "Incorrect. Try again.")
	      (setq user-input
		    (read-string
		     (format "Recall card %d (abbreviation): "
			     (1+ index))))
	      (setq user-input (upcase user-input))
	      (setq full-name (mr-poker-abbrev-to-full user-input)))
	    (message "Correct! Card %s is %s." (1+ index) full-name)))
    (setq index (1+ index))))))


(provide 'mr-poker)
;;; mr-poker.el ends here
