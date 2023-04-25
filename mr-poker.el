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
  "Shuffle the given list using the Fisher-Yates shuffle algorithm and return the shuffled list."
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

(defun mr-poker-recall ()
  "Recall the shuffled cards displayed in the *Shuffled Cards* buffer and prompt the user to recall each card one by one to test if they are in the correct order."
  (interactive)
  (with-current-buffer "*Shuffled Cards*"
    (let ((cards (split-string (buffer-string) "\n" t))
	  (index 0))
      (while (< index (length cards))
	(let ((card (read-string (format "Recall card %d: " (1+ index)) nil nil (nth index cards))))
	  (while (not (equal card (nth index cards)))
	    (message "Incorrect. Try again.")
	    (setq card (read-string (format "Recall card %d: " (1+ index)) nil nil)))
	  (message "Correct!")
	  (setq index (1+ index)))))))


(provide 'mr-poker)
;;; mr-poker.el ends here
