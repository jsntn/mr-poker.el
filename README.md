![](https://jsntn.github.io/mr-poker.el/static/img_3946.jpg)

# mr-poker.el

mr-poker.el is an Emacs package that allows you to practice memorizing poker cards. It provides functions for shuffling a deck of cards, displaying a specified number of cards, recalling the shuffled cards.

## Installation

mr-poker.el can be installed using [straight.el](https://github.com/radian-software/straight.el). To install, add the following to your Emacs configuration file:

```emacs
(straight-use-package
 '(mr-poker :type git :host github :repo "jsntn/mr-poker.el"))
```

## Upgrade

Use `M-x straight-pull-package` to get the latest version.

## Usage

After installing mr-poker.el, you can use the following functions:

`mr-poker-display-instructions`

This function displays instructions for using the Mr Poker, and the instructions are displayed in a new *Mr Poker Instructions* buffer on the screen.

`mr-poker-shuffle-and-display`

This function shuffles the deck of cards and displays a specified number of cards in a new buffer. To use it, type `M-x mr-poker-shuffle-and-display` and enter the number of cards you want to shuffle and display.

`mr-poker-recall`

This function allows you to recall the shuffled cards displayed in the *Shuffled Cards* buffer. It recognizes the abbreviation in upper or lower case and prompts you to recall each card one by one to test if they are in the correct order. To use it, display the *Shuffled Cards* buffer, type `M-x mr-poker-recall`, and follow the prompts.

With universal argument (`C-u`), recall in reverse order.

To recall a card, enter its abbreviation in the following format:

Suit (S, H, D, C) + Value (A, 2-9, T, J, Q, K)

For example:

- SA for Spades Ace
- H8 for Hearts 8
- DT for Diamonds 10  --> T for 10
- CQ for Clubs Queen

It supports the PAUSE by `C-g`.

`mr-poker-recall-resume`

This function allows you to resume the mr-poker-recall function from your last
left off (`C-g`).

To resume the reverse order recall, use universal argument (`C-u`).

## License

mr-poker.el is licensed under the GPL 3.0 License. See LICENSE for details.
