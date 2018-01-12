# swap-regions.el [![MELPA](http://melpa.org/packages/swap-regions-badge.svg)](http://melpa.org/#/swap-regions)

You can swap text in two regions using

    M-x swap-regions [select the first region] C-M-c [select the second region] C-M-c

Note that <kbd>C-M-c</kbd> runs `exit-recursive-edit` which is bound
by default in vanilla Emacs. And while you are selecting regions, you
can run any Emacs command thanks to [Recursive
Editing](https://www.gnu.org/software/emacs/manual/html_node/elisp/Recursive-Editing.html).
