fretboard.el
============

A guitar fretboard visualization in Emacs.

[![](https://raw.githubusercontent.com/vifon/fretboard.el/master/screenshot.png)](https://raw.githubusercontent.com/vifon/fretboard.el/master/screenshot.png)

USAGE
-----

Use `M-x fretboard` to open the fretboard visualization.

Use the keys corresponding to the notes to highlight them.
To highlight semitones, prefix the key with `#` (yes, prefix!).

INSTALLATION
------------

To install `fretboard.el` using `straight.el`, use the following code:

```elisp
(use-package fretboard
  :straight (:host github :repo "vifon/fretboard.el")
  :defer t)
```
