;;; fretboard.el --- A guitar fretboard visualization  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Wojciech Siewierski

;; Author: Wojciech Siewierski
;; URL: https://github.com/vifon/fretboard.el
;; Keywords: games
;; Version: 0.9
;; Package-Requires: ((emacs "28.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A guitar fretboard visualization tool.

;; Use the `fretboard' command to open a buffer with an interactive
;; guitar fretboard visualization.

;; Use the keys corresponding to the notes to highlight them.
;; To highlight semitones, prefix the key with # (yes, prefix!).

;;; Code:

(require 'hi-lock)


(defgroup fretboard nil
  "A guitar fretboard visualizer."
  :group 'applications)

(defgroup fretboard-faces nil
  "Faces in `fretboard-mode'."
  :group 'fretboard)

(defcustom fretboard-highlight-faces
  (mapcar #'intern hi-lock-face-defaults)
  "Faces to use for note highlighting in `fretboard-mode'."
  :type '(repeat face))


(defun fretboard-highlight-note ()
  "Highlight the note corresponding to the pressed key.
The key is being read directly from the used keybinding."
  (interactive)
  (let* ((note (reverse (upcase (this-command-keys))))
         (regex (rx symbol-start (literal note) symbol-end))
         (note-id (- (elt note 0) ?A))
         (face (elt fretboard-highlight-faces note-id))
         (font-lock-pattern `((,regex . ',face))))
    (if (assoc regex font-lock-keywords)
        (font-lock-remove-keywords nil font-lock-pattern)
      (font-lock-add-keywords nil font-lock-pattern))
    (font-lock-flush)))


(defvar fretboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'fretboard-highlight-note)
    (define-key map (kbd "d") #'fretboard-highlight-note)
    (define-key map (kbd "e") #'fretboard-highlight-note)
    (define-key map (kbd "f") #'fretboard-highlight-note)
    (define-key map (kbd "g") #'fretboard-highlight-note)
    (define-key map (kbd "a") #'fretboard-highlight-note)
    (define-key map (kbd "b") #'fretboard-highlight-note)

    (define-key map (kbd "# c") #'fretboard-highlight-note)
    (define-key map (kbd "# d") #'fretboard-highlight-note)
    (define-key map (kbd "# f") #'fretboard-highlight-note)
    (define-key map (kbd "# g") #'fretboard-highlight-note)
    (define-key map (kbd "# a") #'fretboard-highlight-note)
    map))

(defface fretboard-fretline-face
  '((((background light)) :foreground "#ddd")
    (((background dark))  :foreground "#333"))
  "Face for the fret lines on the fretboard.")

(defface fretboard-semitone-face
  '((((background light)) :foreground "#777")
    (((background dark))  :foreground "#999"))
  "Face for the semitones.")

(defface fretboard-fret-number-face
  '((((background light)) :foreground "#33f")
    (((background dark))  :foreground "#707"))
  "Face used for the fret numbers.")

(defcustom fretboard-text
  "

 E  ||  F  | F# |  G  | G# | A | A# |  B  | C  | C# | D | D# |  E  | F  |
 B  ||  C  | C# |  D  | D# | E | F  |  F# | G  | G# | A | A# |  B  | C  |
 G  ||  G# | A  |  A# | B  | C | C# |  D  | D# | E  | F | F# |  G  | G# |
 D  ||  D# | E  |  F  | F# | G | G# |  A  | A# | B  | C | C# |  D  | D# |
 A  ||  A# | B  |  C  | C# | D | D# |  E  | F  | F# | G | G# |  A  | A# |
 E  ||  F  | F# |  G  | G# | A | A# |  B  | C  | C# | D | D# |  E  | F  |

                  III        V        VII        IX            XII
"
  "The contents of the buffer created with `fretboard'.

If customized, should keep the general conventions to preserve
the correct syntax highlighting."
  :group 'fretboard
  :type 'string)

(defvar fretboard-mode-font-lock-keywords
  '(("|+" . 'fretboard-fretline-face)
    ("[XVI]+" . 'fretboard-fret-number-face)
    ("[A-G]#" . 'fretboard-semitone-face)
    ("\\([A-G]\\)[^#]" 1 'bold)))

(defvar fretboard-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "_" st)
    st))

(define-derived-mode fretboard-mode special-mode "fretboard"
  "Major mode for the fretboard visualization."
  (read-only-mode 1)
  (text-scale-set 2)
  (setq-local revert-buffer-function (lambda (_ignore-auto _noconfirm)
                                       (fretboard)))
  (setq font-lock-defaults
        '(fretboard-mode-font-lock-keywords
          t)))

;;;###autoload
(defun fretboard ()
  "Display an interactive guitar fretboard."
  (interactive)
  (pop-to-buffer (get-buffer-create "*fretboard*"))
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert fretboard-text))
  (goto-char (point-min))
  (fretboard-mode))


(provide 'fretboard)
;;; fretboard.el ends here
