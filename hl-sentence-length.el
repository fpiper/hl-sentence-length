;;; hl-sentence-length.el --- Highlights sentences by word count

;; Copyright (C) 2022 Ferdinand Pieper

;; Author: Ferdinand Pieper <mail@pie.tf>
;; URL: https://github.com/fpiper/hl-sentence-length

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Minor mode to highlight sentences based on the number of words.
;; This is intended to help writers focus on varying their sentence
;; lengths. By default it cycles trough eight different colors. To
;; increase this set `hl-sentence-length-cycle' and customize the
;; faces for higher word counts or define new ones.
;;
;;; Code:

(defface hl-sentence-length-face-1
  '((t (:background "#7d9fb2")))
  "Face for sentences of length 1.")
(defface hl-sentence-length-face-2
  '((t (:background "#d5b195")))
  "Face for sentences of length 2.")
(defface hl-sentence-length-face-3
  '((t (:background "#6f9e91")))
  "Face for sentences of length 3.")
(defface hl-sentence-length-face-4
  '((t (:background "#cc8fb1")))
  "Face for sentences of length 4.")
(defface hl-sentence-length-face-5
  '((t (:background "#a3d0e9")))
  "Face for sentences of length 5.")
(defface hl-sentence-length-face-6
  '((t (:background "#e6d1a1")))
  "Face for sentences of length 6.")
(defface hl-sentence-length-face-7
  '((t (:background "#f0eba8")))
  "Face for sentences of length 7.")
(defface hl-sentence-length-face-8
  '((t (:background "#707070")))
  "Face for sentences of length 8.")
;; Faces 9-12 declared just for convenience. Easily customize using
;; `customize-face' or use-package's `:custom-face' keyword.
(defface hl-sentence-length-face-9
  '((t (:background "#ffffff")))
  "Face for sentences of length 9.")
(defface hl-sentence-length-face-10
  '((t (:background "#ffffff")))
  "Face for sentences of length 10.")
(defface hl-sentence-length-face-11
  '((t (:background "#ffffff")))
  "Face for sentences of length 11.")
(defface hl-sentence-length-face-12
  '((t (:background "#ffffff")))
  "Face for sentences of length 12.")

(defvar hl-sentence-length-cycle 8
  "Number of faces to cycle trough.")

(defun hl-sentence-length-sentence-beginning ()
  "Return point at beginning of the current sentence."
  (save-excursion
    (backward-sentence)
    (point)))

(defun hl-sentence-length-sentence-end ()
  "Return point at end of the current sentence."
  (save-excursion
    (backward-sentence)
    (forward-sentence)
    (point)))

(defun hl-sentence-length-sentence-length ()
  "Return length of current sentence."
  (save-mark-and-excursion
    (backward-sentence)
    (set-mark (point))
    (forward-sentence)
    (count-words-region (region-beginning) (region-end))))

(defun hl-sentence-length-sentence-face ()
  "Return the appropriate face for the current sentence."
  (let* ((length (hl-sentence-length-sentence-length))
         (modulo (mod length hl-sentence-length-cycle))
         (facenumber (if (eq modulo 0)
                         hl-sentence-length-cycle
                       modulo)))
    (intern (format "hl-sentence-length-face-%i" facenumber))))

(defun hl-sentence-length-highlight-sentence ()
  "Highlight the current sentence based on its length."
  (let ((face (hl-sentence-length-sentence-face))
        (overlay (make-overlay (hl-sentence-length-sentence-beginning) (hl-sentence-length-sentence-end))))
    (overlay-put overlay 'origin 'hl-sentence-length)
    (overlay-put overlay 'face face)))

(define-minor-mode hl-sentence-length-mode
  "Highlight the word count of sentences."
  nil nil nil
  (if hl-sentence-length-mode
      (add-hook 'post-command-hook 'hl-sentence-length-update nil t)
    (progn
      (remove-hook 'post-command-hook 'hl-sentence-length-update t)
      (hl-sentence-length-remove-highlights))))

(defun hl-sentence-length-create-highlights ()
  (save-mark-and-excursion
    (let ((pmax (point-max)))
      (beginning-of-buffer)
      (while (< (point) pmax)
        (forward-sentence)
        (hl-sentence-length-highlight-sentence)))))

(defun hl-sentence-length-remove-highlights ()
  (remove-overlays nil nil 'origin 'hl-sentence-length))

(defun hl-sentence-length-update ()
  "Update the highlighted sentence lengths."
  ;; FIXME There is probably a better way to do this.
  (hl-sentence-length-remove-highlights)
  (hl-sentence-length-create-highlights))

;;; hl-sentence-length.el ends here
