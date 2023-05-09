;;; selrgn.el --- Select region by things  -*- lexical-binding:t -*-

;; Copyright (C) 2023 Iku Iwasa

;; Author: Iku Iwasa <iku.iwasa@gmail.com>
;; Version: 0.1.0
;; Keywords: matching
;; Package-Requires: ((emacs "27.1") (compat "29.1.1.1"))
;; Homepage: https://github.com/iquiw/selrgn


;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;;
;; Select region by things.
;;

;;; Code:
(require 'text-property-search)
(require 'thingatpt)

(require 'compat)

(defvar-keymap selrgn-command-map
  :prefix 'selrgn-prefix-command
  "#" #'selrgn-number
  "'" #'selrgn-symbol
  "+" #'selrgn-enlarge
  "-" #'selrgn-shrink
  "/" #'selrgn-filename
  "4" #'selrgn-ipv4
  "U" #'selrgn-uuid
  "e" #'selrgn-email
  "f" #'selrgn-face
  "m" #'selrgn-mac
  "p" #'selrgn-pair
  "s" #'selrgn-string
  "u" #'selrgn-url
  "v" #'selrgn-version
  "w" #'selrgn-word)

(defun selrgn-enlarge (num)
  "Enlarge both sides of region by NUM characters."
  (interactive "p")
  (when (use-region-p)
    (let ((beg (- (region-beginning) num))
          (end (+ (region-end) num)))
      (set-mark (min end (point-max)))
      (goto-char (max beg (point-min))))))

(defun selrgn-shrink (num)
  "Shrink both sides of region by NUM characters."
  (interactive "p")
  (when (use-region-p)
    (let ((beg (+ (region-beginning) num))
          (end (- (region-end) num)))
      (when (< beg end)
        (set-mark end)
        (goto-char beg)))))

(defun selrgn--face-change-p (face condition)
  "Return nil if FACE does not match with CONDITION."
  (cond
   ((not condition) t)
   ((listp condition) (not (member face condition)))
   (t (not (equal face condition)))))

(defun selrgn-face ()
  "Select region with the same face."
  (interactive)
  (when-let* ((face (face-at-point))
              (prop-end (save-excursion
                          (text-property-search-forward
                           'face face #'selrgn--face-change-p)))
              (prop-beg (save-excursion
                          (text-property-search-backward
                           'face face #'selrgn--face-change-p))))
    (selrgn--select-bounds
     (cons
      (prop-match-end prop-beg)
      (prop-match-beginning prop-end)))))

(defun selrgn-pair ()
  "Select pair region."
  (interactive)
  (let ((ppss (syntax-ppss)))
    (when (< 0 (ppss-depth ppss))
      (let ((beg (ppss-innermost-start ppss)))
        (selrgn--select-bounds
         (cons beg
               (save-excursion
                 (goto-char beg)
                 (forward-sexp)
                 (point))))))))

(defun selrgn-string ()
  "Select inside string region."
  (interactive)
  (let ((ppss (syntax-ppss)))
    (when (ppss-string-terminator ppss)
      (selrgn--select-bounds
       (cons
        (1+ (ppss-comment-or-string-start ppss))
        (save-excursion
          (while (and (< 0 (skip-syntax-forward "^\"|"))
                      (progn
                        (forward-char 1)
                        (ppss-string-terminator (syntax-ppss))))
                                        ; nop
            )
          (1- (point))))))))

;;
;; Select region by regexp.
;;
(defun selrgn-ipv4 ()
  "Select IPv4 address region."
  (interactive)
  (selrgn--regexp "0-9." "\\(?:\\(?:0\\|[1-9][0-9]\\{0,2\\}\\)\\.\\)\\{3\\}\\(?:0\\|[1-9][0-9]\\{0,2\\}\\)"))

(defun selrgn-mac ()
  "Select MAC address region."
  (interactive)
  (selrgn--regexp "0-9a-fA-F:" "\\(?:[0-9a-fA-F]\\{2\\}:\\)\\{5\\}\\(?:[0-9a-fA-F]\\{2\\}\\)"))

(defun selrgn-version ()
  "Select version-like region."
  (interactive)
  (selrgn--regexp "0-9." "\\(?:[0-9]+\\.\\)+[0-9]+"))

(defun selrgn--regexp (chars regexp)
  "Select region matched with CHARS and REGEXP.
First, it skip the CHARS backwards and regexp match with REGEXP."
  (when-let ((bounds (save-excursion
                       (when (and (skip-chars-backward chars)
                                  (looking-at regexp))
                         (cons (point) (match-end 0))))))
    (selrgn--select-bounds bounds)))

;;
;; Select region using thingatpt.
;;
(defun selrgn-email ()
  "Select E-mail region."
  (interactive)
  (selrgn--thing 'email))

(defun selrgn-filename ()
  "Select filename region."
  (interactive)
  (selrgn--thing 'filename))

(defun selrgn-number ()
  "Select number region."
  (interactive)
  (selrgn--thing 'number))

(defun selrgn-symbol ()
  "Select word region."
  (interactive)
  (selrgn--thing 'symbol))

(defun selrgn-url ()
  "Select URL region."
  (interactive)
  (selrgn--thing 'url))

(defun selrgn-uuid ()
  "Select UUID region."
  (interactive)
  (selrgn--thing 'uuid))

(defun selrgn-word ()
  "Select word region."
  (interactive)
  (selrgn--thing 'word))

(defun selrgn--thing (thing)
  "Select a thing at the point.
THING is the same meaing as in `bounds-of-thing-at-point'."
  (when-let ((bounds (bounds-of-thing-at-point thing)))
    (selrgn--select-bounds bounds)))

;;
;; Select region by bounds.
;;
(defun selrgn--select-bounds (bounds)
  "Select region specified by BOUNDS.
BOUNDS is a cons of (beg . end) region."
  (let ((beg (car bounds))
        (end (cdr bounds)))
    (when (/= beg end)
      ;; copied from expand-region `er--prepare-expanding'.
      (push-mark nil t) ;; one for keeping starting position
      (push-mark nil t) ;; one for replace by set-mark in expansions
      (set-mark end)
      (goto-char beg))))

(provide 'selrgn)
;;; selrgn.el ends here
