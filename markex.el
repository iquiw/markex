;;; markex.el --- Mark things extra  -*- lexical-binding:t -*-

;; Copyright (C) 2023 Iku Iwasa

;; Author: Iku Iwasa <iku.iwasa@gmail.com>
;; Version: 0.1.0
;; Keywords: matching
;; Package-Requires: ((emacs "27.1") (compat "29.1.1.1"))
;; Homepage: https://github.com/iquiw/markex


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

(defvar-keymap markex-command-map
  :prefix 'markex-prefix-command
  "#" #'markex-number
  "'" #'markex-symbol
  "+" #'markex-enlarge
  "-" #'markex-shrink
  "/" #'markex-filename
  "4" #'markex-ipv4
  "U" #'markex-uuid
  "e" #'markex-email
  "f" #'markex-face
  "m" #'markex-mac
  "p" #'markex-pair
  "s" #'markex-string
  "u" #'markex-url
  "v" #'markex-version
  "w" #'markex-word)

(defun markex-enlarge (num)
  "Enlarge both sides of region by NUM characters."
  (interactive "p")
  (when (use-region-p)
    (let ((beg (- (region-beginning) num))
          (end (+ (region-end) num)))
      (set-mark (min end (point-max)))
      (goto-char (max beg (point-min))))))

(defun markex-shrink (num)
  "Shrink both sides of region by NUM characters."
  (interactive "p")
  (when (use-region-p)
    (let ((beg (+ (region-beginning) num))
          (end (- (region-end) num)))
      (when (< beg end)
        (set-mark end)
        (goto-char beg)))))

(defun markex--face-change-p (face condition)
  "Return nil if FACE does not match with CONDITION."
  (cond
   ((not condition) t)
   ((listp condition) (not (member face condition)))
   (t (not (equal face condition)))))

(defun markex-face ()
  "Select region with the same face."
  (interactive)
  (when-let* ((face (face-at-point))
              (prop-end (save-excursion
                          (text-property-search-forward
                           'face face #'markex--face-change-p)))
              (prop-beg (save-excursion
                          (text-property-search-backward
                           'face face #'markex--face-change-p))))
    (markex--select-bounds
     (cons
      (prop-match-end prop-beg)
      (prop-match-beginning prop-end)))))

;;
;; Select region by syntax.
;;
(defun markex-pair ()
  "Select pair region."
  (interactive)
  (let ((ppss (if (= (syntax-class (syntax-after (point))) 4)
                  (save-excursion (syntax-ppss (1+ (point))))
                (syntax-ppss))))
    (when (< 0 (ppss-depth ppss))
      (let ((beg (ppss-innermost-start ppss)))
        (markex--select-bounds
         (cons beg
               (save-excursion
                 (goto-char beg)
                 (forward-sexp)
                 (point))))))))

(defun markex-string ()
  "Select inside string region."
  (interactive)
  (let ((ppss (syntax-ppss)))
    (when (or (ppss-string-terminator ppss)
              ;; in case of start of string.
              (and (not (eobp))
                   (setq ppss (save-excursion (syntax-ppss (1+ (point)))))
                   (ppss-string-terminator ppss)))
      (let ((beg (1+ (ppss-comment-or-string-start ppss))))
        (markex--select-bounds
         (cons
          beg
          (save-excursion
            (let ((start (point)))
              (goto-char beg)
              (while (and (< 0 (skip-syntax-forward "^\"|"))
                          (progn
                            (forward-char 1)
                            (ppss-string-terminator (syntax-ppss))))
                ;; nop
                )
              (if (eq (point) start)
                  start
                (1- (point)))))))))))

;;
;; Select region by regexp.
;;
(defun markex-ipv4 ()
  "Select IPv4 address region."
  (interactive)
  (markex--regexp "0-9." "\\(?:\\(?:0\\|[1-9][0-9]\\{0,2\\}\\)\\.\\)\\{3\\}\\(?:0\\|[1-9][0-9]\\{0,2\\}\\)"))

(defun markex-mac ()
  "Select MAC address region."
  (interactive)
  (markex--regexp "0-9a-fA-F:" "\\(?:[0-9a-fA-F]\\{2\\}:\\)\\{5\\}\\(?:[0-9a-fA-F]\\{2\\}\\)"))

(defun markex-version ()
  "Select version-like region."
  (interactive)
  (markex--regexp "0-9." "\\(?:[0-9]+\\.\\)+[0-9]+"))

(defun markex--regexp (chars regexp)
  "Select region matched with CHARS and REGEXP.
First, it skip the CHARS backwards and regexp match with REGEXP."
  (when-let ((bounds (save-excursion
                       (when (and (skip-chars-backward chars)
                                  (looking-at regexp))
                         (cons (point) (match-end 0))))))
    (markex--select-bounds bounds)))

;;
;; Select region using thingatpt.
;;
(defun markex-email ()
  "Select E-mail region."
  (interactive)
  (markex--thing 'email))

(defun markex-filename ()
  "Select filename region."
  (interactive)
  (markex--thing 'filename))

(defun markex-number ()
  "Select number region."
  (interactive)
  (markex--thing 'number))

(defun markex-symbol ()
  "Select word region."
  (interactive)
  (markex--thing 'symbol))

(defun markex-url ()
  "Select URL region."
  (interactive)
  (markex--thing 'url))

(defun markex-uuid ()
  "Select UUID region."
  (interactive)
  (markex--thing 'uuid))

(defun markex-word ()
  "Select word region."
  (interactive)
  (markex--thing 'word))

(defun markex--thing (thing)
  "Select a thing at the point.
THING is the same meaing as in `bounds-of-thing-at-point'."
  (when-let ((bounds (bounds-of-thing-at-point thing)))
    (markex--select-bounds bounds)))

;;
;; Select region by bounds.
;;
(defun markex--select-bounds (bounds)
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

(provide 'markex)
;;; markex.el ends here