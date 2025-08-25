;;; markex.el --- Mark things extra  -*- lexical-binding:t -*-

;; Copyright (C) 2023 Iku Iwasa

;; Author: Iku Iwasa <iku.iwasa@gmail.com>
;; Version: 0.1.0
;; Keywords: matching
;; Package-Requires: ((emacs "28.1") (compat "29.1.1.1"))
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
  :repeat (:exit (markex-reset))
  "C-g" #'markex-reset
  "SPC" #'markex-space
  "#" #'markex-number
  "'" #'markex-symbol
  "+" #'markex-enlarge
  "-" #'markex-shrink
  "/" #'markex-filename
  "4" #'markex-ipv4
  "U" #'markex-uuid
  "a" #'markex-add-region-boundary
  "c" #'markex-change-region-boundary
  "e" #'markex-email
  "f" #'markex-face
  "l" #'markex-line
  "m" #'markex-mac
  "n" #'markex-non-space
  "p" #'markex-pair
  "s" #'markex-string
  "u" #'markex-url
  "v" #'markex-version
  "w" #'markex-word
  "x" #'markex-delete-region-boundary
  "y" #'markex-yank-swap)

(defvar-local markex--origin (make-marker))

;;
;; Region manipulation.
;;
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
      (when (<= beg end)
        (set-mark end)
        (goto-char beg)))))

(defun markex-add-region-boundary (char)
  "Add one character before region beginnig and one after region end with CHAR."
  (interactive "cAdd char: ")
  (when (use-region-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (save-excursion
        (goto-char beg)
        (insert char)
        (goto-char (1+ end))
        (if-let ((pair-char (matching-paren char)))
            (insert pair-char)
          (insert char))))))

(defun markex-change-region-boundary (char)
  "Replace one character before region beginnig and one after region end with CHAR."
  (interactive "cReplace char: ")
  (when (use-region-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (unless (or (= beg (point-min))
                  (= end (point-max)))
        (save-excursion
          (goto-char (1- beg))
          (delete-char 1)
          (insert char)
          (goto-char end)
          (delete-char 1)
          (if-let ((pair-char (matching-paren char)))
              (insert pair-char)
            (insert char)))
        (forward-char)))))

(defun markex-delete-region-boundary ()
  "Delete one character before region beginnig and one after region end."
  (interactive)
  (when (use-region-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (unless (or (= beg (point-min))
                  (= end (point-max)))
        (delete-region (1- beg) beg)
        (delete-region (1- end) end)))))

(defun markex-yank-swap ()
  "Swap selected region contents with the latest `kill-ring'."
  (interactive)
  (markex--reset-marker)
  (when (use-region-p)
    (let* ((select-enable-primary nil)
           (text (current-kill 0 t))
           (beg (region-beginning)))
      (let ((deactivate-mark nil))
        (kill-region beg (region-end))
        (insert-for-yank text))
      (setq this-command #'yank))))

(defun markex-reset ()
  "Reset selection and back to the original point."
  (interactive)
  (when (marker-position markex--origin)
    (goto-char markex--origin)
    (markex--reset-marker t))
  (deactivate-mark))

;;
;; Select region by face.
;;
(defun markex--face-change-p (face condition)
  "Return nil if FACE does not match with CONDITION."
  (cond
   ((not condition) t)
   ((listp condition) (not (member face condition)))
   (t (not (equal face condition)))))

(defun markex-face ()
  "Select region with the same face."
  (interactive)
  (markex--reset-marker)
  (when-let* ((face (face-at-point))
              (end (save-excursion
                     (let ((prop (text-property-search-forward
                                  'face face #'markex--face-change-p)))
                       (if prop
                           (prop-match-beginning prop)
                         (point-max)))))
              (beg (save-excursion
                     (let ((prop (text-property-search-backward
                                  'face face #'markex--face-change-p)))
                       (if prop
                           (prop-match-end prop)
                         (point-min))))))
    (markex--select-bounds (cons beg end))))

;;
;; Select region by syntax.
;;
(defun markex-pair ()
  "Select pair region."
  (interactive)
  (markex--reset-marker)
  (let ((ppss (if (= (syntax-class (syntax-after (point))) 4)
                  (save-excursion (syntax-ppss (1+ (point))))
                (syntax-ppss))))
    (when (< 0 (ppss-depth ppss))
      (let ((beg (ppss-innermost-start ppss)))
        (markex--select-bounds
         (cons beg
               (save-excursion
                 (goto-char beg)
                 (forward-list)
                 (point))))))))

(defun markex-string ()
  "Select inside string region."
  (interactive)
  (markex--reset-marker)
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
            (goto-char beg)
            (while (and (not (eobp))
                        (progn
                          (forward-char 1)
                          (ppss-string-terminator (syntax-ppss))))
              (skip-syntax-forward "^\"|"))
            (if (eq (point) beg)
                beg
              (1- (point))))))))))

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

(defun markex-number ()
  "Select number region."
  (interactive)
  (or (markex--regexp "#x0-9a-fA-F" "\\(0x\\|#x\\)\\([a-fA-F0-9]+\\)")
      (markex--regexp "0-9.-" "-?[0-9]+\\.?[0-9]*")))

(defun markex-space ()
  "Select space region."
  (interactive)
  (markex--regexp "[:blank:]" "[[:blank:]]+"))

(defun markex-non-space ()
  "Select non-space region."
  (interactive)
  (markex--regexp "^[:blank:][:cntrl:]" "[^[:blank:][:cntrl:]]+"))

(defun markex--regexp (chars regexp)
  "Select region matched with CHARS and REGEXP.
First, it skip the CHARS backwards and regexp match with REGEXP."
  (markex--reset-marker)
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
  (markex--reset-marker)
  (when-let ((bounds (bounds-of-thing-at-point thing)))
    (markex--select-bounds bounds)))

;;
;; Select region by structure.
;;
(defun markex-line ()
  "Select line region with trimming spaces around."
  (interactive)
  (markex--reset-marker)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (markex--select-bounds
     (cons
      (save-excursion
        (goto-char beg)
        (skip-syntax-forward " " end)
        (point))
      (save-excursion
        (goto-char end)
        (skip-chars-backward " " beg)
        (point))))))

;;
;; Select region by bounds.
;;
(defun markex--select-bounds (bounds)
  "Select region specified by BOUNDS.
BOUNDS is a cons of (beg . end) region."
  (let ((beg (car bounds))
        (end (cdr bounds)))
    (when (/= beg end)
      (unless (markex--repeat-in-progress)
        ;; copied from expand-region `er--prepare-expanding'.
        (markex--set-marker (point))
        (push-mark nil t) ;; one for keeping starting position
        (push-mark nil t)) ;; one for replace by set-mark in expansions
      (set-mark end)
      (goto-char beg))))

;;
;; Marker manipulation.
;;
(defun markex--repeat-in-progress ()
  "Return non-nil if repeat is in progress."
  (and (boundp 'repeat-mode) repeat-mode repeat-in-progress))

(defun markex--reset-marker (&optional force)
  "Reset the marker if FORCE is non-nil or repeat is not in progress."
  (when (or force (not (markex--repeat-in-progress)))
    (set-marker markex--origin nil)))

(defun markex--set-marker (point)
  "Set the marker to the POINT."
  (set-marker markex--origin (point)))

(provide 'markex)
;;; markex.el ends here
