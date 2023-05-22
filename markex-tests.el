(require 'ert)

(require 'markex)

(cl-defun markex--test-match (func contents point mark &key start)
  "Call FUNC in a buffer with CONTENTS.
Check if region beginnig equals to POINT and region end equals to MARK.
If START is non-nil, go to the point."
  (with-temp-buffer
    (insert contents)
    (when start
      (goto-char start))
    (should (funcall func))
    (should (eq (point) point))
    (should (eq (mark) mark))))

(cl-defun markex--test-unmatch (func contents &key start)
  "Call FUNC in a buffer with CONTENTS.
If START is non-nil, go to the point.
Check if point is not moved."
  (with-temp-buffer
    (insert contents)
    (setq start (or start (point)))
    (goto-char start)
    (should (not (funcall func)))
    (should (eq (point) start))
    (should (eq (mark) nil))))

(ert-deftest markex-ipv4--test-match ()
  (markex--test-match #'markex-ipv4 "172.16.0.1" 1 11)
  (markex--test-match #'markex-ipv4 " 172.16.0.1  " 2 12 :start 5)
  (markex--test-match #'markex-ipv4 "```172.16.0.1```" 4 14 :start 10))

(ert-deftest markex-ipv4--test-unmatch ()
  (markex--test-unmatch #'markex-ipv4 "17216.0.1")
  (markex--test-unmatch #'markex-ipv4 "foobar" :start 5))

(ert-deftest markex-mac--test-match ()
  (markex--test-match #'markex-mac "00:00:00:00:00:00" 1 18)
  (markex--test-match #'markex-mac " 00:11:22:aa:bb:cc  " 2 19 :start 2)
  (markex--test-match #'markex-mac " \"FF:FF:FF:FF:FF:FF\" " 3 20 :start 19))

(ert-deftest markex-mac--test-unmatch ()
  (markex--test-unmatch #'markex-mac "00:00:0000:00:00")
  (markex--test-unmatch #'markex-mac " 00:11:22:gg:bb:cc  " :start 5)
  (markex--test-unmatch #'markex-mac " \"FF:FF:FF:FF::FF:FF\" " :start 19))

(ert-deftest markex-version--test-match ()
  (markex--test-match #'markex-version "1.2.3" 1 6)
  (markex--test-match #'markex-version "foo-10.200.300 " 5 15 :start 5)
  (markex--test-match #'markex-version " \"2023.05.09.1\" " 3 15 :start 10))

(ert-deftest markex-version--test-unmatch ()
  (markex--test-unmatch #'markex-version "foobar")
  (markex--test-unmatch #'markex-version " gg:bb:cc  " :start 5)
  (markex--test-unmatch #'markex-version " \"'!@#'\" " :start 3))

(ert-deftest markex-number--test-match ()
  (markex--test-match #'markex-number "(setq foo 123)" 11 14 :start 13)
  (markex--test-match #'markex-number " 0x001122334455 " 2 16 :start 15)
  (markex--test-match #'markex-number " #xaabbCCDDeeff " 2 16 :start 3)
  (markex--test-match #'markex-number "-14.5" 1 6 :start 3))

(ert-deftest markex-number--test-unmatch ()
  (markex--test-unmatch #'markex-number "(setq foo 123)" :start 3)
  (markex--test-unmatch #'markex-number "123  456" :start 5)
  (markex--test-unmatch #'markex-number "#xgghhIIJJkkll" :start 5))

(ert-deftest markex-string--test-match ()
  (markex--test-match #'markex-string "\"Hello, wolrd.\"" 2 15 :start 3)
  (markex--test-match #'markex-string "  \"foo\\\"bar\" " 4 12 :start 5)
  (markex--test-match #'markex-string " \"foo\" " 3 6 :start 2)
  (markex--test-match #'markex-string " \"foo\" " 3 6 :start 6)
  (markex--test-match #'markex-string "\" \"" 2 3 :start 2))

(ert-deftest markex-string--test-unmatch ()
  (markex--test-unmatch #'markex-string "foo")
  (markex--test-unmatch #'markex-string " \"foo\" " :start 1)
  (markex--test-unmatch #'markex-string " \"foo\" " :start 7)
  (markex--test-unmatch #'markex-string " \"\" " :start 2)
  (markex--test-unmatch #'markex-string " \"\" " :start 3))

(ert-deftest markex-pair--test-match ()
  (markex--test-match #'markex-pair " (Hello wolrd) " 2 15 :start 5)
  (markex--test-match #'markex-pair " (Hello wolrd) " 2 15 :start 14)
  (markex--test-match #'markex-pair " (Hello wolrd) " 2 15 :start 2)
  (markex--test-match #'markex-pair "(setq foo (list a b c)) " 11 23 :start 15)
  (markex--test-match #'markex-pair "(setq foo (list a b c)) " 11 23 :start 11)
  (markex--test-match #'markex-pair "(setq foo (list a b c)) " 1 24 :start 10))

(ert-deftest markex-pair--test-unmatch ()
  (markex--test-unmatch #'markex-pair " Hello wolrd " :start 7)
  (markex--test-unmatch #'markex-pair " (Hello wolrd) " :start 1)
  (markex--test-unmatch #'markex-pair " (Hello wolrd) " :start 15))

(ert-deftest markex-face--test-match ()
  (with-temp-buffer
    (insert "cat <<EOF
foo
EOF
")
    (put-text-property 10 18 'face 'font-lock-doc-face)
    (goto-char 12)
    (should (markex-face))
    (should (eq (point) 10))
    (should (eq (mark) 18)))

  (with-temp-buffer
    (insert "echo")
    (put-text-property 1 5 'face 'font-lock-builtin-face)
    (goto-char 1)
    (should (markex-face))
    (should (eq (point) 1))
    (should (eq (mark) 5)))

  (with-temp-buffer
    (insert "$(echo foo)")
    (put-text-property 3 11 'face 'font-lock-constant-face)
    (put-text-property 3 7 'face '(font-lock-constant-face font-lock-builtin-face))
    (goto-char 8)
    (should (markex-face))
    (should (eq (point) 3))
    (should (eq (mark) 11))))

(ert-deftest markex-face--test-unmatch ()
  (with-temp-buffer
    (insert "cat <<EOF
foo
EOF
")
    (put-text-property 10 18 'face 'font-lock-doc-face)
    (goto-char 1)
    (should (not (markex-face)))
    (should (eq (point) 1))
    (should (not (mark))))

  (with-temp-buffer
    (insert "echo foo")
    (put-text-property 1 4 'face 'font-lock-builtin-face)
    (goto-char 5)
    (should (not (markex-face)))
    (should (eq (point) 5))
    (should (not (mark)))))

(ert-deftest markex-email--test-match ()
  (markex--test-match #'markex-email "\"foo@example.com\"" 2 17 :start 8))

(ert-deftest markex-filename--test-match ()
  (markex--test-match #'markex-filename "[foo](/usr/local/etc/foo)" 7 25 :start 10))

(ert-deftest markex-url--test-match ()
  (markex--test-match #'markex-url "[foo](https://example.com/foo)" 7 30 :start 20))

(ert-deftest markex-uuid--test-match ()
  (markex--test-match #'markex-uuid "\"cfdf577b-5225-47ce-acdb-d4687d8420aa\"" 2 38 :start 2))

(ert-deftest markex-symbol--test-match ()
  (markex--test-match #'markex-symbol " (foo-bar baz)" 3 10 :start 3))

(ert-deftest markex-word--test-match ()
  (markex--test-match #'markex-word "foo-bar-baz" 5 8 :start 6))

(ert-deftest markex-enlarge--test ()
  (with-temp-buffer
    (transient-mark-mode 1)
    (insert "foo bar baz!")
    (set-mark 8)
    (goto-char 5)
    (markex-enlarge 1)
    (should (eq (point) 4))
    (should (eq (mark) 9))

    (markex-enlarge 3)
    (should (eq (point) 1))
    (should (eq (mark) 12))

    (markex-enlarge 1)
    (should (eq (point) 1))
    (should (eq (mark) 13))

    (markex-enlarge 1)
    (should (eq (point) 1))
    (should (eq (mark) 13))))

(ert-deftest markex-shrink--test ()
  (with-temp-buffer
    (transient-mark-mode 1)
    (insert "foo bar baz!")
    (set-mark 13)
    (goto-char 1)
    (markex-shrink 1)
    (should (eq (point) 2))
    (should (eq (mark) 12))

    (markex-shrink 4)
    (should (eq (point) 6))
    (should (eq (mark) 8))

    (markex-shrink 1)
    (should (eq (point) 7))
    (should (eq (mark) 7))

    (markex-shrink 1)
    (should (eq (point) 7))
    (should (eq (mark) 7))))
