(require 'ert)

(require 'markex)

(cl-defun markex--test-match (func contents point mark &key mode start)
  "Call FUNC in a buffer with CONTENTS.
Check if region beginnig equals to POINT and region end equals to MARK.
If MODE is non-nil, enable the mode.
If START is non-nil, go to the point."
  (with-temp-buffer
    (insert contents)
    (when mode
      (funcall mode))
    (when start
      (goto-char start))
    (should (funcall func))
    (should (eq (point) point))
    (should (eq (mark) mark))))

(cl-defun markex--test-unmatch (func contents &key mode start)
  "Call FUNC in a buffer with CONTENTS.
If MODE is non-nil, enable the mode.
If START is non-nil, go to the point.
Check if point is not moved."
  (with-temp-buffer
    (insert contents)
    (when mode
      (funcall mode))
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
