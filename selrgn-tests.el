(require 'ert)

(require 'selrgn)

(cl-defun selrgn--test-match (func contents point mark &key mode start)
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

(cl-defun selrgn--test-unmatch (func contents &key mode start)
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

(ert-deftest selrgn-ipv4--test-match ()
  (selrgn--test-match #'selrgn-ipv4 "172.16.0.1" 1 11)
  (selrgn--test-match #'selrgn-ipv4 " 172.16.0.1  " 2 12 :start 5)
  (selrgn--test-match #'selrgn-ipv4 "```172.16.0.1```" 4 14 :start 10))

(ert-deftest selrgn-ipv4--test-unmatch ()
  (selrgn--test-unmatch #'selrgn-ipv4 "17216.0.1")
  (selrgn--test-unmatch #'selrgn-ipv4 "foobar" :start 5))

(ert-deftest selrgn-mac--test-match ()
  (selrgn--test-match #'selrgn-mac "00:00:00:00:00:00" 1 18)
  (selrgn--test-match #'selrgn-mac " 00:11:22:aa:bb:cc  " 2 19 :start 2)
  (selrgn--test-match #'selrgn-mac " \"FF:FF:FF:FF:FF:FF\" " 3 20 :start 19))

(ert-deftest selrgn-mac--test-unmatch ()
  (selrgn--test-unmatch #'selrgn-mac "00:00:0000:00:00")
  (selrgn--test-unmatch #'selrgn-mac " 00:11:22:gg:bb:cc  " :start 5)
  (selrgn--test-unmatch #'selrgn-mac " \"FF:FF:FF:FF::FF:FF\" " :start 19))

(ert-deftest selrgn-version--test-match ()
  (selrgn--test-match #'selrgn-version "1.2.3" 1 6)
  (selrgn--test-match #'selrgn-version "foo-10.200.300 " 5 15 :start 5)
  (selrgn--test-match #'selrgn-version " \"2023.05.09.1\" " 3 15 :start 10))

(ert-deftest selrgn-version--test-unmatch ()
  (selrgn--test-unmatch #'selrgn-version "foobar")
  (selrgn--test-unmatch #'selrgn-version " gg:bb:cc  " :start 5)
  (selrgn--test-unmatch #'selrgn-version " \"'!@#'\" " :start 3))

