(require 'ert)

(require 'selrgn)

(defun selrgn--test-match (func contents start point mark)
  "Call FUNC in a buffer with CONTENTS, at START point.
Check if region beginnig equals to POINT and region end equals to MARK."
  (with-temp-buffer
    (insert contents)
    (when start
      (goto-char start))
    (should (funcall func))
    (should (eq (point) point))
    (should (eq (mark) mark))))

(defun selrgn--test-unmatch (func contents start)
  "Call FUNC in a buffer with CONTENTS, at START point.
Check if point is not moved."
  (with-temp-buffer
    (insert contents)
    (setq start (or start (point)))
    (goto-char start)
    (should (not (funcall func)))
    (should (eq (point) start))
    (should (eq (mark) nil))))

(ert-deftest selrgn-ipv4--test-match ()
  (selrgn--test-match #'selrgn-ipv4 "172.16.0.1" nil 1 11)
  (selrgn--test-match #'selrgn-ipv4 " 172.16.0.1  " 5 2 12))

(ert-deftest selrgn-ipv4--test-unmatch ()
  (selrgn--test-unmatch #'selrgn-ipv4 "17216.0.1" nil)
  (selrgn--test-unmatch #'selrgn-ipv4 "foobar" 5))
