;;;; (executable-interpret (format "emacs -batch -L %s -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "yasnippet.elc")) buffer-file-name))
(require 'test-simple)
(test-simple-start)

(require 'yasnippet)
(assert-t (load-file "./yatemplate.el") "Can't load ./yatemplate.el")

(note "yatemplate-fill-alist")

(require 'cl-lib)
(defun yatemplates ()
  (yatemplate-fill-alist)
  (mapcar
   'car
   (cl-remove-if-not
    (lambda (pair)
      (ignore-errors (eq 'yatemplate-expand-yas-buffer (aref (cdr pair) 1))))
    auto-insert-alist)))

(setq yatemplate-dir "/tmp/yatemplate-test")
(mkdir yatemplate-dir t)
(setq default-directory yatemplate-dir)

(note "yatemplate-fill-alist: no entry")
(assert-nil (yatemplates))

(note "yatemplate-fill-alist: one entry")
(write-region "a" nil "01:test.el")
(assert-equal '("test.el$") (yatemplates))

(note "yatemplate-fill-alist: not duplicate")
(assert-equal '("test.el$") (yatemplates))

(note "yatemplate-fill-alist: two entries")
(write-region "a" nil "02:.*.py")
(assert-equal '("test.el$" ".*.py$") (yatemplates))

(note "yatemplate-fill-alist: rename file")
(rename-file "01:test.el" "10:test.el")
;; now 02:.*.py and 10:test.el
(assert-equal '(".*.py$" "test.el$") (yatemplates))

(note "yatemplate-fill-alist: delete file")
(delete-file "10:test.el")
;; now 02:.*.py
(assert-equal '(".*.py$") (yatemplates))


(delete-directory yatemplate-dir t)

(end-tests)
