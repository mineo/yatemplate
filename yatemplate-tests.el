;;;; (executable-interpret (format "emacs -batch -L %s -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "yasnippet.elc")) buffer-file-name))
(require 'test-simple)
(test-simple-start)

(require 'yasnippet)
(assert-t (load-file "./yatemplate.el") "Can't load ./yatemplate.el")

(note "yatemplate-fill-alist")

(require 'cl-lib)
(defun yatemplates ()
  (yatemplate-fill-alist)
  (cl-remove-if-not
   (lambda (pair)
     (ignore-errors (eq 'yatemplate-expand-yas-buffer (aref (cdr pair) 1))))
   auto-insert-alist))

(setq yatemplate-dir "/tmp/yatemplate-test")
(mkdir yatemplate-dir t)
(assert-nil (yatemplates))
(write-region "a" nil "/tmp/yatemplate-test/01:test.el")
(assert-equal '("test.el") (mapcar 'car (yatemplates)))
(assert-equal '("test.el") (mapcar 'car (yatemplates)))
(delete-directory yatemplate-dir t)

(end-tests)
