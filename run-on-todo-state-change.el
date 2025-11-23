;;; run-on-todo-state-change.el --- Run arbitrary code on TODO state change -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Cash Prokop-Weaver
;;
;; Author: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Maintainer: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Created: July 01, 2024
;; Modified: July 01, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/cashweaver/run-on-todo-state-change
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Run arbitrary code on org-mode TODO state change.
;;
;;  Set a property of "RUN_ON_<TODO state>" to one or more functions and they'll
;;  run when the headline's state is set to <TODO state>.
;;
;;  Example: The following will run your functions on DONE.
;;
;;  (defun you/foo ()
;;    ...)
;;
;;  * Foo
;;  :PROPERTIES:
;;  :RUN_ON_DONE: (you/foo)
;;  :RUN_ON_DONE+: (lambda () (you/foo 1))
;;  :END:
;;
;;; Code:

(defgroup run-on-todo-state-change nil
  "Run arbitrary code on org-mode TODO state change."
  :group 'org)

(defcustom run-on-todo-state-change--prop-prefix "RUN_ON_"
  "org-mode property prefix."
  :group 'run-on-todo-state-change
  :type 'string)

(defun run-on-todo-state-change-prop (state)
  "Return run-on- org-mode element property based on STATE."
  (concat run-on-todo-state-change--prop-prefix (upcase state)))

(defun run-on-todo-state-change ()
  "Run arbitrary code on org-mode TODO state change."
  (when-let* ((new-state org-state)
              (fns-string
               (org-extras-get-property
                (point) (run-on-todo-state-change-prop new-state)))
              (fn-strings
               (let ((paren-counter 0)
                     (previous-counter 0))
                 (mapcar
                  #'string-join
                  (-split-when
                   (lambda (char)
                     (setf previous-counter paren-counter)
                     (cond
                      ((string= char "(")
                       (cl-incf paren-counter))
                      ((string= char ")")
                       (cl-decf paren-counter)))
                     (= 0 paren-counter previous-counter))
                   (butlast
                    (cdr (split-string (format "%s " fns-string) ""))))))))
    (dolist (fn-string fn-strings)
      (eval (read fn-string)))))

(provide 'run-on-todo-state-change)
;;; run-on-todo-state-change.el ends here
