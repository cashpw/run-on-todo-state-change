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

(defcustom run-on-todo-state-change--fn-prefix "run-on-todo-state-change--"
  "Prefix for per-state functions.
For example: `run-on-todo-state-change--todo'"
  :group 'run-on-todo-state-change
  :type 'string)

(defun run-on-todo-state-change--TODO ()
  "Run when a todo state is set to \"TODO\"."
  ;; You should override this.
  )

(defun run-on-todo-state-change--DONE ()
  "Run when a todo state is set to \"DONE\"."
  ;; You should override this.
  )

(defun run-on-todo-state-change-prop (state)
  "Return run-on- org-mode element property based on STATE."
  (concat run-on-todo-state-change--prop-prefix (upcase state)))

(defun run-on-todo-state-change ()
  "Run arbitrary code on org-mode TODO state change."
  (run-on-todo-state-change--run-prop-fns)
  (run-on-todo-state-change--run-fns))

(defun run-on-todo-state-change--run-fns ()
  "Run arbitrary code on org-mode TODO state change."
  (let* ((new-state org-state)
         (fn (intern (concat run-on-todo-state-change--fn-prefix new-state))))
    (when (functionp fn)
      (funcall fn))))

(defun run-on-todo-state-change--run-prop-fns ()
  "Run functions set through \"RUN_ON_\" properties."
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
