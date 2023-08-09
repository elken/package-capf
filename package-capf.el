;;; package-capf.el --- Completion-At-Point Function for Emcas packages -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Ellis Kenyő
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: February 22, 2023
;; Modified: February 22, 2023
;; Version: 0.0.1
;; Homepage: https://github.com/elken/package-capf
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Completion-At-Point Function for querying Emacs packages.
;;
;; Optionally always refresh the list of packages with
;; `package-capf-always-refetch', otherwise use the current user-defined list.
;;
;;; Code:

(require 'rx)
(require 'package)

(defgroup package-capf nil
  "use-package CAPE."
  :group 'completion)

(defcustom package-capf-always-refetch nil
  "Set to non-nil to always refresh the package list.
Not recommended to use this setting with the global completion list."
  :type 'boolean
  :group 'package-capf)

(defcustom package-capf-annotation "  "
  "Set the annotation to use. Defaults to a package."
  :type 'boolean
  :group 'package-capf)

(defcustom package-capf-completion-symbols '(use-package use-package! package!)
  "A list of symbols to complete against."
  :type '(repeat symbol)
  :group 'package-capf)

(defvar package-capf--candidates nil)
(defun package-capf--candidates ()
  "Return a list of candidate packages."
  (when (or package-capf-always-refetch
            (null package-capf--candidates))
    (package-read-all-archive-contents)
    (setq package-capf--candidates
          (delete-dups
           (append (mapcar #'car package-alist)
                   (mapcar #'car package-archive-contents)
                   (mapcar #'car package--builtins)
                   nil))))

  package-capf--candidates)

(defvar package-capf--properties
  (list :annotation-function (lambda (_) package-capf-annotation)
        :company-kind (lambda (_) 'module)
        :exclusive 'no)
  "Completion extra properties for `package-capf'.")

(defun package-capf--list (input)
  "Use INPUT to compute and filter a new completion table."
  (cons (apply-partially #'string-prefix-p input)
        (package-capf-candidates input)))

(defun package-capf--completion-table ()
  "Return a suitable function to create a completion table."
  (lambda (str pred action)
    (let ((packages (package-capf--list str)))
      (if (eq action 'metadata)
          '(metadata (category . package-capf))
        (complete-with-action action packages str pred)))))

(defun package-capf--looking-at-package ()
  "Return non-nill if we should complete the list of packages."
  (looking-at
   (rx
    "("
    (eval
     `(or
       ,@(mapcar #'symbol-name package-capf-completion-symbols))))))

;;;###autoload
(defun package-capf (&optional interactive)
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions #'package-capf))
        (or (completion-at-point) (user-error "package-capf: No completions")))
    (when (package-capf--looking-at-package)
      (let ((bounds (or (bounds-of-thing-at-point 'word) (cons (point) (point)))))
        `(,(car bounds) ,(cdr bounds)
          ,(package-capf--completion-table)
          ,@package-capf--properties)))))

(provide 'package-capf)
;;; package-capf.el ends here
