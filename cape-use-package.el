;;; cape-use-package.el --- Completion-At-Point Extension for use-package -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Ellis Kenyő
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: February 22, 2023
;; Modified: February 22, 2023
;; Version: 0.0.1
;; Homepage: https://github.com/elken/cape-use-package
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Completion-At-Point Extension for use-package
;;
;;; Code:

(require 'package)
(require 'cape)

(defgroup cape-use-package nil
  "use-package CAPE."
  :group 'cape)

(defcustom cape-use-package-always-refetch nil
  "Set to non-nil to always refresh the package list.
Not recommended to use this setting with the global completion list."
  :type 'boolean
  :group 'cape-use-package)

(defcustom cape-use-package-annotation-icon t
  "Set to non-nil to use an icon as the annotation."
  :type 'boolean
  :group 'cape-use-package)

(defvar cape-use-package--candidates nil)
(defun cape-use-package--candidates ()
  "Return a list of candidate packages."
  (when (or cape-use-package-always-refetch
            (null cape-use-package--candidates))
    (package-read-all-archive-contents)
    (setq cape-use-package--candidates
          (delete-dups
           (append (mapcar #'car package-alist)
                   (mapcar #'car package-archive-contents)
                   (mapcar #'car package--builtins)
                   nil))))

  cape-use-package--candidates)

(defvar cape-use-package--properties
  (list :annotation-function (lambda (_) (if cape-use-package-annotation-icon "  " " [pkg]"))
        :company-kind (lambda (_) 'module)
        :exclusive 'no)
  "Completion extra properties for `cape-use-package'.")

;;;###autoload
(defun cape-use-package (&optional interactive)
  (interactive (list t))
  (if interactive
      (cape-interactive #'cape-use-package)
    (when (string-prefix-p "use-package" (symbol-name (car (list-at-point))))
     (let ((bounds (cape--bounds 'word)))
       `(,(car bounds) ,(cdr bounds)
         ,(cape--table-with-properties (cape-use-package--candidates) :category 'cape-use-package)
         ,@cape-use-package--properties)))))

(provide 'cape-use-package)
;;; cape-use-package.el ends here
