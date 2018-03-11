;;; etelmgr-tlpdb.el --- TeXLive Manager in Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Sébastien Le Callonnec

;; Author: Sébastien Le Callonnec <sebastien@weblogism.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides facilities to handle the TeXLive PDB format.

;;; Code:


(cl-defstruct etelmgr-pdbobj
  name
  category
  catalogue
  rev
  shortdesc
  longdesc
  (depend '())
  (runfiles '())
  (srcfiles '())
  containersize
  containerchecksum)

(defun etelmgr-pdbobj-extract-value (str)
  (if (string-match "\\([^ ]*\\) \\(.*\\)" str)
      (cons (match-string 1 str) (cons (match-string 2 str) '()))
    (list str)))

(defun etelmgr-tlpdb-parse-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((current-line nil)
          (current-pdb '())
          (current-pdbobj (make-etelmgr-pdbobj))
          (line-split nil)
          (tlpdb (make-hash-table :test 'equal)))
      (while (not (eobp))
        (setq current-line (replace-regexp-in-string "\n\\'" "" (thing-at-point 'line t)))
        (if (equal current-line "")
            (progn
              (setq current-pdb (cons current-pdbobj current-pdb))
              (setq current-pdbobj (make-etelmgr-pdbobj))))

        (setq line-split (etelmgr-pdbobj-extract-value current-line))
        (cond
         ((equal (car line-split) "name")
          (setf (etelmgr-pdbobj-name current-pdbobj) (cadr line-split)))
         ((equal (car line-split) "runfiles")
          (setf (etelmgr-pdbobj-runfiles current-pdbobj)
                (cons (cadr line-split) (etelmgr-pdbobj-runfiles current-pdbobj))))
         ((equal (car line-split) "longdesc")
          (setf (etelmgr-pdbobj-longdesc current-pdbobj)
                (concat (etelmgr-pdbobj-longdesc current-pdbobj) " " (cadr line-split))))
         ((equal (car line-split) "shortdesc")
          (setf (etelmgr-pdbobj-shortdesc current-pdbobj) (cadr line-split)))
         ((equal (car line-split) "category")
          (setf (etelmgr-pdbobj-category current-pdbobj) (cadr line-split)))
         ((equal (car line-split) "revision")
          (setf (etelmgr-pdbobj-rev current-pdbobj) (cadr line-split)))
         ((equal (car line-split) "catalogue")
          (setf (etelmgr-pdbobj-catalogue current-pdbobj) (cadr line-split))))

        (forward-line))
      current-pdb)))

(provide 'etelmgr-tlpdb)
;;; etelmgr-tlpdb.el ends here
