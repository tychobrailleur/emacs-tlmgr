;;; etelmgr.el --- TeXLive Manager in Emacs.  -*- lexical-binding: t; -*-

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

;; This package provides TeXLive Manager features in Emacs.

;;; Code:


(require 'etelmgr-tlpdb)
(require 'seq)

(defgroup etelmgr nil
  "etelmgr group")

(defcustom etelmgr-texlive-dir
  "/usr/local/texlive/2016"
  "Directory where TeXLive is installed"
  ;; :set (lambda (sym val)
  ;;        (unless val
  ;;          (set-default sym (replace-regexp-in-string
  ;;                            "\n\\'" ""
  ;;                            (shell-command-to-string "kpsewhich -var-value SELFAUTOPARENT")))
  ;;          (set-default sym val)))
  :type 'directory
  :group 'etelmgr)

(defconst etelmgr-infra-location "tlpkg")
(defconst etelmgr-database-name "texlive.tlpdb")


(defun etelmgr-fetch-packages ()
  (seq-sort '(lambda (a b) (string-lessp (etelmgr-pdbobj-name a) (etelmgr-pdbobj-name b)))
            (seq-filter '(lambda (e) (equal (etelmgr-pdbobj-category e) "Package"))
                        (etelmgr-tlpdb-parse-file
                         (concat (file-name-as-directory (expand-file-name etelmgr-texlive-dir))
                                 (file-name-as-directory etelmgr-infra-location)
                                 etelmgr-database-name)))))

(defun etelmgr-convert-to-entry (pdbobj)
  (list (etelmgr-pdbobj-name pdbobj) `[,(etelmgr-pdbobj-name pdbobj)
                                       ,(or (etelmgr-pdbobj-rev pdbobj) "")
                                       ,(or (etelmgr-pdbobj-shortdesc pdbobj) "")]))

(defun etelmgr-list-packages ()
  (interactive)
  (let ((buf (get-buffer-create "*TeXLive Packages*"))
        (packages (etelmgr-fetch-packages)))
    (with-current-buffer buf
      (setq tabulated-list-format
            `[("Package" 25 nil)
              ("Version" 13 nil)
              ("Description" 60 nil)])
      (tabulated-list-init-header)
      (setq tabulated-list-entries (mapcar #'etelmgr-convert-to-entry packages))
      (tabulated-list-print 1))
    (switch-to-buffer buf)))



;;; etelmgr.el ends here
