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
(require 'url)

(defgroup etelmgr nil
  "etelmgr group")

(defcustom etelmgr-texlive-dir
  "/usr/local/texlive/2016"
  "Directory where TeXLive is installed"
  ;; FIXME – TeXLive location should be computed automatically...
  ;; :set (lambda (sym val)
  ;;        (unless val
  ;;          (set-default sym (replace-regexp-in-string
  ;;                            "\n\\'" ""
  ;;                            (shell-command-to-string "kpsewhich -var-value SELFAUTOPARENT")))
  ;;          (set-default sym val)))
  :type 'directory
  :group 'etelmgr)


;; List of mirrors
;; ftp.heanet.ie/pub/CTAN/tex/systems/texlive/tlnet/tlpkg/installer/ctan-mirrors.pl
(defcustom etelmgr-repository-url
  "http://mirrors.ircam.fr/pub/CTAN/systems/texlive/tlnet"
  ;;  "http://ftp.heanet.ie/pub/CTAN/tex/systems/texlive/tlnet"
  "TeXLive repository URL"
  :type 'string
  :group 'etelmgr)

(defconst etelmgr-infra-location "tlpkg")
(defconst etelmgr-database-name "texlive.tlpdb")

;; Download current TeXLive DB from Mirror and decompress.
(defun etelmgr--download-tlpdb (root)
  (message (format "Downloading TL repo: %s" root))
  (let ((temp-file (make-temp-file "etelmgr" nil ".xz"))
        (buf (get-buffer-create "*texlive-repo*"))
        (remote-tlpdb (format "%s/tlpkg/texlive.tlpdb.xz" root)))
    (url-copy-file remote-tlpdb temp-file t t)
    (shell-command (concat "unxz " temp-file))
    ;;    (message (format "File size: %d" (nth 7 (file-attributes (file-name-sans-extension temp-file) 'string))))
    (expand-file-name (file-name-sans-extension temp-file))))

(define-derived-mode etelmgr-mode tabulated-list-mode "Etelmgr"
  "Major mode for browsing a list of TeXLive packages."
  (setq buffer-read-only t)
  (setq tabulated-list-format
        `[("Package" 25 nil)
          ("Version" 8 nil)
          ("Installed" 10 nil)
          ("Description" 60 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun etelmgr--sort-package-by-name (a b)
  (string-lessp (etelmgr-pdbobj-name a) (etelmgr-pdbobj-name b)))

(defun etelmgr--fetch-packages ()
  (seq-sort '#etelmgr--sort-package-by-name
            ;; Only retrieve packages, and abritrarily exclude texlive config.
            (seq-filter '(lambda (e) (and (equal (etelmgr-pdbobj-category e) "Package")
                                          (not (equal (etelmgr-pdbobj-name e) "00texlive.config"))))
                        (etelmgr-tlpdb-parse-file
                         (concat (file-name-as-directory (expand-file-name etelmgr-texlive-dir))
                                 (file-name-as-directory etelmgr-infra-location)
                                 etelmgr-database-name)))))

(defun etelmgr--convert-to-entry (pdbobj)
  (list (etelmgr-pdbobj-name pdbobj) `[,(etelmgr-pdbobj-name pdbobj)
                                       ,(or (etelmgr-pdbobj-rev pdbobj) "")
                                       "installed"
                                       ,(or (etelmgr-pdbobj-shortdesc pdbobj) "")]))

(defun etelmgr-list-packages ()
  (interactive)
  (let* ((buf (get-buffer-create "*TeXLive Packages*"))
        (latest-tlpdb (etelmgr--download-tlpdb etelmgr-repository-url)))
    (with-current-buffer buf
      (etelmgr-mode)
      (setq packages (etelmgr--fetch-packages))
      (setq tabulated-list-entries (mapcar #'etelmgr--convert-to-entry packages))
      (tabulated-list-print 1))
    (switch-to-buffer buf)))

(provide 'etelmgr)
;;; etelmgr.el ends here
