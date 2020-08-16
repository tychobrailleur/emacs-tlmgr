;;; etelmgr.el --- TeXLive Manager in Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018–2020  Sébastien Le Callonnec

;; Author: Sébastien Le Callonnec <sebastien@weblogism.com>
;; Maintainer: Sébastien Le Callonnec <sebastien@weblogism.com>
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
  "/usr/local/texlive/2020"
  "Directory where TeXLive is installed."
  ;; FIXME – TeXLive location should be computed automatically...
  ;; :set (lambda (sym val)
  ;;        (unless val
  ;;          (set-default sym (replace-regexp-in-string
  ;;                            "\n\\'" ""
  ;;                            (shell-command-to-string "kpsewhich -var-value SELFAUTOPARENT")))
  ;;          (set-default sym val)))
  :type 'directory
  :group 'etelmgr)


(defface etelmgr-package-name '((t :inherit link))
  "Face used to display package name as a link.")

;; List of mirrors
;; ftp.heanet.ie/pub/CTAN/tex/systems/texlive/tlnet/tlpkg/installer/ctan-mirrors.pl
(defcustom etelmgr-repository-url
  "http://mirrors.ircam.fr/pub/CTAN/systems/texlive/tlnet"
  ;;  "http://ftp.heanet.ie/pub/CTAN/tex/systems/texlive/tlnet"
  "TeXLive repository URL."
  :type 'string
  :group 'etelmgr)

(defconst etelmgr-infra-location "tlpkg")
(defconst etelmgr-database-name "texlive.tlpdb")

;; Download current TeXLive DB from Mirror and decompress.
(defun etelmgr--download-tlpdb (base-url)
  "Download the current TeXLiveDB from the mirror at BASE-URL and decompress."
  (message (format "Downloading TL repo: %s" base-url))
  (let ((temp-file (make-temp-file "etelmgr" nil ".xz"))
        (buf (get-buffer-create "*texlive-repo*"))
        (remote-tlpdb (format "%s/tlpkg/texlive.tlpdb.xz" base-url)))
    (url-copy-file remote-tlpdb temp-file t t)
    (shell-command (concat "unxz " temp-file))
    (expand-file-name (file-name-sans-extension temp-file))))

(defun etelmgr-view-package ()
  "Show the details of the selected package."
  (interactive)
  (let ((selected-entry (tabulated-list-get-id))
        (entry-buffer (get-buffer-create "Etelmgr Package")))
    (with-current-buffer entry-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Selected entry: %s" selected-entry))
      (setq buffer-read-only t)
      (setq mark-active nil)
      (switch-to-buffer-other-window entry-buffer))))


(defvar etelmgr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'etelmgr-view-package)
    map))

(define-derived-mode etelmgr-mode tabulated-list-mode "Etelmgr"
  "Major mode for browsing a list of TeXLive packages."
  (setq buffer-read-only t)
  (setq tabulated-list-format
        `[("Package" 25 nil)
          ("Version" 8 nil)
          ("Installed" 10 nil)
          ("Description" 60 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (use-local-map etelmgr-mode-map))

(defun etelmgr--sort-package-by-name (a b)
  (string-lessp (etelmgr-pdbobj-name a) (etelmgr-pdbobj-name b)))

(defun etelmgr--fetch-packages ()
  "List packages installed on the system."
  (seq-sort #'etelmgr--sort-package-by-name
            ;; Only retrieve packages, and abritrarily exclude texlive config.
            (seq-filter '(lambda (e) (and (equal (etelmgr-pdbobj-category e) "Package")
                                     (not (equal (etelmgr-pdbobj-name e) "00texlive.config"))))
                        (etelmgr-tlpdb-parse-file
                         (concat (file-name-as-directory (expand-file-name etelmgr-texlive-dir))
                                 (file-name-as-directory etelmgr-infra-location)
                                 etelmgr-database-name)))))

(defun etelmgr--convert-to-entry (pdbobj)
  "Convert tlpdb database PDBOBJ to tabulated entries."
  (list (etelmgr-pdbobj-name pdbobj) `[,(propertize (etelmgr-pdbobj-name pdbobj)
                                                    'face 'etelmgr-package-name)
                                       ,(or (etelmgr-pdbobj-rev pdbobj) "")
                                       "installed"
                                       ,(or (etelmgr-pdbobj-shortdesc pdbobj) "")]))

(defun etelmgr-list-packages ()
  "List the TeXLive packages using `tabulated-list-mode'."
  (interactive)
  (let* ((buf (get-buffer-create "*TeXLive Packages*"))
         (latest-tlpdb (etelmgr--download-tlpdb etelmgr-repository-url)))
    ;; TODO USe downloaded latest-tlpdb
    (with-current-buffer buf
      (etelmgr-mode)
      (setq packages (etelmgr--fetch-packages))
      (setq tabulated-list-entries (mapcar #'etelmgr--convert-to-entry packages))
      (tabulated-list-print 1))
    (switch-to-buffer buf)))


(provide 'etelmgr)
;;; etelmgr.el ends here
