;;; major-mode-icons.el --- display icon for major-mode on mode-line.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Authors: stardiviner <numbchild@gmail.com>
;; Keywords: icons, mode-line
;; homepage: http://github.com/stardiviner/major-mode-icons

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:



;;; Requirements:

;; Package-Requires: ((emacs "24.4"))


;;; Code:
;;; ----------------------------------------------------------------------------

(defgroup major-mode-icons nil
  "Show icon for current buffer's major-mode."
  :group 'mode-line)

(defvar major-mode-icons/icons-default-path
  (concat
   (file-name-directory (or load-file-name
                            (buffer-file-name)))
   "icons")
  )

(defcustom major-mode-icons/icons-path major-mode-icons/icons-default-path
  "Path to icons."
  :group 'major-mode-icons)


;;; separate settings for only active mode-line.

(defvar major-mode-icons/mode-line-selected-window nil)

(defun major-mode-icons/mode-line-record-selected-window ()
  (setq major-mode-icons/mode-line-selected-window (selected-window)))

(defun major-mode-icons/mode-line-update-all ()
  (force-mode-line-update t))

(add-hook 'post-command-hook 'major-mode-icons/mode-line-record-selected-window)

(add-hook 'buffer-list-update-hook 'major-mode-icons/mode-line-update-all)

(defun major-mode-icons/active ()
  "Detect whether current window is the selected window."
  (eq major-mode-icons/mode-line-selected-window (selected-window)))


;; major mode with icon
(defvar major-mode-icons/major-mode-list
  '(((emacs-lisp-mode inferior-emacs-lisp-mode) . ("Elisp" "Emacs"))
    ((lisp-mode
      inferior-lisp-mode
      slime-repl-mode sly-mrepl-mode) . ("Lisp" "Common-Lisp"))
    ((scheme-mode) . ("Scheme" "Scheme"))
    ((clojure-mode
      cider-repl-mode) . ("Clojure" "Clojure"))
    ((clojurescript-mode) . ("ClojureScript" "ClojureScript"))
    ((python-mode) . ("Python" "Python"))
    ((enh-ruby-mode ruby-mode) . ("Ruby" "Ruby"))
    ((inf-ruby-mode) . ("Inf-Ruby" "inf-ruby"))
    ((c-mode) . ("C" "C"))
    ((c++-mode) . ("C++" "C++"))
    ((csharp-mode) . ("C#" "C#"))
    ((go-mode) . ("Go" "Go"))
    ((swift-mode) . ("Swift" "Swift"))
    ((rust-mode) . ("Rust" "Rust"))
    ((java-mode) . ("Java" "Java"))
    ((php-mode) . ("PHP" "PHP"))
    ((web-mode html-mode) . ("HTML" "HTML"))
    ((css-mode) . ("CSS" "CSS"))
    ((javascript-mode
      js-mode js2-mode js3-mode inferior-js-mode) . ("JavaScript" "JavaScript"))
    ((coffee-mode) . ("CoffeeScript" "CoffeeScript"))
    ((org-mode) . ("Org" "Org-mode"))
    ((tex-mode latex-mode TeX-mode LaTeX-mode) . ("TeX/LaTeX" "TeX"))
    ((bibtex-mode) . ("BibTeX" "BibTeX"))
    ((markdown-mode) . ("Markdown" "Markdown"))
    ((yaml-mode) . ("YAML" "YAML"))
    ((rst-mode) . ("reStructuredText" "reStructuredText"))
    ((eshell-mode) . ("Shell" "Command-Line"))
    ((sh-mode shell-mode) . ("Shell Script" "Shell"))
    ((term-mode) . ("Terminal" "term"))
    ((powershell-mode) . ("PowerShell" "powershell"))
    ((ess-mode R-mode) . ("R" "R"))
    ((julia-mode ess-julia-mode) . ("Julia" "Julia"))
    ((gnuplot-mode) . ("gnuplot" "gnuplot"))
    ((octave-mode) . ("Octave" "Octave"))
    ((matlab-mode) . ("Matlab" "Matlab"))
    ((haskell-mode) . ("Haskell" "Haskell"))
    ((scala-mode) . ("Scala" "Scala"))
    ((erlang-mode) . ("Erlang" "Erlang"))
    ((prolog-mode) . ("Prolog" "Prolog"))
    ((ocaml-mode) . ("OCaml" "OCaml"))
    ((sql-mode) . ("SQL" "SQL"))
    ((xml-mode nxml-mode) . ("XML" "XML"))
    ((json-mode) . ("JSON" "JSON"))
    ((diff-mode ediff-mode magit-diff-mode) . ("diff" "diff"))
    ((asm-mode nasm-mode) . ("Assembly" "Assembly"))
    ((android-mode) . ("Android" "Android"))
    ((qt-mode) . ("Qt" "Qt"))
    ((arduino-mode) . ("Arduino" "Arduino"))
    ((systemd-mode) . ("Systemd" "Systemd"))
    ((docker-mode) . ("Docker" "Docker"))
    ((projectile-rails-mode) . ("Rails" "Rails"))
    ((slim-mode) . ("Slim" "Slim"))
    ((sass-mode) . ("Sass" "Sass"))
    ((spice-mode) . ("SPICE" "Electric"))
    )
  "Pairs: ([mode-list] . ([name] [icon-name]))."
  )

(defun major-mode-icons/major-mode-list-match ()
  "Return the matched item in `major-mode-list'."
  (assoc
   (cl-some ; or use (remove nil '(nil nil (clojure-mode) nil nil ...))
    (lambda (elem)
      (when (not (null elem))
        elem))
    (mapcar
     (lambda (element)
       (member major-mode element))
     (map-keys major-mode-icons/major-mode-list)))
   major-mode-icons/major-mode-list))


(defun major-mode-icons/major-mode-icon (&optional extra)
  "Display icon for current buffer's `major-mode' and `EXTRA' info."
  ;; FIXME: only show icon for first element in major-mode alist.
  (let* ((match (major-mode-icons/major-mode-list-match))
         (name (or (car (cdr match))
                   ;; return current major-mode as string for `propertize'
                   ;; when not in `major-mode-alist'.
                   mode-name ; "%m"
                   ))
         (icon (car (cdr (cdr match)))))
    (list
     (propertize
      (format "%s" name)
      'face (if (active)
                '(:family "Segoe Print" :foreground "cyan" :height 80)
              'mode-line-inactive)
      'display
      (let ((icon-path
             (concat major-mode-icons/icons-path icon ".xpm")))
        (if (and (active)
                 (file-exists-p icon-path)
                 (image-type-available-p 'xpm))
            (create-image icon-path 'xpm nil :ascent 'center)))
      )
     (propertize " ")
     ;;; extra
     (if extra
         (propertize (format "%s" (or extra ""))
                     'face (if (active)
                               '(:foreground "DarkGreen")
                             'mode-line-inactive)))
     )
    ))

;;; auto show extra info
(defun major-mode-icons/major-mode-extra ()
  "Extend function `major-mode-icon' with extra info."
  (let ((extra
         (case major-mode
           ('clojure-mode
            (if (not (equal (cider--modeline-info) "not connected"))
                (cider--project-name nrepl-project-dir)))
           ('enh-ruby-mode
            (if global-rbenv-mode
                (rbenv--active-ruby-version) ; `rbenv--modestring'
              ))
           ('python-mode
            (if pyvenv-mode
                ;; `pyvenv-mode-line-indicator' -> `pyvenv-virtual-env-name'
                pyvenv-virtual-env-name
              ;; conda: `conda-env-current-name'
              ))
           )))))

;;;###autoload
(defun major-mode-icons/show ()
  "Show icon on mode-line."
  (major-mode-icons/major-mode-icon (major-mode-icons/major-mode-extra)))

;;;###autoload
(define-minor-mode major-mode-icons-mode
  "A minor mode of showing icon for major-mode of current buffer."
  :lighter 'major-mode-icons/show
  :global t)

;;; ----------------------------------------------------------------------------

(provide 'major-mode-icons)

;;; major-mode-icons.el ends here
