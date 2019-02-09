(require 'package)

(add-to-list 'package-archives'("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives'("melpa" . "http://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(setq package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu" . 5)
        ("melpa" . 0)))

;; Use-package allows for automatic installation of packages, which
;; is great when you want to use Emacs on multiple systems. Also, it
;; loads packages when needed, keeping the Emacs startup time short.
(require 'use-package)

(org-babel-load-file "~/.emacs.d/configuration.org")
