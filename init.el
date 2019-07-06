(require 'package)

(add-to-list 'package-archives'("melpa-stable" . "http://stable.melpa.org/packages/"))

;; For important compatibility libraries like cl-lib
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Only add non-stable melpa when emacs major version is larger than 24. Emacs
;; 25.1 introduced the package-archive-priorities variable, so for older
;; versions just stick with the stable archive to prevent installing unwanted
;; (unstable) packages.
(when (> emacs-major-version 24)
  (add-to-list 'package-archives'("melpa" . "http://melpa.org/packages/"))
  (setq package-archive-priorities
        '(("melpa-stable" . 10)
          ("gnu" . 5)
          ("melpa" . 0))))


(package-initialize)

;; Bootstrap use-package. Use-package allows for automatic installation of
;; packages, which is great when you want to use Emacs on multiple systems.
;; Also, it loads packages on demand, keeping startup time short.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; To reduce startup time, only tangle configuration when the .org file is newer
;; than the .el file. Otherwise, simply load the .el file.
(let* ((my-init-file-org (concat user-emacs-directory "configuration.org"))
       (my-init-file-el (concat (file-name-sans-extension my-init-file-org) ".el")))
  (if (file-newer-than-file-p my-init-file-org my-init-file-el)
      (org-babel-load-file my-init-file-org)
    (load-file my-init-file-el)))

