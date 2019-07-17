(require 'package)

(add-to-list 'package-archives'("melpa-stable" . "https://stable.melpa.org/packages/"))

;; For important compatibility libraries like cl-lib
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

;; Only add non-stable melpa when emacs major version is larger than 24. Emacs
;; 25.1 introduced the package-archive-priorities variable, so for older
;; versions just stick with the stable archive to prevent installing unwanted
;; (unstable) packages.
(when (> emacs-major-version 24)
  (add-to-list 'package-archives'("melpa" . "https://melpa.org/packages/"))
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

(defcustom my-configuration-file (concat user-emacs-directory "configuration.org")
  "Custom configuration file, loaded by `user-init-file'.")

;; To reduce startup time, only tangle configuration when the .org file is newer
;; than the .el file. Otherwise, simply load the .el file.
(let ((my-config-file-el (concat (file-name-sans-extension my-configuration-file) ".el")))
  (if (file-newer-than-file-p my-configuration-file my-config-file-el)
      (org-babel-load-file my-configuration-file)
    (load-file my-config-file-el)))

