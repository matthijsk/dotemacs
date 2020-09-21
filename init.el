(require 'package)

;; Workaround for issue in Emacs 26 where, depending on the installed version of
;; GnuTLS, retrieving a package archive over https fails with =bad request=
;; errors. See
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request
;; and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341 for a discussion.
(when (and (version< emacs-version "26.3")
           (>= libgnutls-version 30606))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

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

(when (< emacs-major-version 27)
  (package-initialize))

;; Bootstrap use-package. Use-package allows for automatic installation of
;; packages, which is great when you want to use Emacs on multiple systems.
;; Also, it loads packages on demand, keeping startup time short.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-hook-name-suffix nil)

(defcustom my-configuration-file (concat user-emacs-directory "configuration.org")
  "Custom configuration file, loaded by `user-init-file'."
  :type 'file
  :group 'initialization)

;; Don't litter the init file with customization changes. Instead, write it to a
;; different file that is not tracked by version control.
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; To reduce startup time, only tangle configuration when the .org file is newer
;; than the .el file. Otherwise, simply load the .el file.
(let ((tangled-file (concat (file-name-sans-extension my-configuration-file) ".el")))
  (if (file-newer-than-file-p my-configuration-file tangled-file)
      (org-babel-load-file my-configuration-file)
    (load-file tangled-file)))

;; To prevent the initial startup message from stepping over this message,
;; customize the variable `inhibit-startup-echo-area-message'.
(message (emacs-init-time))
