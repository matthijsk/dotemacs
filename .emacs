
;; START GLOBAL EMACS
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(require 'use-package)

(use-package diminish
  :ensure t)

(setq initial-buffer-choice t)

(server-start)

(setq fill-column 100)

;; Highlight matching parentheses.
(show-paren-mode 1)

;; Never indent with tabs.
(setq-default indent-tabs-mode nil)

;; Allow for basic paging in emacs shells.
(setenv "PAGER" "/bin/cat")

;; Scroll by one line when reaching bottom of buffer.
(setq scroll-conservatively 101)
(setq scroll-margin 3)

;; Don't speed up when scrolling faster.
(setq mouse-wheel-progressive-speed nil)
;; Scroll by three lines when using the mouse wheel.
(setq mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control)))))

;; Load custom theme.
(load-theme 'wombat)

;; Disable tool bar, menu bar and scroll bars.
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-scroll-bar-mode nil)

;; Start maximized
(toggle-frame-maximized)

;; Allow narrowing.
(put 'narrow-to-region 'disabled nil)

;; Allow upcasing and lowercasing of regions.
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

;; Hooks.
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; Key bindings:
;; Remap C-x C-b to ibuffer instead of the default.
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Load alternate file (useful for switching .c and .h)
(global-set-key (kbd "<f6>") 'ff-find-other-file)

;; Autoscroll compilation window and stop on first error.
(setq compilation-scroll-output 'first-error)

;; Exhibit expected X clipboard behaviour.
(global-set-key (kbd "<mouse-2>") 'x-clipboard-yank)
(setq-default x-select-enable-primary t)

;; Set c indentation to 2
(setq c-default-style "linux"
      c-basic-offset 2)

;; Set xml-mode when loading cbproj and groupproj files.
(add-to-list 'auto-mode-alist '("\\.cbproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.groupproj\\'" . xml-mode))

;; Set c++-mode when loading .h files. All I do is c++ all day.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Set c++-mode when loading .rc and .rh files.
(add-to-list 'auto-mode-alist '("\\.rh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.rc\\'" . c++-mode))

;; Increase warning limit to 100 MB for large files.
(setq large-file-warning-threshold (* 100 1024 1024))

;; Default to displaying line numbers as relative.
(setq display-line-numbers-type 'relative)

;; Display time as 24h
(setq display-time-24hr-format t)

;; END GLOBAL EMACS


;; START CMAKE-MODE
(use-package cmake-mode
  :ensure t
  :defer t)

;; END CMAKE-MODE


;; START NINJA-MODE
(use-package ninja-mode
  :ensure t
  :defer t)

;; END NINJA-MODE


;; START SMART-MODE-LINE
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'respectful)
  (sml/setup))

;; END SMART-MODE-LINE


;; START DEFAULT-TEXT-SCALE
;; Allow scaling of all buffers.
(use-package default-text-scale
  :ensure t
  :bind (("C-M-="       . default-text-scale-increase)
         ("C-<mouse-4>" . default-text-scale-increase)
         ("C-M--"       . default-text-scale-decrease)
         ("C-<mouse-5>" . default-text-scale-decrease)))


;; END DEFAULT-TEXT-SCALE


;; START HELM
(use-package helm
  :diminish helm-mode
  :ensure t
  :bind (("M-x"     . helm-M-x)
         ("C-x b"   . helm-mini)
         ("<C-tab>" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x C-h" . helm-resume-existing)
         ("C-s"     . helm-occur)
         ("C-x r l" . helm-bookmarks)
         :map helm-map
         ;; Use <C-tab> and <C-S-tab> to navigate helm buffers.
         ("<C-tab>"   . helm-next-line)
         ("<C-S-tab>" . helm-previous-line))

  :config
  (defun helm-resume-existing ()
    "Resume previous helm session with prefix to choose among existing helm buffers."
    (interactive)
    (helm-resume t))

  (helm-mode 1)

  ;; :custom
  (setq helm-buffer-max-length nil)
  (setq helm-split-window-inside-p t))

;; END HELM


;; START PROJECTILE
(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind ("<f9>" . projectile-compile-project)
  :init
  (add-hook 'c++-mode-hook 'projectile-mode)
  (add-hook 'cmake-mode-hook 'projectile-mode)

  :config
  (projectile-global-mode)

  ;; :custom
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-use-git-grep t))

;; END PROJECTILE


;; START HELM-PROJECTILE
(use-package helm-projectile
  :ensure t
  :after projectile
  :init
  (setq helm-projectile-fuzzy-match nil)
  (setq projectile-switch-project-action 'helm-projectile)
  :config
  (helm-projectile-on))

;; END HELM-PROJECTILE


;; START ORG MODE
;; Org html export requires htmlize
(use-package htmlize
  :ensure t
  :defer t)

(use-package org
  ;; Global key bindings.
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cc" . org-capture)
         ("\C-cb" . org-iswitchb)
         ("\C-ci" . clock-in)
         ("\C-co" . org-clock-out))
  :init
  (setq org-todo-keywords
        '((sequence "TODO" "IN PROGRESS" "REVIEW" "DONE" )))

  :config
  (defun iso-week-number ()
    "Returns the ISO week number for today."
    (org-days-to-iso-week (org-today)))

  (defun clock-in-monday ()
    "Creates a new \"Week <WEEK-NUMBER>\" heading."
    (interactive)
    (if (not (org-at-heading-p))
        (user-error "Not at a heading"))
    (beginning-of-line)
    (org-insert-heading)
    (insert (format "Week %s" (iso-week-number)))
    (clock-in t))

  (defun clock-in (&optional monday)
    "Clock in with org mode."
    (interactive)
    (if (not (org-at-heading-p))
        (user-error "Not at a heading"))
    (org-insert-heading-after-current)
    (org-insert-time-stamp (current-time) nil t)
    (if monday
        (org-demote))
    (org-clock-in))

  (org-clock-persistence-insinuate)

  ;; Org mode babel language support.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (C . t)))

  ;; Do not interpret "_" and "^" for sub and superscript when
  ;; exporting.
  (setq org-export-with-sub-superscripts nil)

  ;; When in org-mode, use expected org-mode tab behaviour when in
  ;; Normal and Insert state. Set jump keys to navigate org links and
  ;; the mark ring.
  (evil-define-key 'normal org-mode-map
    [tab] 'org-cycle
    (kbd "C-]") 'org-open-at-point
    (kbd "C-o") 'org-mark-ring-goto)

  (evil-define-key 'insert org-mode-map [tab] 'org-cycle)

  ;; :custom
  (setq org-outline-path-complete-in-steps nil)

  ;; Save the running clock when Emacs exits.
  (setq org-clock-persist 'clock)

  ;; Flushright tags to column 100
  (setq org-tags-column -100))

;; END ORG MODE


;; START EVIL
(use-package evil
  :ensure t
  :demand t
  :diminish undo-tree-mode
  :bind (:map evil-normal-state-map
              ([tab] . other-window)
              ("C-s" . save-buffer)
              ("C-/" . comment-line)
              ("C-f" . helm-occur)

         :map evil-motion-state-map
              ([tab] . other-window)
              ("SPC" . scroll-up-command)
              ("DEL" . scroll-down-command)
              ("C-f" . helm-occur)

         :map evil-insert-state-map
              ("C-s" . save-buffer)
         )
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-symbol-word-search t)
  (setq evil-shift-width 2)

  :config
  ;; Jump to tag and recenter
  (advice-add 'evil-jump-to-tag     :after 'evil-scroll-line-to-center)
  (advice-add 'evil-jump-backward   :after 'evil-scroll-line-to-center)
  (advice-add 'evil-jump-forward    :after 'evil-scroll-line-to-center)
  (advice-add 'evil-search-next     :after 'evil-scroll-line-to-center)
  (advice-add 'evil-search-previous :after 'evil-scroll-line-to-center)

  ;; Ex commands.
  (evil-ex-define-cmd "A"  'ff-find-other-file)
  (evil-ex-define-cmd "ls" 'ibuffer)
  (evil-ex-define-cmd "e"  'helm-find-files)

  ;; Set evil mode when in these modes.
  (add-hook 'with-editor-mode-hook 'evil-normal-state)

  ;; Set emacs state when in these modes.
  (evil-set-initial-state 'eshell-mode          'emacs)
  (evil-set-initial-state 'shell-mode           'emacs)
  (evil-set-initial-state 'dired-mode           'emacs)
  (evil-set-initial-state 'Info-mode            'emacs)
  (evil-set-initial-state 'calendar-mode        'emacs)
  (evil-set-initial-state 'Custom-mode          'emacs)
  (evil-set-initial-state 'messages-buffer-mode 'emacs)
  (evil-set-initial-state 'magit-staging-mode   'emacs)
  (evil-set-initial-state 'xref-buffer-mode     'emacs))

;; END EVIL


;; START EVIL-LEADER
(use-package evil-leader
  :ensure t
  :after evil
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key "e"   '(lambda() (interactive) (find-file user-init-file))

                       "sh"   'eshell

                       "wc"  'evil-window-delete
                       "x0"  'delete-window

                       "ww"  'evil-window-next
                       "xo"  'other-window

                       "wo"  'delete-other-windows
                       "x1"  'delete-other-windows

                       "ws"  'evil-window-split
                       "x2"  'split-window-below

                       "wv"  'evil-window-vsplit
                       "x3"  'split-window-right

                       "wh"  'evil-window-left
                       "wj"  'evil-window-down
                       "wk"  'evil-window-up
                       "wl"  'evil-window-right

                       "xk"  'kill-buffer
                       "rb"  'revert-buffer
                       "x#"  'server-edit

                       "b"   'helm-mini
                       "xf"  'helm-find-files
                       "hb"  'helm-bookmarks
                       "hs"  'helm-semantic
                       "xh"  'helm-resume-existing

                       "l"   'whitespace-mode
                       "hl"  'hl-line-mode
                       "rl"  'display-line-numbers-mode

                       "m"   'compile
                       "c"   'compile

                       "pf"  'helm-projectile-find-file
                       "psg" 'helm-projectile-grep
                       "pa"  'helm-projectile-find-other-file)

  ;; Enable evil leader.
  (global-evil-leader-mode)

  ;; Start evil.
  (evil-mode))

;; END EVIL-LEADER


;; START MAGIT
(use-package magit
  :ensure t
  :defer t
  :bind (:map evil-leader--default-map
              ("st" . magit-staging)
              ("f"  . magit-file-popup))
  :init
  (setq vc-handled-backends nil)

  :config

  ;; Improve staging performance on windows
  ;; See https://github.com/magit/magit/issues/2395
  (define-derived-mode magit-staging-mode magit-status-mode "Magit staging"
    "Mode for showing staged and unstaged changes."
    :group 'magit-status)

  (defun magit-staging-refresh-buffer ()
    (magit-insert-section (status)
                          (magit-insert-untracked-files)
                          (magit-insert-unstaged-changes)
                          (magit-insert-staged-changes)))

  (defun magit-staging ()
    (interactive)
    (magit-mode-setup #'magit-staging-mode))

  (magit-define-popup-switch 'magit-log-popup ?f "first parent" "--first-parent")

  (evil-define-key 'normal magit-blame-mode-map (kbd "q") 'magit-blame-quit)

  ;; :custom
  (setq magit-refresh-verbose t))

;; END MAGIT


;; START GTAGS
;; Force treating of .h files as C++ source
(setenv "GTAGSFORCECPP" "true")

;; END GTAGS

;; START HELM GTAGS
(use-package helm-gtags
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  ;; :custom
  :config
  (setq helm-gtags-path-style 'absolute)
  (setq helm-gtags-use-input-at-cursor t)
  (setq helm-gtags-auto-update t)
  (setq helm-gtags-pulse-at-cursor t)
  :config
  (evil-define-key 'normal c++-mode-map (kbd "C-]") 'helm-gtags-dwim))

;; END HELM GTAGS


;; START COMPANY
(use-package company
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'cmake-mode-hook 'company-mode)
  :config
  (setq company-dabbrev-downcase nil) ;; Be case sensitive about completion
  (setq company-dabbrev-ignore-case nil) ;; Be case sensitive about completion
  (setq company-async-timeout 10)
  ;; :custom
  (setq company-idle-delay nil))

;; END COMPANY


;; START HELM-COMPANY
(use-package helm-company
  :ensure t
  :bind (:map evil-insert-state-map
              ([tab] . helm-company))
  ;; :custom
  :config
  (setq helm-company-fuzzy-match nil))

;; END HELM-COMPANY


;; START SEMANTIC
(use-package semantic
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'semantic-mode)
  ;; :custom
  :config
  ;; Ensure semantic is not used by company
  (setq company-backends (delete 'company-semantic company-backends))
  (global-semantic-stickyfunc-mode)
  (evil-define-key 'normal c++-mode-map (kbd "C-}") 'semantic-ia-fast-jump))

;; END SEMANTIC
