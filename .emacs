
;; START GLOBAL EMACS
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
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

;; Auto-close parenthesis, etc.
(electric-pair-mode 1)

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

;; END GLOBAL EMACS


;; START SMART-MODE-LINE
(use-package smart-mode-line
  :ensure t
  :demand
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
  :demand
  :bind (("M-x"     . helm-M-x)
         ("C-x b"   . helm-mini)
         ("<C-tab>" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ;; Resume previous helm session with prefix to choose among existing
         ;; helm buffers.
         ("C-x C-h" . (lambda() (interactive) (helm-resume t)))
         ("C-s"     . helm-occur)
         ("C-x r l" . helm-bookmarks)
         :map helm-map
         ;; Use <C-tab> and <C-S-tab> to navigate helm buffers.
         ("<C-tab>"   . helm-next-line)
         ("<C-S-tab>" . helm-previous-line))

  :config
  (helm-mode 1)

  :custom
  (helm-split-window-inside-p t))

;; END HELM


;; START PROJECTILE
(use-package projectile
  :after helm
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind ("<f9>" . projectile-compile-project)

  :config
  (projectile-mode)

  :custom
  (projectile-completion-system 'helm)
  (projectile-indexing-method 'alien)
  (projectile-enable-caching t)
  (projectile-use-git-grep t))

;; END PROJECTILE


;; START HELM-PROJECTILE
(use-package helm-projectile
  :after (helm projectile)
  :config
  (helm-projectile-on)

  :custom
  (projectile-switch-project-action 'helm-projectile)
  (helm-projectile-fuzzy-match nil))

;; END HELM-PROJECTILE


;; START ORG MODE
(use-package org
  :after evil
  ;; Global key bindings.
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cc" . org-capture)
         ("\C-cb" . org-iswitchb)
         ("\C-ci" . clock-in)
         ("\C-co" . org-clock-out))
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
    (org-insert-heading-after-current)
    (org-insert-time-stamp (current-time) nil t)
    (org-demote)
    (org-clock-in))

  (defun clock-in ()
    "Clock in with org mode."
    (interactive)
    (if (not (org-at-heading-p))
        (user-error "Not at a heading"))
    (org-insert-heading-after-current)
    (org-insert-time-stamp (current-time) nil t)
    (org-clock-in))

  (org-clock-persistence-insinuate)

  ;; Org mode babel language support.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sh . t)
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

  :custom
  (org-todo-keywords
        '((sequence "TODO" "IN PROGRESS" "REVIEW" "DONE" )))

  (org-outline-path-complete-in-steps nil)

  ;; Save the running clock when Emacs exits.
  (org-clock-persist 'clock))

;; END ORG MODE


;; START EVIL
(use-package evil
  :ensure t
  :diminish undo-tree-mode
  :demand
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
  (evil-set-initial-state 'xref-buffer-mode     'emacs)

  :custom
  (evil-want-C-u-scroll t "Scroll up using C-u.")
  (evil-symbol-word-search t))

;; END EVIL


;; START EVIL-LEADER
(use-package evil-leader
  :ensure t
  :demand
  :after evil
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key "e"   '(lambda() (interactive) (find-file user-init-file))

                       "sh"   'eshell

                       "wc"  'delete-window
                       "x0"  'delete-window

                       "ww"  'other-window
                       "xo"  'other-window

                       "wo"  'delete-other-windows
                       "x1"  'delete-other-windows

                       "ws"  'split-window-below
                       "x2"  'split-window-below

                       "wv"  'split-window-right
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
                       "xh"  '(lambda() (interactive) (helm-resume t))

                       "l"   'whitespace-mode
                       "hl"  'hl-line-mode
                       "rl"  'toggle-nlinum-relative

                       "m"   'compile
                       "c"   'compile

                       "st"  'magit-status
                       "f"   'magit-file-popup

                       "pf"  'helm-projectile-find-file
                       "psg" 'helm-projectile-grep
                       "pa"  'helm-projectile-find-other-file)

  ;; Enable evil leader.
  (global-evil-leader-mode)

  ;; Start evil.
  (evil-mode))

;; END EVIL-LEADER


;; START NLINUM
(use-package nlinum
  :after evil-leader
  :defer t)

;; END NLINUM

;; START NLINUM-RELATIVE
(use-package nlinum-relative
  :after (nlinum)
  :defer t
  :config
  (nlinum-relative-setup-evil))

(defun toggle-nlinum-relative ()
  "Toggles relative line numbers using `nlinum-relative'.

This differs from `nlinum-relative-toggle' in that line numbers in the
margin are enabled and disabled when toggled. `nlinum-relative-toggle'
only toggles when `nlinum-mode' is enabled."
  (interactive)
  (if (bound-and-true-p nlinum-mode)
      (nlinum-mode -1)
    (nlinum-mode 1))
  (nlinum-relative-on))

;; END NLINUM-RELATIVE


;; START MAGIT
(eval-when-compile
  (require 'magit))

(use-package magit
  :diminish auto-revert-mode
  :defer t
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

  :custom
  (magit-refresh-verbose t))

;; END MAGIT


;; PRETTY CONTROL-L
(use-package pp-c-l
  :config
  (pretty-control-l-mode t))
;; END PRETTY CONTROL-L


;; START ACE-JUMP-MODE
(use-package ace-jump-mode
  :after evil
  :bind ( :map global-map
               ("C-x SPC" . ace-jump-mode-pop-mark)
               ("C-c SPC" . ace-jump-mode)
          :map evil-normal-state-map
               ("SPC" . ace-jump-mode)))
;; END ACE JUMP MODE


