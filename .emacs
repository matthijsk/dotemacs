(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t)

;; START GLOBAL EMACS
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
(setq scroll-margin 5)

;; Load custom theme.
(load-theme 'wombat)

;; Disable tool bar, menu bar and scroll bars.
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-scroll-bar-mode nil)

;; Allow narrowing.
(put 'narrow-to-region 'disabled nil)

;; Allow upcasing and lowercasing of regions.
(put 'upcase-region 'disabled nil)
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

;; START DEFAULT-TEXT-SCALE
;; Allow scaling of all buffers.
(use-package default-text-scale
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)))

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
         :map helm-map
         ;; Use <C-tab> and <C-S-tab> to navigate helm buffers.
         ("<C-tab>"   . helm-next-line)
         ("<C-S-tab>" . helm-previous-line))
  :config
  (helm-mode 1)

  (setq helm-split-window-inside-p t)
  (setq helm-autoresize-mode t)

  (setq helm-M-x-fuzzy-match t)

  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)

  (setq helm-ff-fuzzy-matching t))

;; END HELM

;; START PROJECTILE
(use-package projectile
  :after helm
  :config
  (projectile-mode)
  (setq projectile-completion-system 'helm)

  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t))

(use-package helm-projectile
  :after (helm projectile)
  :config
  (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-projectile))

;; END PROJECTILE

;; START ORG MODE
(use-package org
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
    (org-insert-heading-after-current)
    (insert (format "Week %s" (iso-week-number)))
    (org-insert-heading-after-current)
    (org-insert-time-stamp (current-time))
    (org-demote)
    (org-clock-in))

  (defun clock-in ()
    "Clock in with org mode."
    (interactive)
    (if (not (org-at-heading-p))
	(user-error "Not at a heading"))
    (org-insert-heading-after-current)
    (org-insert-time-stamp (current-time))
    (org-clock-in))

  (setq org-todo-keywords
	'((sequence "TODO" "IN PROGRESS" "REVIEW" "DONE" )))

  (setq org-outline-path-complete-in-steps nil)

  ;; Save the running clock when Emacs exits.
  (setq org-clock-persist 'clock)
  (org-clock-persistence-insinuate)

  ;; Org mode babel language support.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sh . t)
     (C . t)))

  ;; Do not interpret "_" and "^" for sub and superscript when
  ;; exporting.
  (setq org-export-with-sub-superscripts nil))

;; END ORG MODE

;; START EVIL
(use-package evil
  :diminish undo-tree-mode
  :ensure t
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

         :map evil-insert-state-map
              ("C-s" . save-buffer)
         )
  :config
  ;; Start evil.
  (evil-mode)

  ;; Jump to tag and recenter
  (advice-add 'evil-jump-to-tag :after 'evil-scroll-line-to-center)
  (advice-add 'evil-jump-backward :after 'evil-scroll-line-to-center)
  (advice-add 'evil-jump-forward  :after 'evil-scroll-line-to-center)

  ;; When in org-mode, use expected org-mode tab behaviour when in Normal state.
  ;; Set jump keys to navigate org links and the mark ring.
  (evil-define-key 'normal org-mode-map [tab] 'org-cycle
		   (kbd "C-]") 'org-open-at-point
		   (kbd "C-o") 'org-mark-ring-goto)

  ;; Ex commands.
  (evil-ex-define-cmd "A" 'ff-find-other-file)
  (evil-ex-define-cmd "ls" 'ibuffer)
  (evil-ex-define-cmd "e" 'helm-find-files)

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
  ;; Enable evil leader.
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key "e"  '(lambda() (interactive) (find-file user-init-file))

		       "wc" 'delete-window
		       "x0" 'delete-window

		       "ww" 'other-window
		       "xo" 'other-window

		       "wo" 'delete-other-windows
		       "x1" 'delete-other-windows

		       "ws" 'split-window-below
		       "x2" 'split-window-below

		       "wv" 'split-window-right
		       "x3" 'split-window-right

		       "wh" 'evil-window-left
		       "wj" 'evil-window-down
		       "wk" 'evil-window-up
		       "wl" 'evil-window-right

		       "bd" 'kill-buffer
		       "xk" 'kill-buffer
		       "rb" 'revert-buffer
		       "x#" 'server-edit

                       "b"  'helm-mini
		       "xf" 'helm-find-files

		       "l"  'whitespace-mode
		       "hl" 'hl-line-mode
		       "rl" 'linum-relative-mode

		       "m"  'compile
		       "c"  'compile

		       "st" 'magit-status
		       "f"  'magit-file-popup

                       "pf" 'helm-projectile-find-file
                       "psg" 'helm-projectile-grep
                       "pa" 'helm-projectile-find-other-file))

;; END EVIL-LEADER

;; START MAGIT
(eval-when-compile
  (require 'magit))

(use-package magit
  :defer t
  :config
  (setq vc-handled-backends nil)
  (setq magit-refresh-verbose t)

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

  (magit-define-popup-switch 'magit-log-popup ?f "first parent" "--first-parent"))

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
