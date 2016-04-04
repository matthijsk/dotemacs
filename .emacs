(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; START GLOBAL EMACS

(server-start)

;; Enable ido mode with smex.
(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
(global-set-key (kbd "M-x") 'smex)

;; Highlight matching parentheses.
(show-paren-mode 1)

;; Never indent with tabs.
(setq-default indent-tabs-mode nil)

;; Auto-close parenthesis, etc.
(electric-pair-mode 1)

;; Allow for basic paging in emacs shells.
(setenv "PAGER" "/bin/cat")

;; Scroll by one line when reaching bottom of buffer.
(setq scroll-conservatively 1)
(setq scroll-margin 5)

;; Load custom theme.
(load-theme 'wombat)

;; Disable tool bar and scroll bars.
(tool-bar-mode -1)
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

;; START ORG MODE

;; Global key bindings.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; Use ido for completion.
(setq org-completion-use-ido t)
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

;; END ORG MODE

;; START EVIL

;; Evil: scroll up using C-u.
(setq evil-want-C-u-scroll t)

;; Enable evil jumper.
(require 'evil-jumper)
(global-evil-jumper-mode)

;; Enable evil leader.
(require 'evil-leader)
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
                     "rb"  'revert-buffer

                     "b"  'ido-switch-buffer
                     "xf" 'ido-find-file

                     "l"  'whitespace-mode
                     "hl" 'hl-line-mode
                     "rl" 'relative-line-numbers-mode

                     "m"  'compile
                     "c"  'compile

                     "st" 'magit-status)

;; Start evil.
(require 'evil)
(evil-mode 1)

;; Switch between windows with tab key.
;; Taken from http://www.emacswiki.org/emacs/Evil#toc12
(define-key evil-normal-state-map [tab] 'other-window)
(define-key evil-motion-state-map [tab] 'other-window)

;; Save buffer with C-s, but only in normal mode.
(define-key evil-normal-state-map (kbd "C-s") 'save-buffer)

;; <SPC> and <DEL> behave like Emacs keys in Normal state.
(define-key evil-normal-state-map " " 'scroll-up-command)
(define-key evil-normal-state-map (kbd "DEL") 'scroll-down-command)
(define-key evil-motion-state-map " " 'scroll-up-command)
(define-key evil-motion-state-map (kbd "DEL") 'scroll-down-command)

;; When in org-mode, use expected org-mode tab behaviour when in Normal state.
;; Set jump keys to navigate org links and the mark ring.
(evil-define-key 'normal org-mode-map [tab] 'org-cycle
                                      (kbd "C-]") 'org-open-at-point
                                      (kbd "C-o") 'org-mark-ring-goto)

;; Ex commands.
(evil-ex-define-cmd "A" 'ff-find-other-file)
(evil-ex-define-cmd "ls" 'ibuffer)
(evil-ex-define-cmd "e" 'ido-find-file)

;; Set evil mode when in these modes.
(evil-set-initial-state 'package-menu-mode 'normal)

;; Set emacs state when in these modes.
(evil-set-initial-state 'eshell-mode 'emacs)
(evil-set-initial-state 'shell-mode  'emacs)
(evil-set-initial-state 'dired-mode  'emacs)
(evil-set-initial-state 'Info-mode   'emacs)
(evil-set-initial-state 'calendar-mode 'emacs)

;; END EVIL

;; START MAGIT

(setq magit-last-seen-setup-instructions "1.4.0")

;; END MAGIT

;; POWERLINE
(require 'powerline)
(require 'powerline-evil)
(powerline-evil-center-color-theme)
