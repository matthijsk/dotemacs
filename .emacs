(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; START GLOBAL EMACS

(server-start)

;; Enable ido mode.
(ido-mode 1)

;; Highlight matching parentheses.
(show-paren-mode 1)

;; Never indent with tabs.
(setq-default indent-tabs-mode nil)

;; Allow for basic paging in emacs shells.
(setenv "PAGER" "/bin/cat")

;; Scroll by one line when reaching bottom of buffer.
(setq scroll-conservatively 1)
(setq scroll-margin 5)

;; Load custom theme.
(load-theme 'wombat)

;; Load alternate file (useful for switching .c and .h)
(global-set-key (kbd "<f6>") 'ff-find-other-file)

;; Disable tool bar and scroll bars
(tool-bar-mode -1)
(set-scroll-bar-mode nil)

;; Allow narrowing.
(put 'narrow-to-region 'disabled nil)

;; END GLOBAL EMACS

;; START ORG MODE

;; Org mode babel language support
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

                     "bd" 'kill-buffer
                     "xk" 'kill-buffer

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

;; When in org-mode, use expected org-mode tab behaviour when in Normal state.
(evil-define-key 'normal org-mode-map [tab] 'org-cycle)

;; Ex commands.
(evil-ex-define-cmd "A" 'ff-find-other-file)

;; Remap <SPC> and <RET> to behave like Emacs keys when in Motion state.
(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location."
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

;; Set evil mode when in these modes.
(evil-set-initial-state 'package-menu-mode 'normal)

;; Set emacs state when in these modes.
(evil-set-initial-state 'eshell-mode 'emacs)
(evil-set-initial-state 'shell-mode 'emacs)

;; END EVIL

;; START MAGIT

(setq magit-last-seen-setup-instructions "1.4.0")

;; END MAGIT
