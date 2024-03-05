;; Optimize startup by setting a very large gc cons threshold value. Restore the
;; value after start up is done to prevent noticeable gc sweeps.
;;
;; https://www.reddit.com/r/emacs/comments/ofhket/further_boost_start_up_time_with_a_simple_tweak/h4fxqyn?utm_source=share&utm_medium=web2x&context=3
(defvar old-file-name-handler file-name-handler-alist)
(setq file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum)

(add-hook 'after-init-hook (lambda nil
                             (setq gc-cons-threshold     1600000
                                   gc-cons-percentage   0.1
                                   file-name-handler-alist old-file-name-handler)))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
