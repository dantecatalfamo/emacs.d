
;; Raise GC limits during startup for speed increase, then reset it
(setq gc-cons-threshold  most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook (lambda() (setq gc-cons-threshold  100000000
                                           gc-cons-percentage 0.1)))

;; Only show menu bar on MacOS
(if (fboundp 'menu-bar-mode) (menu-bar-mode (if (eq system-type 'darwin) 1 -1)))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
