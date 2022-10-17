(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(ispell-dictionary nil)
 '(package-selected-packages '(counsel swiper doom-modeline ivy use-package))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack"
				:foundry "SRC"
		  		:slant normal
		   		:weight normal
				:height 241
				:width normal)))))

(setq inhibit-startup-message t)    ;; Disable landing page on startup
(setq visible-bell t)               ;; Enable visual bell, replacing audible bell
(scroll-bar-mode -1)                ;; Disable visible scrollbar
(tool-bar-mode -1)                  ;; Disable the toolbar
(tooltip-mode -1)                   ;; Disable the tooltips
(menu-bar-mode -1)                  ;; Disable the menu bar
(set-fringe-mode 10)                ;; Give some breathing room

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
						 ("org" . "https://orgmode.org/elpa/")
						 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package ivy
    :diminish
    :bind (("C-s" . swiper)
    :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
    :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
    :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill)))
(ivy-mode 1)
(use-package swiper)
(use-package counsel)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
