;; Global settings
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Font
(set-face-attribute 'default nil :font "Hack" :height 220)

;; Package management
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
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
         ("C-d" . ivy-reverse-i-search-kill))
  :config)
(ivy-mode 1)

(use-package all-the-icons)

(use-package doom-modeline
    :init (doom-modeline-mode 1))

(use-package doom-themes
    :config
    (setq doom-themes-enable-bold t
	  doom-themes-enable-italic t)
    (load-theme 'doom-monokai-pro t))

(custom-set-variables
 '(package-selected-packages
   '(use-package doom-themes doom-modeline counsel all-the-icons)))
(custom-set-faces)
