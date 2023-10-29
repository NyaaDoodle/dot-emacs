;; straight.el Bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; Loading config files as Org files
(org-babel-load-file (expand-file-name "~/.emacs.d/current-init.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/custom-macros.org"))
