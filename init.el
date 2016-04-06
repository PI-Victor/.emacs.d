;; List of packages that the config is using:
;; fancy-battery-mode - battery indicator
;; fci-mode - fill column for better word wrap
;; go-autocomplete - autocomplete for golang
;; helm helm-ag helm-projectile - project management
;; projectile - same as above


;; graphical display settings
;; check to see if we're running x11, otherwise revert back to standard theme
(if (display-graphic-p)
    (progn
      ;; add custom theme
      (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
      (load-theme 'deeper-blue t)
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1))
  (load-theme 'misterioso t))



;; set package repositories
(require 'package)


;; activate all the packages (in particular autoloads)
(package-initialize)



;; list the repositories containing them
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
       ("gnu" . "http://elpa.gnu.org/packages/")
       ("marmalade" . "http://marmalade-repo.org/packages/")))


;; list the packages you want
(setq package-list '(fancy-battery
		     sunshine
		     go-autocomplete
		     go-direx ;; requires go get -u github.com/jstemmer/gotags
		     go-mode
		     golint
		     govet
		     ag
		     helm-spotify
		     move-text
		     window-purpose
		     go-errcheck
		     go-gopath
		     go-playground
		     go-projectile
		     projectile
		     helm
		     helm-projectile
		     helm-ag
		     fill-column-indicator
		     ac-python
		     anaconda-mode
		     auto-virtualenv
		     ein))

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; emacs general settings
;; show the column number
(setq column-number-mode t)
;; set word wrapping for comments to 79 chars
(setq-default fill-column 79)
;; enable fancy-battery
(add-hook 'after-init-hook #'fancy-battery-mode)
;; enable visual line wrapper
(setq line-move-visual t)
;; turn on visual line mode wrapping
(global-visual-line-mode)
;; set word wrapping for comments to 79 chars
(global-linum-mode)
(setq-default fill-column 79)
;; Load custom modules 
(add-to-list 'load-path "~/.emacs.d/custom")
;; enable flyspell-mode for highlighting spelling errors
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'find-file-hooks 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a772eafba4eda0ed57a5d651a96804487a1dacbfbf8658084bfe84546a7c7008" default)))
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized)))))


(require 'go-autocomplete)
(require 'auto-complete-config)

(ac-config-default)


;;change default auto-save directory to avoid any clashes with git

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
              emacs-tmp-dir)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; whatever is above needs to be refactored into a coherent configuration for
;; emacs

(require 'projectile)
;; enable projectile for managing projects
(projectile-global-mode)
;; enable caching of projects
(setq projectile-enable-caching t)
;; open projectile at startup

(defun open-project ()
  (interactive)
  (purpose-load-window-layout "golang_golint")
  (helm-projectile))

(defun open-file ()
		  (interactive)
		  (helm-projectile))

(defun search-for-file ()
  (interactive)
  (projectile-find-file))

(defun search-for-expression ()
  (interactive)
  (projectile-grep))
;; These are go specifics, should be under a hook 
(defun jump-into-definition ()
  ;; opens a go file as a source tree
  (require 'go-direx)
  (global-set-key (kbd "<f5>") 'go-direx-switch-to-buffer)
  ;; jumps into a function definition
  (local-set-key (kbd "M-.") 'godef-jump))

(add-hook 'go-mode-hook 'jump-into-definition)

(global-set-key (kbd "<f10>") 'open-project)
(global-set-key (kbd "<f9>") 'open-file)
(global-set-key (kbd "<f8>") 'search-for-file)
;; this might need a hook, since it's only available for golang;
;; opens a go file as a source tree
(require 'go-direx)
(global-set-key (kbd "<f5>") 'go-direx-switch-to-buffer)
(global-set-key (kbd "<f7>") 'search-for-expression)

;; requirements for Fill Column Indicator
(require 'fill-column-indicator)
;; define a global way of accessing the package, otherwise it needs a hook to
;; load
(define-globalized-minor-mode my-global-fci-mode fci-mode turn-on-fci-mode)
(my-global-fci-mode 1)
;; set fill column indicator width and color
(setq fci-rule-width 1)
(setq fci-rule-column 79)
(setq fci-rule-color "green")

;; sunshine setup
(setq sunshine-location "Brno, Czech Republic")
(setq sunshine-appid "19e6375d5a446d2a94d4b0df498a23ce")
(setq sunshine-units 'metric)
(setq sunshine-show-icons t)

;; Load the window manager for emacs
(require 'window-purpose)
(purpose-mode)

