(if (display-graphic-p) ;; check to see if we're running x11, otherwise revert back to standard theme
    (progn
      (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"));; add custom theme
      (load-theme 'adwaita t)
      (tool-bar-mode -1))
  (load-theme 'wombat t))

(package-initialize) ;; You might already have this line
(add-to-list 'load-path "~/.emacs.d/custom/") ;; add custom for rust-el
(autoload 'rust-mode "rust-mode" nil t)
(require 'go-mode-autoloads)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a772eafba4eda0ed57a5d651a96804487a1dacbfbf8658084bfe84546a7c7008" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

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
