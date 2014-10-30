(load-theme 'misterioso)
(add-to-list 'load-path "~/.emacs.d/custom/")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
;;add marmelade package repor for online downloading
;;to load package listing M-x eval-buffer
;;M-x package-refresh-contents
;; to list packages from marmelade M-x package-list-packages
;;Now, to install packages, move your cursor to them and press i. 
;;This will mark the packages for installation. When you're done
;; with marking, press x, and ELPA will install the packages for 
;;you (under ~/.emacs.d/elpa/).  or using M-x package-install 
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
