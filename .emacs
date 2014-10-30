(load-theme 'misterioso)
(add-to-list 'load-path "~/.emacs.d/custom/")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
