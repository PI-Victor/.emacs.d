;; because go-vet and goimports don't find all errors like they should, i'm
;; writing a wrapper function around golint to run on each save of a go-file

(defun golint-before-save ()
  (interactive)
  (message "Trying to golint"))


(defun go-mode-setup ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'golint-before-save)
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'go-mode-setup)
;; run gofmt on save
;; (add-hook 'before-save-hook 'gofmt-before-save)
(setq visual-line-mode t)
