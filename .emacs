(require 'package)

(setq package-list '(evil
		     evil-leader))
;;Add Melpa as the default Emacs Package repository
;;only contains a very limited number of packages
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;Activate all the packages (in particular autoloads)
(package-initialize)



;;Update your local package index
(unless package-archive-contents
  (package-refresh-contents))

;;Install all missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


(require 'evil)
(evil-mode t)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader " ")
(evil-leader/set-key
  "b" 'switch-to-buffer
  "w" 'save-buffer)
(custom-set-variables
 ;;;custom-set-variables was added by Custom.
 ;;;If you edit it by hand, you could mess it up, so be careful.
 ;;;Your init file should contain only one such instance.
 ;;;If there is more than one, they won't work right.
 '(package-selected-packages (quote (evil-leader evil))))
(custom-set-faces
 ;;;custom-set-faces was added by Custom.
 ;;;If you edit it by hand, you could mess it up, so be careful.
 ;;;Your init file should contain only one such instance.
 ;;;If there is more than one, they won't work right.
 )
