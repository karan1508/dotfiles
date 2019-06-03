(require 'package)

;; encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq package-list '(evil
		     org-bullets
		     evil-collection
		     projectile
		     elpy
		     evil-leader
		     material-theme
		     dracula-theme
		     helm
		     magit
		     helm-projectile
		     restart-emacs
		     evil-surround))

;;Add Melpa as the default Emacs Package repository
;;only contains a very limited number of packages
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;Activate all the packages (in particular autoloads)
(package-initialize)

;;python mode
(advice-add 'python-mode :before 'elpy-enable)

;;abbrev mode
(setq-default abbrev-mode t)
(setq abbrev-file-name "~/emacs_abbrev.el")



;; line numbers
(global-linum-mode t)

;; terminal fixes
(add-hook 'term-exec-hook
	  (function
	   (lambda ()
	     (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))


;;Update your local package index
(unless package-archive-contents
  (package-refresh-contents))

;;Install all missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 50)

;; eye candy
;; (load-theme 'dracula t)
(load-theme 'material t)
(global-font-lock-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Set default font
(set-face-attribute 'default nil
		    :family "Source Code pro"
		    :height 110
		    :weight 'normal
		    :width 'normal)

;; org-mode
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))

;; helm
(require 'helm-config)

;; projectile
(require 'helm-projectile)
(helm-projectile-on)


(require 'evil)
(evil-mode t)

;; (use-package evil-surround
;;   :ensure t
;;   :config
;;   (global-evil-surround-mode 1))

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "b" 'helm-buffers-list
  "ff" 'helm-recentf
  "w" 'save-buffer
  "gs" 'magit-status
  "qer" 'restart-emacs
  "pf" 'helm-projectile-find-file
  "pp" 'helm-projectile-switch-project
  "m" 'helm-M-x
  "wm" 'maximize-window
  "ev" (lambda() (interactive)(find-file "~/.emacs")))

;; Evil-mode
(evil-collection-init)

;; global keybindings
(global-set-key (kbd "M-x") 'helm-M-x)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (evil-leader evil)))
 '(python-shell-interpreter "python3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
