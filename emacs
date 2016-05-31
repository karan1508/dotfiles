(setq user-full-name "Karan Dewan")
(setq user-mail-address "karandewan@gmail.com")

;;Paths
(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin:/home/abe
dra/.cabal/bin" (getenv "PATH")))
(setenv "GOPATH" (concat (getenv "HOME") "/src/golang"))
(add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))
(require 'cl)

;;====================
;;PACKAGE MANAGEMENT==
;;====================

(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

;;Define default packages
;;This is the list of packages used in this configuration.

(defvar kd/packages '(ac-slime
                      ace-jump-mode
		      anaconda-mode
                      auto-complete
                      autopair
		      company
		      company-anaconda
		      company-c-headers
		      dark-mint-theme
		      feature-mode
                      flycheck
                      gist
                      graphviz-dot-mode
                      magit
                      marmalade
                      o-blog
                      org
                      paredit
                      puppet-mode
                      projectile
                      restclient
                      rvm
                      scala-mode
                      smex
                      sml-mode
                      solarized-theme
                      web-mode
                      writegood-mode
                      )
  "Default packages")

;; Install default packages

(defun kd/packages-installed-p ()
  (loop for pkg in kd/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (kd/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg kd/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)


;;Marking text
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;;Display Settings
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;;tab Width
(setq tab-width 2
      indent-tabs-mode nil)

;;Disable backup files
(setq make-backup-files nil)

;; Yes and No
(defalias 'yes-or-no-p 'y-or-n-p)


(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

(if (not window-system)
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t))
(show-paren-mode t)

;; Color codes

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;; Smex setup
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; IDO mode
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;;Column number mode
(setq column-number-mode t)

;;Autopair
(require 'autopair)
(autopair-global-mode)

;;auto complete
(require 'auto-complete-config)
(ac-config-default)

;;========================
;;Buffer cleanup and stuff
;;=======================

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

(setq-default show-trailing-whitespace t)

;; Fly spelll
(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")

;;Shell script mode
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

;; conf mode
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

(load-theme 'dark-mint t)

;;Switch window
(global-set-key (kbd "C-M-z") 'switch-window)

;;Acejump mode
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;;Setting up doxygen related code wrapping
(when (require 'doxymacs nil 'noerror)
  (add-hook 'prog-mode-hook '(lambda () (doxymacs-mode))))

;;Show line numbers
(global-linum-mode)

;;Projectile mode
(projectile-global-mode)

;;Python anaconda-mode hook
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

;;Org mode
(setq org-log-done t)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

