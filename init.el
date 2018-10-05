

;;---------------------------- package ------------------------------------------
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

;;---------------------------- UI ------------------------------------------
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;---------------------------- custom ------------------------------------------
(setq inhibit-startup-screen t)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;---------------------------- theme ------------------------------------------
(require 'doom-themes)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-one t)
(doom-themes-visual-bell-config)
(doom-themes-neotree-config)
(doom-themes-treemacs-config)
(doom-themes-org-config)

;;---------------------------- mode ------------------------------------------
(require 'better-defaults)
(require 'pallet)
(pallet-mode t)

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :ensure)

(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-init))

(use-package indent-guide
      :ensure t
      :hook (after-init . indent-guide-global-mode))

(require 'which-key)
(which-key-mode)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)

(defun neotree-projectile-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
(add-hook 'neotree-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
                (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
                (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
                (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
                (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
                (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))
(setq neo-window-fixed-size nil)

(global-company-mode t)

;; removes line numbers from neotree, still need to remove from home
(global-linum-mode)
(add-hook 'neo-after-create-hook (lambda (_unused) (linum-mode -1)))


;;未选中的窗口不显示光标
(setq-default cursor-in-non-selected-windows nil)

;;美化lambda
(prettify-symbols-mode)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq multi-term-program "/usr/local/bin/zsh")

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(global-anzu-mode +1)

(evil-mode t)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "SPC" 'counsel-M-x
  "fed" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "ff" 'counsel-find-file
  "dd" 'neotree-toggle
  "bk" 'kill-buffer
  "gs" 'magit-status
  "bl" 'counsel-ibuffer
  "qr," 'anzu-query-replace
  "qr." 'anzu-query-replace-at-cursor
  "fs" 'swiper
  "jj" 'dumb-jump-go
  "jb" 'dumb-jump-back
  "po" 'projectile-switch-open-project
  "ps" 'counsel-ag
  "pf" 'counsel-git)


;;---------------------------- keymap ------------------------------------------
(global-set-key [f9] 'neotree-projectile-dir)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-s") 'swiper)


;;---------------------------- custom ------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-window-width 40))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
