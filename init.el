
;;---------------------------- package ------------------------------------------
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

;;---------------------------- UI ------------------------------------------
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode)
(global-linum-mode)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;---------------------------- custom ------------------------------------------
(setq inhibit-startup-screen t)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;;未选中的窗口不显示光标
(setq-default cursor-in-non-selected-windows nil)
;;美化lambda
(global-prettify-symbols-mode t)
(setq initial-major-mode (quote text-mode))

;;auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;;---------------------------- theme ------------------------------------------
(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
 )

;;---------------------------- mode ------------------------------------------
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(add-hook 'shell-mode-hook (lambda () (company-mode -1)))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(add-hook 'java-mode-hook
          (lambda () (setq-local tab-width 2)))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  ;;(setq dashboard-startup-banner [VALUE]) ;;"path/to/your/image.png"
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (dashboard-setup-startup-hook))


(use-package sublimity
  :config
  (sublimity-mode 1)
  )

(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-move-cursor-after-expanding nil)
  (setq emmet-expand-jsx-className? t)
  )


(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package auctex-latexmk
  :config
  (auctex-latexmk-setup))

(use-package company-auctex
  :config
  (company-auctex-init))

(use-package better-defaults)

(use-package pallet
  :config
  (pallet-mode t))

(use-package dumb-jump
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :ensure t)

(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-init))

(use-package indent-guide
      :ensure t
      :hook (after-init . indent-guide-global-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package neotree
  :init
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-window-fixed-size nil)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :config
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
            (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle))))

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


;; removes line numbers from neotree, still need to remove from home
(add-hook 'neo-after-create-hook (lambda (_unused) (linum-mode -1)))

(global-company-mode t)

(use-package multi-term
  :init
  (setq multi-term-program "/usr/local/bin/zsh"))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package anzu
  :config
  (global-anzu-mode +1))

(use-package dired-rainbow
  :ensure t)

(use-package meghanada
  :ensure t
  :config
  (add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 2)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
  (cond
   ((eq system-type 'windows-nt)
    (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
    (setq meghanada-maven-path "mvn.cmd"))
   (t
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn"))))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package symon
  :ensure t
  :config
  (symon-mode))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(defun new-empty-buffer ()
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf))


(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "SPC" 'counsel-M-x
    "fed" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
    "ff" 'counsel-find-file
    "fn" 'new-empty-buffer
    "bk" 'kill-current-buffer
    "gs" 'magit-status
    "bl" 'counsel-ibuffer
    "qr," 'anzu-query-replace
    "qr." 'anzu-query-replace-at-cursor
    "fs" 'swiper
    "jj" 'dumb-jump-go
    "jb" 'dumb-jump-back
    "po" 'projectile-switch-open-project
    "ps" 'counsel-ag
    "pd" 'neotree-projectile-dir
    "pf" 'counsel-git))

;;---------------------------- keymap ------------------------------------------
(global-set-key [f8] 'neotree-projectile-dir)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-s") 'swiper)


;;---------------------------- custom ------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-window-width 32)
 '(company-idle-delay 0.01))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; deamon
(server-start)
