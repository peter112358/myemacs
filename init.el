(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

(require 'doom-themes)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-one t)
(doom-themes-visual-bell-config)
(doom-themes-neotree-config)
(doom-themes-treemacs-config)
(doom-themes-org-config)

(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-init))

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)

(defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (ffip-project-root))
          (file-name (buffer-file-name)))
      (if project-dir
          (progn
            (neotree-dir project-dir)
            (neotree-find file-name))
        (message "Could not find git project root."))))


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
(global-set-key [f9] 'neotree-projectile-dir)

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

(evil-mode t)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "SPC" 'counsel-M-x
  "fed" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "ff" 'find-file
  "dd" 'neotree-toggle
  "bk" 'kill-buffer
  "gs" 'magit-status
  "bl" 'counsel-ibuffer
  "fs" 'swiper
  "ps" 'counsel-ag 
  "pf" 'counsel-git)


(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
