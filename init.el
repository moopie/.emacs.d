(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

(if (equal system-type 'windows-nt)
    (progn (setq explicit-shell-file-name
                 "C:/Program Files (x86)/Git/bin/sh.exe")
           (setq shell-file-name explicit-shell-file-name)
           (setq explicit-sh.exe-args '("--login" "-i"))
           (setenv "SHELL" shell-file-name)
           (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

(package-initialize)

(setq package-list
      '(
        ;; themes
        base16-theme
        gruvbox-theme

        ;; Evil
        evil
        evil-leader

        ;; Git
        magit
        git-gutter
        git-timemachine

        ;; Helm
        helm
        helm-ag
        helm-projectile
        helm-swoop

        ;; languages

        ;; web stuff
        emmet-mode
        web-mode
        ac-html-bootstrap
        ac-html-csswatcher

        ;; editor
        lsp-mode
        dap-mode
        treesit-auto

        ;; languages
        csharp-mode

        ;; Utils
        anzu
        buffer-move
        highlight-symbol
        multi-term
        smex
        ))

(unless package-archive-contents
  (condition-case err
      (package-refresh-contents)
    (error
     (message "Skipping package refresh: %s" (error-message-string err)))))

(dolist (package package-list)
  (unless (package-installed-p package)
    (condition-case err
        (package-install package)
      (error
       (message "Skipping package %s: %s" package (error-message-string err))))))

(add-to-list 'load-path
             (expand-file-name "themes" user-emacs-directory))

(defvar my-frame-geometry-file
  (expand-file-name "frame-geometry.el" user-emacs-directory))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq ring-bell-function 'ignore)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(setq inhibit-startup-message t)

(setq magit-last-seen-setup-instructions "1.4.0")

(defun my-available-font ()
  (seq-find
   (lambda (font)
     (find-font (font-spec :name font)))
   (if (eq system-type 'windows-nt)
       '("Consolas" "Cascadia Mono" "Courier New")
     '("Source Code Pro" "Iosevka" "JetBrains Mono" "Fira Code" "Menlo" "Monaco" "Monospace"))))

(defun my-save-frame-geometry ()
  (when (display-graphic-p)
    (with-temp-file my-frame-geometry-file
      (prin1
       `((top . ,(frame-parameter nil 'top))
         (left . ,(frame-parameter nil 'left))
         (width . ,(frame-width))
         (height . ,(frame-height)))
       (current-buffer)))))

(defun my-restore-frame-geometry ()
  (when (and (display-graphic-p)
             (file-exists-p my-frame-geometry-file))
    (with-temp-buffer
      (insert-file-contents my-frame-geometry-file)
      (let ((params (read (current-buffer))))
        (when params
          (modify-frame-parameters nil params))))))

(if (display-graphic-p)
    (let ((font (my-available-font)))
      (when font
        (set-face-attribute 'default nil
                            :height 120
                            :font font))))

(progn
  (ido-mode t)
  (setq ido-enable-flex-matching t)

  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "C-x C-b") 'ibuffer)

  (show-paren-mode 1)
  (setq-default indent-tabs-mode nil)

  (global-git-gutter-mode +1)

  (setq x-select-enable-clipboard t
        x-select-enable-primary t
        save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        require-final-newline t
        visible-bell t
        load-prefer-newer t
        ediff-window-setup-function 'ediff-setup-windows-plain
        save-place-file (concat user-emacs-directory "places")
        backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups")))))
(defun my-emacs-theme ()
  (if (member 'gruvbox-dark-medium (custom-available-themes))
      (load-theme 'gruvbox-dark-medium t)
    (load-theme 'base16-default-dark t)))

(defun my-hilight-symbol-hook ()
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace))

(defun my-anzu-mode ()
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp))

(defun my-buffer-move ()
  (windmove-default-keybindings)
  (global-set-key (kbd "C-h")         'buf-move-left)
  (global-set-key (kbd "C-l")         'buf-move-right)
  (global-set-key (kbd "C-j")         'buf-move-down)
  (global-set-key (kbd "C-k")         'buf-move-up))

(defun my-smex-mode ()
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(defun my-evil-conf ()
  (setq evil-disable-insert-state-bindings t)
  (evil-mode 1)
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (add-hook 'neotree-mode-hook
            (lambda ()
                (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))
(defun my-web-mode ()
  (when (require 'web-mode nil t)
    (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (setq web-mode-content-alist
          '(("xml" . "*\\.config\\'")))))

(defun my-treesit-mode ()
  (when (require 'treesit-auto nil t)
    (setq treesit-auto-install 'prompt)
    (global-treesit-auto-mode)
    (setq major-mode-remap-alist
          '((javascript-mode . js-ts-mode)
            (js-mode . js-ts-mode)
            (typescript-mode . typescript-ts-mode)
            (tsx-mode . tsx-ts-mode)
            (json-mode . json-ts-mode)
            (css-mode . css-ts-mode)
            (csharp-mode . csharp-ts-mode)))))

(defun my-lsp-mode ()
  (when (require 'lsp-mode nil t)
    (setq lsp-keymap-prefix "C-c l"
          lsp-completion-provider :none
          lsp-prefer-flymake t
          lsp-enable-snippet t
          read-process-output-max (* 1024 1024))
    (setq-default tab-always-indent 'complete)
    (dolist (hook '(js-ts-mode-hook
                    typescript-ts-mode-hook
                    tsx-ts-mode-hook
                    csharp-ts-mode-hook))
      (add-hook hook #'lsp-deferred))))

(defun my-dap-mode ()
  (when (require 'dap-mode nil t)
    (dap-auto-configure-mode 1)
    (require 'dap-node nil t)
    (require 'dap-netcore nil t)))

(defun my-after-init-hook ()
  (my-web-mode)
  (my-treesit-mode)
  (my-lsp-mode)
  (my-dap-mode)
  (my-emacs-theme)
  (projectile-global-mode)
  (my-hilight-symbol-hook)
  (my-anzu-mode)
  (my-buffer-move)
  (my-smex-mode)
  (winner-mode 1)
  (eldoc-mode)
  (my-evil-conf))

(setq projectile-require-project-root nil
      projectile-enable-caching t)

(add-hook 'after-init-hook 'my-after-init-hook)
(add-hook 'after-init-hook 'my-restore-frame-geometry)
(add-hook 'kill-emacs-hook 'my-save-frame-geometry)

(require 'server)
(unless (server-running-p) (server-start))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
