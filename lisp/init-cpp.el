;;; init-cpp.el --- Emacs C++ IDE Configuration (Eglot) -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. 智能感知与代码导航 (Eglot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'eglot)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c-or-c++-mode-hook 'eglot-ensure)

(use-package company
  :ensure t
  :hook (prog-mode. company-mode)
  :bind (:map company-active-map
              ("<tab>". company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. 用户体验增强 (UI & Highlighting)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode. modern-c++-font-lock-mode))

(use-package imenu-list
  :ensure t
  :bind ("C-.". imenu-list-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. 集成调试 (DAP)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dap-mode
  :ensure t
  :config
  ;; 必须加载dap-gdb-lldb来启用对LLDB的支持
  (require 'dap-gdb-lldb)
  ;; 启用dap-mode的UI组件，如局部变量、调用栈等窗口
  (dap-ui-mode 1)
  ;; 在光标下显示变量值的悬浮提示
  (dap-tooltip-mode 1)
  ;; (可选) 在程序中断时自动弹出调试控制面板 (hydra)
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))

  ;; 注册一个可复用的C++调试启动模板
  (dap-register-debug-template
   "C++ (LLDB) Launch"
   (list :type "lldb-vscode"
         :request "launch"
         :name "C++ (LLDB) Launch"
         :program "${workspaceFolder}/build/debug/${fileBasenameNoExtension}"
         :cwd "${workspaceFolder}"
         :args
         :stopOnEntry nil)))

;; Highlights the word/symbol at point and any other occurrences in
;; view. Also allows to jump to the next or previous occurrence.
;; https://github.com/nschum/highlight-symbol.el
(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-on-navigation-p t)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

;; Emacs minor mode that highlights numeric literals in source code.
;; https://github.com/Fanael/highlight-numbers
(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; .h files to open in c++-mode rather than c-mode.
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;; code style
;; (setq c-default-style "stroustrup")
;; (setq c-basic-indent 4)
;; (setq c-basic-offset 4)

;; emacs-fu: don’t indent inside of C++ namespaces
;; http://brrian.tumblr.com/post/9018043954/emacs-fu-dont-indent-inside-of-c-namespaces
(c-set-offset 'innamespace 0)

(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (add-hook 'after-init-hook (lambda () (setq ivy-height (/ (window-height) 2))))
  )

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  )

(use-package treemacs :ensure t)
(use-package treemacs-projectile :ensure t)


(provide 'init-cpp)
;;; init-cpp.el ends here
