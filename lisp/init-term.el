;;; init-term.el --- Terminal configuration for Emacs with vterm -*- lexical-binding: t -*-

;; 确保包管理源已设置
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless package--initialized (package-initialize))

;; 确保 use-package 可用
(unless (package-installed-p 'use-package)
  (condition-case err
      (progn
        (package-refresh-contents)
        (package-install 'use-package))
    (error (message "Failed to install use-package: %s" err))))
(require 'use-package)

;; 配置 vterm
(use-package vterm
  :ensure t
  :bind (("C-c v" . my/vterm-below)) ;; 绑定 C-c v 到自定义函数
  :config
  (setq vterm-shell "/bin/zsh") ;; 使用 macOS 默认的 zsh
  (setq vterm-max-scrollback 10000) ;; 设置最大回滚行数
  (setq vterm-buffer-name-string "vterm-%s") ;; 缓冲区命名格式
  ;; 优化显示
  (setq vterm-timer-delay 0.01) ;; 减少刷新延迟
  (add-hook 'vterm-mode-hook
            (lambda ()
              (set (make-local-variable 'buffer-face-mode-face) '(:family "Menlo" :height 120))
              (buffer-face-mode t))) ;; 使用 Menlo 字体，调整大小
  ;; 定义 my/vterm-below 函数，确保 vterm 加载后可用
  (with-eval-after-load 'vterm
    (defun my/vterm-below ()
      "Open or switch to vterm in a window below, occupying 20% of the frame height."
      (interactive)
      (let* ((frame-height (frame-height))           ;; 获取当前帧的总行数
             (desired-height (round (* frame-height 0.2))) ;; 计算 20% 高度
             (current-window (selected-window))      ;; 保存当前窗口
             (vterm-buffer (get-buffer-create "vterm"))) ;; 获取或创建 vterm 缓冲区
        ;; 如果 vterm 缓冲区未运行进程，则启动
        (unless (get-buffer-process vterm-buffer)
          (with-current-buffer vterm-buffer
            (vterm-mode)))
        ;; 分割窗口并显示 vterm
        (let ((new-window (split-window-below (- frame-height desired-height))))
          (set-window-buffer new-window vterm-buffer)
          (set-window-text-height new-window desired-height)
          (select-window new-window))))))

(provide 'init-term)
;;; init-term.el ends here
