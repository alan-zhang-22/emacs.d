;;; init-julia.el --- Support for the Julia language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package vterm
  :ensure t)
(use-package julia-mode
  :ensure t)
(use-package julia-vterm
  :ensure t
  :hook (julia-mode . julia-vterm-mode))

(use-package ob-julia-vterm
  :ensure t)

;;; to enable julia-vterm code block evaluation by adding the following lines
(require 'org)
(add-to-list 'org-babel-load-languages '(julia-vterm . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

;;; To use julia as the language name, define the following aliases.
(defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
(defalias 'org-babel-variable-assignments:julia 'org-babel-variable-assignments:julia-vterm)

(provide 'init-julia)
;;; init-julia.el ends here
