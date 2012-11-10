
(dolist (package '(python-mode ipython))
  (unless (package-installed-p package)
    (package-install package)))

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(when (executable-find "ipython")
  (require 'ipython)
  (setq org-babel-python-mode 'python-mode))

;;(require 'cython-mode)
;;(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
;;(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
;;(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))
