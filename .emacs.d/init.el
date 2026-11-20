;;; init.el --- Entry point; ~/.emacs.d/init.el loads this file -*- lexical-binding: t -*-
(org-babel-load-file (expand-file-name "~/rc/.emacs.d/myInit.org"))
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
