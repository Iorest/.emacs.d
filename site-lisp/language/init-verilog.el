;; init-verilog.el --- verilog. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; verilog
;;

;;; Code:

(use-package verilog-mode
  :ensure t
  :defer t
  :mode (("\\.[st]*v[hp]*\\'" . verilog-mode) ;.v, .sv, .svh, .tv, .vp
         ("\\.psl\\'"         . verilog-mode)
         ("\\.vams\\'"        . verilog-mode)
         ("\\.vinc\\'"        . verilog-mode)))

(use-package veri-kompass
  :ensure t
  :defer t)

(provide 'init-verilog)
;;; init-verilog.el ends here
