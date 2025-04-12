;;; Package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package tempel
  :bind
  (("M-+" . tempel-complete)
   (:map tempel-map
	 ("TAB" . tempel-next)
	 ("[tab]" . tempel-next)
	 ("S-TAB" . tempel-prev)
	 ("[backtab]" . tempel-prev))))

;;; setup-snippets.el ends here
