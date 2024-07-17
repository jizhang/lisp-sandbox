(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable"
	       . "https://stable.melpa.org/packages/"))
(package-initialize)

(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(tool-bar-mode -1)
