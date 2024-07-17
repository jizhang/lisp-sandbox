(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable"
	       . "https://stable.melpa.org/packages/"))
(package-initialize)

(push '(fullscreen . maximized) default-frame-alist)
(tool-bar-mode -1)
