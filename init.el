(package-initialize)
(setq package-archives
    '(("gnu" . "http://elpa.gnu.org/packages/")
      ("marmalade" . "http://marmalade-repo.org/packages/")
      ("melpa" . "http://melpa.milkbox.net/packages/")
      ("melpa-stable" . "http://stable.melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(evil-leader/set-key
  "!" 'shell-command)

(setq user-full-name "Andrew Sanchez"
      user-mail-address "inbox.asanchez@gmail.com")

(evil-leader/set-key
  "bd" 'kill-this-buffer
  "bs" (lambda ()
	 (interactive)
	 (pop-to-buffer "*scratch*"))
  "fd" (lambda ()
	   (interactive)
	   (find-file "~/projects/emacs-config/init.org")))

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

  (use-package evil
    :config
    (evil-mode 1))

  (use-package evil-nerd-commenter
      :config
      (evilnc-default-hotkeys))

  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))

(add-to-list 'evil-emacs-state-modes 'dired-mode)

(use-package helm
    :init
    (require 'helm-config)
    (evil-leader/set-key
      "ff" 'helm-find-files
      "fm" 'helm-multi-files
      "fb" 'helm-filtered-bookmarks
      "bb" 'helm-buffers-list
      "sg" 'helm-google-suggest
      "<SPC>" 'helm-M-x
      "sj" 'helm-semantic-or-imenu
      "ha" 'helm-apropos
      "ss" 'helm-occur)
    :bind (("M-y" . helm-show-kill-ring)
           ("C-h a" . helm-apropos)
           ("M-x" . helm-M-x)
           ("C-x b" . helm-buffers-list)))

(use-package helm-descbinds
  :bind (("C-h b" . helm-descbinds)))

(use-package hydra)

(use-package org
  :load-path "~/.emacs.d/packages/org-mode/lisp"
  :config
  (setq org-hide-leading-stars t)
  (setq org-default-notes-file "/Users/andrew/org/notes.org")
  (setq org-capture-templates
	'(
	  ("t" "TODO" entry (file+headline "/Users/andrew/org/gtd.org" "Tasks")
	  "* TODO %? \n%U\n" :empty-lines 1)
	  ("j" "Journal" entry (file+datetree "/Users/andrew/org/journal.org")
	  "* %?\nEntered on %U\n")
	  ("n" "Note" entry (file+headline "/Users/andrew/org/notes.org" "Notes")
	  "* %i\n")))
  (setq org-refile-targets '((nil :maxlevel . 5)
			     (org-agenda-files :maxlevel . 4)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-src-fontify-natively t)
  (setq org-agenda-files '("/Users/andrew/org/gtd.org"))
  (setq org-agenda-custom-commands
	'(("w" "work" tags-todo "TODO=\"TODO\"+category=\"work\"")))

  (use-package org-pomodoro
    :config
    (setq org-pomodoro-length 30)
    (setq org-pomodoro-start-sound "/Users/andrew/Music/Miscellaneous/Timer_Sounds/mindfullness_bell.mp3")
    (setq org-pomodoro-finish-sound "/Users/andrew/Music/Miscellaneous/Timer_Sounds/mindfullness_bell.mp3")
    (setq org-pomodoro-start-sound-p t))
  ;; Hydras
  (evil-leader/set-key-for-mode 'org-mode
    "h" 'hydra-org-headings/body))

 (evil-leader/set-key
     "oa" 'org-agenda
     "oc" 'org-capture
     "ot" 'hydra-org-clock/body)

(defhydra hydra-org-clock (:color blue :hint nil)
    "

    Clock   In/out^     ^Edit^   ^Summary     (_?_)
    -----------------------------------------
	    _i_n         _e_dit   _g_oto entry
	    _c_ontinue   _q_uit   _d_isplay
	    _o_ut        ^ ^      _r_eport
	    _p_omodoro
    "
    ("i" org-clock-in)
    ("o" org-clock-out)
    ("c" org-clock-in-last)
    ("e" org-clock-modify-effort-estimate)
    ("q" org-clock-cancel)
    ("p" org-pomodoro)
    ("g" org-clock-goto)
    ("d" org-clock-display)
    ("r" org-clock-report)
    ("?" (org-info "Clocking commands")))

(defhydra hydra-org-headings ()
"Headings"
    ("t" org-todo "org-todo")
    (":" org-set-tags-command "org-set-tags-command")
    ("n" org-narrow-to-subtree "org-narrow-to-subtree")
    ("w" widen "widen")
    ("l" org-demote-subtree "org-demote-subtree")
    ("h" org-promote-subtree "org-promote-subtree")
    ("K" org-backward-heading-same-level "org-backward-heading-same-level")
    ("J" org-forward-heading-same-level "org-forward-heading-same-level")
    ("k" outline-previous-visible-heading "outline-previous-visible-heading")
    ("j" outline-next-visible-heading "outline-next-visible-heading"))

(tool-bar-mode -1)

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

(use-package golden-ratio
  :config
  (golden-ratio-mode 1)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-next)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-right)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-left)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-down)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-up))

(use-package winner
  :config
  (winner-mode 1)
  (evil-leader/set-key
    "wu" 'winner-undo
    "wr" 'winner-redo))

(use-package zoom-frm)
(defhydra hydra-zoom (global-map "C-=")
  "zoom"
  ("g" text-scale-increase)
  ("l" text-scale-decrease)
  ("i" zoom-in)
  ("o" zoom-out))

;; Special dir for backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(use-package magit
  :config
  (evil-leader/set-key
    "gs" 'magit-status))

(show-paren-mode 1)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)
(fset 'yes-or-no-p 'y-or-n-p)

(use-package which-key
    :config
    (which-key-mode))

(use-package python
  :config
  (setq python-shell-exec-path '("~/anaconda3/bin/python")))

(use-package anaconda-mode)
(add-hook 'python-mode-hook
	  'anaconda-mode
	  'anaconda-eldoc-mode)

(use-package smartparens
    :init
    (require 'smartparens-config)
    :config
    (autoload 'smartparens-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'smartparens-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)
    (add-hook 'ielm-mode-hook             #'smartparens-mode)
    (add-hook 'lisp-mode-hook             #'smartparens-mode)
    (add-hook 'lisp-interaction-mode-hook #'smartparens-mode)
    (add-hook 'scheme-mode-hook           #'smartparens-mode))

(use-package projectile
  :init
  (projectile-mode)
  :config
  (evil-leader/set-key
  "p" 'projectile-command-map))

(use-package helm-projectile
  :config
  (require 'helm-projectile)
  (helm-projectile-on))

(use-package yasnippet :load-path "/Users/andrew/projects/emacs/packages/yasnippet"
  :config
  (require 'yasnippet)
  (yas-global-mode 1))

(use-package solarized-theme)
(load-theme 'solarized-dark t)
(set-face-attribute 'default t :font 
  "-*-Source Code Pro-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")

(use-package company)
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))
