(custom-set-variables
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

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

(setq user-full-name "Andrew Sanchez"
      user-mail-address "inbox.asanchez@gmail.com")

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

(use-package evil-lisp-state
    :init (setq evil-lisp-state-global t)
    :config (evil-lisp-state-leader "<SPC> k"))

  (use-package bind-map)

(add-to-list 'evil-emacs-state-modes 'dired-mode)
(add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)
(evil-set-initial-state 'Info-mode 'emacs)

(evil-leader/set-key
  "!" 'shell-command)

(use-package smart-mode-line
    :init
    (setq sml/no-confirm-load-theme t)
    :config
    (sml/setup))
(display-time-mode 1)

(use-package helm
    :init
    (require 'helm-config)
    (evil-leader/set-key
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

(defhydra hydra-buffers (:color blue)
  "Buffers"
  ("d" kill-this-buffer "kill buffer")
  ("s" (lambda ()
	 (interactive)
	 (pop-to-buffer "*scratch*"))
   "scratch")
  ("b" helm-buffers-list "helm buffers list"))

(evil-leader/set-key "b" 'hydra-buffers/body)

(evil-leader/set-key "f" 'hydra-files/body)

(defhydra hydra-files (:color blue)
  "Files"

  ("d" (lambda () (interactive)
	 (find-file "~/projects/emacs-config/init.org"))
   "dot-file")

  ("g" (lambda () (interactive)
	(find-file "~/org/agenda/gtd.org"))
   "gtd")
  ("f" helm-find-files "helm-find-files")
  ("m" helm-multi-files "helm-multi-files")
  ("b" helm-filtered-bookmarks "helm-filtered-bookmarks"))

(defhydra hydra-applications ()
  "Applications"
  ("d" deft "deft find file"))
(evil-leader/set-key "a" 'hydra-applications/body)

(use-package org
  :load-path "~/.emacs.d/packages/org-mode/lisp"
  :init
  (evil-leader/set-key
      "oa" 'org-agenda
      "oc" 'org-capture
      "ot" 'hydra-org-clock/body)
  :config
  (setq org-hide-leading-stars t)
  (setq org-default-notes-file "/Users/andrew/org/notes.org")
  (setq org-todo-keywords
    '((sequence "TODO" "NEXT" "|" "DONE")))
  (setq org-capture-templates
	'(("t" "TODO" entry (file+headline "/Users/andrew/org/gtd.org" "Tasks")
	  "* TODO %? \n%U\n" :empty-lines 1)
	  ("j" "Journal" entry (file+datetree "/Users/andrew/org/journal.org")
	  "* %?\nEntered on %U\n")
	  ("n" "Note" entry (file+headline "/Users/andrew/org/notes.org" "Notes")
	  "* %i\n")))
  (setq org-refile-targets '((nil :maxlevel . 3)
			     (org-agenda-files :maxlevel . 3)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-src-fontify-natively t)
  (setq org-agenda-files '("/Users/andrew/org/gtd.org"))
  (setq org-agenda-custom-commands
	'(("w" "work" tags-todo "TODO=\"TODO\"+category=\"pmi\"")))
  (evil-leader/set-key-for-mode 'org-mode
    "h" 'hydra-org-headings/body)
  ;; Hydras
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
      ("?" (org-info "Clocking commands"))))

  (use-package org-pomodoro
    :config
    (setq org-pomodoro-length 30)
    (setq org-pomodoro-start-sound "/Users/andrew/Music/Miscellaneous/Timer_Sounds/mindfullness_bell.mp3")
    (setq org-pomodoro-finish-sound "/Users/andrew/Music/Miscellaneous/Timer_Sounds/mindfullness_bell.mp3")
    (setq org-pomodoro-start-sound-p t))

(tool-bar-mode -1)

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
    (setq python-shell-exec-path '("~/anaconda3/bin/python"))
    (evil-leader/set-key-for-mode 'python-mode
      "a" 'hydra-anaconda/body)
  (defhydra hydra-anaconda (:color blue :hint nil)
  "
  ^Anaconda^
  ----------
  _d_: find definitions
  _a_: find assignments
  _r_: find references
  _b_: go back
  _s_: show doc
  "
      ("d" anaconda-mode-find-definitions)
      ("a" anaconda-mode-find-assignments)
      ("r" anaconda-mode-find-references)
      ("b" anaconda-mode-go-back)
      ("s" anaconda-mode-show-doc)
      ("y" yapfify-buffer)))

(use-package yapfify)
(use-package anaconda-mode)
(add-hook 'python-mode-hook
	'anaconda-mode
	'anaconda-eldoc-mode)

(defun python-shell-completion-native-try ()
  "Return non-nil if can trigger native completion."
  (let ((python-shell-completion-native-enable t)
        (python-shell-completion-native-output-timeout
          python-shell-completion-native-try-output-timeout))
     (python-shell-completion-native-get-completions
      (get-buffer-process (current-buffer))
      nil "_")))

(use-package smartparens
    :config
    ;; (evil-leader/set-key
    ;; 	"k" 'hydra-smartparens/body)

    (autoload 'smartparens-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'smartparens-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)
    (add-hook 'ielm-mode-hook             #'smartparens-mode)
    (add-hook 'lisp-mode-hook             #'smartparens-mode)
    (add-hook 'lisp-interaction-mode-hook #'smartparens-mode)
    (add-hook 'scheme-mode-hook           #'smartparens-mode)
    (add-hook 'python-mode-hook           #'smartparens-mode)
  :init
  (require 'smartparens-config)
  (defhydra hydra-smartparens (:hint nil)
    "
Sexps (quit with _q_)

^Nav^            ^Barf/Slurp^                 ^Depth^
^---^------------^----------^-----------------^-----^-----------------
_f_: forward     _<left>_:    slurp forward   _R_:      splice
_b_: backward    _<right>_:   barf forward    _r_:      raise
_u_: backward ↑  _C-<left>_:  slurp backward  _<up>_:   raise backward
_d_: forward ↓   _C-<right>_: barf backward   _<down>_: raise forward
_p_: backward ↓
_n_: forward ↑

^Kill^           ^Misc^                       ^Wrap^
^----^-----------^----^-----------------------^----^------------------
_w_: copy        _j_: join                    _(_: wrap with ( )
_k_: kill        _s_: split                   _{_: wrap with { }
^^               _t_: transpose               _'_: wrap with ' '
^^               _c_: convolute               _\"_: wrap with \" \"
^^               _i_: indent defun"
    ("q" nil)
    ;; Wrapping
    ("(" (lambda (a) (interactive "P") (sp-wrap-with-pair "(")))
    ("{" (lambda (a) (interactive "P") (sp-wrap-with-pair "{")))
    ("'" (lambda (a) (interactive "P") (sp-wrap-with-pair "'")))
    ("\"" (lambda (a) (interactive "P") (sp-wrap-with-pair "\"")))
    ;; Navigation
    ("f" sp-forward-sexp )
    ("b" sp-backward-sexp)
    ("u" sp-backward-up-sexp)
    ("d" sp-down-sexp)
    ("p" sp-backward-down-sexp)
    ("n" sp-up-sexp)
    ;; Kill/copy
    ("w" sp-copy-sexp)
    ("k" sp-kill-sexp)
    ;; Misc
    ("t" sp-transpose-sexp)
    ("j" sp-join-sexp)
    ("s" sp-split-sexp)
    ("c" sp-convolute-sexp)
    ("i" sp-indent-defun)
    ;; Depth changing
    ("R" sp-splice-sexp)
    ("r" sp-splice-sexp-killing-around)
    ("<up>" sp-splice-sexp-killing-backward)
    ("<down>" sp-splice-sexp-killing-forward)
    ;; Barfing/slurping
    ("<right>" sp-forward-slurp-sexp)
    ("<left>" sp-forward-barf-sexp)
    ("C-<left>" sp-backward-barf-sexp)
    ("C-<right>" sp-backward-slurp-sexp)))

(use-package projectile
  :config
  (projectile-mode)
  (evil-leader/set-key
  "p" 'projectile-command-map))

(use-package helm-projectile
  :config
  (require 'helm-projectile)
  (helm-projectile-on))

(use-package yasnippet
  :load-path "~/.emacs.d/packages/yasnippet"
  :config
  (require 'yasnippet)
  (yas-global-mode 1)
  (evil-leader/set-key
    "y" 'hydra-yasnippet/body)

(defhydra hydra-yasnippet (:color blue :hint nil)
  "
	      ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
	 _a_ll
"
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("g" yas/global-mode)
  ("m" yas/minor-mode)
  ("a" yas-reload-all)))

(use-package solarized-theme)
(load-theme 'solarized-dark t)
(set-face-attribute 'default t :font 
  "-*-Source Code Pro-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
(set-face-attribute 'default nil :height 140)

(use-package company
  :config
  (global-company-mode)
  (add-to-list 'company-backends 'company-anaconda))

(use-package helm-wordnet
  :load-path "packages/helm-wordnet"
  :config
  (setq helm-wordnet-prog "/usr/local/bin/wn"))
  (evil-leader/set-key
    "wd" 'helm-wordnet)

(use-package google-translate
  :config
  (setq google-translate-default-source-language "nl")
  (setq google-translate-default-target-language "en")
  (evil-leader/set-key
    "wp" 'google-translate-at-point
    "ww" 'google-translate-smooth-translate))

(use-package linum-relative
    :config
    (linum-relative-global-mode))
(setq column-number-mode t)

(use-package flycheck
  :init (global-flycheck-mode))


