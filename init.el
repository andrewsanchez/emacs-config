;; Personal info and constants

(defconst as/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))
(defconst as/elisp (concat (getenv "HOME") "/projects/emacs-config/"))
(add-to-list 'load-path as/elisp)
(setq user-full-name "Andrew Sanchez"
      user-mail-address "inbox.asanchez@gmail.com")

;; Package management

(require 'package)
    (add-to-list 'load-path "packages")
    (setq package-archives
        '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
          ("melpa" . "http://melpa.milkbox.net/packages/")
          ("melpa-stable" . "http://stable.melpa.org/packages/")
          ("elpy" . "https://jorgenschaefer.github.io/packages/")
          ("org" . "http://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (eval-when-compile
    (require 'use-package))
  (setq use-package-always-ensure t)
  (setq use-package-verbose t)
(use-package diminish
  :config
  (diminish 'undo-tree-mode))

;; Tangle init.el
;; [[https://emacs.stackexchange.com/questions/20707/automatically-tangle-org-files-in-a-specific-directory#20733][Automatically tangle]] this file on save.


(defun as/tangle-dotfiles ()
  "If the current file is in 'as/elisp' blocks are tangled"
  (when (equal (file-name-directory (directory-file-name buffer-file-name))
               as/elisp)
    (org-babel-tangle)
    (message "%s tangled" buffer-file-name)))
(add-hook 'after-save-hook #'as/tangle-dotfiles)

;; Helpful

(use-package helpful
  :commands
  (helpful-callable
   helpful-function
   helpful-macro
   helpful-command
   helpful-key
   helpful-variable
   helpful-at-point)
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h o") #'helpful-at-point))

;; Custom set variables

(setq custom-file (expand-file-name "custom.el" as/elisp))
(when (file-exists-p custom-file)
  (load custom-file))

;; General Binding Config
;; Also check out [[https://github.com/nonsequitur/smex][smex]] 

(global-set-key (kbd "C-6") 'evil-switch-to-windows-last-buffer)
(global-set-key (kbd "C-w") 'evil-window-next)
(global-set-key (kbd "C-x f") 'helm-find-files)

  (use-package which-key
      :defer 10
      :config
      (which-key-mode))

;; Toggles

(evil-leader/set-key "tf" 'flyspell-mode)

;; Evil

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (setq evil-leader/in-all-states t)
  (setq evil-leader/non-normal-prefix "C-")
  (evil-leader/set-leader "<SPC>"))

(use-package evil
    :config
    (evil-mode 1))

(use-package evil-nerd-commenter
  :config
  (evil-leader/set-key
    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line
    "."  'evilnc-copy-and-comment-operator
    "\\" 'evilnc-comment-operator))

(use-package evil-surround
    :config
    (global-evil-surround-mode 1))

(use-package evil-lisp-state
    :init (setq evil-lisp-state-global t)
    :config (evil-lisp-state-leader "<SPC> k"))

;; (use-package bind-map) 

(use-package evil-matchit
  :config
  (require 'evil-matchit)
  (global-evil-matchit-mode 1))

;; Exclude list

(add-to-list 'evil-emacs-state-modes 'dired-mode)
(add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)
(add-to-list 'evil-emacs-state-modes 'realgud-short-key-mode)
(evil-set-initial-state 'realgud-short-key-mode 'emacs)
(evil-set-initial-state 'Info-mode 'emacs)

;; Navigation

(use-package avy
  :init
  (defhydra hydra-avy (:color blue)
    "avy-goto"
    ("c" avy-goto-char "char")
    ("C" avy-goto-char-2 "char-2")
    ("w" avy-goto-word-1 "word")
    ("s" avy-goto-subword-1 "subword")
    ("l" avy-goto-line "line")
    ("u" link-hint-open-link "open-URI")
    ("U" link-hint-copy-link "copy-URI"))
  (evil-leader/set-key "j" 'hydra-avy/body)
  :commands hydra-avy/body
  :config
  (use-package link-hint))

;; Misc

(use-package expand-region
  :commands (er/expand-region)
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(evil-leader/set-key
  "!" 'shell-command)

(use-package smart-mode-line
    :init
    (setq sml/no-confirm-load-theme t)
    (sml/setup))
(display-time-mode 1)

(use-package helm
  :init
  (evil-leader/set-key
      "sg" 'helm-google-suggest
      "<SPC>" 'helm-M-x
      "sj" 'helm-semantic-or-imenu
      "ha" 'helm-apropos
      "ho" 'helm-info-org
      "hi" 'helm-info
      "ss" 'helm-occur)
  :commands
  (helm-google-suggest helm-M-x helm-semantic-or-imenu helm-apropos helm-occur)
  :bind (("M-y" . helm-show-kill-ring)
	 ("C-h a" . helm-apropos)
	 ("M-x" . helm-M-x)
	 ("C-x b" . helm-buffers-list))
  :config
  (require 'helm-config))

  (use-package helm-descbinds
    :bind (("C-h b" . helm-descbinds)))

(use-package hydra)

(defhydra hydra-buffers (:color blue)
  "Buffers"
  ("d" kill-this-buffer "kill buffer")
  ("s" (lambda () (interactive) (pop-to-buffer "*scratch*")) "*scratch*")
  ("i" (lambda () (interactive) (pop-to-buffer "*info*")) "*info*")
  ("h" (lambda () (interactive) (pop-to-buffer "*Help*")) "*Help*")
  ("e" (lambda () (interactive) (pop-to-buffer "*eshell*")) "*eshell*")
  ("m" (lambda () (interactive) (pop-to-buffer "*Messages*")) "*Messages*")
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
  ("b" helm-filtered-bookmarks "helm-filtered-bookmarks")
  ("t" neotree-toggle "neotree-toggle")
  ("r" helm-recentf "recentf-open-files"))

;; Applications

;; (defhydra hydra-applications ()
;;   "Applications"
;;   ("d" deft "deft find file"))
(evil-leader/set-key "ad" 'deft)
(evil-leader/set-key "am" 'mu4e)

;; Yasnippet

(use-package yasnippet
  :diminish yas-minor-mode
  :init
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
    ("a" yas-reload-all))
  (evil-leader/set-key "y" 'hydra-yasnippet/body)
  :defer 15
  ; not sure why this doesn't work
  ; :commands hydra-yasnippet/body
  :config
  (require 'yasnippet)
  (yas-global-mode 1))

;; General

(use-package org
  :mode (("\\.org$" . org-mode))
  :commands (org-agenda org-capture)
  :load-path "~/.emacs.d/packages/org-mode/lisp"
  :diminish org-mode
  :init
  (evil-leader/set-key
      "oa" 'org-agenda
      "oc" 'org-capture
      "ot" 'hydra-org-clock/body)
  :config
  (setq as/agenda (concat (getenv "HOME") "/org/agenda/"))
  (setq as/views (concat (getenv "HOME") "/org/views/"))
  (setq as/gtd (concat as/agenda "gtd.org"))

  (load "org-config")
  (add-hook 'org-mode-hook 'toggle-truncate-lines)
  (setq org-hide-leading-stars t)
  (setq org-default-notes-file "/Users/andrew/org/notes.org")
  (setq org-todo-keywords
    '((sequence "TODO" "|" "DONE")))
  (setq org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 2)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-completion-use-ido nil)
  (setq org-refile-use-outline-path t) 
  (defun as/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (setq org-refile-target-verify-function 'as/verify-refile-target)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-src-fontify-natively t))

  (use-package org-pomodoro
    :commands org-pomodoro
    :config
    (setq mindfulness-bell "/Users/andrew/Music/Miscellaneous/Timer_Sounds/mindfullness_bell.mp3") 
    (setq mindfulness-chimes "/Users/andrew/Music/Miscellaneous/Timer_Sounds/chimes.mp3") 
    (setq org-pomodoro-length 15)
    (setq org-pomodoro-short-break-length .5)
    (setq org-pomodoro-start-sound mindfulness-bell)
    (setq org-pomodoro-finished-sound mindfulness-bell)
    (setq org-pomodoro-short-break-sound mindfulness-bell)
    (setq org-pomodoro-long-break-sound mindfulness-chimes)
    (setq org-pomodoro-start-sound-p t))

;; Toolbar
;;    Save space by not showing the toolbar

(tool-bar-mode -1)

;; Golden ratio mode

(use-package golden-ratio
  :commands
  (evil-window-next
   evil-window-right
   evil-window-left
   evil-window-down
   evil-window-up)
  :config
  (golden-ratio-mode 1)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-next)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-right)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-left)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-down)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-up))

(use-package winner
  :commands
  (winner-undo winner-redo)
  :config
  (winner-mode)
  (evil-leader/set-key
    "wu" 'winner-undo
    "wr" 'winner-redo))

(use-package zoom-frm
  :commands hydra-zoom)

(defhydra hydra-zoom (global-map "M-=")
  "zoom"
  ("g" text-scale-increase)
  ("l" text-scale-decrease)
  ("i" zoom-in)
  ("o" zoom-out))

;; Special dir for backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(use-package magit
  :init (evil-leader/set-key "gs" 'magit-status)
  :commands magit-status
  :config
  (setq magit-git-executable "~/usr/bin/git"))
  (setq magit-git-executable "~/usr/bin/git")
  (add-hook 'git-commit-mode-hook 'git-commit-turn-on-flyspell)
  (evil-set-initial-state 'git-commit-mode 'normal))

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
(setq initial-scratch-message "") ;; Uh, I know what Scratch is for

;; My own

(use-package visual-fill-column
  :init
  (global-visual-fill-column-mode)
  (evil-leader/set-key "tl" 'visual-fill-column-mode)
  :config
  (add-hook 'rst-mode-hook 'visual-line-mode))
;; This doesn't actually seem to be faster...
;; (setq tramp-default-method "ssh")
(setq tramp-inline-compress-start-size 1000000)

;; General

(use-package python
    :mode ("\\.py\\'" . python-mode)
    :interpreter ("python" . python-mode)
    :config
    ; org-babel
    (use-package ob-ipython
      :config
      (require 'ob-ipython))
    (org-babel-do-load-languages
     'org-babel-load-languages
     (append org-babel-load-languages
             '((ipython . t)
               (python . t))))
    (add-hook 'before-save-hook 'py-isort-before-save)
    (add-hook 'python-mode-hook 'smartparens-mode)
    (add-hook 'inferior-python-mode-hook 'smartparens-mode)
    (add-hook 'rst-mode-hook 'turn-on-flyspell)
    (setq python-shell-exec-path '("~/anaconda3/bin/python"))

    (defhydra hydra-python (:color blue :hint nil)
    "
    ^Navigation^                  ^Elpy^                 ^Formatting^
    -------------------------------------------------------------------------
    _d_: find definitions      _t_: elpy-test           _y_: yapfify-buffer
    _a_: find assignments      _z_: switch to shell     _i_: py-isort-buffer
    _r_: find references       _c_: send region/buffer  _f_: flycheck
    _s_: show doc              _C_: send statement      _x_: sphinx-doc
    _v_: pyvenv-activate     
    _V_: pyvenv-deactivate
    _w_: venv-workon
    _W_: venv-deactivate
    "
        ("d" elpy-goto-definition)
        ("a" anaconda-mode-find-assignments)
        ("r" xref-find-references)
        ("s" elpy-doc)
        ("y" yapfify-buffer)
        ("v" pyvenv-activate)
        ("V" pyvenv-deactivate)
        ("w" venv-workon)
        ("W" venv-deactivate)
        ("i" py-isort-buffer)
        ("f" hydra-flycheck/body)
        ("x" sphinx-doc)
        ("t" elpy-hydra/body)
        ("z" elpy-shell-switch-to-shell)
        ("c" elpy-shell-send-region-or-buffer)
        ("C" elpy-shell-send-current-statement))
        (evil-leader/set-key-for-mode 'python-mode "m" 'hydra-python/body)
        (evil-leader/set-key-for-mode 'rst-mode "m" 'hydra-python/body)

    (use-package anaconda-mode
      :commands hydra-python/body
      :config
      (anaconda-mode)
      (anaconda-eldoc-mode)
      (add-to-list 'company-backends 'company-anaconda))
    (use-package sphinx-doc :commands (sphinx-doc)
      :config (sphinx-doc-mode))
    (use-package helm-pydoc :commands helm-pydoc)
    (use-package py-isort :commands py-isort-buffer
      :config (require 'py-isort))
    (use-package virtualenvwrapper :commands (venv-workon)
      :config 
      (venv-initialize-interactive-shells)
      (venv-initialize-eshell)
      (setq venv-location "/Users/andrew/miniconda3/envs"))
    (use-package elpy
      :init (with-eval-after-load 'python (elpy-enable))
      :config 
      (elpy-use-ipython)
      (pyvenv-mode)

      (defhydra elpy-hydra (:color red)
        "
        Elpy in venv: %`pyvenv-virtual-env-name
        "
        ("t" (progn (call-interactively 'elpy-test-pytest-runner) (elpy-nav-errors/body)) "pytest" :color blue)
        ("w" (venv-workon) "workon venv…")
        ("q" nil "quit")
        ("Q" (kill-buffer "*compilation*") "quit and kill compilation buffer" :color blue))

      (defhydra elpy-nav-errors (:color red)
        " Navigate errors "
        ("n" next-error "next error")
        ("p" previous-error "previous error")
        ("s" (progn
               (switch-to-buffer-other-window "*compilation*")
               (goto-char (point-max))) "switch to compilation buffer" :color blue)
        ("q" nil "quit")
        ("Q" (kill-buffer "*compilation*") "quit and kill compilation buffer" :color blue))))

;; Fix faulty completion bug
;;    Source:  https://github.com/jorgenschaefer/elpy/issues/887
;;    Fixes this error message:
;;    Warning (python): Your ‘python-shell-interpreter’ doesn’t seem to support readline, yet ‘python-shell-completion-native’ was t and "ipython3" is not part of the ‘python-shell-completion-native-disabled-interpreters’ list. Native completions have been disabled locally.


(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))


(use-package yapfify :commands yapfify-buffer)

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Smartparens

(use-package smartparens
  :defer 5
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (autoload 'smartparens-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)
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
  :init
  (evil-leader/set-key "p" 'helm-projectile)
  :load-path "~/.emacs.d/packages/projectile"
  :commands helm-projectile
  :config
  (evil-leader/set-key "p" 'projectile-command-map)
  (projectile-mode)
  (setq projectile-enable-caching t)
  (use-package helm-projectile
    :config
    (require 'helm-projectile)
    (helm-projectile-on)))

(use-package solarized-theme
  :config
  (evil-leader/set-key "tt" 'toggle-theme))
(load-theme 'solarized-light t)
(setq active-theme 'solarized-light)
(defun toggle-theme ()
  (interactive)
  (if (eq active-theme 'solarized-light)
      (setq active-theme 'solarized-dark)
    (setq active-theme 'solarized-light))
  (load-theme active-theme))
(set-face-attribute 'default t :font 
  "-*-Source Code Pro-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
(set-face-attribute 'default nil :height 140)

(use-package company
  :config
  (global-company-mode))

(use-package helm-wordnet
  :commands helm-wordnet
  :load-path "packages/helm-wordnet"
  :config
  (setq helm-wordnet-prog "/usr/local/bin/wn"))
  (evil-leader/set-key
    "wd" 'helm-wordnet)

(use-package google-translate
  :commands (google-translate-at-point google-translate-smooth-translate)
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
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
    :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
    :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("n"  flycheck-next-error                                       "Next")
    ("p"  flycheck-previous-error                                   "Previous")
    ("gg" flycheck-first-error                                      "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil)))

;; Deft

(use-package deft
  :commands deft
  :config
  (setq deft-directory "~/org")
  (setq deft-extensions '("txt" "org"))
  (setq deft-default-extension "org")
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t))

;; Elfeed

(use-package elfeed
    :commands elfeed
    :config
    (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
    (use-package elfeed-org
      :config
      (require 'elfeed-org)
      (elfeed-org)
      (setq rmh-elfeed-org-files (list "~/org/elfeed.org"))))

(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
   smtpmail-auth-credentials
     '(("smtp.gmail.com" 587 "inbox.asanchez@gmail.com" nil))
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587)
(setq message-kill-buffer-on-exit t)

;; MU4E

(add-to-list 'load-path "/usr/local/Cellar/mu/HEAD-65863e4_1/share/emacs/site-lisp")
(require 'mu4e)
(add-hook 'message-send-hook (lambda () (mu4e-update-mail-and-index t)))
(require 'org-mu4e) ;; store org-mode links to messages
;;store link to message if in header view, not to header query
(setq org-mu4e-link-query-in-headers-mode nil)
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-sent-messages-behavior 'delete)
(setq mu4e-mu-binary "/usr/local/bin/mu")
(setq mu4e-get-mail-command "/usr/local/bin/mbsync -a")
(setq mu4e-maildir (expand-file-name "~/Maildir"))
(setq mu4e-drafts-folder "/drafts") 
(setq mu4e-sent-folder "/sent")
(setq mu4e-trash-folder "/trash")
(setq mu4e-refile-folder "/all")
(setq mu4e-maildir-shortcuts
      '(("/INBOX". ?i)
        ("/sent" . ?s)
        ("/trash" . ?t)
        ("/all" . ?a)
        ("/drafts" . ?d)))
(evil-set-initial-state 'mu4e-view-mode 'motion)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)
(add-hook 'mu4e-compose-mode-hook 'turn-on-flyspell)
(add-hook 'mu4e-compose-mode-hook 'visual-line-mode)
(add-hook 'mu4e-view-mode-hook 'visual-line-mode)

;; Hydra

(defhydra hydra-mail (:color blue)
  "Mail"
  ("M" mu4e "mu4e")
  ("m" mu4e~main-menu "mu4e main menu"))

;; Keyfreq

(use-package keyfreq
  :defer 5
  :config
  (require 'keyfreq)
  (setq keyfreq-excluded-commands
	'(self-insert-command
	  abort-recursive-edit
	  forward-char
	  backward-char
	  previous-line
	  next-line
	  evil-a-WORD
	  evil-append
	  evil-backward-char
	  evil-backward-word-begin
	  evil-change
	  evil-change-line
	  evil-complete-next
	  evil-complete-previous
	  evil-delete
	  evil-delete-backward-char-and-join
	  evil-delete-char
	  evil-delete-line
	  evil-emacs-state
	  evil-end-of-line
	  evil-escape-emacs-state
	  evil-escape-insert-state
	  evil-escape-isearch
	  evil-escape-minibuffer
	  evil-escape-motion-state
	  evil-escape-visual-state))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package restart-emacs
  :init
  (evil-leader/set-key "qr" 'restart-emacs)
  :commands restart-emacs)

(use-package neotree :load-path "~/.emacs.d/packages/neotree"
  :commands  neotree-toggle
  :config
  (require 'neotree)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter))

(use-package realgud
  :commands realgud
  :config
  (load-library "realgud")
  (add-hook 'realgud-short-key-mode-hook
      (lambda ()
        (local-set-key "\C-c" realgud:shortkey-mode-map)))
	(setq realgud:pdb-command-name "python -m pdb"))

(use-package gnugo
  :commands gnugo
  :config
  (add-to-list 'evil-emacs-state-modes 'gnugo-board-mode)
  (add-hook 'gnugo-start-game-hook 'gnugo-image-display-mode)
  (setq gnugo-xpms 'gnugo-imgen-create-xpms))
