(require 'color-theme)

;;
;; Generic settings
;;
(color-theme-zenburn)
(whole-line-or-region-mode 1)

;;
;; Global key bindings
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "M-i") 'ido-goto-symbol)
(global-set-key (kbd "M-o") 'open-previous-line)
(global-set-key (kbd "C-o") 'open-next-line)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/usr/share/emacs/site-lisp/auto-complete/ac-dict")
(ac-config-default)

;;
;; Generic hook customisations
;;
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; remove trailing whitespace
(remove-hook 'coding-hook 'turn-on-hl-line-mode) ;; no highlight line (enabled by starterkit by default)

;;
;; Ruby specific customizations
;;
(require 'ruby-electric)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)

;;
;; C++ specific customizations
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;;
;; Functions
;;
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")
