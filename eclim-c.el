;; eclim-c.el --- an interface to the Eclipse IDE.
;;
;; Copyright (C) 2009  Yves Senn <yves senn * gmx ch>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Contributors
;;
;; - Tassilo Horn <tassilo@member.fsf.org>
;;
;;; Conventions
;;
;; Conventions used in this file: Name internal variables and functions
;; "eclim--<descriptive-name>", and name eclim command invocations
;; "eclim/command-name", like eclim/project-list.

;;* Eclim C

(require 'json)
(require 'cl)

(setq eclim-c--debug nil)

(defun eclim-c--debug-message (x)
  (if eclim-c--debug
      (message (format "Eclim C debug: %s" x)))
  x)

(define-key eclim-mode-map (kbd "C-c C-c s") 'eclim-c-method-signature-at-point)
(define-key eclim-mode-map [f3] 'eclim-c-find-declaration)
(define-key eclim-mode-map [(meta ?.)] 'eclim-c-find-declaration)
(define-key eclim-mode-map [(control ?x) ?4 f3] 'eclim-c-find-declaration-other-window)
(define-key eclim-mode-map [(control x) ?4 ?.] 'eclim-c-find-declaration-other-window)
(define-key eclim-mode-map [(shift f3)] 'eclim-c-find-references)
(define-key eclim-mode-map [(control f3)] 'eclim-c-call-hierarchy)
(define-key eclim-mode-map (kbd "s-SPC") 'eclim-complete)
(define-key eclim-mode-map (kbd "C-c C-c f t") 'eclim-c-find-type)
(define-key eclim-mode-map (kbd "C-c C-c f f") 'eclim-c-find-generic)
(define-key eclim-mode-map (kbd "C-c C-c d") 'eclim-c-doc-comment)
(define-key eclim-mode-map (kbd "C-c C-c f s") 'eclim-c-format)
(define-key eclim-mode-map (kbd "C-c C-c g") 'eclim-c-generate-getter-and-setter)

(defvar eclim-c-show-documentation-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "<tab>") 'forward-button)
    (define-key map (kbd "S-<tab>") 'backward-button)
    (define-key map (kbd "q") 'eclim-quit-window)
    map))


(defgroup eclim-c nil
  "C: editing, browsing, refactoring"
  :group 'eclim)

(defcustom eclim-c-major-modes '(c-mode jde-mode)
  "This variable contains a list of major modes to edit c
files. There are certain operations, that eclim will only perform when
the current buffer is contained within this list"
  :group 'eclim-c
  :type 'list)

;; Could this value be taken from Eclipse somehow?"
(defcustom eclim-c-documentation-root nil
  "Root directory of C HTML documentation.

If Android is used then Eclipse may refer standard C elements from the copy of
C documentation under Android docs, so don't forget to set
`eclim-c-android-documentation-root' too in that case."
  :group 'eclim-c
  :type 'directory)

;; Could this value be taken from Eclipse somehow?"
(defcustom eclim-c-android-documentation-root nil
  "Root directory of Android HTML documentation."
  :group 'eclim-c
  :type 'directory)


(defvar eclim--c-search-types '("all"
                                   "annotation"
                                   "class"
                                   "classOrEnum"
                                   "classOrInterface"
                                   "constructor"
                                   "enum"
                                   "field"
                                   "interface"
                                   "method"
                                   "package"
                                   "type"))

(defvar eclim--c-search-scopes '("all"
                                    "project"
                                    "type"))

(defvar eclim--c-search-contexts '("all"
                                      "declarations"
                                      "implementors"
                                      "references"))

(defvar eclim--is-completing nil)

(defun eclim/c-src-update (&optional save-others)
  "If `eclim-auto-save' is non-nil, save the current c
buffer. In addition, if `save-others' is non-nil, also save any
other unsaved buffer. Finally, tell eclim to update its c
sources."
  (when eclim-auto-save
    (when (buffer-modified-p) (save-buffer)) ;; auto-save current buffer, prompt on saving others
    (when save-others (save-some-buffers nil (lambda () (string-match "\\.c$" (buffer-file-name)))))))

(defadvice delete-file (around eclim--delete-file activate)
  "Advice the `delete-file' function to trigger a source update
in eclim when appropriate."
  (let ((pr nil)
        (fn nil))
    (ignore-errors
      (and (setq pr (eclim--project-name filename))
           (setq fn (file-relative-name filename (eclim--project-dir filename)))))
    ad-do-it
    (when (and pr fn)
      (ignore-errors (apply 'eclim--call-process (list "c_src_update" "-p" pr "-f" fn))))))

(defun eclim--c-parser-read (str)
  (first
   (read-from-string
    (format "(%s)"
            (replace-regexp-in-string
             "[<>(),?]"
             (lambda (m) (assoc-default m '(("<" . "((") (">" . "))")
                                            ("(" . "((") (")" ."))")
                                            ("," . ")(")
                                            ("?" . "\\\\?"))))
             str)))))

(defun eclim--c-parse-method-signature (signature)
  (flet ((parser3/parse-arg (arg)
                            (let ((arg-rev (reverse arg)))
                              (cond ((null arg) nil)
                                    ((= (length arg) 1) (list (list :type (first arg))))
                                    ((listp (first arg-rev)) (list (cons :type arg)))
                                    (t (list (cons :name (first arg-rev)) (cons :type (reverse (rest arg-rev)))))))))
    (let ((ast (reverse (eclim--c-parser-read signature))))
      (list (cons :arglist (mapcar #'parser3/parse-arg (first ast)))
            (cons :name (second ast))
            (cons :return (reverse (rest (rest ast))))))))

(defun eclim--c-current-type-name (&optional type)
  "Searches backward in the current buffer until a type
declaration has been found. TYPE may be either 'class',
'interface', 'enum' or nil, meaning 'match all of the above'."
  (save-excursion
    (if (re-search-backward
         (concat (or type "\\(class\\|interface\\|enum\\)") "\\s-+\\([^<{\s-]+\\)") nil t)
        (match-string 2)
      "")))

(defun eclim--c-current-class-name ()
  "Searches backward in the current buffer until a class declaration
has been found."
  (eclim--c-current-type-name "\\(class\\)"))

(defun eclim-c-format ()
  "Format the source code of the current c source file."
  (interactive)
  (eclim/execute-command "c_format" "-p" "-f" ("-h" 0) ("-t" (1- (point-max))) "-e"))

(defun eclim-c-generate-getter-and-setter (project file offset encoding)
  "Generates getter and setter methods for the symbol at point."
  (interactive (list (eclim--project-name)
                     (eclim--project-current-file)
                     (eclim--byte-offset)
                     (eclim--current-encoding)))

  (eclim--call-process "c_bean_properties"
                       "-p" project
                       "-f" file
                       "-o" (number-to-string offset)
                       "-e" encoding
                       "-r" (cdr (eclim--c-identifier-at-point t))
                       "-t" "gettersetter")
  (revert-buffer t t t))

(defun eclim-c-constructor ()
  (interactive)
  (eclim/execute-command "c_constructor" "-p" "-f" "-o"))

(defun eclim/c-call-hierarchy (project file offset length encoding)
  (eclim--call-process "c_callhierarchy"
                       "-p" project
                       "-f" file
                       "-o" (number-to-string offset)
                       "-l" (number-to-string length)
                       "-e" encoding))

(defun eclim-c-call-hierarchy (project file encoding)
  (interactive (list (eclim--project-name)
                     (eclim--project-current-file)
                     (eclim--current-encoding)))
  (let ((boundary "\\([<>()\\[\\.\s\t\n!=,;]\\|]\\)"))
    (save-excursion
      (if (re-search-backward boundary nil t)
        (forward-char))
      (let ((top-node (eclim/c-call-hierarchy project file (eclim--byte-offset)
                                                 (length (cdr (eclim--c-identifier-at-point t))) encoding)))
        (pop-to-buffer "*eclim: call hierarchy*" t)
        (special-mode)
        (let ((buffer-read-only nil))
          (erase-buffer)
          (eclim--c-insert-call-hierarchy-node
           project
           top-node
           0))))))
(defun eclim--c-insert-call-hierarchy-node (project node level)
  (let ((declaration (cdr (assoc 'name node))))
    (insert (format (concat "%-"(number-to-string (* level 2)) "s=> ") ""))
    (lexical-let ((position (cdr (assoc 'position node))))
      (if position
        (insert-text-button declaration
                            'follow-link t
                            'help-echo declaration
                            'action #'(lambda (&rest ignore)
                                        (eclim--visit-declaration position)))
        (insert declaration)))
    (newline)
    (loop for caller across (cdr (assoc 'callers node))
          do (eclim--c-insert-call-hierarchy-node project caller (1+ level)))))

(defun eclim--c-insert-file-path-for-hierarchy-nodes (node)
                                        ;Can't use *-find-type here because it will pop a buffer
                                        ;that isn't part of the project which then breaks future
                                        ;*-find-type calls and isn't what we want here anyway.
  (eclim/with-results hits ("c_search" ("-p" (cdr (assoc 'qualified node))) ("-t" "type") ("-x" "declarations") ("-s" "workspace"))
    (add-to-list 'node `(file-path . ,(assoc-default 'message (elt hits 0))))
    (let ((children (cdr (assoc 'children node))))
      (loop for child across children do
            (eclim--c-insert-file-path-for-hierarchy-nodes child)))
    node))

(defun eclim--c-insert-hierarchy-node (project node level)
  (let ((declaration (cdr (assoc 'name node)))
        (qualified-name (cdr (assoc 'qualified node))))
    (insert (format (concat "%-"(number-to-string (* level 2)) "s=> ") ""))
    (lexical-let ((file-path (cdr (assoc 'file-path node))))
      (if file-path
          (insert-text-button declaration
                              'follow-link t
                              'help-echo qualified-name
                              'action (lambda (&rest ignore)
                                        (eclim--find-file file-path)))
        (insert declaration))))
  (newline)
  (let ((children (cdr (assoc 'children node))))
    (loop for child across children do
          (eclim--c-insert-hierarchy-node project child (+ level 1)))))

(defun alist-edit-key (key val alist)
  "Destructively edit a key in an alist"
  (setf alist (cons (cons key val) (assq-delete-all key alist))))

(defvar eclim-c--ctx-switch-crap
  "EThread_CtxSwitchingCapability_DO_NOT_USE_T ethread_ctx_switching_capability_DO_NOT_USE")

(defun eclim/c--completions ()
  (let ((completions (eclim/execute-command "c_complete" "-p" "-f" "-e" ("-l" "standard") "-o")))
    (flet ((post-process-completion (completion)
              (let* ((info (assoc-default 'info completion))
                     (new-info (replace-regexp-in-string
                                eclim-c--ctx-switch-crap "CTX_SWITCH" info)))
                (alist-edit-key 'info new-info completion))))
      (mapcar 'post-process-completion completions))))

(defun eclim-c-find-declaration-other-window ()
  "Find and display the declaration of the c identifier at point."
  (interactive)
  (let ((eclim--other-window t))
    (eclim-c-find-declaration)))

(defun eclim-c--is-api-file-name (file-name)
  (string-match-p "_api\\.h$" file-name))

(defun eclim-c--is-api-match (match)
  (eclim-c--is-api-file-name (assoc-default 'filename match)))

(defun eclim-c-find-declaration ()
  "Find and display the declaration of the c identifier at point."
  (interactive)
  (let ((i (eclim--c-identifier-at-point t)))
    (eclim/with-results hits
      ("c_search" "-n" "-f"
       ("-o" (car i))
       ("-l" (length (cdr i)))
       ("-x" "declaration"))
      (let* ((results (reverse (remove-duplicates hits :test 'equal)))
             (reordered-results
              (vector-move-some-to-end 'eclim-c--is-api-match results)))
        (eclim--find-display-results
         (cdr i) reordered-results t)))))

(defun eclim-c-find-references ()
  "Find and display references for the c identifier at point."
  (interactive)
  (let ((i (eclim--c-identifier-at-point t)))
    (eclim/with-results hits ("c_search" "-n" "-f" ("-o" (car i)) ("-l" (length (cdr i))) ("-x" "references"))
      (eclim--find-display-results (cdr i) hits))))

(defun eclim-c-find-type (type-name &optional case-insensitive)
  "Searches the project for a given class. The TYPE-NAME is the
pattern, which will be used for the search. If invoked with the
universal argument the search will be made CASE-INSENSITIVE."
  (interactive (list (read-string "Name: " (let ((case-fold-search nil)
                                                 (current-symbol (symbol-name (symbol-at-point))))
                                             (if (string-match-p "^[A-Z]" current-symbol)
                                                 current-symbol
                                               (eclim--c-current-type-name))))
                     "P"))
  (eclim-c-find-generic "workspace" "declarations" "type" type-name case-insensitive t))

(defun eclim-c-find-generic (scope context type pattern &optional case-insensitive open-single-file)
  "Searches within SCOPE (all/project/type) for a
TYPE (all/annotation/class/classOrEnum/classOrInterface/constructor/enum/field/interface/method/package/type)
matching the given
CONTEXT (all/declarations/implementors/references) and
PATTERN. If invoked with the universal argument the search will
be made CASE-INSENSITIVE."
  (interactive (list (eclim--completing-read "Scope: " eclim--c-search-scopes)
                     (eclim--completing-read "Context: " eclim--c-search-contexts)
                     (eclim--completing-read "Type: " eclim--c-search-types)
                     (read-string "Pattern: ")
                     "P"))
  (eclim/with-results hits ("c_search" ("-p" pattern) ("-t" type) ("-x" context) ("-s" scope) (if case-insensitive '("-i" "")))
    (eclim--find-display-results pattern hits open-single-file)))

(defun eclim--c-identifier-at-point (&optional full position)
  "Returns a cons cell (BEG . IDENTIFIER) where BEG is the start
buffer byte offset of the token/identifier at point, and
IDENTIFIER is the string from BEG to (point). If argument FULL is
non-nill, IDENTIFIER will contain the whole identifier, not just
the start. If argument POSITION is non-nil, BEG will contain the
position of the identifier instead of the byte offset (which only
matters for buffers containing non-ASCII characters)."
  (let ((boundary "\\([-<>()\\[\\.\s\t\n&*~%^:!=,;]\\|]\\)"))
    ;; TODO: make this work for dos buffers
    (save-excursion
      (if (and full (re-search-forward boundary nil t))
          (backward-char))
      (let ((end (point))
            (start (progn
                     (if (re-search-backward boundary nil t) (forward-char))
                     (point))))
        (cons (if position (point) (eclim--byte-offset))
              (eclim-c--debug-message (buffer-substring-no-properties start end)))))))

(defun eclim--c-package-components (package)
  "Returns the components of a C package statement."
  (split-string package "\\."))

(defun eclim--c-current-package ()
  "Returns the package for the class in the current buffer."
  (save-excursion
    (goto-char 0)
    (if (re-search-forward "package \\(.*?\\);" (point-max) t)
        (match-string-no-properties 1))))

(defun format-type (type)
  (cond ((null type) nil)
        ((listp (first type))
         (append (list "<") (rest (mapcan (lambda (type) (append (list ", ") (format-type type))) (first type))) (list ">")
               (format-type (rest type))))
        (t (cons (let ((type-name (symbol-name (first type))))
                   (when (string-match "\\(.*\\.\\)?\\(.*\\)" type-name)
                     (match-string 2 type-name)))
                 (format-type (rest type))))))

(provide 'eclim-c)
