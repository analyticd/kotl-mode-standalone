(provide 'needed-missing-hyperbole-bits)

;; Copied bits to remove hyperbole dependence

;; Copied htz.el over from hyperbole proper
(require 'htz)

;; From hload-path.el
(defconst hyperb:emacs-p
  (and (not (featurep 'xemacs)) emacs-version)
  "Version string if running under GNU Emacs, else nil")

;; From hypb.el
(defun hypb:indirect-function (obj)
  "Return the function at the end of OBJ's function chain.
Resolves autoloadable function symbols properly."
  (let ((func
         (if (fboundp 'indirect-function)
             (indirect-function obj)
           (while (symbolp obj)
             (setq obj (symbol-function obj)))
           obj)))
    ;; Handle functions with autoload bodies.
    (if (and (symbolp obj) (listp func) (eq (car func) 'autoload))
        (let ((load-file (car (cdr func))))
          (load load-file)
          ;; Prevent infinite recursion
          (if (equal func (symbol-function obj))
              (error "(hypb:indirect-function): Autoload of '%s' failed" obj)
            (hypb:indirect-function obj)))
      func)))

;; From hact.el
(defun action:params (action)
  "Returns unmodified ACTION parameter list.
Autoloads action function if need be to get the parameter list."
  (when (and (symbolp action) (fboundp action))
    (setq action (hypb:indirect-function action)))
  (cond ((null action) nil)
        ((listp action)
         (if (eq (car action) 'autoload)
             (error "(action:params): Autoload not supported: %s" action)
           (car (cdr action))))
        ((hypb:emacs-byte-code-p action)
         (if (fboundp 'compiled-function-arglist)
             (compiled-function-arglist action)
           (action:params-emacs action)))
        ((symbolp action)
         (car (cdr (and (fboundp action) (hypb:indirect-function action)))))))

;; From set.el
(defvar set:equal-op 'equal
  "Comparison function used by set operators.
It must be a function of two arguments which returns non-nil only when
the arguments are equivalent.")

(defun set:member (elt set)
  "Returns non-nil if ELT is an element of SET.
The value is actually the tail of SET whose car is ELT.
Uses `set:equal-op' for comparison."
  (while (and set (not (funcall set:equal-op elt (car set))))
    (setq set (cdr set)))
  set)

(defmacro set:add (elt set)
  "Adds element ELT to SET and then returns SET.
Uses `set:equal-op' for comparison.
Use (setq set (set:add elt set)) to assure set is always properly modified."
  `(cond ((set:member ,elt ,set) ,set)
         (,set (setq ,set (cons ,elt ,set)))
         (t (list ,elt))))

(defmacro set:remove (elt set)
  "Removes element ELT from SET and returns new set.
Assumes SET is a valid set.  Uses `set:equal-op' for comparison.
Use (setq set (set:remove elt set)) to assure set is always properly modified."
  `(let ((rest (set:member ,elt ,set))
         (rtn ,set))
     (if rest
         (cond ((= (length rtn) 1) (setq rtn nil))
               ((= (length rest) 1)
                (setcdr (nthcdr (- (length rtn) 2) rtn) nil))
               (t (setcar rest (car (cdr rest)))
                  (setcdr rest (cdr (cdr rest))))))
     rtn))

;; From hinit.el
(defvar   hyperb:user-email "foo@bar.org"
  "Email address for the current user.  Set automatically by `hyperb:init'.")

;; From hypb.el
(defun hypb:emacs-byte-code-p (obj)
  "Return non-nil iff OBJ is an Emacs byte compiled object."
  (or (and (fboundp 'compiled-function-p) (compiled-function-p obj))
      (and (fboundp 'byte-code-function-p) (byte-code-function-p obj))))

(defun hypb:functionp (obj)
  "Returns t if OBJ is a function, nil otherwise."
  (cond
   ((symbolp obj) (fboundp obj))
   ((subrp obj))
   ((hypb:emacs-byte-code-p obj))
   ((consp obj)
    (if (eq (car obj) 'lambda) (listp (car (cdr obj)))))
   (t nil)))

(defun hypb:replace-match-string (regexp str newtext &optional literal)
  "Replaces all matches for REGEXP in STR with NEWTEXT string and returns the result.
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat \\ in NEWTEXT string as special:
  \\& means substitute original matched text,
  \\N means substitute match for \(...\) number N,
  \\\\ means insert one \\.
NEWTEXT may instead be a function of one argument (the string to replace in)
that returns a replacement string."
  (unless (stringp str)
    (error "(hypb:replace-match-string): 2nd arg must be a string: %s" str))
  (unless (or (stringp newtext) (hypb:functionp newtext))
    (error "(hypb:replace-match-string): 3rd arg must be a string or function: %s"
           newtext))
  (let ((rtn-str "")
        (start 0)
        (special)
        match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
            start (match-end 0)
            rtn-str
            (concat
             rtn-str
             (substring str prev-start match)
             (cond ((hypb:functionp newtext)
                    (hypb:replace-match-string
                     regexp (substring str match start)
                     (funcall newtext str) literal))
                   (literal newtext)
                   (t (mapconcat
                       (lambda (c)
                         (if special
                             (progn
                               (setq special nil)
                               (cond ((eq c ?\\) "\\")
                                     ((eq c ?&)
                                      (substring str
                                                 (match-beginning 0)
                                                 (match-end 0)))
                                     ((and (>= c ?0) (<= c ?9))
                                      (if (> c (+ ?0 (length
                                                      (match-data))))
                                          ;; Invalid match num
                                          (error "(hypb:replace-match-string) Invalid match num: %c" c)
                                        (setq c (- c ?0))
                                        (substring str
                                                   (match-beginning c)
                                                   (match-end c))))
                                     (t (char-to-string c))))
                           (if (eq c ?\\) (progn (setq special t) nil)
                             (char-to-string c))))
                       newtext ""))))))
    (concat rtn-str (substring str start))))
