(provide 'needed-missing-hyperbole-bits)

;; Copied bits to remove hyperbole dependence

;; Copied htz.el and set.el over from hyperbole proper
(require 'htz)
(require 'set)

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
  "Returns unmodified ACTION parameter list."
  (cond ((null action) nil)
	((symbolp action)
	 (car (cdr
	       (and (fboundp action) (hypb:indirect-function action)))))
	((listp action)
	 (if (eq (car action) 'autoload)
	     (error "(action:params): Autoload not supported: %s" action)
	   (car (cdr action))))
	((hypb:emacs-byte-code-p action)
	 (if (fboundp 'compiled-function-arglist)
	     (compiled-function-arglist action)
	   ;; Turn into a list for extraction.  Under Emacs 25, the
	   ;; result could be a parameter list or an integer, a
	   ;; bitstring representing a variable length argument list,
	   ;; in which case there is no present way to get the
	   ;; argument list, so just return nil.  See "(elisp)Byte-Code
	   ;; Objects".
	   (let ((params (car (cdr (cons nil (append action nil))))))
	     (if (listp params) params))))))

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

;;; From hargs.el, needed for mail tree
(defvar hargs:reading-p nil
  "t only when Hyperbole is prompting user for input, else nil.")

(defun hattr:get (obj-symbol attr-symbol)
  "Returns value of OBJ-SYMBOL's attribute ATTR-SYMBOL."
  (get obj-symbol attr-symbol))

(defvar hbut:current nil
  "The currently selected Hyperbole button. Available to action routines.")

(defun hargs:select-event-window ()
  "Select window, if any, that mouse was over during last event."
  (if (featurep 'xemacs)
      (if current-mouse-event
	  (select-window
	   (or (event-window current-mouse-event)
	       (selected-window))))
    (let* ((event last-command-event)
	   (window (posn-window (event-start event))))
      (if (and (eq window (minibuffer-window))
	       (not (minibuffer-window-active-p
		     (minibuffer-window))))
	  (error "Attempt to select inactive minibuffer window")
	(select-window (or window (selected-window)))))))

(defun hargs:read (prompt &optional predicate default err val-type)
  "PROMPTs without completion for a value matching PREDICATE and returns it.
PREDICATE is an optional boolean function of one argument.  Optional DEFAULT
is a string to insert after PROMPT as the default return value.  Optional
ERR is a string to display temporarily when an invalid value is given.
Optional VAL-TYPE is a symbol indicating the type of value to be read.  If
VAL-TYPE equals `sexpression', then return that type; otherwise return the
string read or nil."
  (let ((bad-val) (val) (stringify)
	      (prev-reading-p hargs:reading-p) (read-func)
	      (owind (selected-window))
	      (obuf (current-buffer)))
    (unwind-protect
	      (progn
	        (cond ((or (null val-type) (eq val-type 'sexpression))
		             (setq read-func 'read-minibuffer
		                   hargs:reading-p 'sexpression))
		            (t (setq read-func 'read-string hargs:reading-p val-type
			                   stringify t)))
	        (while (progn (and default (not (stringp default))
			                       (setq default (prin1-to-string default)))
			                  (condition-case ()
			                      (or bad-val (setq val (funcall read-func prompt default)))
			                    (error (setq bad-val t)))
			                  (if bad-val
			                      t
			                    (and stringify
			                         ;; Remove any double quoting of strings.
			                         (string-match "\\`\"\\([^\"]*\\)\"\\'" val)
			                         (setq val (match-string 1 val)))
			                    (and predicate (not (funcall predicate val)))))
	          (if bad-val (setq bad-val nil) (setq default val))
	          (beep)
	          (if err (progn (message err) (sit-for 3))))
	        val)
      (setq hargs:reading-p prev-reading-p)
      (select-window owind)
      (switch-to-buffer obuf))))

(defun hmail:invoke (&optional address cc subject)
  "Invoke user preferred mail composer: vm-mail, mh-send or mail.
Optional arguments are ADDRESS, CC list and SUBJECT of mail."
  ;; Next 3 lines prevent blank lines between fields due to
  ;; fill-region-as-paragraph within mail-setup.
  (if (equal address "") (setq address nil))
  (if (equal cc "") (setq cc nil))
  (if (equal subject "") (setq subject nil))
  (compose-mail address subject (if cc (list (cons "CC" cc)))))

(defun hypb:insert-region (buffer start end invisible-flag)
  "Insert into BUFFER the contents of the region from START to END within the current buffer.
INVISIBLE-FLAG, if non-nil, means invisible text in an outline region is
copied, otherwise, it is omitted."
  (if invisible-flag
      ;; Skip hidden blank lines between cells but include hidden outline text.
      (while (< start end)
	      (if (not (get-text-property start 'invisible))
	          (append-to-buffer buffer start (1+ start)))
	      (setq start (1+ start)))
    ;; Skip both hidden blank lines between cells and hidden outline text.
    (while (< start end)
      (or (kview:char-invisible-p start) (append-to-buffer buffer start (1+ start)))
      (setq start (1+ start)))))

(defun hmail:region (start end &optional buf invisible-flag)
  "Start composing mail with region between START and END included in message.
Invisible text is expanded and included in the mail only if INVISIBLE-FLAG is
non-nil.  Optional BUF contains the region and defaults to the current
buffer.  It may be a buffer or buffer name."
  (interactive (list (region-beginning) (region-end) (current-buffer)
		                 (y-or-n-p "Include invisible text? ")))
  (or buf (setq buf (current-buffer)))
  (if (stringp buf) (setq buf (get-buffer buf)))
  (let (mail-buf)
    (hmail:invoke)
    (setq mail-buf (current-buffer))
    (save-excursion
      (if (search-forward mail-header-separator nil t)
	        ;; Within header, so move to body
	        (goto-char (point-max)))
      (set-buffer buf)
      (hypb:insert-region mail-buf start end invisible-flag))))

(defun hargs:iform-read (iform &optional modifying)
  "Reads action arguments according to IFORM, a list with car = 'interactive.
Optional MODIFYING non-nil indicates current button is being modified, so
button's current values should be presented as defaults.  Otherwise, uses
hargs:defaults as list of defaults, if any.
See also documentation for `interactive'."
  ;; This is mostly a translation of `call-interactively' to Lisp.
  ;;
  ;; Save this now, since use of minibuffer will clobber it.
  (setq prefix-arg current-prefix-arg)
  (if (not (and (listp iform) (eq (car iform) 'interactive)))
      (error
       "(hargs:iform-read): arg must be a list whose car = 'interactive.")
    (setq iform (car (cdr iform)))
    (if (or (null iform) (and (stringp iform) (equal iform "")))
	      nil
      (let ((prev-reading-p hargs:reading-p))
	      (unwind-protect
	          (progn
	            (setq hargs:reading-p t)
	            (if (not (stringp iform))
		              (let ((defaults (if modifying
				                              (hattr:get 'hbut:current 'args)
				                            (and (boundp 'hargs:defaults)
					                               (listp hargs:defaults)
					                               hargs:defaults)
				                            )))
		                (eval iform))
		            (let ((i 0) (start 0) (end (length iform))
		                  (ientry) (results) (val) (default)
		                  (defaults (if modifying
				                            (hattr:get 'hbut:current 'args)
				                          (and (boundp 'hargs:defaults)
				                               (listp hargs:defaults)
				                               hargs:defaults)
				                          )))
		              ;;
		              ;; Handle special initial interactive string chars.
		              ;;
		              ;;   `*' means error if buffer is read-only.
		              ;;   Notion of when action cannot be performed due to
		              ;;   read-only buffer is view-specific, so here, we just
		              ;;   ignore a read-only specification since it is checked for
		              ;;   earlier by any ebut edit code.
		              ;;
		              ;;   `@' means select window of last mouse event.
		              ;;
		              ;;   `^' means activate/deactivate mark depending on invocation thru shift translation
		              ;;   See `this-command-keys-shift-translated' for somewhat of an explanation.
		              ;;
		              ;;   `_' means keep region in same state (active or inactive)
		              ;;   after this command.  (XEmacs only.)
		              ;;
		              (while (cond 
			                    ((eq (aref iform i) ?*))
			                    ((eq (aref iform i) ?@)
			                     (hargs:select-event-window)
			                     t)
			                    ((eq (aref iform i) ?^)
			                     (handle-shift-selection))
			                    ((eq (aref iform i) ?_)
			                     (setq zmacs-region-stays t)))
		                (setq i (1+ i) start i))
		              ;;
		              (while (and (< start end)
			                        (string-match "\n\\|\\'" iform start))
		                (setq start (match-end 0)
			                    ientry (substring iform i (match-beginning 0))
			                    i start
			                    default (car defaults)
			                    default (if (or (null default) (stringp default))
				                              default
				                            (prin1-to-string default))
			                    val (hargs:get ientry default (car results))
			                    defaults (cdr defaults)
			                    results (cond ((or (null val) (not (listp val)))
					                               (cons val results))
					                              ;; Is a list of args?
					                              ((eq (car val) 'args)
					                               (append (nreverse (cdr val)) results))
					                              (t ;; regular list value
					                               (cons val results)))))
		              (nreverse results))))
	        (setq hargs:reading-p prev-reading-p))))))

