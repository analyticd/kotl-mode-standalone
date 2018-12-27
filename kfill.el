;;; kfill.el --- Fill and justify koutline cells
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    23-Jan-94
;;
;; Copyright (C) 1994-2016  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.
;; It was originally adapted from Kyle Jones' filladapt library.

;;; Commentary:

;;; Code:

;; Quiet byte compiler warnings for this free variable.
(eval-when-compile
  (defvar filladapt-function-table nil))

;; The above formerly looked like this, but the filladapt package is old
;; and unmaintained and causes improper paragraph filling for
;; kotl-mode under modern GNU Emacs versions.   -- RSW 12/17/2017
;; (eval-when-compile
;;  (unless (require 'filladapt nil t)
;;    (defvar filladapt-function-table nil)))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar kfill:function-table
  (if (featurep 'filladapt)
      filladapt-function-table
    (list (cons 'fill-paragraph (symbol-function 'fill-paragraph))))
  "Table containing the old function definitions that kfill overrides.")

(defvar kfill:prefix-table
  '(
    ;; Lists with hanging indents, e.g.
    ;; 1. xxxxx   or   1)  xxxxx   etc.
    ;;    xxxxx            xxx
    ;;
    ;; Be sure pattern does not match to:  (last word in parens starts
    ;; newline)
    (" *(?\\([0-9][0-9a-z.]*\\|[a-z][0-9a-z.]\\)) +" . kfill:hanging-list)
    (" *\\([0-9]+[a-z.]+[0-9a-z.]*\\|[0-9]+\\|[a-z]\\)\\([.>] +\\|  +\\)"
     . kfill:hanging-list)
    ;; Included text in news or mail replies
    ("[ \t]*\\(>+ *\\)+" . kfill:normal-included-text)
    ;; Included text generated by SUPERCITE.  We can't hope to match all
    ;; the possible variations, your mileage may vary.
    ("[ \t]*[A-Za-z0-9][^'`\"< \t\n\r]*>[ \t]*" . kfill:supercite-included-text)
    ;; Lisp comments
    ("[ \t]*\\(;+[ \t]*\\)+" . kfill:lisp-comment)
    ;; UNIX shell comments
    ("[ \t]*\\(#+[ \t]*\\)+" . kfill:sh-comment)
    ;; Postscript comments
    ("[ \t]*\\(%+[ \t]*\\)+" . kfill:postscript-comment)
    ;; C++ comments
    ("[ \t]*//[/ \t]*" . kfill:c++-comment)
    ("[?!~*+ -]+ " . kfill:hanging-list)
    ;; This keeps normal paragraphs from interacting unpleasantly with
    ;; the types given above.
    ("[^ \t/#%?!~*+-]" . kfill:normal)
    )
"Value is an alist of the form

   ((REGXP . FUNCTION) ...)

When fill-paragraph is called, the REGEXP of each alist element is compared
with the beginning of the current line.  If a match is found the corresponding
FUNCTION is called.  FUNCTION is called with one argument, which is non-nil
when invoked on the behalf of fill-paragraph.  It is the job of FUNCTION to
set the values of the paragraph-* variables (or set a clipping region, if
paragraph-start and paragraph-separate cannot be made discerning enough) so
that fill-paragraph works correctly in various contexts.")

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst kfill:hanging-expression
  (cons 'or
	(delq nil (mapcar (lambda (pattern-type)
			    (if (eq (cdr pattern-type) 'kfill:hanging-list)
				(list 'looking-at (car pattern-type))))
			  kfill:prefix-table)))
  "Conditional expression used to test for hanging indented lists.")

(defvar prior-fill-prefix nil
  "Prior string inserted at front of new line during filling, or nil for none.
Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'prior-fill-prefix)

;;; ************************************************************************
;;; Public functions 
;;; ************************************************************************

(defun kfill:forward-line (&optional n)
  "Move N lines forward (backward if N is negative) to the start of line.
If there isn’t room, go as far as possible (no error).  Return the
number of lines that could not be moved, otherwise 0."
  (or (integerp n) (setq n 1))
  (let ((opoint (point)))
    (forward-visible-line n)
    (if (< n 0)
	nil
      (skip-chars-forward "\n\r"))
;    (- (abs n) (count-matches "\n" opoint (point)))
    0))

(defun kfill:do-auto-fill ()
  (save-restriction
    (if (null fill-prefix)
	(let ((paragraph-ignore-fill-prefix nil)
	      ;; Need this or Emacs ignores fill-prefix when inside a
	      ;; comment.
	      (comment-multi-line t)
	      fill-prefix)
	  (kfill:adapt nil)
	  (do-auto-fill))
      (do-auto-fill))))

;;; Redefine this built-in function.

(defun fill-paragraph (arg &optional skip-prefix-remove)
  "Fill paragraph at or after point.  Prefix ARG means justify as well."
  (interactive "*P")
  (if (not (eq major-mode 'kotl-mode))
      (kfill:funcall 'fill-paragraph arg)
    ;; This may be called from `fill-region-as-paragraph' in "filladapt.el"
    ;; which narrows the region to the current paragraph.  A side-effect is
    ;; that the cell identifier and indent information needed by this function
    ;; when in kotl-mode is no longer visible.  So we temporarily rewiden the
    ;; buffer here.  Don't rewiden past the paragraph of interest or any
    ;; following blank line may be removed by the filling routines.
    (save-restriction
      (if (eq major-mode 'kotl-mode)
	  (narrow-to-region 1 (point-max)))
      ;; Emacs expects a specific symbol here.
      (if (and arg (not (symbolp arg))) (setq arg 'full))
      (or skip-prefix-remove (kfill:remove-paragraph-prefix))
      (catch 'done
	(if (null fill-prefix)
	    (let ((paragraph-ignore-fill-prefix nil)
		  ;; Need this or Emacs ignores fill-prefix when
		  ;; inside a comment.
		  (comment-multi-line t)
		  (paragraph-start paragraph-start)
		  (paragraph-separate paragraph-separate)
		  fill-prefix)
	      (if (kfill:adapt t)
		  (throw 'done (kfill:funcall 'fill-paragraph arg)))))
	;; Kfill:adapt failed or fill-prefix is set, so do a basic
	;; paragraph fill as adapted from par-align.el.
	(kfill:fallback-fill-paragraph arg skip-prefix-remove)))))

;;;
;;; Redefine this built-in function so that it sets `prior-fill-prefix' also.
;;;
(defun set-fill-prefix (&optional turn-off)
  "Set `fill-prefix' to the current line up to point or remove it if optional TURN-OFF flag is non-nil.
Also sets `prior-fill-prefix' to the previous value of `fill-prefix'.
Filling removes any prior fill prefix, adjusts line lengths and then adds the
fill prefix at the beginning of each line."
  (interactive)
  (setq prior-fill-prefix fill-prefix
	fill-prefix (if turn-off
			nil
		      (buffer-substring
		       (save-excursion (beginning-of-line) (point))
		       (point))))
  (if (equal prior-fill-prefix "")
      (setq prior-fill-prefix nil))
  (if (equal fill-prefix "")
      (setq fill-prefix nil))
  (cond (fill-prefix
	 (message "fill-prefix: \"%s\"; prior-fill-prefix: \"%s\""
		  fill-prefix (or prior-fill-prefix "")))
	(prior-fill-prefix
	 (message "fill-prefix cancelled; prior-fill-prefix: \"%s\""
		  prior-fill-prefix))
	(t (message "fill-prefix and prior-fill-prefix cancelled"))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun kfill:adapt (paragraph)
  (let ((table kfill:prefix-table)
	case-fold-search
	success )
    (save-excursion
      (beginning-of-line)
      (while table
	(if (not (looking-at (car (car table))))
	    (setq table (cdr table))
	  (funcall (cdr (car table)) paragraph)
	  (setq success t table nil))))
    success ))

(defun kfill:c++-comment (paragraph)
  (setq fill-prefix (buffer-substring (match-beginning 0) (match-end 0)))
  (if paragraph
      (setq paragraph-separate "^[^ \t/]")))

(defun kfill:fallback-fill-paragraph (justify-flag &optional leave-prefix)
  (save-excursion
    (end-of-line)
    ;; Backward to para begin
    (re-search-backward (concat "\\`\\|" paragraph-separate))
    (kfill:forward-line 1)
    (let* ((region-start (point))
	   (filladapt-mode 
	    (if prior-fill-prefix
		;; filladapt-mode must be disabled for this command or it
		;; will override the removal of prior-fill-prefix.
		nil
	      (or (if (boundp 'filladapt-mode) filladapt-mode)
		  adaptive-fill-mode)))
	   (adaptive-fill-mode filladapt-mode)
	   from)
      (kfill:forward-line -1)
      (setq from (point))
      (forward-paragraph)
      ;; Forward to real paragraph end
      (re-search-forward (concat "\\'\\|" paragraph-separate))
      (or (= (point) (point-max)) (beginning-of-line))
      (or leave-prefix
	;; Remove any leading occurrences of `prior-fill-prefix'.
	(kfill:replace-string prior-fill-prefix "" nil region-start (point)))
      (or (and fill-paragraph-function
	       (let ((function fill-paragraph-function)
		     fill-paragraph-function)
		 (goto-char region-start)
		 (funcall function justify-flag)))
	  (fill-region-as-paragraph from (point) justify-flag)))))

(defun kfill:funcall (function &rest args)
  "Call the original FUNCTION with rest of ARGS that kfill overloaded."
  (apply (cdr (assq function kfill:function-table)) args))

(defun kfill:hanging-list (paragraph)
  (let (prefix match beg end)
    (setq prefix (make-string (- (match-end 0) (match-beginning 0)) ?\ ))
    (if paragraph
	(progn
	  (setq match (buffer-substring (match-beginning 0) (match-end 0)))
	  (if (string-match "^ +$" match)
	      (save-excursion
		(while (and (not (bobp)) (looking-at prefix))
		  (kfill:forward-line -1))

		(cond ((eval kfill:hanging-expression)
		       ;; Point is in front of a hanging list.
		       (setq beg (point)))
		      (t (setq beg (progn (kfill:forward-line 1) (point))))))
	    (setq beg (point)))
	  (save-excursion
	    (kfill:forward-line)
	    (while (and (looking-at prefix)
			(not (equal (char-after (match-end 0)) ?\ )))
	      (kfill:forward-line))
	    (setq end (point)))
	  (narrow-to-region beg end)))
    (setq fill-prefix prefix)))

(defun kfill:lisp-comment (paragraph)
  (setq fill-prefix (buffer-substring (match-beginning 0) (match-end 0)))
  (if paragraph
      (setq paragraph-separate
	    (concat "^" fill-prefix " *;\\|^"
		    (kfill:negate-string fill-prefix)))))

(defun kfill:negate-string (string)
  (let ((len (length string))
	(i 0) string-list)
    (setq string-list (cons "\\(" nil))
    (while (< i len)
      (setq string-list
	    (cons (if (= i (1- len)) "" "\\|")
		  (cons "]"
			(cons (substring string i (1+ i))
			      (cons "[^"
				    (cons (regexp-quote (substring string 0 i))
					  string-list)))))
	    i (1+ i)))
    (setq string-list (cons "\\)" string-list))
    (apply 'concat (nreverse string-list))))

(defun kfill:normal (paragraph)
  (if paragraph
      (setq paragraph-separate
	    (concat paragraph-separate "\\|^[ \t/#%?!~*+-]"))))

(defun kfill:normal-included-text (paragraph)
  (setq fill-prefix (buffer-substring (match-beginning 0) (match-end 0)))
  (if paragraph
      (setq paragraph-separate
	    (concat "^" fill-prefix " *>\\|^"
		    (kfill:negate-string fill-prefix)))))

(defun kfill:postscript-comment (paragraph)
  (setq fill-prefix (buffer-substring (match-beginning 0) (match-end 0)))
  (if paragraph
      (setq paragraph-separate
	    (concat "^" fill-prefix " *%\\|^"
		    (kfill:negate-string fill-prefix)))))

(defun kfill:remove-paragraph-prefix (&optional indent-str)
  "Remove fill prefix from current paragraph."
  (save-excursion
    (end-of-line)
    ;; Backward to para begin
    (re-search-backward (concat "\\`\\|" paragraph-separate))
    (kfill:forward-line 1)
    (let ((region-start (point)))
      (kfill:forward-line -1)
      (forward-paragraph)
      ;; Forward to real paragraph end
      (re-search-forward (concat "\\'\\|" paragraph-separate))
      (or (= (point) (point-max)) (beginning-of-line))
      (kfill:replace-string (or fill-prefix prior-fill-prefix)
				(if (eq major-mode 'kotl-mode)
				    (or indent-str
					(make-string (kcell-view:indent) ?  ))
				  "")
				nil region-start (point)))))

(defun kfill:replace-string (fill-str-prev fill-str &optional suffix start end)
  "Replace whitespace separated FILL-STR-PREV with FILL-STR.
Optional SUFFIX non-nil means replace at ends of lines, default is beginnings.
Optional arguments START and END specify the replace region, default is the
current region."
  (if fill-str-prev
      (progn (if start
		 (let ((s (min start end)))
		   (setq end (max start end)
			 start s))
	       (setq start (region-beginning)
		     end (region-end)))
	     (if (not fill-str) (setq fill-str ""))
	     (save-excursion
	       (save-restriction
		 (narrow-to-region start end)
		 (goto-char (point-min))
		 (let ((prefix
			(concat
			 (if suffix nil "^")
			 "[ \t]*"
			 (regexp-quote
			  ;; Get non-whitespace separated fill-str-prev
			  (substring
			   fill-str-prev
			   (or (string-match "[^ \t]" fill-str-prev) 0)
			   (if (string-match
				"[ \t]*\\(.*[^ \t]\\)[ \t]*$"
				fill-str-prev)
			       (match-end 1))))
			 "[ \t]*"
			 (if suffix "$"))))
		   (while (re-search-forward prefix nil t)
		     (replace-match fill-str nil t))))))))

(defun kfill:sh-comment (paragraph)
  (setq fill-prefix (buffer-substring (match-beginning 0) (match-end 0)))
  (if paragraph
      (setq paragraph-separate
	    (concat "^" fill-prefix " *#\\|^"
		    (kfill:negate-string fill-prefix)))))

(defun kfill:supercite-included-text (paragraph)
  (setq fill-prefix (buffer-substring (match-beginning 0) (match-end 0)))
  (if paragraph
      (setq paragraph-separate
	    (concat "^" (kfill:negate-string fill-prefix)))))

(provide 'kfill)

;;; kfill.el ends here