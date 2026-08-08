;;; ps-blank-lines-tree.el --- Org file as a tree of nodes with explicit gaps -*- lexical-binding: t; -*-

;;; Commentary:

;; The representation underneath blank-line recovery (see
;; `design-docs/blank-line-recovery.md').  An Org file parses into a *tree of
;; nodes*, never into a flat line sequence with a parallel gap vector.  Two
;; reasons, both load-bearing:
;;
;; 1. A node's body is atomic.  Reordering headings changes only their order,
;;    but reordering lines inside prose destroys its meaning — so a body is
;;    carried verbatim and is never split for alignment, never reordered.  A
;;    happy consequence: a blank line inside a `#+begin_src' block lives inside
;;    a body and is simply unreachable, where a flat model would classify it as
;;    a boundary gap and could delete it.
;;
;; 2. Gap ownership turns on the *level relationship* between adjacent nodes,
;;    which a flat model does not have.  The blank after `* A' belongs to A even
;;    when A has no body and the next line is its first child's heading — so
;;    reordering A's children keeps the blank welded to A.
;;
;; Exactly two gap slots per node, plus two for the file:
;;
;;   lead — blanks immediately AFTER this node's heading line, whatever follows
;;          (its own body, or its first child's heading).
;;   sep  — blanks immediately BEFORE this node's heading line.  nil when the
;;          node is the first content inside its parent, because that gap is
;;          then the parent's `lead'.  So `sep' and `lead' never name the same
;;          blank run.
;;   bof  — blanks before the first content line (the root's `lead').
;;   eof  — trailing blanks.
;;
;; Between them these account for every blank line in a file exactly once, so
;; `ps/blank-lines-render' is total and `render ∘ parse' is the identity.
;;
;; `sep' is *stored* on the node that follows the gap because that is the slot a
;; blank run before a heading lives in.  It is *resolved* on the ordered pair of
;; adjacent nodes — storing it here does not make it a property of the node.
;;
;; Two deliberate conventions:
;;
;; - A whitespace-only line counts as blank, matching `org-element'.  Rendering
;;   therefore emits a truly empty line where the source had spaces.  Trailing
;;   whitespace is explicitly out of scope for this feature.
;; - A file with mixed line endings parses to nil rather than being silently
;;   normalised.  Callers report it and skip the file.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)

;;; Data structures

(cl-defstruct (ps/blank-lines-node (:constructor ps/blank-lines--make-node))
  "One Org heading with its own body and its two gap slots.

`body' is the node's own section only — not its descendants — held as a list
of verbatim lines and treated as an atomic unit.  No code outside the parser
and renderer may index into it; the engine may compare two bodies or swap one
wholesale, nothing else."
  level                ; integer, from the leading stars
  heading-line         ; the heading line, verbatim
  raw-value            ; title with stars, keyword, priority and tags removed
  todo tags            ; display and diagnostics only, never part of identity
  body                 ; list of verbatim lines, or nil
  lead                 ; integer — blanks after `heading-line'
  sep                  ; integer, or nil when first content inside the parent
  children             ; list of ps/blank-lines-node, document order
  path                 ; list of ancestor `raw-value's, outermost first
  sibling-index)       ; 0-based position among siblings

(cl-defstruct (ps/blank-lines-file (:constructor ps/blank-lines--make-file))
  "A parsed Org file: the preamble, the node tree, and the file-level gaps."
  preamble             ; list of verbatim lines before the first heading, or nil
  nodes                ; list of top-level ps/blank-lines-node
  bof                  ; integer — blanks before the first content line
  eof                  ; integer — trailing blanks
  eol                  ; "\n" or "\r\n"
  final-newline)       ; t when the source ended with a line terminator

;;; Line helpers

(defconst ps/blank-lines--blank-re "\\`[ \t]*\\'"
  "A line is blank when it matches this: empty or whitespace-only.")

(defun ps/blank-lines-blank-p (line)
  "Return non-nil when LINE counts as blank."
  (and (stringp line) (string-match-p ps/blank-lines--blank-re line)))

(defun ps/blank-lines-count-whitespace-only (text)
  "Return how many lines of TEXT are whitespace-only but not empty.

Gaps are counts, so rendering emits truly empty lines and these lose their
spaces.  It cannot change content, but it would otherwise show up in a review
diff with no explanation, so callers report the count instead."
  (seq-count (lambda (line)
               (and (not (equal line "")) (ps/blank-lines-blank-p line)))
             (split-string (ps/blank-lines--normalize-eol text) "\n")))

(defun ps/blank-lines-strip (text)
  "Return the non-blank lines of TEXT, in order.
This is the content of TEXT for the purposes of the safety invariant."
  (seq-remove #'ps/blank-lines-blank-p
              (split-string (ps/blank-lines--normalize-eol text) "\n")))

(defun ps/blank-lines-strip-equal-p (a b)
  "Return non-nil when texts A and B have identical non-blank lines."
  (equal (ps/blank-lines-strip a) (ps/blank-lines-strip b)))

(defun ps/blank-lines--normalize-eol (text)
  "Return TEXT with CRLF terminators reduced to LF."
  (replace-regexp-in-string "\r\n" "\n" text))

(defun ps/blank-lines--split (text)
  "Split TEXT into a plist (:lines :eol :final-newline), or nil.

Returns nil when TEXT mixes CRLF and bare LF terminators, which cannot be
round-tripped from a line list plus a single terminator."
  (let* ((crlf (string-match-p "\r\n" text))
         (bare-lf (string-match-p "\\(?:\\`\\|[^\r]\\)\n" text)))
    (unless (and crlf bare-lf)
      (let* ((eol (if crlf "\r\n" "\n"))
             (norm (ps/blank-lines--normalize-eol text))
             (parts (split-string norm "\n"))
             final)
        (cond
         ((equal norm "") (setq parts nil))
         ((and (> (length parts) 1) (equal (car (last parts)) ""))
          (setq final t parts (butlast parts))))
        (list :lines parts :eol eol :final-newline final)))))

(defun ps/blank-lines--count-leading-blanks (lines)
  "Return how many leading elements of LINES are blank."
  (let ((n 0))
    (while (and lines (ps/blank-lines-blank-p (car lines)))
      (setq n (1+ n) lines (cdr lines)))
    n))

(defun ps/blank-lines--trim-region (lines)
  "Split LINES into (LEADING-BLANKS MIDDLE TRAILING-BLANKS).
MIDDLE runs from the first non-blank line to the last, inclusive, so any
blank lines *inside* it are preserved verbatim."
  (let* ((lead (ps/blank-lines--count-leading-blanks lines))
         (rest (nthcdr lead lines))
         (trail (ps/blank-lines--count-leading-blanks (reverse rest))))
    (list lead (butlast rest trail) trail)))

;;; Parsing

(defun ps/blank-lines--headlines (text)
  "Return headline records for TEXT, in document order.
Each record is a plist (:line :level :raw-value :todo :tags), where :line is
a 0-based index into TEXT's lines.  TEXT must already use LF terminators."
  (with-temp-buffer
    (insert text)
    (let ((org-inhibit-startup t)
          (org-element-use-cache nil))
      (delay-mode-hooks (org-mode)))
    (let ((org-element-use-cache nil))
      (org-element-map (org-element-parse-buffer 'headline) 'headline
        (lambda (h)
          (list :line (1- (line-number-at-pos (org-element-property :begin h)))
                :level (org-element-property :level h)
                :raw-value (or (org-element-property :raw-value h) "")
                :todo (org-element-property :todo-keyword h)
                :tags (org-element-property :tags h)))))))

(defun ps/blank-lines--slice (lines headlines)
  "Slice LINES at HEADLINES into per-heading regions.

Returns (PREAMBLE-REGION . REGIONS) where PREAMBLE-REGION is the lines before
the first heading and REGIONS is a list of the lines strictly between each
heading line and the next (the last one running to end of file)."
  (let* ((starts (mapcar (lambda (h) (plist-get h :line)) headlines))
         (n (length lines))
         (preamble (seq-take lines (or (car starts) n)))
         (regions '()))
    (while starts
      (let ((from (1+ (car starts)))
            (to (or (cadr starts) n)))
        (push (seq-subseq lines from to) regions)
        (setq starts (cdr starts))))
    (cons preamble (nreverse regions))))

(defun ps/blank-lines--flat-nodes (lines headlines)
  "Build the flat, document-order node list for HEADLINES over LINES.

Fills `lead', `body' and `sep' by the ownership rule: a region that contains
content splits into lead / body / trailing, where trailing becomes the next
heading's `sep'; a region that is all blank belongs to the *preceding* node's
`lead' when the next heading is deeper (it precedes that node's first child),
and to the next heading's `sep' otherwise.

Returns (NODES PREAMBLE BOF EOF), where NODES is flat and un-nested."
  (pcase-let* ((`(,preamble-region . ,regions) (ps/blank-lines--slice lines headlines))
               (`(,bof ,preamble ,pre-trail) (ps/blank-lines--trim-region preamble-region))
               (nodes '())
               (eof 0)
               ;; The gap owed to the next heading, carried forward as we walk.
               (pending (and preamble pre-trail)))
    ;; A preamble region that is entirely blank is the root's `lead' — i.e. the
    ;; file's `bof' — and the first heading then has no `sep' of its own.
    (unless preamble (setq bof (+ bof pre-trail)))
    ;; With no headings at all, the preamble's trailing blanks are the file's.
    (when (null headlines) (setq eof pre-trail))
    (cl-loop
     for h in headlines
     for region in regions
     for i from 0
     do (pcase-let* ((`(,lead ,body ,trail) (ps/blank-lines--trim-region region))
                     (next (nth (1+ i) headlines)))
          (push (ps/blank-lines--make-node
                 :level (plist-get h :level)
                 :heading-line (nth (plist-get h :line) lines)
                 :raw-value (plist-get h :raw-value)
                 :todo (plist-get h :todo)
                 :tags (plist-get h :tags)
                 :body body
                 :lead (if body lead
                         ;; All-blank region: the level of what follows decides.
                         (if (and next (> (plist-get next :level) (plist-get h :level)))
                             (+ lead trail)
                           0))
                 :sep pending)
                nodes)
          (setq pending
                (cond
                 ((null next) nil)
                 (body trail)
                 ((> (plist-get next :level) (plist-get h :level)) nil)
                 (t (+ lead trail))))
          (when (null next)
            (setq eof (if body trail (+ lead trail))))))
    (list (nreverse nodes) preamble bof eof)))

(defun ps/blank-lines--nest (nodes)
  "Nest the flat, document-order NODES into a tree by heading level.
Sets `children', `path' and `sibling-index'.  Returns the top-level nodes."
  (let ((roots '())
        (stack '()))                    ; innermost first
    (dolist (node nodes)
      (while (and stack (>= (ps/blank-lines-node-level (car stack))
                            (ps/blank-lines-node-level node)))
        (pop stack))
      (let ((parent (car stack)))
        (setf (ps/blank-lines-node-path node)
              (nreverse (mapcar #'ps/blank-lines-node-raw-value stack)))
        (if parent
            (progn
              (setf (ps/blank-lines-node-sibling-index node)
                    (length (ps/blank-lines-node-children parent)))
              (setf (ps/blank-lines-node-children parent)
                    (append (ps/blank-lines-node-children parent) (list node))))
          (setf (ps/blank-lines-node-sibling-index node) (length roots))
          (push node roots)))
      (push node stack))
    (nreverse roots)))

(defun ps/blank-lines-parse (text)
  "Parse TEXT into a `ps/blank-lines-file', or nil when unsupported.
Returns nil only for mixed line endings; see `ps/blank-lines--split'."
  (when-let* ((split (ps/blank-lines--split text)))
    (let* ((lines (plist-get split :lines))
           (norm (mapconcat #'identity lines "\n"))
           (headlines (ps/blank-lines--headlines norm)))
      (pcase-let ((`(,nodes ,preamble ,bof ,eof)
                   (ps/blank-lines--flat-nodes lines headlines)))
        (ps/blank-lines--make-file
         :preamble preamble
         :nodes (ps/blank-lines--nest nodes)
         :bof bof
         :eof eof
         :eol (plist-get split :eol)
         :final-newline (plist-get split :final-newline))))))

;;; Rendering

(defun ps/blank-lines--render-node (node acc)
  "Push NODE's lines onto ACC (reversed) and return the new ACC."
  (dotimes (_ (or (ps/blank-lines-node-sep node) 0))
    (push "" acc))
  (push (ps/blank-lines-node-heading-line node) acc)
  (dotimes (_ (or (ps/blank-lines-node-lead node) 0))
    (push "" acc))
  (dolist (line (ps/blank-lines-node-body node))
    (push line acc))
  (dolist (child (ps/blank-lines-node-children node))
    (setq acc (ps/blank-lines--render-node child acc)))
  acc)

(defun ps/blank-lines-render (file)
  "Render FILE, a `ps/blank-lines-file', back to text."
  (let ((acc '()))
    (dotimes (_ (ps/blank-lines-file-bof file)) (push "" acc))
    (dolist (line (ps/blank-lines-file-preamble file)) (push line acc))
    (dolist (node (ps/blank-lines-file-nodes file))
      (setq acc (ps/blank-lines--render-node node acc)))
    (dotimes (_ (ps/blank-lines-file-eof file)) (push "" acc))
    (let ((lines (nreverse acc))
          (eol (ps/blank-lines-file-eol file)))
      (concat (mapconcat #'identity lines eol)
              (if (ps/blank-lines-file-final-newline file) eol "")))))

;;; Walking

(defun ps/blank-lines-node-walk (file)
  "Return every node of FILE as a flat list, in document order."
  (let ((out '()))
    (cl-labels ((visit (node)
                  (push node out)
                  (mapc #'visit (ps/blank-lines-node-children node))))
      (mapc #'visit (ps/blank-lines-file-nodes file)))
    (nreverse out)))

;;; Explain (development aid)

(defun ps/blank-lines--explain-line (node)
  "Return a one-line description of NODE for the explain buffer."
  (format "%s%-4s %-5s %s%s"
          (make-string (* 2 (1- (ps/blank-lines-node-level node))) ?\s)
          (format "*%d" (ps/blank-lines-node-level node))
          (format "%s/%s"
                  (if (ps/blank-lines-node-sep node)
                      (number-to-string (ps/blank-lines-node-sep node))
                    "-")
                  (ps/blank-lines-node-lead node))
          (ps/blank-lines-node-raw-value node)
          (if (ps/blank-lines-node-body node)
              (format "   [body %d]" (length (ps/blank-lines-node-body node)))
            "")))

;;;###autoload
(defun ps/blank-lines-explain-buffer ()
  "Show how the current Org buffer decomposes into nodes and gaps.
Each row is `sep/lead' followed by the heading title.  A `-' sep means the
node is the first content inside its parent, so the gap before it is that
parent's lead.  Used to check the ownership rule against real files."
  (interactive)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (name (buffer-name))
         (file (ps/blank-lines-parse text)))
    (if (null file)
        (user-error "Mixed line endings; this file cannot be decomposed")
      (let ((buf (get-buffer-create "*Org Blank Line Tree*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "%s\n\n" name))
            (insert (format "bof %d   eof %d   eol %s   final-newline %s\n"
                            (ps/blank-lines-file-bof file)
                            (ps/blank-lines-file-eof file)
                            (if (equal (ps/blank-lines-file-eol file) "\r\n") "CRLF" "LF")
                            (if (ps/blank-lines-file-final-newline file) "yes" "no")))
            (insert (format "preamble %d line(s)\n\n"
                            (length (ps/blank-lines-file-preamble file))))
            (insert "     sep/lead\n")
            (dolist (node (ps/blank-lines-node-walk file))
              (insert (ps/blank-lines--explain-line node) "\n"))
            (insert (format "\nround-trip: %s\n"
                            (if (equal (ps/blank-lines-render file) text)
                                "exact" "MISMATCH")))
            (goto-char (point-min)))
          (special-mode))
        (display-buffer buf)))))

(provide 'ps-blank-lines-tree)
;;; ps-blank-lines-tree.el ends here
