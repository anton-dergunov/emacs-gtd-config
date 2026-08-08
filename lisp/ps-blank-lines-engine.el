;;; ps-blank-lines-engine.el --- Transfer blank lines from an ancestor version -*- lexical-binding: t; -*-

;;; Commentary:

;; Given the working-tree version of an Org file and a healthy ancestor version
;; of the same file, produce a proposal that carries the ancestor's blank lines
;; onto the working tree's content.  Pure: text in, text out, no I/O.
;;
;; Content always comes from the working tree — mobile is the source of truth
;; for what the user wrote.  Only `lead', `sep', `bof' and `eof' come from the
;; ancestor.  Node bodies, heading lines and child order are copied untouched,
;; so a proposal cannot change content; `ps/blank-lines-propose' re-checks that
;; with `ps/blank-lines-strip-equal-p' before returning, and refuses on failure.
;;
;; Resolution, in order:
;;
;;   lead — copy verbatim from the matched ancestor node.  Never rule-driven:
;;          it is unambiguously a property of the node and travels with it.
;;   sep  — a boundary gap, so it is keyed on the ordered *pair* of adjacent
;;          nodes even though it is stored on the follower.  Ladder:
;;            1. exact edge      the same two nodes were adjacent in the ancestor
;;            2. half-edge       the same predecessor, same follower level
;;            3. fitted rule     see `ps/blank-lines-rule-cell'
;;            4. keep            no evidence, so leave the gap alone
;;          Rungs 3 and 4 fire only at seams created by a move, an insertion or
;;          a deletion — a handful of gaps per file, often none.
;;   body — the working tree's, except for the whole-body swap below.
;;
;; A node whose `sep' is nil is the first content inside its parent; that gap
;; is the parent's `lead' and is resolved there.  Nil `sep' is structural, so
;; it is read off the working tree and never resolved.
;;
;; Bodies are opaque, so blank lines lost *inside* one cannot be repaired
;; line-by-line — that would mean aligning prose.  One safe case covers it: if
;; the ancestor body and the working-tree body have identical non-blank lines
;; and differ only in blanks, the ancestor's body is adopted wholesale.  The
;; precondition is itself the proof that content does not change.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ps-blank-lines-tree)

;;; Customization

(defgroup ps-blank-lines nil
  "Recover blank lines lost to Org editors that strip them."
  :group 'org)

(defcustom ps/blank-lines-new-edge-strategy 'learned
  "How to resolve a boundary gap that has no counterpart in the ancestor.

`learned' predicts from a rule fitted to the user's own healthy files.
`zero' never guesses and returns 0, which is wrong for every level-1 seam
the corpus contains — see `design-docs/blank-line-recovery.md'."
  :type '(choice (const :tag "Predict from the fitted rule" learned)
                 (const :tag "Never guess; use 0" zero))
  :group 'ps-blank-lines)

(defcustom ps/blank-lines-match-floor 0.55
  "Similarity below which two nodes are not considered the same node."
  :type 'number
  :group 'ps-blank-lines)

(defcustom ps/blank-lines-rule-min-samples 5
  "Fewest observations a fitted rule cell needs before it is trusted."
  :type 'integer
  :group 'ps-blank-lines)

;;; Data structures

(cl-defstruct (ps/blank-lines-change (:constructor ps/blank-lines--make-change))
  "One proposed change to one gap, with where it came from."
  title                ; heading title, or "<file>" for bof/eof/preamble
  slot                 ; 'lead | 'sep | 'bof | 'eof | 'body | 'preamble
  from to              ; current and proposed blank-run lengths
  source               ; 'verbatim | 'exact-edge | 'half-edge | 'rule
                       ;   | 'keep | 'unmatched | 'body-swap
  detail)              ; human string for the summary

;;; Similarity

(defun ps/blank-lines--string-similarity (a b)
  "Return how similar strings A and B are, in [0,1]."
  (let ((a (or a "")) (b (or b "")))
    (cond
     ((and (equal a "") (equal b "")) 1.0)
     ((equal a b) 1.0)
     (t (let ((longest (max (length a) (length b))))
          (max 0.0 (- 1.0 (/ (float (string-distance a b)) longest))))))))

(defun ps/blank-lines--body-similarity (a b)
  "Return the Jaccard similarity of bodies A and B over their non-blank lines.

Returns nil when both are empty: two bodyless headings are not evidence of
sameness, and scoring them 1.0 would let any two bare siblings match."
  (let ((sa (seq-remove #'ps/blank-lines-blank-p a))
        (sb (seq-remove #'ps/blank-lines-blank-p b)))
    (cond
     ((and (null sa) (null sb)) nil)
     ((or (null sa) (null sb)) 0.0)
     (t (let* ((ua (seq-uniq sa))
               (ub (seq-uniq sb))
               (inter (length (seq-intersection ua ub)))
               (union (length (seq-union ua ub))))
          (if (zerop union) 1.0 (/ (float inter) union)))))))

(defun ps/blank-lines--path-similarity (a b)
  "Return how similar outline paths A and B are, in [0,1]."
  (cond
   ((and (null a) (null b)) 1.0)
   ((or (null a) (null b)) 0.0)
   (t (let ((common 0))
        (cl-loop for x in a for y in b
                 while (equal x y) do (setq common (1+ common)))
        (/ (float common) (max (length a) (length b)))))))

(defconst ps/blank-lines--match-weights
  '((title . 0.45) (body . 0.30) (path . 0.15) (level . 0.07) (sibling . 0.03))
  "Relative weight of each identity signal.
When the body signal is unavailable (both bodies empty) its weight is
redistributed over the rest, so bare siblings are judged on path, level and
position rather than being scored as identical.")

(defun ps/blank-lines-similarity (a b)
  "Return the identity similarity of nodes A and B, in [0,1]."
  (let* ((w ps/blank-lines--match-weights)
         (body (ps/blank-lines--body-similarity
                (ps/blank-lines-node-body a) (ps/blank-lines-node-body b)))
         (parts
          (list (cons (alist-get 'title w)
                      (ps/blank-lines--string-similarity
                       (ps/blank-lines-node-raw-value a)
                       (ps/blank-lines-node-raw-value b)))
                (cons (alist-get 'path w)
                      (ps/blank-lines--path-similarity
                       (ps/blank-lines-node-path a) (ps/blank-lines-node-path b)))
                (cons (alist-get 'level w)
                      (if (= (ps/blank-lines-node-level a)
                             (ps/blank-lines-node-level b))
                          1.0 0.0))
                (cons (alist-get 'sibling w)
                      (if (equal (ps/blank-lines-node-sibling-index a)
                                 (ps/blank-lines-node-sibling-index b))
                          1.0 0.0)))))
    (when body (push (cons (alist-get 'body w) body) parts))
    (let ((total (apply #'+ (mapcar #'car parts)))
          (sum (apply #'+ (mapcar (lambda (p) (* (car p) (cdr p))) parts))))
      (if (zerop total) 0.0 (/ sum total)))))

;;; Matching

(defun ps/blank-lines--match-key (node)
  "Return the exact-identity key for NODE: its level, title and path."
  (list (ps/blank-lines-node-level node)
        (ps/blank-lines-node-raw-value node)
        (ps/blank-lines-node-path node)))

(defun ps/blank-lines--match-unambiguous (wnodes anodes)
  "Match nodes that share an exact key which is unique on both sides.

Returns (TABLE REMAINING-W REMAINING-A).  This handles the overwhelming
majority of nodes and keeps the quadratic pass below small."
  (let ((wbuckets (make-hash-table :test #'equal))
        (abuckets (make-hash-table :test #'equal))
        (table (make-hash-table :test #'eq))
        (rest-w '()) (rest-a '()) (taken (make-hash-table :test #'eq)))
    (dolist (n wnodes) (push n (gethash (ps/blank-lines--match-key n) wbuckets)))
    (dolist (n anodes) (push n (gethash (ps/blank-lines--match-key n) abuckets)))
    (dolist (n wnodes)
      (let* ((key (ps/blank-lines--match-key n))
             (ws (gethash key wbuckets))
             (as (gethash key abuckets)))
        (if (and (= (length ws) 1) (= (length as) 1))
            (progn (puthash n (car as) table)
                   (puthash (car as) t taken))
          (push n rest-w))))
    (dolist (n anodes)
      (unless (gethash n taken) (push n rest-a)))
    (list table (nreverse rest-w) (nreverse rest-a))))

(defun ps/blank-lines-match (wfile afile)
  "Match the nodes of WFILE against those of AFILE.

Returns a hash mapping each working-tree node to its ancestor node, or
absent when the node is new.  Exact, unambiguous keys are paired first; the
remainder is scored pairwise and paired greedily above
`ps/blank-lines-match-floor'."
  (let ((wnodes (ps/blank-lines-node-walk wfile))
        (anodes (ps/blank-lines-node-walk afile)))
    (pcase-let ((`(,table ,rest-w ,rest-a)
                 (ps/blank-lines--match-unambiguous wnodes anodes)))
      (let ((pairs '()))
        (dolist (w rest-w)
          (dolist (a rest-a)
            (let ((score (ps/blank-lines-similarity w a)))
              (when (>= score ps/blank-lines-match-floor)
                (push (list score w a) pairs)))))
        (setq pairs (sort pairs (lambda (x y) (> (car x) (car y)))))
        (let ((used-w (make-hash-table :test #'eq))
              (used-a (make-hash-table :test #'eq)))
          (pcase-dolist (`(,_score ,w ,a) pairs)
            (unless (or (gethash w used-w) (gethash a used-a))
              (puthash w a table)
              (puthash w t used-w)
              (puthash a t used-a)))))
      table)))

;;; The fitted rule

(defun ps/blank-lines-rule-cell (level pred-level pred-has-body)
  "Return the rule cell for a boundary gap before a heading.

LEVEL is the heading's level, PRED-LEVEL the nearest preceding heading's,
and PRED-HAS-BODY whether prose intervened.

Levels 1 and 3+ are decided by the heading alone (100% and ~0% in the
corpus).  Level 2 is decided by what precedes it, and the prose distinction
is kept for every level-2 case because it is worth 25x: a level-2 heading
after a bare sibling carries a blank 20% of the time, after a sibling with
prose only 0.8%.  See `design-docs/blank-line-recovery.md'."
  (cond
   ((= level 1) 'l1)
   ((>= level 3) 'l3+)
   ;; No preceding heading at all: this is the first heading in the file,
   ;; following the preamble.  Kept distinct so it cannot pollute `l1'.
   ((null pred-level) 'l2-after-preamble)
   ((< pred-level level)
    (if pred-has-body 'l2-parent-prose 'l2-parent-bare))
   ((> pred-level level)
    (if pred-has-body 'l2-from-deeper-prose 'l2-from-deeper-bare))
   (t (if pred-has-body 'l2-sibling-prose 'l2-sibling-bare))))

(defun ps/blank-lines-rule-empty ()
  "Return a fresh, empty fitted-rule accumulator."
  (make-hash-table :test #'eq))

(defun ps/blank-lines--rule-add (rule cell value)
  "Record one observation of VALUE in CELL of RULE."
  (let ((counts (or (gethash cell rule)
                    (puthash cell (make-hash-table :test #'eql) rule))))
    (puthash value (1+ (or (gethash value counts) 0)) counts)))

(defun ps/blank-lines-rule-observe (rule file)
  "Fold every boundary gap of FILE into RULE.  Returns RULE.

FILE must be a *healthy* version — fitting on the working tree would teach
the model the damage, since the mobile apps have already removed 111 lead
and 104 boundary blanks from the corpus."
  (let ((nodes (ps/blank-lines-node-walk file))
        (prev nil))
    (dolist (node nodes)
      (when-let* ((sep (ps/blank-lines-node-sep node)))
        (ps/blank-lines--rule-add
         rule
         (ps/blank-lines-rule-cell
          (ps/blank-lines-node-level node)
          (and prev (ps/blank-lines-node-level prev))
          (and prev (ps/blank-lines-node-body prev) t))
         sep))
      (setq prev node)))
  rule)

(defun ps/blank-lines-rule-predict (rule cell)
  "Return (VALUE . DETAIL) predicted for CELL by RULE, or nil when untrusted."
  (when-let* ((counts (and rule (gethash cell rule))))
    (let ((total 0) (best nil) (best-n 0))
      (maphash (lambda (value n)
                 (setq total (+ total n))
                 (when (> n best-n) (setq best value best-n n)))
               counts)
      (when (>= total ps/blank-lines-rule-min-samples)
        (cons best (format "rule: %s, %d/%d" cell best-n total))))))

(defun ps/blank-lines-rule-report (rule)
  "Return RULE as a readable alist of (CELL VALUE N TOTAL), for inspection."
  (let ((out '()))
    (maphash
     (lambda (cell counts)
       (let ((total 0) (best nil) (best-n 0))
         (maphash (lambda (value n)
                    (setq total (+ total n))
                    (when (> n best-n) (setq best value best-n n)))
                  counts)
         (push (list cell best best-n total) out)))
     rule)
    (sort out (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b)))))))

;;; Resolution

(defun ps/blank-lines--doc-order-index (nodes)
  "Return a hash mapping each node in NODES to its document-order index."
  (let ((table (make-hash-table :test #'eq))
        (i 0))
    (dolist (node nodes) (puthash node i table) (setq i (1+ i)))
    table))

(defun ps/blank-lines--resolve-sep (node prev match aindex anodes rule strategy)
  "Resolve the boundary gap before NODE.  Returns (VALUE SOURCE DETAIL).

PREV is NODE's predecessor in working-tree document order, or nil when NODE
is the file's first heading — in which case the edge is the one from the
preamble, and the ancestor's own first heading is its counterpart.  MATCH is
the node map, AINDEX the ancestor's document-order table, ANODES its nodes."
  (let* ((anode (gethash node match))
         (aprev (and prev (gethash prev match)))
         (asep (and anode (ps/blank-lines-node-sep anode))))
    (cond
     ;; 1. Exact edge — the same two nodes were adjacent in the ancestor.
     ((and anode asep
           (if prev
               (and aprev (equal (gethash anode aindex) (1+ (gethash aprev aindex))))
             (equal (gethash anode aindex) 0)))
      (list asep 'exact-edge "from the same edge in the ancestor"))
     ;; 2. Half-edge — the predecessor was there; adopt whatever followed it,
     ;; but only when that follower was at the same level.  The gap is a
     ;; function of both endpoints, and holding the predecessor fixed is only
     ;; half the evidence: a predecessor once followed by a `***' says nothing
     ;; about the same predecessor now followed by a `*', which always takes a
     ;; blank.  Without this test the rung confidently deletes real blank lines.
     ((when-let* ((after (if prev
                             (when-let* ((ai (and aprev (gethash aprev aindex))))
                               (nth (1+ ai) anodes))
                           (car anodes)))
                  ((= (ps/blank-lines-node-level after)
                      (ps/blank-lines-node-level node)))
                  (sep (ps/blank-lines-node-sep after)))
        (list sep 'half-edge "from what followed the same predecessor")))
     ;; 3. Fitted rule.
     ((when-let* (((eq strategy 'learned))
                  (pred (ps/blank-lines-rule-predict
                         rule
                         (ps/blank-lines-rule-cell
                          (ps/blank-lines-node-level node)
                          (and prev (ps/blank-lines-node-level prev))
                          (and prev (ps/blank-lines-node-body prev) t)))))
        (list (car pred) 'rule (cdr pred))))
     ;; 4. No evidence — keep what the working tree has.
     ;;
     ;; Not 0.  A gap the ancestor knows nothing about is not thereby known to
     ;; be unwanted, and returning 0 here deletes blank lines the user put in
     ;; by hand.  For the case that motivated a zero default — a node newly
     ;; typed on mobile — the current value already is 0, so this is the same
     ;; answer.  Removals still happen, but only when memory positively says
     ;; the gap was absent, which is what takes out Beorg's over-insertions.
     (t (list (ps/blank-lines-node-sep node) 'keep "no evidence; left as is")))))

(defun ps/blank-lines--maybe-swap-body (wbody abody)
  "Return ABODY when it differs from WBODY only in blank lines, else nil."
  (and abody
       (not (equal wbody abody))
       (equal (seq-remove #'ps/blank-lines-blank-p wbody)
              (seq-remove #'ps/blank-lines-blank-p abody))
       abody))

(defun ps/blank-lines-resolve (wfile afile match rule strategy)
  "Rewrite WFILE's gaps from AFILE.  Returns the list of changes made.

WFILE is mutated in place; its heading lines and child order are not
touched, and a body is replaced only by one with identical non-blank lines."
  (let* ((wnodes (ps/blank-lines-node-walk wfile))
         (anodes (ps/blank-lines-node-walk afile))
         (aindex (ps/blank-lines--doc-order-index anodes))
         (changes '())
         (prev nil))
    (cl-flet ((record (title slot from to source detail)
                (unless (equal from to)
                  (push (ps/blank-lines--make-change
                         :title title :slot slot :from from :to to
                         :source source :detail detail)
                        changes))))
      ;; A leading blank line is visible in the buffer, so `bof' is restored.
      (record "<file>" 'bof (ps/blank-lines-file-bof wfile)
              (ps/blank-lines-file-bof afile) 'verbatim "from the ancestor")
      (setf (ps/blank-lines-file-bof wfile) (ps/blank-lines-file-bof afile))
      ;; `eof' is restored upward only.  A trailing blank line is invisible, and
      ;; on the live corpus it drifts in both directions between versions for
      ;; reasons unrelated to this damage — restoring it symmetrically put a
      ;; hunk in nearly every review.  But the damage itself is always removal:
      ;; the mobile apps strip trailing whitespace and never add it.  So a
      ;; shortfall against the ancestor is repaired and a surplus is left alone.
      (when (> (ps/blank-lines-file-eof afile) (ps/blank-lines-file-eof wfile))
        (record "<file>" 'eof (ps/blank-lines-file-eof wfile)
                (ps/blank-lines-file-eof afile) 'verbatim "from the ancestor")
        (setf (ps/blank-lines-file-eof wfile) (ps/blank-lines-file-eof afile)))
      (when-let* ((swap (ps/blank-lines--maybe-swap-body
                         (ps/blank-lines-file-preamble wfile)
                         (ps/blank-lines-file-preamble afile))))
        (record "<file>" 'preamble
                (length (ps/blank-lines-file-preamble wfile)) (length swap)
                'body-swap "preamble blank lines restored")
        (setf (ps/blank-lines-file-preamble wfile) swap))
      ;; Per-node gaps.
      (dolist (node wnodes)
        (let ((anode (gethash node match))
              (title (ps/blank-lines-node-raw-value node)))
          ;; lead — verbatim from the matched node, or left alone.
          (if anode
              (progn
                (record title 'lead (ps/blank-lines-node-lead node)
                        (ps/blank-lines-node-lead anode) 'verbatim
                        "from the same node in the ancestor")
                (setf (ps/blank-lines-node-lead node)
                      (ps/blank-lines-node-lead anode)))
            (record title 'lead (ps/blank-lines-node-lead node)
                    (ps/blank-lines-node-lead node) 'unmatched "new node; left as is"))
          ;; body — whole-body swap only.
          (when anode
            (when-let* ((swap (ps/blank-lines--maybe-swap-body
                               (ps/blank-lines-node-body node)
                               (ps/blank-lines-node-body anode))))
              (record title 'body (length (ps/blank-lines-node-body node))
                      (length swap) 'body-swap "body blank lines restored")
              (setf (ps/blank-lines-node-body node) swap)))
          ;; sep — the ladder.  A nil sep is structural and stays nil.
          (when (ps/blank-lines-node-sep node)
            (pcase-let ((`(,value ,source ,detail)
                         (ps/blank-lines--resolve-sep
                          node prev match aindex anodes rule strategy)))
              (record title 'sep (ps/blank-lines-node-sep node) value source detail)
              (setf (ps/blank-lines-node-sep node) value)))
          (setq prev node))))
    (nreverse changes)))

;;; Public API

(cl-defun ps/blank-lines-propose (wtext atext &key rule (strategy nil strategy-p))
  "Propose WTEXT with ATEXT's blank lines.  Pure.

Returns a plist:
  :ok        t when the proposal is safe to use
  :error     `parse-working' / `parse-ancestor' / `strip-invariant' when not
  :text      the proposed text
  :restored  blank lines added
  :removed   blank lines taken away
  :changes   list of `ps/blank-lines-change', for provenance in the summary"
  (let ((strategy (if strategy-p strategy ps/blank-lines-new-edge-strategy))
        (wfile (and (stringp wtext) (ps/blank-lines-parse wtext)))
        (afile (and (stringp atext) (ps/blank-lines-parse atext))))
    (cond
     ((null wfile) (list :ok nil :error 'parse-working :text wtext))
     ((null afile) (list :ok nil :error 'parse-ancestor :text wtext))
     (t
      (let* ((match (ps/blank-lines-match wfile afile))
             (changes (ps/blank-lines-resolve wfile afile match rule strategy))
             (text (ps/blank-lines-render wfile))
             (deltas (mapcar (lambda (c)
                               (- (ps/blank-lines-change-to c)
                                  (ps/blank-lines-change-from c)))
                             (seq-remove
                              (lambda (c) (memq (ps/blank-lines-change-slot c)
                                                '(body preamble)))
                              changes))))
        (if (not (ps/blank-lines-strip-equal-p text wtext))
            ;; Unreachable by construction; kept because "unreachable" is a
            ;; claim about code, and this is the last place to catch it being
            ;; wrong before a proposal reaches the user.
            (list :ok nil :error 'strip-invariant :text wtext :changes changes)
          (list :ok t
                :text text
                :restored (apply #'+ 0 (seq-filter (lambda (d) (> d 0)) deltas))
                :removed (- (apply #'+ 0 (seq-filter (lambda (d) (< d 0)) deltas)))
                :changes changes)))))))

(defun ps/blank-lines-score-recoverable (wtext atext)
  "Return how many blank lines ATEXT would restore in WTEXT, from memory only.

Used to pick between ancestor candidates, so it deliberately ignores the
fitted rule: a candidate should win on what it remembers, not on what the
corpus-wide rule would have guessed anyway."
  (let ((result (ps/blank-lines-propose wtext atext :strategy 'zero)))
    (if (plist-get result :ok) (plist-get result :restored) 0)))

(provide 'ps-blank-lines-engine)
;;; ps-blank-lines-engine.el ends here
