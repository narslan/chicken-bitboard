;; Language diagram maker

;; Input: BNF-esque grammar description for a single rule, of the form:

;; "...literal string..."
;; (elidable "...literal string...")
;; (seq <rule>...)
;; (choice <rule>...)
;; <rule name>
;; (optional <rule>)
;; (zero-or-more <rule>)
;; (one-or-more <rule>)
;; (comment "...text..." <rule>)

;; Output: A diagram object.

;; All Diagram objects have properties x, y, width, and height, inheight, and outheight.

;; (x,y) is the coordinates of the top left hand corner, from a top-left
;; origin.

;; Width and height are the proportions of the bounding box.

;; THe diagram can be considered to have an input port on the left
;; hand side, inheight from the top, and an output port on the
;; of the right hand side, outheight from the top.

;; Diagram objects are either:

;; * Literal boxes, which fill their width and height, and have a
;; literal content string and an elidable flag

;; * Rule name boxes, which fill their width and height, and have a
;; rule name content symbol

;; * Optional boxes, which have an embedded diagram object.

;; * one-or-more boxes, which have an embedded diagram object.

;; * zero-or-more boxes, which have an embedded diagram object.

;; * sequence boxes, which have a list of embedded diagram objects, in order, and render them left to right with arrows.

;; * alternate boxes, which have a set of embedded diagram objects, and render them in a vertical stack with fanning arrows.

(use sxpath-lolevel)
(use matchable)
(use extras)
(use data-structures)
(use srfi-69)
(use srfi-1)

;; KEEPING TRACK OF SYMBOLS

(define-record-type symbol-stats
  (make-symbol-stats defined? uses)
  symbol-stats?
  (defined? symbol-defined? (setter symbol-defined?))
  (uses symbol-uses (setter symbol-uses)))

(define *symbols* (make-hash-table))

(define (note-symbol-defined! sym)
  (if (hash-table-exists? *symbols* sym)
      (set! (symbol-defined?
             (hash-table-ref *symbols* sym))
            #t)
      (set! (hash-table-ref *symbols* sym)
            (make-symbol-stats #t 0))))

(define (note-symbol-used! sym)
  (if (hash-table-exists? *symbols* sym)
      (let ((stats (hash-table-ref *symbols* sym)))
        (set! (symbol-uses stats)
              (+ 1 (symbol-uses stats))))
      (set! (hash-table-ref *symbols* sym)
            (make-symbol-stats #f 1))))

(define (report-on-symbols port)
  (let ((stats
         (hash-table-fold
          *symbols*
          (lambda (sym ss acc)
            (let ((defined-used-once (vector-ref acc 0))
                  (defined-used (vector-ref acc 1))
                  (defined-unused (vector-ref acc 2))
                  (undefined-used (vector-ref acc 3)))
              (if (symbol-defined? ss)
                  (cond
                   ((> (symbol-uses ss) 1)
                         (vector defined-used-once
                                 (cons sym defined-used)
                                 defined-unused
                                 undefined-used))
                   ((> (symbol-uses ss) 0)
                         (vector (cons sym defined-used-once)
                                 defined-used
                                 defined-unused
                                 undefined-used))
                   (else (vector defined-used-once
                                 defined-used
                                 (cons sym defined-unused)
                                 undefined-used)))
                  (vector defined-used-once
                          defined-used
                          defined-unused
                          (cons sym undefined-used)))))
          (vector '() '() '() '()))))
    (let ((defined-used-once (vector-ref stats 0))
          (defined-used (vector-ref stats 1))
          (defined-unused (vector-ref stats 2))
          (undefined-used (vector-ref stats 3)))

      (fprintf port "Defined and used repeatedly: ~A\n" defined-used)
      (fprintf port "Defined and used only once: ~A\n" defined-used-once)
      (fprintf port "Defined but not used: ~A\n" defined-unused)
      (fprintf port "Undefined, but used: ~A\n" undefined-used))))

;; GRAPHICS FORMATTING STUFF

(define *debug-mode* #f)

(define *text-cell-height* 20)
(define *text-cell-width* 10)

(define (text-width string)
  (+ *text-cell-width* (* *text-cell-width* (fold
               (lambda (a b)
                 (max (string-length a) b))
               0
               (string-split string "\n" #t))))) ;; FIXME: Learn how to work out real text widths

(define (text-height string)
  (+ (/ *text-cell-height* 2) (* *text-cell-height* (length (string-split string "\n" #t)))))

(define *literal-width* 10)
(define *literal-height* 10)

(define *optional-dw* 20)
(define *optional-dh* 10)
(define *optional-dx* (/ *optional-dw* 2))
(define *optional-dy* *optional-dh*)

(define *one-or-more-dw* 40)
(define *one-or-more-dh* 10)
(define *one-or-more-dx* (/ *one-or-more-dw* 2))
(define *one-or-more-dy* 0)

(define *zero-or-more-dw* 40)
(define *zero-or-more-dh* 20)
(define *zero-or-more-dx* (/ *zero-or-more-dw* 2))
(define *zero-or-more-dy* (/ *zero-or-more-dh* 2))

(define *sequence-left-margin* 10)
(define *sequence-sep* 10)
(define *sequence-right-margin* 10)

(define *choice-left-margin* 20)
(define *choice-right-margin* 20)
(define *choice-top-margin* 0) ; *choice-sep* gets added to this before use
(define *choice-sep* 10)
(define *choice-bottom-margin* 10)
(define *choice-bypass-height* 10)

(define *comment-dw* 10)
(define *comment-dh* 10)
(define *comment-dx* (/ *comment-dw* 2))
(define *comment-dy* (/ *comment-dh* 2))
(define *comment-sep* 5)
(define *comment-margin* 0)

(define *label-width* 200)
(define *label-indent* (/ *label-width* 2))
(define *label-arrow* 10)
(define *label-sep* 10)

;; DIAGRAM OBJECTS

;; These record the structure of the diagram by allocating regions
;; of 2D space to bits of it.
;; inheight and outheight are the heights (relative to diagram-y) of the input and output
;; lines from that piece of diagram, to allow other bits of diagram to line up correctly.

(define-record diagram
  x y width height
  inheight outheight

  type
  content
  elidable?

  children)

;; Virtual accessors give (x,y) of the input and output points

(define (diagram-inx diagram)
  (diagram-x diagram))

(define (diagram-iny diagram)
  (+ (diagram-y diagram) (diagram-inheight diagram)))

(define (diagram-outx diagram)
  (+ (diagram-width diagram) (diagram-x diagram)))

(define (diagram-outy diagram)
  (+ (diagram-y diagram) (diagram-outheight diagram)))

(define (diagram->list diagram)
  (list 'x: (diagram-x diagram)
        'y: (diagram-y diagram)
        'width: (diagram-width diagram)
        'height: (diagram-height diagram)
        'inheight: (diagram-inheight diagram)
        'outheight: (diagram-outheight diagram)
        'type: (diagram-type diagram)
        'content: (diagram-content diagram)
        'elidable?: (diagram-elidable? diagram)
        'children: (map diagram->list (diagram-children diagram))))

;; Move a diagram (and its subdiagrams) by the indicated vector
(define (relocate-diagram diagram dx dy)
  (make-diagram
   (+ (diagram-x diagram) dx)
   (+ (diagram-y diagram) dy)
   (diagram-width diagram)
   (diagram-height diagram)
   (diagram-inheight diagram)
   (diagram-outheight diagram)
   (diagram-type diagram)
   (diagram-content diagram)
   (diagram-elidable? diagram)
   (map (lambda (subdiagram)
          (relocate-diagram subdiagram dx dy))
        (diagram-children diagram))))

;; Type-specific constructors.
;; Each should construct its diagram with x and y equal to 0
;; as parent container constructors will relocate them into the correct
;; positions.

(define (make-terminal type content elidable?)
  (let ((width (+ *literal-width* (text-width content)))
        (height (+ *literal-height*  (text-height content))))
    (make-diagram 0 0 width height
                  (/ height 2) (/ height 2)
                  type
                  content
                  elidable?
                  '())))

(define (make-literal content elidable?)
  (make-terminal 'literal content elidable?))

(define (make-rule content)
  (make-terminal 'rule (symbol->string content) #f))

(define (make-single-embed type diagram dx dy dw dh)
  (let ((width (+ dw (diagram-width diagram)))
        (height (+ dh (diagram-height diagram))))
    (make-diagram
     0 0
     width height
     (+ dy (diagram-inheight diagram)) (+ dy (diagram-outheight diagram))
     type
     #f
     #f
     (list (relocate-diagram diagram dx dy)))))

(define (make-optional diagram)
  (make-single-embed 'optional diagram *optional-dx* *optional-dy* *optional-dw* *optional-dh*))

(define (make-one-or-more diagram)
  (make-single-embed 'one-or-more diagram *one-or-more-dx* *one-or-more-dy* *one-or-more-dw* *one-or-more-dh*))

(define (make-zero-or-more diagram)
  (make-single-embed 'zero-or-more diagram *zero-or-more-dx* *zero-or-more-dy* *zero-or-more-dw* *zero-or-more-dh*))

(define (make-comment text diagram)
  (let ((tw (text-width text))
        (th (text-height text)))
    (make-diagram
     0 0
     (+ *comment-dw* (max (diagram-width diagram) tw))
     (+ (diagram-height diagram) th *comment-dh* *comment-sep*)
     (+ (diagram-inheight diagram) *comment-dy*) (+ (diagram-outheight diagram) *comment-dy*)
     'comment
     text
     #f
     (list (relocate-diagram
            diagram
            (+ *comment-dx* ;; Horizontally center the subdiagram if needed
               (if (> tw (diagram-width diagram))
                   (/ (- tw (diagram-width diagram)) 2)
                   0))
            *comment-dy*)))))

(define (make-sequence diagrams)
  (cond
   ((= (length diagrams) 0) (error "Zero-length sequences make no sense!"))
   ((= (length diagrams) 1) (car diagrams))
   (else
    (let loop
        ((x-offset *sequence-left-margin*)
         (subdiagrams-so-far '())
         (diagrams-to-do diagrams)
         (min-y 0)
         (max-y 0)
         (inheight #f)
         (last-outheight #f)
         (vertical-alignment-offset 0))

      (if (null? diagrams-to-do)
          (make-diagram
           0 0
           (+ *sequence-right-margin* x-offset) (- max-y min-y)
           (- min-y) (- vertical-alignment-offset min-y)
            'sequence
            #f
            #f
            (reverse (map (cut relocate-diagram <> 0 (- min-y)) subdiagrams-so-far)))
          (let* ((diagram (car diagrams-to-do))
                 (vertical-relocation (- vertical-alignment-offset (diagram-inheight diagram)))
                 (diagram*
                  (relocate-diagram diagram
                                    (+ x-offset *sequence-sep*)
                                    vertical-relocation)))

            (loop (+ x-offset *sequence-sep* (diagram-width diagram))
                  (cons diagram* subdiagrams-so-far)
                  (cdr diagrams-to-do)
                  (min min-y vertical-relocation)
                  (max max-y (+ (diagram-height diagram) vertical-relocation))
                  (if inheight inheight (diagram-inheight diagram))
                  (diagram-outheight diagram)
                  (+ (diagram-outheight diagram) vertical-relocation))))))))

(define (make-choice diagrams elidable?)
  (cond
   ((= (length diagrams) 0) (error "Zero-length choices make no sense!"))
   ((= (length diagrams) 1) (car diagrams))
   (else
    (let loop
        ((y-offset (if elidable? (+ *choice-top-margin* *choice-bypass-height*) *choice-top-margin*))
         (subdiagrams-so-far '())
         (diagrams-to-do diagrams)
         (width 0))

      (if (null? diagrams-to-do)
          (make-diagram
           0 0
           (+ *choice-left-margin* *choice-right-margin* width)
           (+ *choice-bottom-margin* y-offset)
           (/ (+ *choice-bottom-margin* y-offset) 2) (/ (+ *choice-bottom-margin* y-offset) 2)
           'choice
           #f
           elidable?
           (reverse subdiagrams-so-far))
          (let* ((diagram (car diagrams-to-do))
                 (diagram*
                  (relocate-diagram diagram
                                    *choice-left-margin*
                                    (+ y-offset *choice-sep*))))

            (loop (+ y-offset *choice-sep* (diagram-height diagram))
                  (cons diagram* subdiagrams-so-far)
                  (cdr diagrams-to-do)
                  (max width (diagram-width diagram)))))))))

;; Rule -> diagram
;; Dispatch on the syntax of rules to create the correct diagram object

(define (rule->diagram rule)
  (cond
   ((string? rule)
    (make-literal rule #f))
   ((symbol? rule)
    (note-symbol-used! rule)
    (make-rule rule))
   ((pair? rule)
    (cond
     ((eq? 'elidable (car rule))
      (if (not (null? (cddr rule))) (error "Too many arguments to (elidable \"literal\"): ~S" rule))
      (make-literal (cadr rule) #t))
     ((eq? 'optional (car rule))
      (if (not (null? (cddr rule))) (error "Too many arguments to (optional <rule>): ~S" rule))
      (make-optional (rule->diagram (cadr rule))))
     ((eq? 'zero-or-more (car rule))
      (if (not (null? (cddr rule))) (error "Too many arguments to (zero-or-more <rule>): ~S" rule))
      (make-zero-or-more (rule->diagram (cadr rule))))
     ((eq? 'one-or-more (car rule))
      (if (not (null? (cddr rule))) (error "Too many arguments to (one-or-more <rule>): ~S" rule))
      (make-one-or-more (rule->diagram (cadr rule))))
     ((eq? 'seq (car rule))
      (make-sequence (map rule->diagram (cdr rule))))
     ((eq? 'choice (car rule))
      (make-choice (map rule->diagram (cdr rule)) #f))
     ((eq? 'optional-choice (car rule))
      (make-choice (map rule->diagram (cdr rule)) #t))
     ((eq? 'comment (car rule))
      (if (not (= (length rule) 3)) (error "Wrong arguments to (comment \"text\" <rule>): ~S" rule))
      (make-comment (cadr rule) (rule->diagram (caddr rule))))
     (else (error "Invalid rule: " rule))))
   (else (error "Invalid rule: " rule))))

;; Rule simplifications:
;;   (s (optional (optional X))) -> (s (optional X))
;;   (s (optional (one-or-more X))) -> (s (zero-or-more X))
;;   (s (optional (choice ...))) -> (s (optional-choice ...))
;;   (s (seq X)) -> (s X)
;;   (s (choice X)) -> (s X)
;;   (s (optional-choice X)) -> (s X)
;;   (s (seq ..(seq X)..)) -> (s (seq ..X..))
;;   (s (choice ..(choice X)..)) -> (s (choice ..X..))
;;   (s (optional-choice ..(optional-choice X)..)) -> (s (optional-choice ..X..))
;;   (s (optional-choice ..(choice X)..)) -> (s (optional-choice ..X..))
;;   (s (choice ..(optional-choice X)..)) -> (s (optional-choice ..X..))
;;   (s (choice ..(optional X)..)) -> (s (optional-choice ..X..))
;;   (s (optional-choice ..(optional X)..)) -> (s (optional-choice ..X..))
;;   else (s X) -> X

(define (simplify-rule rule)
  (match rule
         (('optional ('optional BODY))
          (simplify-rule `(optional ,BODY)))
         (('optional ('one-or-more BODY))
          (simplify-rule `(zero-or-more ,BODY)))
         (('zero-or-more ('optional BODY))
          (simplify-rule `(zero-or-more ,BODY)))
         (('one-or-more ('optional BODY))
          (simplify-rule `(zero-or-more ,BODY)))
         (('optional ('choice . BODY))
          (simplify-rule `(optional-choice ,@BODY)))
         (('seq . BODY)
          (simplify-seq BODY))
         (('choice . BODY)
          (simplify-choice BODY #f))
         (('optional-choice . BODY)
          (simplify-choice BODY #t))
         (else rule)))

(define (simplify-seq rules)
  (cond
   ((= (length rules) 1) ; (seq X) -> (simplify X)
    (simplify-rule (car rules)))
   (else
    (let loop ((rules-to-do rules) ; (seq ...(seq X)...) -> (seq (map simplify ...X...))
               (result '()))
      (if (null? rules-to-do)
          (cons 'seq (reverse result))
          (let ((rule (simplify-rule (car rules-to-do))))
            (if (pair? rule)
                (if (eq? (car rule) 'seq)
                    (loop (append (cdr rule) (cdr rules-to-do)) ; Nested seq
                          result)
                    (loop (cdr rules-to-do) ; Non-seq nested rule
                          (cons rule result)))
                (loop (cdr rules-to-do) ; Non-pair rule
                      (cons rule result)))))))))

(define (simplify-choice rules elidable?)
  (cond
   ((= (length rules) 1) ; (choice X) -> (simplify X)
    (if elidable?
        (simplify-rule (cons 'optional rules))
        (simplify-rule (car rules))))
   (else
    (let loop ((rules-to-do rules) ; (choice ...(choice X)...) -> (choice (map simplify ...X...))
               (result '())
               (final-elidable? elidable?))
      (if (null? rules-to-do)
          (cons (if final-elidable? 'optional-choice 'choice) (reverse result))
          (let ((rule (simplify-rule (car rules-to-do))))
            (if (pair? rule)
                (cond
                 ((eq? (car rule) 'choice) ; Nested choice
                  (loop (append (cdr rule) (cdr rules-to-do))
                        result final-elidable?))
                 ((eq? (car rule) 'optional) ; Nested optional
                  (loop (cons (cadr rule) (cdr rules-to-do))
                        result #t)) ; Result is always elidable
                 ((eq? (car rule) 'optional-choice) ; Nested optional-choice
                  (loop (append (cdr rule) (cdr rules-to-do))
                        result #t)) ; Result is always elidable
                 (else
                  (loop (cdr rules-to-do) ; Non-choice nested rule
                        (cons rule result) final-elidable?)))
                (loop (cdr rules-to-do) ; Non-pair rule
                      (cons rule result) final-elidable?))))))))

;; diagram -> svg fragment rendering logic
;; This is done once the diagram tree has been constructed
;; So the final positions of things are known

(define (make-rect x y w h style)
  `(rect (@ (x ,(number->string x))
            (y ,(number->string y))
            (fill "none")
            (stroke-width "1")
            ,@style
            (width ,(number->string w))
            (height ,(number->string h)))))

(define (make-rounded-blob x y w h style)
  `(rect (@ (x ,(number->string x))
            (y ,(number->string y))
            (rx ,(number->string (/ h 2)))
            (ry ,(number->string (/ h 2)))
            (fill "none")
            (stroke-width "1")
            ,@style
            (width ,(number->string w))
            (height ,(number->string h)))))

;; (x,y) is midpoint of the text
(define (make-text x y* text style)
  (let ((all-lines (string-split text "\n" #t)))
    (let loop ((lines all-lines)
               (y (- y* (- (/ *text-cell-height* 2)) (* *text-cell-height* (/ (length all-lines) 2)))) ;; Vertical centering (hacky)
               (result '()))
      (if (null? lines)
          (cons 'g result) ; Wrap result in an SVG group for neatness
          (loop
           (cdr lines)
           (+ y *text-cell-height*)
           (cons
            `(text (@ (x ,(number->string x))
                      (y ,(number->string y))
                      (text-anchor "middle") ,@style) ,(car lines))
            result))))))

;; coords is a list of things to turn into strings, and append with spaces between
(define (coords-to-path coords)
  (string-join (map ->string coords) " "))

(define (make-line . coords)
  `(path (@ (d ,(coords-to-path coords))
            (stroke-width "1")
            (stroke "black")
            (fill "none"))))

;; (bx,by) is the base of the arrowhead
;; (dx,dy) is the vector from there to the tip of the arrowhead
;; w is the ratio of one half the width of the arrowhead to the length; eg, w = 0.5 for
;; a square bounding box, and smaller for a sharper arrow.
(define (make-arrow bx by dx dy w)
  (let* ((pdx (* w (- dy)))
         (pdy (* w dx))
         (qdx (* w dy))
         (qdy (* w (- dx)))
         (px (+ bx pdx))
         (py (+ by pdy))
         (qx (+ bx qdx))
         (qy (+ by qdy))
         (tx (+ bx dx))
         (ty (+ by dy)))
  `(path (@ (d ,(coords-to-path
                 (list
                  'M tx ty
                  'L px py
                  'L qx qy
                  'z)))
            (fill "black")))))

(define (make-group . paths)
  (list (cons 'g (append paths))))

(define (literal->svg* diagram)
  (make-group
   (make-rect (diagram-x diagram) (diagram-y diagram)
              (diagram-width diagram) (diagram-height diagram)
              (if (diagram-elidable? diagram)
                  '((stroke "grey") (stroke-dasharray "5,5"))
                  '((stroke "black"))))
   (make-text (+ (diagram-x diagram) (/ (diagram-width diagram) 2))
              (+ (diagram-y diagram) (/ (diagram-height diagram) 2))
              (diagram-content diagram) '((font-weight "bold") (font-family "monospace") (font-size "16")))))

(define (rule->svg* diagram)
  (make-group
   (make-rounded-blob (diagram-x diagram) (diagram-y diagram)
              (diagram-width diagram) (diagram-height diagram)
              '((stroke "black")))
   (make-text (+ (diagram-x diagram) (/ (diagram-width diagram) 2))
              (+ (diagram-y diagram) (/ (diagram-height diagram) 2))
              (diagram-content diagram) '((font-style "italic")))))

;; This function is highly useful in laying out graphics...
(define (mid a b) (/ (+ a b) 2))

;; Standard template for diagrams that embed a single diagram
;; with some kind of bordering
(define (single->svg* diagram extra-paths)
  (let* ((child (car (diagram-children diagram)))

         ;; Calculate useful points around the border between the
         ;; child and the parent.
         ;; All these points are halfway between child and parent, in
         ;; different places.

         ; Midpoint of in connector line
         (ix (mid (diagram-inx diagram) (diagram-inx child)))
         (iy (mid (diagram-iny diagram) (diagram-iny child)))

         ; Point to left of top left of child
         (p1x ix)
         (p1y (diagram-y child))

         ; Point above top left of child
         (p2x (diagram-x child))
         (p2y (mid (diagram-y diagram) (diagram-y child)))

         ; Point above top right of child
         (p3x (diagram-outx child))
         (p3y p2y)

         ; Point to right of top right of child
         (p4x (mid (diagram-outx child) (diagram-outx diagram)))
         (p4y (diagram-y child))

         ; Midpoint of out connector line
         (ox p4x)
         (oy (mid (diagram-outy child) (diagram-outy diagram)))

         ; Point to right of bottom right of child
         (p5x p4x)
         (p5y (+ (diagram-y child) (diagram-height child)))

         (bottom-y (+ (diagram-y diagram) (diagram-height diagram)))

         ; Point below bottom right of child
         (p6x p3x)
         (p6y (mid p5y bottom-y))

         ; Point below bottom left of child
         (p7x (diagram-inx child))
         (p7y p6y)

         ; Point to left of bottom left of child
         (p8x ix)
         (p8y p5y))
    (make-group
     (list
      ; In connector line
      (make-line 'M (diagram-inx diagram) (diagram-iny diagram)
                 'L (diagram-inx child) (diagram-iny child))

      ; Out connector line
      (make-line 'M (diagram-outx child) (diagram-outy child)
                 'L (diagram-outx diagram) (diagram-outy diagram)))

      ; Type-specific paths
     (extra-paths ix iy p1x p1y p2x p2y p3x p3y p4x p4y ox oy p5x p5y p6x p6y p7x p7y p8x p8y)

     (diagram->svg* child))))

(define (optional->svg* diagram)
  (single->svg*
   diagram
   (lambda (ix iy p1x p1y p2x p2y p3x p3y p4x p4y ox oy p5x p5y p6x p6y p7x p7y p8x p8y)
     (list
      ; Bypass path
      (make-line 'M (diagram-inx diagram) (diagram-iny diagram)
                 'Q ix iy p1x p1y
                 'Q p1x p2y p2x p2y
                 'L p3x p3y
                 'Q p4x p3y p4x p4y
                 'Q ox oy (diagram-outx diagram) (diagram-outy diagram))

      ;; Bypass arrow
      (make-arrow (mid p2x p3x)
                  p3y
                  (+ (/ *optional-dy* 2))
                  0
                  0.5)))))

(define (one-or-more->svg* diagram)
  (let* ((child (car (diagram-children diagram))))
    (single->svg*
     diagram
     (lambda (ix iy p1x p1y p2x p2y p3x p3y p4x p4y ox oy p5x p5y p6x p6y p7x p7y p8x p8y)
       (list
        ; Loopback path
        (make-line 'M (diagram-inx child) (diagram-iny child)
                   'Q ix iy p8x p8y
                   'Q p8x p7y p7x p7y
                   'L p6x p6y
                   'Q p5x p6y p5x p5y
                   'Q ox oy (diagram-outx child) (diagram-outy child))

        ; Loopback arrow
        (make-arrow (mid p7x p6x)
                    p7y
                    (- (/ *zero-or-more-dy* 2))
                    0
                    0.5))))))


(define (zero-or-more->svg* diagram)
  (let* ((child (car (diagram-children diagram))))
    (single->svg*
     diagram
     (lambda (ix iy p1x p1y p2x p2y p3x p3y p4x p4y ox oy p5x p5y p6x p6y p7x p7y p8x p8y)
       (list
        ; Bypass path
        (make-line 'M (diagram-inx diagram) (diagram-iny diagram)
                   'Q ix iy p1x p1y
                   'Q p1x p2y p2x p2y
                   'L p3x p3y
                   'Q p4x p3y p4x p4y
                   'Q ox oy (diagram-outx diagram) (diagram-outy diagram))
        ; Bypass arrow
        (make-arrow (mid p2x p3x)
                    p3y
                    (+ (/ *optional-dy* 2))
                    0
                    0.5)
        ; Loopback path
        (make-line 'M (diagram-inx child) (diagram-iny child)
                   'Q ix iy p8x p8y
                   'Q p8x p7y p7x p7y
                   'L p6x p6y
                   'Q p5x p6y p5x p5y
                   'Q ox oy (diagram-outx child) (diagram-outy child))

        ; Loopback arrow
        (make-arrow (mid p7x p6x)
                    p7y
                    (- (/ *zero-or-more-dy* 2))
                    0
                    0.5))))))

(define (sequence->svg* diagram)
  (let loop ((lx (diagram-inx diagram))
             (ly (diagram-iny diagram))
             (children (diagram-children diagram))
             (paths-so-far '()))
    (if (null? children)
        (make-group (cons
                     (make-line 'M lx ly
                                'L (diagram-outx diagram) (diagram-outy diagram))
                     paths-so-far))
        (let* ((child (car children)))
          (loop (diagram-outx child) (diagram-outy child)
                (cdr children)
                (append (cons (make-line
                               'M lx ly
                               'L (diagram-inx child) (diagram-iny child))
                              (diagram->svg* child))
                        paths-so-far))))))

(define (choice->svg* diagram)
  (let loop ((children (diagram-children diagram))
             (paths-so-far
              (if (diagram-elidable? diagram)
                   (let* ((bypass-inx (+ (diagram-x diagram) *choice-left-margin*))
                          (bypass-iny (+ (diagram-y diagram) *choice-top-margin* (/ *choice-bypass-height* 2)))
                          (bypass-outx (+ (diagram-x diagram) (diagram-width diagram) (- *choice-right-margin*)))
                          (bypass-outy bypass-iny)
                          (midix (mid (diagram-inx diagram) bypass-inx))
                          (midiy (mid (diagram-iny diagram) bypass-iny))
                          (midox (mid bypass-outx (diagram-outx diagram)))
                          (midoy (mid bypass-outy (diagram-outy diagram))))
                     (make-group
                      ; Bypass line
                      (make-line 'M (diagram-inx diagram) (diagram-iny diagram)
                                 'Q midix (diagram-iny diagram) midix midiy
                                 'Q midix bypass-iny bypass-inx bypass-iny
                                 'L bypass-outx bypass-outy
                                 'Q midox bypass-outy midox midoy
                                 'Q midox (diagram-outy diagram) (diagram-outx diagram) (diagram-outy diagram))
                      (make-arrow (mid bypass-inx bypass-outx)
                                  bypass-iny
                                  (/ *choice-bypass-height* 2)
                                  0
                                  0.5)))
                  '())))
    (if (null? children)
        (make-group paths-so-far)
        (let* ((child (car children))
               (midix (mid (diagram-inx diagram) (diagram-inx child)))
               (midiy (mid (diagram-iny diagram) (diagram-iny child)))
               (joinox (+ (diagram-x diagram) (diagram-width diagram) (- *choice-right-margin*)))
               (joinoy (diagram-outy child))
               (midox (mid joinox (diagram-outx diagram)))
               (midoy (mid joinoy (diagram-outy diagram))))
          (loop (cdr children)
                (append
                 (list
                  ; Input line
                  (make-line 'M (diagram-inx diagram) (diagram-iny diagram)
                             'Q midix (diagram-iny diagram) midix midiy
                             'Q midix (diagram-iny child) (diagram-inx child) (diagram-iny child))

                  ; Output line
                  (make-line 'M (diagram-outx child) (diagram-outy child)
                             'L joinox joinoy
                             'Q midox joinoy midox midoy
                             'Q midox (diagram-outy diagram) (diagram-outx diagram) (diagram-outy diagram)))
                 (diagram->svg* child)
                 paths-so-far))))))

(define (comment->svg* diagram)
  (single->svg*
   diagram
   (lambda (ix iy p1x p1y p2x p2y p3x p3y p4x p4y ox oy p5x p5y p6x p6y p7x p7y p8x p8y)
     (list
      ; Dotted box
      (make-rect (+ (diagram-x diagram) *comment-margin*) (+ (diagram-y diagram) *comment-margin*)
                 (- (diagram-width diagram)(* *comment-margin* 2) ) (- (diagram-height diagram) (* *comment-margin* 2))
                 '((stroke-dasharray "1,3") (stroke "black")))
      ; Text
      (make-text (+ (diagram-x diagram) (/ (diagram-width diagram) 2))
                 (+ p8y *comment-sep* (/ (text-height (diagram-content diagram)) 2))
                 (diagram-content diagram) '())))))

(define (diagram->svg* diagram)
  (append
   (list
    (if *debug-mode*
        (make-rect (diagram-x diagram) (diagram-y diagram)
                   (diagram-width diagram) (diagram-height diagram)
                   '((stroke "pink")))
        '()))
   (case (diagram-type diagram) ;; Eargh what was I THINKING? Too much C programming.
     ((literal) (literal->svg* diagram)) ;; Dude, seriously, just put a ->svg* closure in the diagram record...
     ((rule) (rule->svg* diagram))
     ((optional) (optional->svg* diagram))
     ((one-or-more) (one-or-more->svg* diagram))
     ((zero-or-more) (zero-or-more->svg* diagram))
     ((sequence) (sequence->svg* diagram))
     ((choice) (choice->svg* diagram))
     ((comment) (comment->svg* diagram))
     (else (error "Unknown diagram node type ~S in ~S" (diagram-type diagram) (diagram->list diagram))))))

;; Wrap an SVG fragment into a complete document

(define (wrap-svg width height paths)
  `(svg (@ (xmlns "http://www.w3.org/2000/svg")
           (version "1.2")
           (baseProfile "tiny")
           (viewBox ,(sprintf "0 0 ~S ~S" width height)))
        ,@paths))

;; Render a ruleset
(define (diagrams->svg diagrams)
  ;; diagrams is an alist mapping rule names to diagrams
  ;; We render them in a vertical stack, with labels to the left
  (let loop ((y-offset 0)
             (diagrams-to-do diagrams)
             (paths-so-far '())
             (max-width 0))
    (if (null? diagrams-to-do)
        (wrap-svg (+ *label-width* max-width *label-arrow*) (- y-offset *label-sep*) paths-so-far)
        (let* ((diagram-pair (car diagrams-to-do))
               (diagram-name (car diagram-pair))
               (diagram (cdr diagram-pair)))
          (loop
           (+ y-offset (diagram-height diagram) *label-sep*)
           (cdr diagrams-to-do)
           (append
            (make-group
             (list
                                        ; Label
              (make-text *label-indent* (+ y-offset (/ (diagram-height diagram) 2))
                         (symbol->string diagram-name) '((font-style "italic")))
                                        ; Input
              (make-arrow
               (- *label-width* *label-arrow*) (+ y-offset (diagram-iny diagram))
               *label-arrow* 0 0.5)
                                        ; Output
              (make-arrow
               (+ *label-width* (diagram-outx diagram)) (+ y-offset (diagram-outy diagram))
               *label-arrow* 0 0.5))
             (diagram->svg* (relocate-diagram diagram *label-width* y-offset)))
            paths-so-far)
           (max max-width (diagram-width diagram)))))))

;; Render a single rule
(define (diagram->svg diagram)
  (wrap-svg (diagram-width diagram) (diagram-height diagram) (diagram->svg* diagram)))

;; Test the rule simplifier

(define simplification-examples
  '((rule . rule)
    ("literal" . "literal")
    ((elidable "literal") . (elidable "literal"))
    ((optional "literal") . (optional "literal"))
    ((optional (optional "literal")) . (optional "literal"))
    ((optional (optional (optional "literal"))) . (optional "literal"))
    ((optional (one-or-more "literal")) . (zero-or-more "literal"))
    ((choice a (choice b c (choice (optional d))) (choice e)) . (optional-choice a b c d e))
))

(define (test-simplifier)
  (map (lambda (example)
         (let* ((input (car example))
                (expected-output (cdr example))
                (output (simplify-rule input)))
           (if (not (equal? output expected-output))
               (error (printf "Simplifer failure: ~S -> ~S (wanted ~S)\n" input output expected-output)))))
       simplification-examples))

;; Rendering Driver

(define (render-single-rule rule)
  (let* ((simplified-rule (simplify-rule rule))
         (diagram (rule->diagram simplified-rule))
         (svg (diagram->svg diagram)))
    (map display (flatten (sxml:sxml->xml svg)))))

(define (render-ruleset rules)
  (let* ((diagrams
          (map
           (lambda (rule-binding)
             (let ((rule-name (car rule-binding))
                   (rule-body (cdr rule-binding)))
               (note-symbol-defined! rule-name)
               (cons rule-name (rule->diagram (simplify-rule rule-body))))) rules))
         (svg (diagrams->svg diagrams)))
    (map display (flatten (sxml:sxml->xml svg))))
  (report-on-symbols (current-error-port)))

;; Command line interface

(define (main args)
  (match args
         (("-d" . rest)
          (set! *debug-mode* #t)
          (main rest))
         (("rule")
          (render-single-rule (read)))
         (("ruleset")
          (render-ruleset (read-file)))
         (else (fprintf (current-error-port)
                        "Usage: banterpixra [-d] {rule|ruleset} < input > output.svg\n
\t-d: Enable debug mode (adds pink boxes showing the layout model)\n
\trule: Render a single rule\n
\truleset: Render a set of rules\n
\n
Rule syntax:\n
\t\"literal\"
\tsymbol
\t(elidable \"literal\")
\t(optional <rule>)
\t(one-or-more <rule>)
\t(zero-or-more <rule>)
\t(seq <rule> <rule>...)
\t(choice <rule> <rule>...)
\t(optional-choice <rule> <rule>...)
\n
banterpixra will do some basic simplifications of the input rule.\n
\n
Ruleset syntax:
\t(label . <rule>)...\n"))))

(main (command-line-arguments))
