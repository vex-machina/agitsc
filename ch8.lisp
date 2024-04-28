; 8.2
(defun my-anyoddp (l)
  (if (null l) nil
               (if (oddp (car l)) t
                                  (my-anyoddp (cdr l)))))

; 8.4
(defun laugh (n)
  (cond ((zerop n) nil)
        (t (append '(HA)
                   (laugh (- n 1))))))

; 8.5
(defun add-up (l)
  (cond ((null l) 0)
        (t (+ (car l)
              (add-up (cdr l))))))

; 8.6
(defun alloddp (l)
  (cond ((null l) t)
        ((evenp (car l)) nil)
        (t (alloddp (cdr l)))))
; 8.7
(defun rec-member (e l)
  (cond ((null l) nil)
        ((equal e (car l)) t)
        (t (rec-member e (cdr l)))))

; 8.8
(defun rec-assoc (k l)
  (cond ((null l) nil)
        ((equal k (caar l)) (car l))
        (t (rec-assoc k (cdr l)))))

; 8.9
(defun rec-nth (n l)
  (cond ((null l) nil)
        ((zerop n) (car l))
        (t (rec-nth (- n 1) (cdr l)))))

; 8.10
(defun add1 (n) (+ n 1))

(defun sub1 (n) (- n 1))

(defun rec-plus (x y)
  (cond ((zerop x) y)
        ((zerop y) x)
        (t (add1 (rec-plus x (sub1 y))))))

; 8.15
(defun inf-rec (x) (inf-rec x))

; 8.17
(defun find-first-odd (l)
  (cond ((null l) nil)
        ((oddp (car l)) (car l))
        (t (find-first-odd (cdr l)))))

; 8.18
(defun last-element (l)
  (cond ((null (cdr l)) (car l))
        (t (last-element (cdr l)))))

; 8.21
(defun add-nums (n)
  (cond ((zerop n) 0)
        (t (+ n (add-nums (- n 1))))))

; 8.22
(defun all-equal (l)
  (cond ((null (cdr l)) t)
        ((not (equal (first l) (second l)))
            nil)
        (t (all-equal (cdr l)))))

; 8.24
(defun count-down (n)
  (cond ((zerop n) nil)
        (t (cons n (count-down (- n 1))))))

; 8.27
(defun square-list (l)
  (cond ((null l) nil)
        (t (cons (expt (car l) 2)
                 (square-list (cdr l))))))

; 8.28
(defun my-nth (n l)
  (cond ((null l) nil)
        ((zerop n) (car l))
        (t (my-nth (- n 1) (cdr l)))))

; 8.29
(defun my-member (e l)
  (cond ((null l) nil)
        ((equal e (car l)) t)
        (t (my-member e (cdr l)))))

; 8.30
(defun my-assoc (k l)
  (cond ((null l) nil)
        ((equal k (caar l)) (car l))
        (t (my-assoc k (cdr l)))))

; 8.31
(defun compare-lengths (l1 l2)
  (cond ((and (null l1) (null l2)) '(SAME LENGTH))
        ((null l1) '(SECOND IS LONGER))
        ((null l2) '(FIRST IS LONGER))
        (t (compare-lengths (cdr l1) (cdr l2)))))

; 8.32 
(defun sum-numeric-elements (l)
  (cond ((null l) 0)
        ((numberp (car l)) (+ (car l) (sum-numeric-elements (cdr l))))
        (t (sum-numeric-elements (cdr l)))))

; 8.33
(defun my-remove (e l)
  (cond ((null l) nil)
        ((equal e (car l)) (my-remove e (cdr l)))
        (t (cons (car l) (my-remove e (cdr l))))))

; 8.34
(defun my-intersection (l1 l2)
  (cond ((or (null l1) (null l2))
           nil)
        ((member (car l1) l2) (cons (car l1) (my-intersection (remove (car l1 ) (cdr l1)) l2)))
        (t (my-intersection (cdr l1) l2))))

; 8.35
(defun my-set-difference (l1 l2)
  (cond ((null l2) l1)
        (t (my-set-difference (remove (car l2) l1) (cdr l2)))))

; 8.36
(defun count-odd (l)
  (cond ((null l) 0)
        ((oddp (car l)) (add1 (count-odd (cdr l))))
        (t (count-odd (cdr l)))))

; 8.39
(defun count-atoms (l)
  (cond ((null l) 1)
        ((atom (car l)) (add1 (count-atoms (cdr l))))
        (t (+ (count-atoms (car l))
              (count-atoms (cdr l))))))

; 8.40
(defun count-cons (l)
  (cond ((null l) 0)
        ((atom (car l)) (add1 (count-cons (cdr l))))
        (t (add1 (+ (count-cons (car l)) (count-cons (cdr l)))))))

; 8.41
(defun sum-tree (l)
  (cond ((null l) 0)
        ((consp (car l)) (+ (sum-tree (car l))
                            (sum-tree (cdr l))))
        ((numberp (car l)) (+ (car l)
                              (sum-tree (cdr l))))
        (t (sum-tree (cdr l)))))

; 8.42
(defun my-subst (e s l)
  (cond ((null l) nil)
        ((eq e s) l)
        ((consp (car l)) (cons (my-subst e s (car l))
                               (my-subst e s (cdr l))))
        ((equal e (car l)) (cons s (my-subst e s (cdr l))))
        (t (cons (car l) (my-subst e s (cdr l))))))

; 8.43
(defun flatten (l)
  (cond ((null l) nil)
        ((consp (car l)) (append (flatten (car l))
                                 (flatten (cdr l))))
        (t (append (list (car l)) (flatten (cdr l))))))

; 8.44
(defun tree-depth (l)
  (cond ((atom l) 0)
        (t (+ 1 (max (tree-depth (car l)) (tree-depth (cdr l)))))))

; 8.45
(defun paren-depth-aux (l acc)
  (cond ((null l) acc)
        ((atom (car l)) (paren-depth-aux (cdr l) acc))
        (t (max (paren-depth-aux (car l) (add1 acc))
                (paren-depth-aux (cdr l) acc)))))

(defun paren-depth (l)
  (paren-depth-aux l 1))

; 8.46
(defun count-up (n)
  (cond ((zerop n) nil)
        (t (append (count-up (- n 1))
                   (list n)))))

; 8.47
(defun make-loaf (n)
  (if (zerop n) nil
                (append (make-loaf (- n 1)) (list 'X))))

; 8.48
(defun bury (s n)
  (cond ((zerop n) s)
        (t (list (bury s (- n 1))))))

; 8.49
(defun pairings (l1 l2)
  (cond ((null l1) nil)
        (t (cons (list (car l1) (car l2))
                 (pairings (cdr l1) (cdr l2))))))

; 8.50
(defun sublists (l)
  (cond ((null l) nil)
        (t (cons l
                 (sublists (cdr l))))))

; 8.51
(defun rec-reverse (acc l)
  (cond ((< acc 0) nil)
        (t (cons (nth acc l)
                 (rec-reverse (- acc 1) l)))))

(defun my-reverse (l)
  (rec-reverse (- (length l) 1) l))

; 8.52
(defun my-union (l1 l2)
  (cond ((null l1) l2)
        (t (append (list (car l1))
                   (my-union (cdr l1)
                             (remove (car l1) l2))))))

; 8.53
(defun largest-even-aux (l acc)
  (cond ((null l) acc)
        ((oddp (car l)) (largest-even-aux (cdr l) acc))
        (t (largest-even-aux (cdr l) (max acc (car l))))))

(defun largest-even (l)
  (largest-even-aux l 0))

; 8.54
(defun huge-aux (n base acc)
  (cond ((zerop n) acc)
        (t (huge-aux (sub1 n)
                     base
                     (* base acc)))))

(defun huge (n)
  (huge-aux n n 1))

; 8.56
(defun eo-aux (l idx acc)
  (cond ((null l) acc)
        ((oddp idx) (eo-aux (cdr l) (add1 idx) acc))
        (t (eo-aux (cdr l) (add1 idx) (append acc (list (car l)))))))

(defun every-other (l)
  (eo-aux l 0 nil))

; 8.57
(defun lh-aux (l idx stop acc)
  (cond ((equal idx stop) acc)
        (t (lh-aux (cdr l)
                   (add1 idx)
                   stop
                   (append acc (list (car l)))))))

(defun left-half (l)
  (let* ((len (length l))
         (half (round (/ len 2))))
        (lh-aux l 0 half nil)))

; 8.58
(defun ml-aux (l1 l2 acc)
  (cond ((and (null l1) (null l2)) acc)
        ((null l1) (append acc l2))
        ((null l2) (append acc l1))
        ((< (car l1) (car l2)) (ml-aux (cdr l1) l2 (append acc (list (car l1)))))
        (t (ml-aux l1 (cdr l2) (append acc (list (car l2)))))))

(defun merge-lists (l1 l2)
  (ml-aux l1 l2 nil))

; 8.60
(setf family
     '((colin nil nil) (deirdre nil nil) (arthur nil nil)
       (kate nil nil) (frank nil nil) (linda nil nil)
       (suzanne colin deirdre) (bruce arthur kate) (charles arthur kate)
       (david arthur kate) (ellen arthur kate) (george frank linda)
       (hillary frank linda) (andre nil nil) (tamara bruce suzanne)
       (vincent bruce suzanne) (wanda nil nil) (ivan george ellen)
       (julie george ellen) (marie george ellen) (nigel andre hillary)
       (frederick nil tamara) (zelda vincent wanda) (joshua ivan wanda)
       (quentin nil nil) (robert quentin julie) (olivia nigel marie)
       (peter nigel marie) (erica nil nil) (yvette robert zelda)
       (diane peter erica)))

(defun father (p)
  (second (assoc p family)))

(defun mother (p)
  (third (assoc p family)))

(defun parents (p)
  (remove-if (lambda (x) (null x)) (rest (assoc p family))))

(defun children (p)
  (let ((ch-kv (remove-if-not #'(lambda (x) (member p (cdr x))) family)))
     (cond ((null p) nil)
           (t (mapcar #'(lambda (x) (first x)) ch-kv)))))

(defun siblings (p)
  (let ((slings (mapcar #'children (parents p))))
     (remove-if #'(lambda (x) (equal p x))
                (union (first slings) (second slings)))))

(defun mapunion (f ls)
  (cond ((null ls) nil)
        (t (reduce #'union (mapcar f ls)))))

(defun grandparents (p)
  (mapunion #'parents (parents p)))

(defun aunts-uncles (p)
  (remove-if #'(lambda (x) (null x))
             (mapunion #'siblings (parents p))))

(defun cousins (p)
  (mapunion #'children (aunts-uncles p)))

(defun remove-nil (l)
  (remove-if #'(lambda (x) (null x)) l))

(defun df-aux (a b acc)
  (cond ((null acc) nil)
        ((member b acc) t)
        (t (df-aux a b (remove-nil (mapunion #'parents acc))))))

(defun descended-from (a b)
  (df-aux a b (parents a)))

(defun anc-aux (buf acc)
  (cond ((null buf) acc)
        (t (anc-aux (remove-nil (mapunion #'parents buf))
                    (append acc buf)))))

(defun ancestors (p)
  (anc-aux (parents p) nil))

(defun ggap-aux (a buf acc)
  (cond ((null buf) nil)
        ((member a buf) acc)
        (t (ggap-aux a
                     (remove-nil (mapunion #'parents buf))
                     (add1 acc)))))

(defun generation-gap (d a)
  (ggap-aux a (parents d) 1))

; 8.61
(defun cu-aux (n acc)
  (cond ((zerop n) acc)
        (t (cu-aux (sub1 n) (cons n acc)))))

(defun tr-count-up (n)
  (cu-aux n nil))

; 8.62
(defun fact-aux (n acc)
  (cond ((zerop n) acc)
        (t (fact-aux (sub1 n) (* n acc)))))

(defun tr-fact (n)
  (fact-aux n 1))

; 8.63
(defun shortlist (l1 l2)
  (if (< (length l1) (length l2)) l1 l2))

(defun uni-aux (loopl reml acc)
  (cond ((null loopl) (append acc reml))
        ((member (car loopl) reml) (uni-aux (cdr loopl) (remove (car loopl) reml) (append acc (list (car loopl)))))
        (t (uni-aux (cdr loopl) reml (append acc (list (car loopl)))))))

(defun tr-union (s1 s2)
  (let* ((slist (shortlist s1 s2))
        (llist (if (equal s1 slist) s2 s1)))
    (uni-aux llist slist nil)))

(defun inters-aux (loop-ls cons-ls acc)
  (cond ((null loop-ls) acc)
        ((member (car loop-ls) cons-ls) (inters-aux (cdr loop-ls)
                                                    cons-ls
                                                    (append acc (list (car loop-ls)))))
        (t (inters-aux (cdr loop-ls) cons-ls acc))))

(defun tr-intersection (s1 s2)
  (let* ((slist (shortlist s1 s2))
        (llist (if (equal s1 slist) s2 s1)))
    (inters-aux llist slist nil)))

(defun sd-aux (s1 s2 acc)
  (cond ((null s1) acc)
        ((member (car s1) s2) (sd-aux (cdr s1) s2 acc))
        (t (sd-aux (cdr s1) s2 (append acc (list (car s1)))))))

(defun tr-set-difference (s1 s2)
  (sd-aux s1 s2 nil))

; 8.64
(defun tree-find-if (pred ls)
  (find-if pred (flatten ls)))

; 8.65
(defun tr-count-slices (ls)
  (labels ((cs-aux (ls acc)
              (cond ((null ls) acc)
                    (t (cs-aux (cdr ls) (add1 acc))))))
    (cs-aux ls 0)))

(defun tr-reverse (ls)
  (labels ((rev-aux (ls acc)
             (cond ((null ls) acc)
                   (t (rev-aux (cdr ls) (cons (car ls) acc))))))
    (rev-aux ls nil)))

; 8.66
(setf arith-sym '(+ - * /))

(defun arith-eval (ls)
  (labels ((ae-aux (ls acc)
            (cond ((null ls) (eval acc))
                  ((consp (car ls)) (ae-aux (cdr ls) (append acc (list (ae-aux (car ls) nil)))))
                  ((member (car ls) arith-sym)
                      (ae-aux (cdr ls) (cons (car ls) acc)))
                  (t (ae-aux (cdr ls) (append acc (list (car ls))))))))
    (ae-aux ls nil)))

; 8.67
(defun legalp (ls)
  (labels ((lp-aux (ls)
            (cond ((null ls) t)
                  ((not (or (numberp (car ls))
                            (member (car ls) arith-sym)))
                        nil)
                  (t (lp-aux (cdr ls))))))
    (lp-aux (flatten ls))))
