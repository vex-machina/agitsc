;; CH7 SOLUTIONS

; 7.1
(defun add1 (x)
  (funcall #'+ 1 x))

; 7.2
(setf DAILY-PLANET
        '((olsen jimmy 123-76-4535 cub-reporter)
          (kent clark 089-52-6787 reporter)
          (lane lois 951-26-1438 reporter)
          (white perry 355-16-7439 editor)))

(defun ssn (l)
  (mapcar #'third l))

; 7.3
(defun zero-map ()
  (mapcar #'zerop '(2 0 3 4 0 -5 6)))

; 7.4
(defun greater-than-fivep (x)
  (funcall #'> x 5))

(defun five-map (l)
  (mapcar #'greater-than-fivep l))

; 7.5
(lambda (x) (- x 7))

; 7.6
(lambda (x)
  (or (equal x nil)
      (equal x t)))

; 7.7
(defun flip-list (l)
  (mapcar
    #'(lambda (s)
        (if (equal s 'UP) 'DOWN 'UP))
    l))

; 7.8
(defun within-range (x k)
  (mapcar
    #'(lambda (a)
        (and (>= a (- k 10))
             (<= a (+ k 10))))
    x))

; 7.9
(defun find-nested (l)
  (find-if (lambda (x) (consp x))
           l))

; 7.10

; a.
(setf note-table
      '((c 1) (c-sharp 2) (d 3) (d-sharp 4) (e 5) (f 6)
        (f-sharp 7) (g 8) (g-sharp 9) (a 10) (a-sharp 11)
        (b 12)))

; b.
(defun numbers (l)
  (mapcar #'(lambda (x) (cadr (assoc x note-table)))
          l))

; c.
(defun notes (l)
  (let ((rev-note-table
          (mapcar #'(lambda (e) (reverse e))
                  note-table)))
    (mapcar #'(lambda (x) (cadr (assoc x rev-note-table)))
            l)))

; e.
(defun raise (n l)
  (mapcar #'(lambda (x) (+ n x))
          l))

; f.
(defun normalize (l)
  (mapcar #'(lambda (n) (mod n 12))
          l))

; g.
(defun transpose (n s)
  (notes (normalize (raise n (numbers s)))))

; 7.11
(defun one-five (l)
  (remove-if (lambda (x)
               (and (> x 1)
                    (< x 5)))
             l))

; 7.12
(defun the-count (s)
  (length (remove-if-not (lambda (x) (equal x 'THE)) s)))

; 7.13
(defun rem-len-two (l)
  (remove-if (lambda (x) (equal 2 (length x)))
             l))

; 7.14
(defun my-intersection (s1 s2)
  (remove-if-not (lambda (x) (member x s1))
                 s2))

(defun my-union (s1 s2)
  (append (remove-if (lambda (x) (member x s2))
                     s1)
          s2))

; 7.15

; a.
(defun rank (l)
  (car l))

(defun suit (l)
  (cadr l))

; b.
(setf my-hand
        '((3 hearts)
          (5 clubs)
          (2 diamonds)
          (4 diamonds)
          (ace spades)))

(defun count-suit (s h)
  (length (remove-if-not (lambda (x) (equal s (suit x)))
                         h)))

; c.
(setf colors
        '((clubs black)
          (diamonds red)
          (hearts red)
          (spades black)))

(defun color-of (card)
  (cadr (assoc (suit card) colors)))

; d.
(defun first-red (h)
  (find-if (lambda (x) (equal 'red (color-of x)))
            h))

; e.
(defun black-cards (h)
  (remove-if-not (lambda (x) (equal 'black (color-of x)))
                  h))

; f.
(defun what-ranks (s h)
  (mapcar #'car (remove-if-not (lambda (x)
                                   (equal s (suit x)))
                               h)))

; g.
(setf all-ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun beforep (x y l)
  (member y (member x l)))

(defun higher-rank-p (c1 c2)
  (not (equal nil (beforep c2 c1 all-ranks))))

(defun high-card (h)
  (assoc
    (find-if (lambda (x) (not (equal nil (assoc x h))))
            (reverse all-ranks))
    h))

; 7.17
(defun len-sum (l)
  (reduce #'+ (mapcar #'length l)))

; 7.19
(defun all-odd (l)
  (every #'oddp l))

; 7.20
(defun none-odd (l)
  (every #'evenp l))

; 7.21
(defun none-odd (l)
  (every #'evenp l))

; 7.22
(defun all-odd (l)
  (every #'oddp l))

; 7.26
(defun my-findif (p l)
  (car (remove-if-not p l)))

; 7.27
(defun my-every (p l)
  (not (consp (remove-if p l))))

; 7.29
(setf database
      '((b1 shape brick) (b1 color green) (b1 size small) (b1 supported-by b2) (b1 supported-by b3)
        (b2 shape brick) (b2 color red) (b2 size small) (b2 supports b1) (b2 left-of b3)
        (b3 shape brick) (b3 color red) (b3 size small) (b3 supports b1) (b1 right-of b2)
        (b4 shape pyramid) (b4 color blue) (b4 size large) (b4 supported-by b5) 
        (b5 shape cube) (b5 color green) (b5 size large) (b5 supports b4) 
        (b6 shape brick) (b6 color purple) (b6 size large)))

; a.
(defun match-element (s1 s2)
  (cond ((or (eq s1 '?)
             (eq s2 '?))
          t)
        (t (eq s1 s2))))

; b.
(defun match-triple (a p)
  (and (match-element (first a) (first p))
       (match-element (second a) (second p))
       (match-element (third a) (third p))))

; c.
(defun fetch (p)
  (remove-if-not (lambda (x)
                   (match-triple p x))
                 database))

; e.
(defun color-query (b)
  `(,b color ?))

; f.
(defun supp-aux (b)
  (fetch (append '(? supports) (list b))))

(defun supporters (b)
  (mapcar #'car (supp-aux b)))

; g.
(defun supp-shape (b)
  (mapcar (lambda (x)
            (third (car (fetch `(,x shape ?)))))
          (supporters b)))

(defun supp-cube-p (b)
  (not (equal nil (find-if (lambda (x) (equal 'cube x))
                   (supp-shape b)))))

; h.
(defun desc1 (b)
  (fetch `(,b ? ?)))

; i.
(defun desc2 (b)
  (mapcar (lambda (x) (cdr x))
          (desc1 b)))

; j.
(defun description (b)
  (reduce #'append (desc2 b)))

; 7.30
(setf words
      '((one un) (two deux) (three trois) (four quatre) (five cinq)))

(setf spanish '(UNO DOS TRES QUATRO CINCO))

(defun trilingual ()
  (mapcar (lambda (x y)
            (append x (list y)))
          words
          spanish))
