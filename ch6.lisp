(defun le-last (l)
  (car (last l)))

(defun le-rev (l)
  (car (reverse l)))

(defun le-nth (l)
  (nth (- (length l) 1) l))

(defun next-to-last-rev (l)
  (cadr (reverse l)))

(defun next-to-last-nth (l)
  (nth (- (length l) 2) l))

(defun my-butlast (l)
  (reverse (cdr (reverse l))))

(defun palindromep (s)
  (equal s (reverse s)))

(defun make-palindrome (s)
  (append s (reverse s)))

(defun add-vowels (s)
  (let ((vowels '(A E I O U)))
    (union s vowels)))

(defun my-subsetp (s1 s2)
  (equal nil (set-difference s1 s2)))

(defun set-equal (s1 s2)
  (and (subsetp s1 s2)
       (subsetp s2 s1)))

(defun proper-subsetp (s1 s2)
  (and (subsetp s1 s2)
       (not (set-equal s1 s2))))

(defun right-side (s)
  (cdr (member '-vs- s)))

(defun left-side (s)
  (reverse (cdr (member '-vs- (reverse s)))))

(defun count-common (s)
  (length (intersection (left-side s) (right-side s))))

(defun compare (s)
  (list (count-common s) 'COMMON 'FEATURES))

(setf books
        '((WAR-AND-PEACE LEO-TOLSTOY)
          (THE-GRAPES-OF-WRATH JOHN-STEINBECK)
          (THE-AENEID VIRGIL)
          (THE-CALL-OF-CTHULHU HP-LOVECRAFT)
          (GREAT-EXPECTATIONS CHARLES-DICKENS)))

(defun who-wrote (title)
  (cadr (assoc title books)))

(defun swap (l)
  (let ((middle (reverse (cdr (reverse (cdr l))))))
    (cons (car (last l)) (append middle (list (first l))))))

(defun rot-left (l)
  (append (cdr l) (list (car l))))

(defun rot-right (l)
  (append (last l) (reverse (cdr (reverse l)))))

(defvar rooms
  '((library (east upstairs-bedroom) (south back-stairs))
    (back-stairs (north library) (south downstairs-bedroom))
    (downstairs-bedroom (north back-stairs) (east dining-room))
    (upstairs-bedroom (west library) (south front-stairs))
    (front-stairs (north upstairs-bedroom) (south living-room))
    (living-room (north front-stairs) (east kitchen) (south dining-room))
    (dining-room (north living-room) (west downstairs-bedroom) (east pantry))
    (kitchen (west living-room) (south pantry))
    (pantry (north kitchen) (west dining-room))))

(defun choices (r)
  (cdr (assoc r rooms)))

(defun look (d r)
  (cadr (assoc d (choices r))))

(setf loc 'pantry)

(defun set-robbie-location (place)
  (setf loc place))

(defun how-many-choices ()
  (length (choices loc)))

(defun upstairsp (r)
  (or (equal r 'upstairs-bedroom)
      (equal r 'library)))

(defun onstairsp (r)
  (or (equal r 'front-stairs)
      (equal r 'back-stairs)))

(defun where ()
  (cond ((upstairsp loc) (append '(ROBBIE IS UPSTAIRS IN THE) (list loc)))
        ((onstairsp loc) (append '(ROBBIE IS ON THE) (list loc)))
        (t (append '(ROBBIE IS DOWNSTAIRS IN THE) (list loc)))))

(defun move (d)
  (cond ((equal nil (look d loc)) '(OUCH! ROBBIE HIT A WALL!))
        (t (set-robbie-location (look d loc)))))

(defun royal-we (l)
  (subst 'WE 'I l))

(princ (royal-we '(IF I LEARN LISP I WILL BE PLEASED)))
(print (where))
