; 9.1
(defun print-fun ()
  (format t "There are old pilots, and there
             ~&are bold pilots, but there are
             ~&no old bold pilots."))

; 9.2
(defun draw-line (n)
  (labels ((dl-aux (n acc)
            (cond ((zerop n) (mapcar #'(lambda (x) (format t x))
                                     acc))
                  (t (dl-aux (1- n) (cons "*" acc))))))
    (dl-aux n nil)))

; 9.3
(defun draw-box (n m)
  (labels ((db-aux (m acc)
              (cond ((zerop m) (mapcar #'(lambda (x) (draw-line n) (format t "~%"))
                                       acc))
                    (t (db-aux (1- m) (cons "dummy" acc))))))
    (db-aux m nil)))

; 9.4
(defun ninety-nine-bottles (n) 
  (cond ((zerop n) (format t "~&D'OH! NO MORE BEER!" n)) 
        (t (format t "~&~S bottles of beer on the wall," n)
           (format t "~&~S bottles of beer!" n)
           (format t "~&Take one down,")
           (format t "~&Pass it around,")
           (format t "~&~S bottles of beer on the wall.~%~%" (1- n))
           (ninety-nine-bottles (- n 1)))))

; 9.5
(defun pr-aux (n ls)
  (cond ((null (nth n ls)) " ")
        (t (nth n ls))))

(defun print-board (ls)
  (cond (t (format t " ~A | ~A | ~A ~%" (pr-aux 0 ls) (pr-aux 1 ls) (pr-aux 2 ls))
           (format t "-----------~%")
           (format t " ~A | ~A | ~S ~%" (pr-aux 3 ls) (pr-aux 4 ls) (pr-aux 5 ls))
           (format t "-----------~%")
           (format t " ~A | ~A | ~A ~%" (pr-aux 6 ls) (pr-aux 7 ls) (pr-aux 8 ls)))))

(defun gross-pay ()
  (format t "~&Wage?: ")
  (let ((h (read)))
    (format t "~&Hours?: ")
    (let ((w (read)))
      (format t "~&Gross Pay: $~S" (* h w)))))

(defun cookie-monster ()
  (cond (t (format t "Give me cookie!!!~%Cookie? ")
            (let ((in (read)))
              (cond ((string-equal in "cookie") (format t "Thank you!...Munch munch munch...BURP"))
                    (t (format t "No want ~A..." in)
                       (cookie-monster)))))))
