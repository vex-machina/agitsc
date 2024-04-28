(defun geq (x y)
  (or (> x y)
      (equal x y)))

(defun oddity (x)
  (cond ((and (oddp x) (> x 0)) (* x x))
        ((and (oddp x) (< x 0)) (* 2 x))
        (t (/ x 2))))

(defun bg (x y)
  (or (and (or (equal x 'BOY)
               (equal x 'GIRL))
           (equal y 'CHILD))
      (and (or (equal x 'MAN)
               (equal x 'WOMAN))
           (equal y 'ADULT))))

(defun rps (p1 p2)
  (cond ((equal p1 p2) 'TIE)
        ((or (and (equal p1 'r) (equal p2 's))
             (and (equal p1 'p) (equal p2 'r))
             (and (equal p1 's) (equal p2 'p)))
           '(P1 WINS))
        (t '(P2 WINS))))

(defun bp_cond (temp scale)
  (cond ((and (equal scale 'FAHRENHEIT)
              (> temp 212))
          t)
        ((and (equal scale 'CELSIUS)
              (> temp 100))
         t)
        (t nil)))

(defun libp_andor (temp scale)
  (or (and (equal scale 'FAHRENHEIT)
           (> temp 212))
      (and (equal scale 'CELSIUS)
           (> temp 100))))

(defun bp_if (temp scale)
  (if (equal scale 'FAHRENHEIT)
        (> temp 212)
      (if (equal scale 'CELSIUS)
            (> temp 100)
          nil)))

(defun la_if (x y)
  (if (equal x t)
        (if (equal y t)
              t
            nil)
      nil))

(defun la_cond (x y)
  (cond ((or (equal x nil)
             (equal y nil))
          nil)
        (t t)))

(defun logical_or (x y)
  (if (equal x nil)
        (if (equal y nil)
              nil
            t)
      t))

(defun nand (x y) (not (and x y)))

(defun la_nand (x y)
  (nand (nand x y) (nand x y)))

(defun lo_nand (x y)
  (nand (nand x x) (nand y y)))

(defun nor (x y) (not (or x y)))

(defun nornot (x) (nor x x))

(defun la_nor (x y)
  (nor (nor x x) (nor y y)))

(defun lo_nor (x y)
  (nor (nor x y) (nor x y)))

(defun throw_die () (+ 1 (random 6)))

(defun throw_dice ()
  (list (throw_die) (throw_die)))

(defun snake-eyes-p (roll)
      (equal roll '(1 1)))

(defun boxcars-p (roll)
      (equal roll '(6 6)))

(defun instant-win-p (sum)
  (or (equal sum 7)
      (equal sum 11)))

(defun instant-loss-p (sum)
  (or (equal sum 2)
      (equal sum 3)
      (equal sum 12)))

(defun say-throw (roll)
  (cond ((boxcars-p roll) 'BOXCARS)
        ((snake-eyes-p roll) 'SNAKEEYES)
        (t (+ (first roll) (second roll)))))

(defun say-wl (sum)
  (if (instant-win-p sum) 'WIN 'LOSE))

(defun craps (roll)
  (let* ((d1 (first roll))
        (d2 (second roll))
        (sum (+ d1 d2))
        (res (say-throw roll)))
    (cond ((or (instant-win-p sum)
               (instant-loss-p sum))
            (list 'THROW d1 'AND d2 '-- res '-- 'YOU (say-wl sum)))
          (t (list 'THROW d1 'AND d2 '-- 'YOUR 'POINT 'IS res)))))
