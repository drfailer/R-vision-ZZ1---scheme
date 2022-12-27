;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exo 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; filter ------------------------------------------------------------------
(define filtre
  (lambda (L P)
    (if (null? L)
	'()
	(if (P (car L))
	    (cons (car L) (filtre (cdr L) P))
	    (filtre (cdr L) P)))))

(filtre '(1 -3 8 -6 5 -9) (lambda (x) (>= x 0)))

;;;;; suppression des doublons ------------------------------------------------
(define supprimerDoublon
  (lambda (L)
    (if (null? L)
	'()
	(cons (car L)
	      (supprimerDoublon
	       (filtre (cdr L) (lambda (x) (not (= x (car L))))))))))

(supprimerDoublon '( 1 2 3 4 1 5 2 6 3 7 6 8 3 9))

;;;;; image -------------------------------------------------------------------
(define image
  (lambda (D f)
    (supprimerDoublon (map f D))))

(image '(1 2 3 4 5 6 7 8 9) (lambda (x) (* 2 x)))
(image '(-4 -3 -2 -1 0 1 2 3 4) (lambda (x) (* x x)))

;;;;; schema recursif ---------------------------------------------------------
(define SR
  (lambda (L I C)
    (if (null? L)
	I
	(C (car L) (SR (cdr L) I C)))))

(define image2
  (lambda (D f)
    (SR D '() (lambda (x l)
		(cons (f x) (filtre l (lambda (y) (not (= y (f x))))))))))

(image2 '(1 2 3 4 5 6 7 8 9) (lambda (x) (* 2 x)))
(image2 '(-4 -3 -2 -1 0 1 2 3 4) (lambda (x) (* x x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exo 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; part --------------------------------------------------------------------
(define part
  (lambda (k n)
    (if (= n 0)
	'(())
	(if (= n k)
	    (map (lambda (x) (cons 1 x)) (part (- k 1) (- n 1)))
	    (if (= k 0)
		(map (lambda (x) (cons 0 x)) (part 0 (- n 1)))
		(append (map (lambda (x) (cons 0 x)) (part k (- n 1)))
			(map (lambda (x) (cons 1 x)) (part (- k 1) (- n 1)))))))))

(part 2 4)

;;;;; MP ----------------------------------------------------------------------

;; solution 1

(define MP
  (lambda (k n)
    (if (= k 0)
	(part 0 n)
	(append (MP (- k 1) n) (part k n)))))

(MP 2 4)

;; solution 2: avec la fonction iota

(define iota
  (lambda (n)
    (if (= 0 n)
	'(0)
	(cons n (iota (- n 1))))))

(define MP2
  (lambda (k n)
    (append-map (lambda (x) (part x n)) (iota k))))

(MP2 2 4)

;;;;; DH ----------------------------------------------------------------------
(define DH
  (lambda (L K)
    (if (null? L) ;; et null? K
	0
	(if (= (car L) (car K))
	    (DH (cdr L) (cdr K))
	    (+ 1 (DH (cdr L) (cdr K)))))))

(DH '(1 2 3 4) '(1 2 7 4))

;;;;; BF ----------------------------------------------------------------------

;; solution 1

(define taille
  (lambda (l)
    (SR l 0 (lambda (x r) (+ 1 r)))))

(define echange
  (lambda (x)
    (if (= x 0) 1 0)))

(define BF ;; on suppose que la taille de la liste est supérieur ou égale à k
  (lambda (L k)
    (if (null? L) ;; ou k == 0
	'(())
	(if (= k (taille L))
	    (list (map echange L))
	    (if (> k 0)
		(append
		 (map (lambda (x) (cons (echange (car L)) x)) (BF (cdr L) (- k 1)))
		 (map (lambda (x) (cons (car L) x)) (BF (cdr L) k)))
		(list L))))))

(BF '(0 0 0) 0)
(BF '(0 0 0) 3)
(BF '(0 0 0) 2)
(BF '(0 0 0 0) 2)

;; solution 2

(define BF2
  (lambda (L k changer)
    (let ((mods (part k (taille L))))
      (map (lambda (x) ;; x => liste qui indique l'emplacement des modifications
	     (map (lambda (y z) ;; y in L et z in [0,1]
	     (if (= 1 z)
		 (changer y)
		 y))
	   L x)) mods))))

(BF2 '(0 0 0) 0 (lambda (x) (if (= x 1) 0 1)))
(BF2 '(0 0 0) 2 (lambda (x) (if (= x 1) 0 1)))
(BF2 '(0 0 0) 3 (lambda (x) (if (= x 1) 0 1)))
(BF2 '(0 0 0 0) 2 (lambda (x) (if (= x 1) 0 1)))

;; marche pour tout :D
(BF2 '(A B B A) 2 (lambda (x) (if (equal? x 'A) 'B 'A)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exo 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; exist2 ------------------------------------------------------------------
(define exist2
  (lambda (L P)
    (if (null? L)
	'()
	(if (P (car L))
	    (list (car L))
	    (exist2 (cdr L) P)))))

(exist2 '(11 2 3 0 9) (lambda (x) (= (modulo x 3) 1)))
(exist2 '(11 2 10 0 9) (lambda (x) (= (modulo x 3) 1)))

;;;;; exist -------------------------------------------------------------------
(define exist
  (lambda (L P)
    (not (null? (exist2 L P)))))

(exist '(11 2 3 0 9) (lambda (x) (= (modulo x 3) 1)))
(exist '(11 2 10 0 9) (lambda (x) (= (modulo x 3) 1)))

;;;;; qqs ---------------------------------------------------------------------
(define qqs
  (lambda (L P)
    (not (exist L (lambda (x) (not (P x)))))))

(qqs '(11 2 3 0 9) (lambda (x) (not (= (modulo x 3) 1))))
(qqs '(11 2 10 0 9) (lambda (x) (not (= (modulo x 3) 1))))

;;;;; neutre? -----------------------------------------------------------------
(define neutre?
  (lambda (E f)
    (if (null? E)
	'()
	(let ((r (exist2 E
			 (lambda (x)
			   (= (f x (car E)) (f (car E) x) x)))))
	  (if (null? r)
	      (neutre? (cdr E) f)
	      r)))))

(neutre? '(1 2 3 4 5 6 7 8 9) (lambda (x y) (+ x y)))
(neutre? '(0 1 2 3 4 5 6 7 8 9) (lambda (x y) (+ x y)))

;;;;; inversible? -------------------------------------------------------------
(define inversible?
  (lambda (E f)
    (let ((r (neutre? E f)))
      (if (null? r)
	  #f
	  (qqs E (lambda (x)
		   (exist E (lambda (y) (= (f x y) (car r))))))))))

(inversible? '(0 1 2 3 4 5 6 7 8 9) (lambda (x y) (+ x y)))
