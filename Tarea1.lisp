;;;;"Miguel Angel Maya Hernandez"
;;;"Fundamentos de Inteligencia Artificial"
;;;"Tarea No. 1"

;;1.-Construya una solo expresion LISP para calcular lo que se indica en cada uno de los siguientes incisos

	;;a) El quinto elemento de la lista (((1 2) 3) 4 (5 (6)) A (B C) D (E (F G))) sin usar la funcion FIFTH
		(nth 4 '(((1 2) 3) 4 (5 (6)) A (B C) D (E (F G))) )

	;;b) El numero de segundos que tiene el año bisiesto 2004
		;60 segundos en 1 minuto
		;60 minutos en 1 hora
		;24 horas en 1 dia
		;366 dias en 1 año bisiesto

		;(* 366 (* 24 (* 60 60)))

	;;c) Si el valor numérico asociado a la variable x es diferente de cero y además menor o igual que el valor asociado a la variable y
		;(AND (NOT (EQUAL x 0)) (<= x y))

	;;d) Una lista que con las dos soluciones reales de la ecuación 
		;((/ (+ (* -1 b) (sqrt (- (exp b 2) (* 4 a c))) ) (* 2 a)) (/ (- (* -1 b) (sqrt (- (exp b 2) (* 4 a c))) ) (* 2 a))

;;2.- Escriba, en notación prefija, las siguientes expresiones aritméticas 

	;;a) 2(4) + (6-8)
		;(+ (* 2 4) (- 6 8))

	;;b)
		;(/ (+ 5 (+ -3 4)) (+ 6 (/ 2 5)))

	;;c) 
		;(sqrt (/ (+ (* -1 (- -4 (/ 3 8))) 1.4502) (exp -1 (exp (- 3 5) (/ 1 3)))))

	;;d)
		;(exp (/ (exp (/ 65.402 (sqrt -1)) (/ 1 5)) 0.17) (/ 1 7))

;;3.- Indique el resultado de evaluar cada una de las siguientes expresiones

	;;a) (cdar '((one two) three four))
		;(two)

	;b) (append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven))	
		;( (eva lisa) karl sven eva lisa karl sven)

	;;c) (subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin))
		;(eva gitan lisa gitan karin)

	;;d) (remove 'sven '(eva sven lisa sven anna))
		;(eva lisa anna)

	;;e) (butlast '(karl adam nilsson gregg alisson vilma) 3)
		;(karl adam nilsson gregg)
	
	;;f) (nth 2 '(a b c d e))
		;c

	;;g) (nthcdr 2 '(a b c d e))
		;(c d e)
	
	;;h) (intersection '(a b c) '(x b z c))
		;(b c)

	;;i) (cdadar '(((((1 2 3) z) y) (x 4)) 7 8 (a b c (5 (6 7 8)))))
		;a- ((((1 2 3) z) y) (x 4))
		;d- ((x 4))
		;a- (x 4)
		;d- (4)
		;(4)
		
