(defun  Find-Zero (estado)
"Regresa los indices donde se encuentra el 0 dentro de una matriz 3x3"
	(loop for i from 0 to 2 do 
		(loop for j from 0 to 2 do
			(cond ((zerop (aref estado i j)) (return-from Find-Zero (list i j)))) 
			))
)

(defun  valid-operator (op  estado)
"Predicado. Valida la aplicación de un operador a un estado...
     el estado es una matriz 3x3
     el operador tiene estructura : [<etiqueta-humana>]"  
  (let  ((posZero (Find-Zero  estado)))                         
  	(cond ((eql  (first op) :Arriba)
	      	(> (first posZero) 0))
	      ((eql  (first op) :Abajo)
		    (< (first posZero) 2))
	      ((eql  (first op) :Izquierda)
		    (> (second posZero) 0))
	      ((eql  (first op) :Derecha)
		    (< (second posZero) 2))
	   	   (T  Nil)) 
  	))


(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle [op]  SIN VALIDACIONES"
    (let  ( (posZero (Find-Zero  estado))
	    	(operador (first op))
	    	(nuevo (make-array '(3 3)))
	    	)      
    (loop for i from 0 to 2 do 
		(loop for j from 0 to 2 do
			(setf (aref nuevo i j) (aref estado i j) )
			))
	 (case operador 
	    (:Arriba 
	    	(setf (aref nuevo (first posZero) (second posZero)) (aref nuevo (1- (first posZero)) (second posZero))
	    		(aref nuevo (1- (first posZero)) (second posZero)) 0))
	    (:Abajo 
	    	(setf (aref nuevo (first posZero) (second posZero)) (aref nuevo (1+ (first posZero)) (second posZero))
	    		(aref nuevo (1+ (first posZero)) (second posZero)) 0))
	    (:Derecha 
	    	(setf (aref nuevo (first posZero) (second posZero)) (aref nuevo (first posZero) (1+ (second posZero)))
	    		(aref nuevo (first posZero) (1+ (second posZero))) 0))
	    (:Izquierda 
	    	(setf (aref nuevo (first posZero) (second posZero)) (aref nuevo (first posZero) (1- (second posZero)))
	    		(aref nuevo (first posZero) (1- (second posZero))) 0))
	    (T "error"))
	  nuevo
	 ))


(defparameter  *ops*  '((:Arriba )     ;; Operadores para el problema 8puzzle	 
			 			(:Abajo  )
			 			(:Izquierda )
 			 			(:Derecha ) ) )


(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
     (let ((descendientes  nil)
	       (nuevo-estado  nil))
           (dolist  (op  *Ops*  descendientes) 
	     	(when (valid-operator  op  estado) 
	     		(setq  nuevo-estado  (apply-operator  op estado))
	            (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))) )

(defun prueba()
  (let ((matriz #2A((3 3 1) (4 8 1) (4 0 3)))(matriz1 #2A((3 1 1) (4 8 1) (4 0 3))) )
    (equalp matriz matriz1)
    ))

(prueba)
