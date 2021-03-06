;;;=======================================================================
;;;  8puzzle.lisp
;;;      Resuelve  el  problema  de  8 puzzle mediante Best First Search
;;;
;;;      Miguel Angel Maya Hernandez
;;;  28 de septiembre, 2016
;;;=======================================================================
(defparameter  *open* '())       ;; Frontera de busqueda...                                              
(defparameter  *memory* '())   ;; Memoria de intentos previos
(defparameter *edoMeta* nil)	;; Estado meta

(defparameter  *ops*  '((:Arriba )     ;; Operadores para el problema 8puzzle	 
			 			(:Izquierda  )
			 			(:Derecha )
 			 			(:Abajo ) ) )

(defparameter  *id*  0)  ;; Identificador del ultimo nodo creado, cada vez que se cree un nodo se debe incrementar
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria

(defparameter *nodosCreados* 0)	
(defparameter *nodosExpandidos* 0)
(defparameter *maxFrontera* 0)
(defparameter *timeInicial* 0)
(defparameter *timeFinal* 0)
;;=======================================================================
;;  CREATE-NODE [estado  op]  
;;      estado - Un estado del problema a resolver (sistema)...
;;      op - El operador cuya aplicación generó el [estado]...
;; 		aptitud - La funcion de error correspondiente al nodo
;;=======================================================================
(defun  create-node (estado  op aptitud)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro "
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
      (incf *nodosCreados*)
      (list  (1-  *id*) aptitud estado  *current-ancestor*  (first op)) )  ;;los nodos generados son descendientes de *current-ancestor*


;;=======================================================================
;;  WRONGAPT   ,   MOVESAPT    ,    CUSTOMAPT   y    SETAPTI
;;
;;		WRONGAPT - Determina la aptitud de un estado segun el numero de elementos en un lugar equivocado respecto al estado meta
;;		MOVESAPT - Determina la aptitud de un estado segun la sumatoria de la distancia Manhatan de todas las piezas a sus lugares correctos
;;  	CUSTOMAPT - Determina la aptitud de un estado segun el promedio de distancias euclidianas
;;		SETAPTI - Permite determinar la aptitud de un estado segun el metodo proporcionado
;;
;;=======================================================================

(defun wrongAPT (estado)
  "Determina la aptitud de un estado segun el numero de elementos en un lugar equivocado respecto al estado meta"
  (let ((wrongs 0))
  	(loop for i from 0 to 2 do 
  	  (loop for j from 0 to 2 do
		(if (and (not (eql (aref estado i j) (aref *edoMeta* i j))) (not (eql 0 (aref estado i j)))) (incf wrongs))))
		wrongs))

(defun movesAPT (estado)
  "Determina la aptitud de un estado segun la sumatoria de la distancia Manhatan de todas las piezas a sus lugares correctos"
	(let ((sumatoria 0) (actual 0) (xi 0) (yi 0))
	  (loop for i from 0 to 2 do 
		(loop for j from 0 to 2 do
	  	  (setq actual (aref estado i j))
		  (loop for i1 from 0 to 2 do
			(loop for j1 from 0 to 2 do
			  (if (eql actual (aref *edoMeta* i1 j1)) (setq xi i1 yi j1))))
				(setq sumatoria (+ sumatoria (+ (abs (- i xi)) (abs (- j yi)) )))))
		sumatoria))

(defun customAPT (estado)
	"Determina la aptitud de un estado segun el promedio de distancias euclidianas"
	(let ((sumatoria 0) (actual 0) (xi 0) (yi 0))
	  (loop for i from 0 to 2 do 
		(loop for j from 0 to 2 do
	  	  (setq actual (aref estado i j))
		    (loop for i1 from 0 to 2 do
			  (loop for j1 from 0 to 2 do
			    (if (eql actual (aref *edoMeta* i1 j1)) (setq xi i1 yi j1))))
		    (setq sumatoria (+ sumatoria (sqrt (+ (* (- i xi) (- i xi)) (* (- j yi) (- j yi))))))))
		(float (/ sumatoria 9)) ))

(defun setApti (estado metodo)
  "Permite determinar la aptitud de un estado segun el metodo proporcionado"
  (let ((aptitud 0))
  (cond ((eql  metodo :wrong-pieces)
         (setq aptitud (wrongAPT estado)))
	    ((eql  metodo :moves-left)
		 (setq aptitud (movesAPT estado)))
	    ((eql  metodo :random-value)
		 (setq aptitud (random 10)))
	    ((eql  metodo :custom-value)
		 (setq aptitud (customAPT estado)))
	   	(T  Nil))))
;;=======================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  

;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             	:wrong-pieces	número de piezas en lugar incorrecto
;;			   	:moves-left   	suma de movimientos necesarios para colocar cada pieza en su lugar
;;				:random-value  	evaluación aleatoria
;;				:custom-value
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;=======================================================================

(defun insert-in-order (nodo lista) 
"Inserta el nodo recibido en *OPEN* segun su valor de aptitud"
	(cond ((null lista) (list nodo))
		((<= (second nodo)(second (first lista))) (cons nodo lista))
		(T (cons (first lista) (insert-in-order nodo (rest lista)))))
	)

(defun insert-to-open (estado  op  metodo) 
"Funcion que inserta en un nodo en *open*, determinando primero su aptitud"
     (let* ((aptitud (setApti estado metodo)) (nodo  (create-node estado  op aptitud)))
         (setq *open* (insert-in-order nodo *open*) )
	           ))

(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *Open*))

;;=======================================================================
;;  Find-Zero [estado]
;;        Regresa los indices donde se encuentra el 0 dentro de una matriz 3x3
;;=======================================================================

(defun  Find-Zero (estado)
"Regresa los indices donde se encuentra el 0 dentro de una matriz 3x3"
	(loop for i from 0 to 2 do 
		(loop for j from 0 to 2 do
			(cond ((zerop (aref estado i j)) (return-from Find-Zero (list i j)))) 
			))
)

;;=======================================================================
;;  VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los indices a utilizar
;;=======================================================================
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

;;=======================================================================
;;  APPLY-OPERATOR [op, estado]
;;        Solución simbólica del problema
;;=======================================================================

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


;;=======================================================================
;;  EXPAND [ estado]
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;=======================================================================
(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
	(incf *nodosExpandidos*)
     (let ((descendientes  nil)
	       (nuevo-estado  nil))
           (dolist  (op  *Ops*  descendientes) 
	     	(when (valid-operator  op  estado) 
	     		(setq  nuevo-estado  (apply-operator  op estado))
	            (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))) )


;;=======================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;=======================================================================
(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos
     el estado es una matriz 3x3
     el nodo tiene estructura : [<Id> <Apt> <estado> <id-ancestro> <operador> ]"  
     (cond ((null  lista-memoria)  Nil)
	        ((equalp  estado  (third (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
		(T  (remember-state?  estado  (rest  lista-memoria))))  )


(defun  filter-memories (lista-estados-y-ops) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*
     la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
     (cond ((null  lista-estados-y-ops)  Nil)
	       ( (or (remember-state? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda en la memoria, filtrarlo...
	       		(remember-state? (first (first  lista-estados-y-ops)) *open*))	  ;; si se encuentra en la frontera de busqueda, filtrarlo...
		       (filter-memories  (rest  lista-estados-y-ops)))
		(T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))) )  ;; de lo contrario, incluirlo en la respuesta

;;=======================================================================
;;  EXTRACT-SOLUTION  y  DISPLAY-SOLUTION
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solución del problema...
;;=======================================================================
(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
     (labels ((locate-node  (id  lista)       ;; función local que busca un nodo por Id  y si lo encuentra regresa el nodo completo
		         (cond ((null  lista)  Nil)
			           ((eql  id  (first (first  lista))) (first  lista))
				   (T  (locate-node  id (rest  lista))))))
	      (let ((current  (locate-node  (first  nodo)  *memory*)))
		    (loop  while  (not (null  current))  do                        
			 (push  current  *solucion*)     ;; agregar a la solución el nodo actual
			 (setq  current  (locate-node  (fourth  current) *memory*))))  ;; y luego cambiar a su antecesor...
	      *solucion*))


(defun  display-solution (lista-nodos)
"Despliega la solución en forma conveniente y numerando los pasos"
    (format  t  "Solucion con ~A  pasos~%" (1- (length  lista-nodos)))
    (let  ((nodo  nil))
         (dotimes  (i (length  lista-nodos))
	      (setq  nodo  (nth  i  lista-nodos))
	      (if  (= i 0)
		   (format t "Inicio en: ~A~%" (third  nodo))  ;; a partir de este estado inicial
	       ;;else
		   (format t "\(~A\) aplicando ~A  se  llega  a  ~A~%"  i (nth 4 nodo)  (third  nodo)))))  )  ;; imprimir el número de paso, operador y estado...

;;=======================================================================
;;  RESET-ALL  ,  CONTEXTUAL-SEARCH   y      MESURE-SEARCH
;;
;; 		RESET-ALL: Reinicia los parametros globales definidos para el problema
;;
;;		CONTEXTUAL-SEARCH: Realiza una busqueda informada para el problema 8puzzle recibiendo un estado inicial, estado meta y el metodo
;;
;;		CONTEXTUAL-SEARCH-WI: Realiza una busqueda informada para el problema 8puzzle recibiendo un estado inicial, estado meta y el metodo. Pero no imprime la solucion
;;
;;		MESURE-SEARCH: Ejecuta la busqueda informada desplegando al final de la informacion de la ejecucion
;;=======================================================================
(defun reset-all () 
"Reinicia todas las variables globales para iniciar una nueva búsqueda..."
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil)
     (setq  *edoMeta* nil)
     (setq  *nodosCreados* 0)
     (setq  *nodosExpandidos* 0)
     (setq  *timeInicial* 0)
     (setq  *timeFinal* 0 )
     (setq  *maxFrontera* 0))


(defun  Contextual-search (edo-inicial  edo-meta  metodo)
"Realiza una busqueda informada para el problema 8puzzle
    los métodos posibles para evaluar la aptitud son: 
    			:wrong-pieces	número de piezas en lugar incorrecto
			   	:moves-left   	suma de movimientos necesarios para colocar cada pieza en su lugar
				:random-value  	evaluación aleatoria
				:custom-value   promedio de las distancias euclidianas del estado con su lugar correcto"

  (reset-all)
  (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil))
  	  (setq *edoMeta* edo-meta)
      (insert-to-open   edo-inicial  nil  metodo)
      (loop until  (or  meta-encontrada (null *open*))  do
      	(if (> (length *open*) *maxFrontera*) (setq *maxFrontera* (length *open*)))
	   	(setq  nodo (get-from-open)   
		     estado  (third  nodo)
		     operador  (nth 4 nodo))
	  	(push  nodo  *memory*)
		(cond ((equalp  edo-meta  estado) 
				(format  t  "Exito. Meta encontrada en ~A  intentos~%" (first  nodo))	
		  		(display-solution  (extract-solution  nodo))
		       (setq  meta-encontrada  T)) 
			(T
				(setq  *current-ancestor*  (first  nodo)) 
				(setq  sucesores  (expand estado))
				(setq  sucesores  (filter-memories  sucesores))
				(loop for  element  in  sucesores  do
					(cond ((equalp  edo-meta  (first element))  
	   					  (format  t  "Exito. Meta encontrada en ~A  intentos~%" (first  nodo))	
		  				  (display-solution  (extract-solution  nodo))
                		  (setq  meta-encontrada  T))
		    			(T (insert-to-open  (first element)  (second element)  metodo)))))))))

(defun  Contextual-search-WI (edo-inicial  edo-meta  metodo)
"Realiza una busqueda informada para el problema 8puzzle sin imprimir
    los métodos posibles para evaluar la aptitud son: 
    			:wrong-pieces	número de piezas en lugar incorrecto
			   	:moves-left   	suma de movimientos necesarios para colocar cada pieza en su lugar
				:random-value  	evaluación aleatoria
				:custom-value   promedio de las distancias euclidianas del estado con su lugar correcto"

  (reset-all)
  (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil))
  	  (setq *edoMeta* edo-meta)
      (insert-to-open   edo-inicial  nil  metodo)
      (loop until  (or  meta-encontrada (null *open*))  do
      	(if (> (length *open*) *maxFrontera*) (setq *maxFrontera* (length *open*)))
	   	(setq  nodo (get-from-open)   
		     estado  (third  nodo)
		     operador  (nth 4 nodo))
	  	(push  nodo  *memory*)
		(cond ((equalp  edo-meta  estado) 
				(extract-solution  nodo)
		       (setq  meta-encontrada  T)) 
			(T
				(setq  *current-ancestor*  (first  nodo)) 
				(setq  sucesores  (expand estado))
				(setq  sucesores  (filter-memories  sucesores))
				(loop for  element  in  sucesores  do
					(cond ((equalp  edo-meta  (first element))  
		  				  (extract-solution  nodo)
                		  (setq  meta-encontrada  T))
		    			(T (insert-to-open  (first element)  (second element)  metodo)))))))))
			     
(defun Mesure-search (edo-inicial  edo-meta  metodo)
	"Ejecuta la busqueda informada desplegando al final de la informacion de la ejecucion"
	(setq *timeInicial* (get-internal-run-time))
	(Contextual-search-WI edo-inicial edo-meta metodo)
	(setq *timeFinal* (get-internal-run-time))
	(format  t  "Nodos creados: ~A  ~%" *nodosCreados*)	
	(format  t  "Nodos expandidos: ~A  ~%" *nodosExpandidos*)	
	(format  t  "Longitud maxima de la Frontera de busqueda: ~A  ~%" *maxFrontera*)	
	(format  t  "Longitud de la solucion: ~A  ~%" (length *solucion*))
	(format  t  "Tiempo para encontrar la solucion: ~A  segundos~%" 
	    (float (/ (- *timeFinal* *timeInicial*) internal-time-units-per-second))))

;;=======================================================================
;;
;;CASOS DE EJEMPLO DEL PROGRAMA
;;
;;=======================================================================

;(Contextual-search #2A((2 8 3) (1 4 5) (7 0 6)) #2A((1 2 3) (8 0 4) (7 6 5)):moves-left)
;(Mesure-search #2A((2 8 3) (1 4 5) (7 0 6)) #2A((1 2 3) (8 0 4) (7 6 5)):moves-left)

;(Contextual-search #2A((2 8 3) (1 4 5) (7 0 6)) #2A((1 2 3) (8 0 4) (7 6 5)):wrong-pieces)
;(Mesure-search #2A((2 8 3) (1 4 5) (7 0 6)) #2A((1 2 3) (8 0 4) (7 6 5)):wrong-pieces)

;(Contextual-search #2A((2 8 3) (1 4 5) (7 0 6)) #2A((1 2 3) (8 0 4) (7 6 5)):random-value)
;(Mesure-search #2A((2 8 3) (1 4 5) (7 0 6)) #2A((1 2 3) (8 0 4) (7 6 5)):random-value)

;(Contextual-search #2A((2 8 3) (1 4 5) (7 0 6)) #2A((1 2 3) (8 0 4) (7 6 5)):custom-value)
;(Mesure-search #2A((2 8 3) (1 4 5) (7 0 6)) #2A((1 2 3) (8 0 4) (7 6 5)):custom-value)
