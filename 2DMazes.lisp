;;;=======================================================================
;;;  2DMazes.lisp
;;;      Resuelve un laberinto en 2D
;;;
;;;      Miguel Angel Maya Hernandez
;;;  3 de Octubre, 2016
;;;=======================================================================
(load "maze_lib.lisp")


(defparameter  *open* '())       ;; Frontera de busqueda...                                              
(defparameter  *memory* '())   ;; Memoria de intentos previos
(defparameter *edoMeta* nil)	;; Estado meta
(defparameter *limX* 0)
(defparameter *limY* 0)
;; Operadores para el problema 8puzzle	
;; Formato de operador (:etiqueta  (x y) movimiento)

;Arriba (N): 0
;Arriba-derecha (NE): 1
;Derecha (E): 2
;Abajo-derecha (SE): 3
;Abajo (S): 4
;Abajo-izquierda (SW): 5
;Izquierda (W): 6
;Arriba-izquierda (NW): 7

(defparameter  *ops*  '((:Arriba (-1 0) 0)     
			(:Arriba-derecha (-1 1) 1)
			(:Derecha (0 1) 2)
			(:Abajo-derecha (1 1) 3)
 			(:Abajo (1 0) 4) 
 			(:Abajo-izquierda (1 -1) 5)
 			(:Izquierda (0 -1) 6)
 			(:Arriba-izquierda (-1 -1) 7)))

(defparameter  *id*  0)  ;; Identificador del ultimo nodo creado, cada vez que se cree un nodo se debe incrementar
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *current-ancestor-deep*  -1)  ;;La profundidad del ancestro común a todos los descendientes que se generen

;;=======================================================================
;;  CREATE-NODE [estado  op]  
;;
;;	ESTRUCTURA DE UN NODO
;;		(<id> <aptitud> <estado> <ancestro> <operador> <profundidad>)
;;=======================================================================
(defun  create-node (estado  op aptitud)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro "
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
      (list  (1-  *id*) aptitud estado  *current-ancestor*  (third op) (1+ *current-ancestor-deep*)) )  ;;los nodos generados son descendientes de *current-ancestor*

;;=======================================================================
;;  FUNCIONES DE APTITUD
;;		
;;=======================================================================

(defun DistanciaMH (x1 y1 x2 y2)
	"Regresa la Distancia Manhathan entre dos coordenadas"
	(+ (abs (- x2 x1)) (abs (- y2 y1))))

(defun setApti (estado metodo)
	(cond ((eql  metodo :A*) 
			(+ (1+ *current-ancestor-deep*) (DistanciaMH (first estado) (second estado) (first *edoMeta*) (second *edoMeta*))))
		((eql metodo :BestFS) (DistanciaMH (first estado) (second estado) (first *edoMeta*) (second *edoMeta*)))
		(T 1)))
;;=======================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;
;;
;;=======================================================================


(defun insert-in-order (nodo lista) 
"Inserta el nodo recibido en *OPEN* segun su valor de aptitud"
	(cond ((null lista) (list nodo))
		((<= (second nodo)(second (first lista))) (cons nodo lista))
		(T (cons (first lista) (insert-in-order nodo (rest lista)))))
	)

(defun insert-open-A* (nodo)
"Busca un nodo en open, si encuentra el mismo estado con mejor aptitud lo deja, si es con peor lo quita e inserta nodo"
	(labels (
	  (find-state (estado aptitud lista) ;;si se encuentra el estado y es mejor -> T
	  	;(format "estado ~a contra ~a apti ~a contra ~a" estado (third (first lista)) aptitud )
	  	(cond ((null lista) (list nil nil))
	  	  ((equal estado (third (first lista)))  (if (< aptitud (second (first lista))) (list T T) (list T nil)))
	  	  (T (find-state estado aptitud (rest lista))))) 
	  (eliminate-state (state lista) 
		(cond ((equal state (third (first lista))) (rest lista))
			(t (cons (first lista) (eliminate-state state (rest lista)))))))
	  (let ((res (find-state (third nodo) (second nodo) *open*)))
	  	;(format t "Nodo ~a encontro ~a ~%" nodo res)
	  	(cond ((and (first res) (second res)) ;;Se encontro y se tiene que remplazar
			(setq *open* (eliminate-state (third nodo) *open*)
				*open* (insert-in-order nodo *open*)))
	  		((and (null (first res)) (null (second res))) 
	  		  (setq *open* (insert-in-order nodo *open*))))))) ;;No se encontro y hay que insertar
	  	
(defun insert-to-open (estado  op metodo)  
"Funcion que determina la forma de insercion respecto al metodo"
    (let* ((aptitud (setApti estado metodo)) (nodo  (create-node estado op aptitud)))
    	(cond ((eql  metodo :A*) (insert-open-A* nodo))
    		  ((eql  metodo :BestFS) (setq *open* (insert-in-order nodo *open*)))
    		  ((eql  metodo :depth-first) (push  nodo  *open*))
	          ((eql  metodo :breath-first) (setq *open*  (append  *open*  (list nodo))))
    	)
    ) 
)

(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *Open*))

;;=======================================================================
;;  VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los indices a utilizar
;;=======================================================================

(defun  valid-operator (op  estado)
"Predicado. Valida la aplicación de un operador a un estado"
  (let  ( (newX (+ (first estado) (first (second op)))) (newY (+ (second estado) (second (second op))))
  		   (p nil) (d nil)) 
  	;;Validamos primero que exista en el mapa
  	(cond ((not (and (>= newX 0) (< newX *limX*) (>= newY 0) (< newY *limY*))) nil)
  	  (T (setq p (get-cell-walls (first estado) (second estado)) d (get-cell-walls newX newY))
  	  	;;validamos que se pueda avanzar en la direccion
  	  	(cond 
  		  ((eql  (first op) :Arriba)
  		  	(/= (logand p 1) 1))
  		  ((eql  (first op) :Derecha)
	      	(/= (logand p 2) 2))
  		  ((eql  (first op) :Abajo)
	      	(/= (logand p 4) 4))
  		  ((eql  (first op) :Izquierda)
	      	(/= (logand p 8) 8))
	      ((eql  (first op) :Arriba-derecha)
	      	(and (/= (logand p 3) 3) 
	      		 (/= (logand d 12) 12) 
	      		 (or (/= (logand p 2) 2) (/= (logand d 8) 8))
	      		 (or (/= (logand p 1) 1) (/= (logand d 4) 4))))
	      ((eql  (first op) :Abajo-derecha)
	      	(and (/= (logand p 6) 6) 
	      		 (/= (logand d 9) 9) 
	      		 (or (/= (logand p 2) 2) (/= (logand d 8) 8))
	      		 (or (/= (logand p 4) 4) (/= (logand d 1) 1))))    
	      ((eql  (first op) :Abajo-izquierda)
	      	(and (/= (logand p 12) 12) 
	      		 (/= (logand d 3) 3) 
	      		 (or (/= (logand p 8) 8) (/= (logand d 2) 2))
	      		 (or (/= (logand p 4) 4) (/= (logand d 1) 1))))
	      ((eql  (first op) :Arriba-izquierda)
	      	(and (/= (logand p 9) 9) 
	      		 (/= (logand d 6) 6) 
	      		 (or (/= (logand p 8) 8) (/= (logand d 2) 2))
	      		 (or (/= (logand p 1) 1) (/= (logand d 4) 4))))
	   	  (T  Nil)) 
  	) )
  )
)

;;=======================================================================
;;  APPLY-OPERATOR [op, estado]
;;        Solución simbólica del problema
;;=======================================================================

(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle [op]  SIN VALIDACIONES"
    (let  ( (nuevo nil) )      
    	(setq nuevo (list (+ (first estado) (first (second op))) (+ (second estado) (second (second op)))))	
	 ))

;;=======================================================================
;;  EXPAND [ estado]
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;=======================================================================

(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
    (let ((descendientes  nil)
        (nuevo-estado  nil))
        (dolist  (op  *Ops*  descendientes) 
	     	(when (valid-operator  op  estado) 
	     		(setq  nuevo-estado  (apply-operator  op estado))
	            (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))))

;;=======================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;=======================================================================
(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos"
     (cond ((null  lista-memoria)  Nil)
	        ((equalp  estado  (third (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
		(T  (remember-state?  estado  (rest  lista-memoria))))  )

(defun  filter-memories (lista-estados-y-ops metodo) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*
     la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
     (cond ((null  lista-estados-y-ops)  Nil)
	       ( (or (remember-state? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda en la memoria, filtrarlo...
	       		(if (eql metodo :BestFS) (remember-state? (first (first  lista-estados-y-ops)) *open*) nil))	  ;; si se encuentra en la frontera de busqueda, filtrarlo...
		       (filter-memories  (rest  lista-estados-y-ops) metodo))
		(T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops) metodo)))) )  ;; de lo contrario, incluirlo en la respuesta
;;=======================================================================
;;  EXTRACT-SOLUTION  
;;
;;=======================================================================

(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
     (labels ((locate-node  (id  lista)
         (cond ((null  lista)  Nil)
	       ((eql  id  (first (first  lista))) (first  lista))
	  (T  (locate-node  id (rest  lista))))))
     (let ((current  (locate-node  (first  nodo)  *memory*)))
       (loop  while  (not (null  current))  do                        
	 (push  (nth 4 current)  *solution*)     
	 (setq  current  (locate-node  (fourth  current) *memory*))))
	      (pop *solution*)))

;;=======================================================================
;;  RESET-ALL  ,  MAZE-SEARCH   y   MESURE-SEARCH
;;
;;=======================================================================

(defun reset-all () 
"Reinicia todas las variables globales para iniciar una nueva búsqueda..."
    (setq  *open*  nil)
    (setq  *memory*  nil)
    (setq  *id*  0)
    (setq  *current-ancestor*  nil)
    (setq  *solution*  nil)
    (setq  *edoMeta* nil) 
    (setq *current-ancestor-deep* -1)
    (setq *limX* (get-maze-rows)) 
    (setq *limY* (get-maze-cols)))

(defun Maze-search (edo-inicial  edo-meta  metodo)
"Realiza una busqueda para un laberinto
    los métodos posibles para evaluar la aptitud son: 
    			:A* Algoritmo A* 
    			:BestFS  Algoritmo Best First Search"
  (reset-all)
  (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil))
  	  (setq *edoMeta* edo-meta )
      (insert-to-open  edo-inicial nil metodo)
      (loop until (or  meta-encontrada (null *open*))  do
	   	(setq  nodo (get-from-open)   
		     estado  (third  nodo)
		     operador  (nth 4 nodo))
	  	(cond ((not (remember-state? estado *memory*)) 
	  	(push  nodo  *memory*)
	  	;(format t "nodo ~a ~%Frontera ~&~a~% Memoria~%~a~%" nodo *open* *memory*)
	  	(cond ((equal  edo-meta  estado)
	   	        (extract-solution  nodo)
                (setq  meta-encontrada  T))
	          (t 
	          	(setq  *current-ancestor*  (first  nodo)) 
	          	(setq  *current-ancestor-deep*  (nth 5 nodo))
		    	(setq  sucesores  (expand estado))
		    	;(format t "sucesores ~a~%" sucesores)
		    	(setq  sucesores  (filter-memories  sucesores metodo))
		    	;(format t "sucesores filtrados~a~%" sucesores)
		    	(loop for  element  in  sucesores  do
	     			(insert-to-open  (first element)  (second element)  metodo))))))  )))

(defun A*-Search () 
	(Maze-search (list (aref *start* 0)(aref *start* 1) )  
		     (list (aref *goal* 0)(aref *goal* 1) )  
		     ':A*))

(defun Best-First-Search () 
	(Maze-search (list (aref *start* 0)(aref *start* 1) )  
		     (list (aref *goal* 0)(aref *goal* 1) )  
		     ':BestFS))

(defun Depth-First-Search() 
	(Maze-search (list (aref *start* 0)(aref *start* 1) )  
		     (list (aref *goal* 0)(aref *goal* 1) )  
		     ':depth-first))

(defun Breath-First-Search () 
	(Maze-search (list (aref *start* 0)(aref *start* 1) )  	
		     (list (aref *goal* 0)(aref *goal* 1) )  
		     ':breath-first))

(add-algorithm 'A*-Search)
(add-algorithm 'Best-First-Search)
(add-algorithm 'Depth-First-Search)
(add-algorithm 'Breath-First-Search)
(start-maze)

;;(or (remember-state? '(5 5) *memory*)
  ;;  (if (eql :BestFS :BestFS) (remember-state?   *open*) nil))
