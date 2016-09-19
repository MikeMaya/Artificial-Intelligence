;;;=======================================================================
;;;  GLOL.lisp
;;;      Resuelve  el  problema  de  Granjero Lobo Oveja y Legumbres
;;;
;;;      Miguel Angel Maya Hernandez
;;;  18 de septiembre, 2016
;;;=======================================================================
(defparameter  *open* '())       ;; Frontera de busqueda...                                              
(defparameter  *memory* '())   ;; Memoria de intentos previos

(defparameter  *ops*  '( (:Un-lobo(1 0 0))     ;; Operadores para el problema GLOL	 
			 (:Una-oveja      (0 1 0))
			 (:Una-legumbre   (0 0 1))
 			 (:Solo-granjero  (0 0 0)) ) )

(defparameter  *id*  0)  ;; Identificador del ultimo nodo creado, cada vez que se cree un nodo se debe incrementar
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro com�n a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenar� la soluci�n recuperada de la memoria

;;=======================================================================
;;  CREATE-NODE [estado  op]  
;;      estado - Un estado del problema a resolver (sistema)...
;;             op - El operador cuya aplicaci�n gener� el [estado]...
;;=======================================================================
(defun  create-node (estado  op)
  "Construye y regresa un nuevo nodo de b�squeda que contiene al estado y operador recibidos como par�metro "
      (incf  *id*)  ;;incrementamos primero para que lo �ltimo en procesarse sea la respuesta
      (list  (1-  *id*)  estado  *current-ancestor*  (first op)) )  ;;los nodos generados son descendientes de *current-ancestor*

;;=======================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  

;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;=======================================================================
(defun insert-to-open (estado  op  metodo) 
"Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
     (let ((nodo  (create-node  estado  op)))
         (cond ((eql  metodo :depth-first)
	                  (push  nodo  *open*))
	           ((eql  metodo :breath-first)
		          (setq *open*  (append  *open*  (list nodo))))
	   	   (T  Nil)))  )


(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *Open*))

;;=======================================================================
;;  BARGE-SHORE [estado]
;;        Regresa la orilla del rio en la que se encuentra la barca en  [estado]
;;           0 - Orilla origen (primer sublista del estado)
;;           1 - Orilla destino (segunda sublista del estado)
;;=======================================================================
(defun  barge-shore (estado)
"Regresa la orilla del r�o en la que se encuentra la barca en el estado recibido como par�metro:  0 - origen  1 - destino"
     (if  (= 1 (fourth (first  estado)))  0  1))


;;=======================================================================
;;  VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los recursos en la orilla donde esta la barca
;;=======================================================================
(defun  valid-operator (op  estado)
"Predicado. Valida la aplicaci�n de un operador a un estado...
     el estado tiene estructura:  [(<Lo0><Ov0><Le0><Gr0>) (<Lo1><Ov1><Le1><Gr1>)],
     el operador tiene estructura : [<etiqueta-humana> <lista operador con (<num Lobos><num Ovejas><num Lechugas)>]"  
  (let*  ((orilla  (barge-shore  estado))                         
	    (lobos  (first  (nth  orilla  estado)))   
	    (ovejas    (second  (nth  orilla  estado)))
	    (lechugas  (third (nth  orilla  estado))))
    (and  (>=  lobos  (first (second op)))              ;;el n�mero de lobos a mover, primer elemento de la lista-operador
          (>=  ovejas   (second (second op)))			;;el n�mero de ovejas a mover, segundo elemento de la lista-operador
    	  (>=  lechugas   (third (second op))) )))	    ;;el n�mero de lechugas a mover, tercer elemento de la lista-operador


;;=======================================================================
;;  VALID-STATE [estado]
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema

;;=======================================================================
(defun  valid-state (estado)
"Predicado. Valida  un estado seg�n las restricciones generales del problema...
       el estado tiene estructura:  [(<m0><c0><b0>) (<m1><c1><b1>)]"
    (let* ((orilla  (flip (barge-shore  estado)))  	;;orilla donde no esta el granjero
        (lo0  (first (nth orilla estado)))          ;;el estado tiene estructura ((<Lo0><Ov0><Le0><Gr0>) (<Lo1><Ov1><Le1><Gr1>)) ...
	    (ov0  (second (nth orilla estado)))
	    (le0 (third (nth orilla estado))))
      (and  (or (> le0 ov0) (zerop le0)) 
	    (or (> ov0 lo0) (zerop ov0)))) )				;; en la orilla donde no esta el granjero, deben haber m�s legumbre que ovejas
													;; y m�s ovejas que lobos

;;=======================================================================
;;  APPLY-OPERATOR [op, estado]
;;        Soluci�n simb�lica del problema
;;=======================================================================

(defun flip (bit)  (boole  BOOLE-XOR  bit  1))

(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let*  ((orilla1  (first  estado))
	       (orilla2  (second  estado))
	       (lo0  (first orilla1))
   	       (ov0   (second orilla1))
	       (le0   (third  orilla1))
	       (gr0   (fourth  orilla1))
	       (lo1  (first orilla2))
   	       (ov1   (second orilla2))
	       (le1   (third  orilla2))
	       (gr1   (fourth  orilla2))
	       (orilla-barca  (barge-shore estado)) 
	       (operador (first op)))      ;; este operador es la etiqueta humana del operador...
	 (case operador 
	    (:Un-lobo (if (= orilla-barca 0) 
			  (list  (list  (- lo0 1) ov0 le0 (flip gr0))   (list  (+ lo1 1) ov1 le1 (flip gr1)))
			  (list  (list  (+ lo0 1) ov0 le0 (flip gr0))   (list  (- lo1 1) ov1 le1 (flip gr1)))))
	    (:Una-oveja (if (= orilla-barca 0)  
			  (list  (list  lo0 (- ov0 1) le0 (flip gr0))   (list  lo1 (+ ov1 1) le1 (flip gr1)))
			  (list  (list  lo0 (+ ov0 1) le0 (flip gr0))   (list  lo1 (- ov1 1) le1 (flip gr1)))))
	    (:Una-legumbre (if (= orilla-barca 0)  
	 		    (list  (list  lo0 ov0 (- le0 1) (flip gr0))   (list  lo1 ov1 (+ le1 1) (flip gr1)))
				(list  (list  lo0 ov0 (+ le0 1) (flip gr0))   (list  lo1 ov1 (- le1 1) (flip gr1)))))
	    (:Solo-granjero 
			    (list  (list  lo0 ov0 le0 (flip gr0))   (list  lo1 ov1 le1 (flip gr1))))
	    (T "error"))))


;;=======================================================================
;;  EXPAND [ estado]
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;=======================================================================
(defun expand (estado)
"Obtiene todos los descendientes v�lidos de un estado, aplicando todos los operadores en *ops* en ese mismo �rden"
     (let ((descendientes  nil)
	       (nuevo-estado  nil))
           (dolist  (op  *Ops*  descendientes) 
	     (setq  nuevo-estado  (apply-operator  op estado))
	     (when (and (valid-operator  op  estado) 
	           (valid-state  nuevo-estado))
	                (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))) )


;;=======================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;=======================================================================
(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos
     el estado tiene estructura:  [(<m0><c0><b0>) (<m1><c1><b1>)],
     el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador> ]"  
     (cond ((null  lista-memoria)  Nil)
	        ((equal  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
		(T  (remember-state?  estado  (rest  lista-memoria))))  )


(defun  filter-memories (lista-estados-y-ops) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado est� en la memoria *memory*
     la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
     (cond ((null  lista-estados-y-ops)  Nil)
	       ((remember-state? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda el primer elemento de la lista, filtrarlo...
		       (filter-memories  (rest  lista-estados-y-ops)))
		(T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))) )  ;; de lo contrario, incluirlo en la respuesta

;;=======================================================================
;;  EXTRACT-SOLUTION  y  DISPLAY-SOLUTION
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de soluci�n del problema...
;;=======================================================================
(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
     (labels ((locate-node  (id  lista)       ;; funci�n local que busca un nodo por Id  y si lo encuentra regresa el nodo completo
		         (cond ((null  lista)  Nil)
			           ((eql  id  (first (first  lista))) (first  lista))
				   (T  (locate-node  id (rest  lista))))))
	      (let ((current  (locate-node  (first  nodo)  *memory*)))
		    (loop  while  (not (null  current))  do                        
			 (push  current  *solucion*)     ;; agregar a la soluci�n el nodo actual
			 (setq  current  (locate-node  (third  current) *memory*))))  ;; y luego cambiar a su antecesor...
	      *solucion*))


(defun  display-solution (lista-nodos)
"Despliega la soluci�n en forma conveniente y numerando los pasos"
    (format  t  "Soluci�n con ~A  pasos~%" (1- (length  lista-nodos)))
    (let  ((nodo  nil))
         (dotimes  (i (length  lista-nodos))
	      (setq  nodo  (nth  i  lista-nodos))
	      (if  (= i 0)
		   (format t "Inicio en: ~A~%" (second  nodo))  ;; a partir de este estado inicial
	       ;;else
		   (format t "\(~A\) aplicando ~A  se  llega  a  ~A~%"  i (fourth  nodo)  (second  nodo)))))  )  ;; imprimir el n�mero de paso, operador y estado...

;;=======================================================================
;;  RESET-ALL  y  BLIND-SEARCH
;;
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solucion del problema...
;;=======================================================================
(defun reset-all () 
"Reinicia todas las variables globales para iniciar una nueva b�squeda..."
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil))


(defun  blind-search (edo-inicial  edo-meta  metodo)
"Realiza una b�squeda ciega, por el m�todo especificado y desde un estado inicial hasta un estado meta
    los m�todos posibles son:  :depth-first - b�squeda en profundidad
                                             :breath-first - b�squeda en anchura"
  (reset-all)
  (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil))

      (insert-to-open   edo-inicial  nil  metodo)
      (loop until  (or  meta-encontrada (null *open*))  do
	   (setq  nodo (get-from-open)   
		     estado  (second  nodo)
		     operador  (third  nodo))
	   (push  nodo  *memory*)
	   (cond ((equal  edo-meta  estado)  
	   (format  t  "�xito. Meta encontrada en ~A  intentos~%" (first  nodo))
		  (display-solution  (extract-solution  nodo))
                  (setq  meta-encontrada  T))
	         (t (setq  *current-ancestor*  (first  nodo)) 
		    (setq  sucesores  (expand estado))
		    (setq  sucesores  (filter-memories  sucesores))
		    (loop for  element  in  sucesores  do
	     (insert-to-open  (first element)  (second element)  metodo))))))  )
			     
     
;;=======================================================================
;;=======================================================================

(blind-search '((1 1 1 1)(0 0 0 0)) '((0 0 0 0)(1 1 1 1)) :depth-first)


