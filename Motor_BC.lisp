
;;;=======================================================================
;;;  Motor_BC.lisp
;;;      Motor de la base de conocimiento que permite la realizacion de consultas
;;;
;;;      Miguel Angel Maya Hernandez
;;;   de Noviembre, 2016
;;;=======================================================================

(defparameter knowledge-vector (make-array 100 :adjustable T :element-type 'list)) ;;Arreglo donde se guarda el conocimiento
(defparameter num_Rows nil) ;;Numero de tuplas en el arreglo

;;=======================================================================
;;    FUNCIONES DE CARGA DE ARCHIVO Y OBTENCION DE CONOCIMIENTO
;;
;;    READ-KNOWLEDGE, PRINT-KNOWLEDGE,  GIVE-KNOWLEDGE
;;=======================================================================

(defun read-knowledge (filename)
  "Funcion que lee del archivo las listas de asociativas y las guarda en un vector(arreglo)"
  (with-open-file (stream filename)
    (let ((numrows (read stream)))
      (setq num_Rows numrows)
      (adjust-array knowledge-vector numrows)
      (read-line stream nil nil) ;; lectura en false de pleca separadora
      (dotimes (row numrows knowledge-vector)
        (setf (aref knowledge-vector row) (read stream nil nil))))))

(defun print-knowledge ()
  "Imprime todas las alist en el vector"
  (dotimes (row num_Rows nil)
  	(format t "~a~&" (aref knowledge-vector row))))

(defun give-knowledge()
  "Genera una copia de la base de conocimiento en un lista"
  (let ((temp nil))
    (dotimes (row num_Rows temp)
      (push (copy-list (aref knowledge-vector row)) temp))))

;;=======================================================================
;;    FUNCIONES PREDICADOS
;;
;;    DOTTED-PAIR-P,  FIND-CELL,  FIND-ALIST
;;=======================================================================

(defun dotted-pair-p (cell)
  "Predicado: Si verifica siuna celda esta en formato dotted pair"
  (if (consp cell) (not(listp(cdr cell))) nil))

(defun find-cell (alist dcell)
  "Predicado: Verifica si una celda aparece en un lista"
  (let* ((validas nil) (segundo (rest dcell)) (cadena (write-to-string segundo)) (sen "(") (largo (- (length cadena) 1)))  
    (setq validas (list (assoc (first dcell) alist)))
    (cond ((eql #\[ (char cadena 0)) 
            (setq validas  (write-to-string (rest (first validas))) 
            sen (concatenate 'string sen (subseq cadena 1 2)))
            (if (eql #\= (char cadena 2)) 
              (setq sen (concatenate 'string sen (subseq cadena 2 3) " " validas " " (subseq cadena 3 largo) ")"))
              (setq sen(concatenate 'string sen " " validas " " (subseq cadena 2 largo) ")" ) ))
	          (eval (read-from-string sen)))
	        (t (not (null (rassoc (rest dcell) validas)))))))

(defun find-alist (alist Lista) 
  "Predicado: si una alist esta en una lista de alists"
  (cond ((null Lista) nil)
         ((equalp alist (first Lista)) t)
         (t (find-alist alist (rest Lista)))))

;;=======================================================================
;;    FUNCIONES DE MODIFICACION DEL LISTAS
;;
;;    REMOVER,  FILTRAR,  QUITAR,  MEZCLAR
;;=======================================================================

(defun remover (elemento lista)
  "Elimina una alist de una lista de alist"
  (cond ((null lista) nil) 
        ((equalp elemento (first lista)) (rest lista))
        (t (cons (first lista) (remover elemento (rest lista))))))

(defun filtrar (lista celda)
  "De una lista de listas de asociativas, filtrar aquellas que no contengan la celda"
  (let ((frente (first lista))) 
    (cond ((null lista) nil)
          ((find-cell frente celda) (cons frente (filtrar (rest lista) celda))) 
          (t (filtrar (rest lista) celda)))))

(defun quitar (lista celda)
  "De una lista de listas de asociativas, filtrar aquellas que contengan la celda"
  (let ((frente (first lista))) 
    (cond ((null lista) nil)
          ((find-cell frente celda) (quitar (rest lista) celda)) 
          (t (cons frente (quitar (rest lista) celda))))))

(defun mezclar (destino origen)
  "Recibe dos lista con lista asociativas y las mezcla, evitando repeticiones"
  (dolist (i origen destino) 
    (cond ((not (find-alist i destino)) (push (copy-list i) destino)))))

;;=======================================================================
;;    FUNCIONES DE CONSULTA Y RESPUESTA
;;
;;    CONSULTA, RESPONDER,  COMPLEMENTO  
;;=======================================================================

(defun complemento (lista)
  "Regresa todas las alist que no aparecen en lista pero si aparecen en la base de conocimiento"
  (let ((respuesta (give-knowledge)))
    (dolist (elemento lista respuesta)
      (setq respuesta (remover elemento respuesta)))))

(defun responder (llave lista)
  "Responde segun el tipo de consulta solicitada. Las llaves validas son
  :exist Lista vacia -> nil, sino resultados
  :not-exist lista vacia -> t, sino 
  :for-all Lista de num_Rows -> t, sino complemento resultados
  :not-for-all Lista de num_Rows -> nil, sino complemento resultados"
  (cond ((eql llave :exist) (cond ((null lista) nil) (t (format t "~a~%" lista) t) ))
        ((eql llave :not-exist) (cond ((null lista) t) (t (format t "~a~%" lista) nil) ))
        ((eql llave :for-all) (cond ((= num_Rows (length lista)) t) (t (format t "~a~%" (complemento lista)) nil)))
        ((eql llave :not-for-all) (cond ((= num_Rows (length lista)) nil) (t (format t "~a~%" (complemento lista)) t)))))

(defun consulta (llave &rest condiciones) 
  "LLaves posibles :exist :not-exist :for-all :not-for-all"
  (responder llave (Procesa (give-knowledge) (give-knowledge) condiciones)))

;;=======================================================================
;;    FUNCION DE PROCESAMIENTO PRINCIPAL
;;=======================================================================

(defun Procesa (source current condiciones)
  "Busca en un source los elementos que cumplan condiciones y los guarda en current"
  (let ((elemento nil))
  (if (dotted-pair-p condiciones) (setq condiciones (list condiciones)))
  (loop for i from 0 to (1- (length condiciones)) do
    (setq elemento (nth i condiciones))
    (cond ((dotted-pair-p elemento) (setq current (filtrar current elemento)))
          ((and (not (dotted-pair-p elemento)) (listp elemento)) (setq current (Procesa current current elemento)))
          ((eql elemento :OR) (incf i) (setq current (mezclar current (Procesa source source (nth i condiciones)))))
          ((eql elemento :NOT) (incf i) (setq current (quitar current (nth i condiciones))))))) 
  current)

;;=======================================================================
;;    CARGA DE LA BASE DEL CONOCIMIENTO
;;=======================================================================

(read-knowledge "data.lisp")

;;=======================================================================
;;    PRUEBAS
;;=======================================================================

;;Prueba de elementos unitarios por separado
(consulta :exist '((Year . 1984)(Genero . Puzzle)) :OR '( Year . 1998))
;;Prueba de elementos en una lista
(consulta :exist '( Year . 1998) :OR '((Year . 1984)(Genero . Puzzle)))
;;Prueba para NOT
(consulta :exist '(Class . Videogame) :NOT '( Year . 1998))
;;Prueba de exist con resultado false
(consulta :exist '(Class . console))
;;Prueba de not exist con resultado true
(consulta :not-exist '( Year . 1600))
;;Prueba de not exist con resultado false
(consulta :not-exist '( Year . 1998))
;;Prueba de for all con resultado true
(consulta :for-all '(Class . Videogame))
;;Prueba de for all con resultado false
;;Prueba de condicioes en el segundo elemento
(consulta :for-all '(Year . [>=1995]))
;;Prueba de not for all con resultado false
(consulta :not-for-all '(Class . Videogame))
;;Prueba de not for all con resultado true
(consulta :not-for-all '(Year . [<2010]))
