;Biblioteca maze-lib.lisp - Versión para descargar.

;Variables propias de esta biblioteca. Favor de no usarse o modificarse dentro de su código.
(defvar *algorithms-list* nil)
(defvar *goal*)
(defvar *start*)
(defvar *solution* nil)
(defvar *exec_time* 100000)
(defvar *num_algorithm* 0)
(defvar *num_laberinto* 0)
(defvar *maze*)

(defclass maze nil 
  ((data
     :initarg :data
     :initform (make-array '(1 1)))
   (start_position
     :initarg :start_position
     :initform #(0 0))
   (goal_position
     :initarg :goal_position
     :initform #(1 1))
   (dimensions
     :initarg :dimensions
     :initform '(6 6))))

(defvar *maze* 
  (make-instance 'maze 
                 :data #2A((3 9 3 9 1 3)
                           (10 10 10 10 12 6)
                           (10 10 12 17 5 3)
                           (8 6 9 4 3 10)
                           (10 9 16 5 6 10)
                           (12 6 12 5 5 4))
                 :start_position #(0 0)
                 :goal_position #(5 5)))

(setq *start* (slot-value *maze* 'start_position))
(setq *goal* (slot-value *maze* 'goal_position))

(defmacro add-algorithm (algoritmo)
  ;Añade un algoritmo a ejecutar.
  `(setq *algorithms-list* (append *algorithms-list* (list ,algoritmo))))

(defun get-maze-data ()
  ;Obtiene los datos del laberinto
  (slot-value *maze* 'data))

(defun get-cell-walls (x y)
 ;Regresa las paredes de una celda del laberinto.
  (let ((maze_size (array-dimensions (get-maze-data))))
    (cond
      ((and (>= x 0) (< x (nth 0 maze_size)) (>= y 0) (< y (nth 1 maze_size))) 
       (aref (get-maze-data) x y))
      (t (error "Coordenadas fuera de las dimensiones del laberinto.")))))



(defun get-maze-rows ()
;Regresa las filas del laberinto.
  (first (slot-value *maze* 'dimensions)))

(defun get-maze-cols ()
;Regresa las columnas del laberinto
  (second (slot-value *maze* 'dimensions)))

(defun start-maze ()
  ;Función para procesar la línea de comandos.
  (loop for k from 1 below (length *posix-argv*) do
        (eval (read-from-string (nth k *posix-argv*)))))

