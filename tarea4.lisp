(defun Collect (predicado lista) 
  (cond ((OR (not (functionp predicado)) (not (listp lista))) (return-from Collect nil)))
  (labels ( (CollectR (pred lis)
      (cond ((null lis) '())
	    ((not (null (apply pred (list (first lis))))) 
	           (cons (first lis)(CollectR pred (rest lis))))
	    (T (CollectR pred (rest lis)))
      )
  )) (CollectR predicado lista))
)

(defun Palindromo (lista) 
  (cond ((not(listp lista)) (return-from Palindromo nil)))
  (labels ( (PalindromoR (l1 l2)
     (cond ((null l1) T)
	   ((equal (first l1) (first l2)) (AND T 
					  (PalindromoR (rest l1) (rest l2))))
	   (T nil)
     )
  )) (PalindromoR lista (reverse lista)))
)

(defun 2Palindromo (cadena)
  (cond ((not (stringp cadena)) (return-from 2Palindromo nil)))
  (labels ((PalR (cad) 
   (cond ((= (length cad) 1) (print (concatenate 'string cad cad)))
	 (T (concatenate 'string (subseq cad 0 1) (PalR (subseq cad 1)) (subseq cad 0 1)))
   )
  ))(PalR cadena) )
)

(defun IterativePalindrome (cadena)
  (cond ((not (stringp cadena)) (return-from IterativePalindrome nil)))
  (let ((nueva '()) (volteada '()))
       (setq nueva cadena volteada (reverse cadena))
      (concatenate 'string nueva volteada)
  )
)

;La llave necesita recibir un T
(defun ListRotate (lista n &key izq der) 
  (cond ((not (null lista)) nil)
	((not (integerp n)) nil)
	((AND (null izq) (null der)) nil)
	((AND (not(null izq)) (not(null der))) nil)
  )
  (setq n (mod n (length lista)))
  (let ((delantera nil) (trasera nil)(index 0))
    (cond ((null lista) nil)
	((not (null izq)) 
	 (dolist (i lista (append (reverse delantera) (reverse trasera)))
	   (cond ((< index n) (setq trasera (cons i trasera)))
		 (T  (setq delantera (cons i delantera)))
	   )
	   (setq index (+ 1 index))))
	((not (null der))
	 (dolist (i lista (append (reverse delantera) (reverse trasera)))
	   (cond ((< index (- (length lista) n)) (setq trasera (cons i trasera)))
		 (T  (setq delantera (cons i delantera)))
	   )
	   (setq index (+ 1 index))))
    )
  )
)

(defun Max&Pos (matrix)
  (cond ((not (listp matrix)) (return-from Max&Pos nil)))
  (let  ( (actual nil)(result '()) (maximo 0) (col 0) (row 0) (com 0))
    (loop for i from 0 to (- (length matrix) 1) do
	 (setq actual (nth i matrix)  maximo nil)
	 (cond ( (listp actual) 
	 (loop for j from 0 to (- (length actual) 1) do
	      (setq com (nth j actual))
	      (cond ((AND (realp com) (null maximo)) 
		     (setq maximo com col i row j)) 
		    ((AND (realp com) (not (null maximo)) (> com maximo))
		     (setq maximo com col i row j)) 
	      )
	 )))
	 (setq result (cons (cons col row) result))
    )
    (reverse result)
  )
)

(defun Combine (func lista)
  (cond ((OR (not (functionp func)) (not (listp lista))) nil))
  (labels ((CombineR (prev lis fun)
    (cond ((null lis) prev)
	  (T (CombineR (apply fun (list prev (first lis))) (rest lis) fun))
    )  
  ))
  (CombineR (apply func (list (first lista) (second lista))) (rest (rest lista)) func)
  )
)

(defun level (cadena lista)
 (cond ((OR (not (stringp cadena)) (not (listp lista))) (return-from level nil)))
 (let ((res nil)) 
  (labels ((nivel (cad lis dp)
      (cond ((null lis) 0)
            ((listp (first lis))  (nivel cad (first lis) (+ 1 dp)))
	    ((AND (stringp (first lis)) (string= (first lis) cad)) 
	     (setq res dp))
	    (T (nivel cad (rest lis) dp))
      )
  )) (nivel cadena lista 0))
  (cond ((null res) nil)
	(T res)
  )
 )
)

(defun StrEncode (cadena)
  (cond ((not (stringp cadena)) (return-from StrEncode nil)))
  (labels ( (Encode (cad elem n copia)
    (cond ((string= "" cad) 
	   (reverse (setq copia (cons (cons  elem n) copia))))
	  ((equal elem (char cad 0)) 
	   (setq copia (Encode (subseq cad 1) elem (+ 1 n) copia)))
	  (T (setq copia (setq copia (cons (cons  elem n) copia))
		   copia (Encode (subseq cad 1) (char cad 0) 1 copia)
		   ))
    )
  ))(Encode cadena (char cadena 0) 1  '() ))
)

(defun StrCypher (cadena code)
  (cond ((not (AND (stringp cadena) (stringp code))) (return-from StrCypher nil)))
  (let ((result "") (numero 0))
    (loop for i from 0 to (- (length cadena) 1) do
	 (setq numero (char cadena i))
	 (setq numero (char-int numero))
	 (cond ((= numero 241) (setq numero (- numero 227)))
	 )
	 (cond ((> numero 110) (setq numero (- numero 96)))
	       ((AND (< numero 111) (> numero 96)) (setq numero (- numero 97)))
	 )
	 (setq result 
	       (concatenate 'string result (string (char code numero))))
    )
    (return-from StrCypher result)
  )
)

(defun MatMult (m1 m2)
  (cond ((OR (not (arrayp m1)) (not (arrayp m2))) nil))
  (let ((x1 0) (x2 0) (y1 0) (y2 0) (res nil) (dim '()) (acum 0)) 
    (setq dim (array-dimensions m1) x1 (first dim) y1 (second dim))
    (setq dim (array-dimensions m2) x2 (first dim) y2 (second dim))
    (setq res (make-array (list y2 x1)))
    (cond ((/= y1 x2) nil)
      (T 
       (loop for x from 0 to (- x1 1) do     
	 (loop for y from 0 to (- y2 1) do 
	      (setq acum 0)
	      (loop for index from 0 to (- y1 1) do
		   (setq acum (+ acum (* (aref m1 x index) (aref m2 index y))))
	      )
	      (setf (aref res x y) acum)
	    )
	  )
       )
      )
    (return-from MatMult res)
    )
)

;;FORMA DEL ARBOL (RAIZ (H IZQ) (H DER))
(defun BTree (elem tree)
  (cond ((OR (not (numberp elem)) (not (listp tree))) nil)  )
  (let ((deep nil))
    (labels (
      (Setear (actual pr el)
	(setq deep pr)
	(setf actual (list el nil nil))
      )
      (Buscar (el tr prof) 
      (cond ((null tr) (setf tr (setear tr prof el)))
	    ((< el (first tr)) 
	     ;(Buscar el (second tr) (+ prof 1)))
	      (setf (second tr) (Buscar el (second tr) (+ prof 1))))
	    ((> el (first tr)) 
	     ;(Buscar el (third tr) (+ prof 1)))
	      (setf (third tr) (Buscar el (third tr) (+ prof 1))))
      )
      (return-from Buscar tr)
    ))(setf elem (Buscar elem tree 0)))
    (return-from Btree deep)
  ) 
)

(defun FilterSubsets (lista pos)
  (let ((result '())(actual '()) (elemento nil)(copia '()))
    (cond ((not (listp lista)) (return-from FilterSubsets nil) )
	  ((>= pos (length lista) ) (return-from FilterSubsets nil) )
    )
    (setq elemento (nth pos lista))
    (loop for i from 0 to (- (length lista) 1) do
      (cond ((/= i pos) (setq copia (cons (nth i lista) copia))) )
    )
    (setq lista (reverse copia))
    (setq result (list elemento))
    (labels ((Subset (k act in)
      (cond ((= k 0) (setq result (cons (reverse act) result)))
	    ((>= in (length lista)) nil)
	    (T (list 
		 (Subset (- k 1) (cons (nth in lista) act) (+ in 1))      
		 (Subset k act (+ in 1) )
	    ))
      )))(loop for i from 1 to (length lista) do
              (setq actual (list  elemento))
	      (Subset i actual 0 )
     ))
    (return-from FilterSubsets result)
  )
)


(defun Subsets (lista k)
    (let ((result '()))
      (cond ((not (listp lista)) (return-from Subsets nil) )
	    )
      (labels ((Subset (cont act in)
         (cond ((= cont 0) (setq result (cons (reverse act) result)))
	    ((>= in (length lista)) nil)
	    (T (list 
		 (Subset (- cont 1) (cons (nth in lista) act) (+ in 1))      
		 (Subset cont act (+ in 1) )
	    ))
      )))(Subset k '() 0 ))
      (return-from Subsets (reverse result))
    )
)

(defmacro If-positive (expresion &key then-do else-do)
  (let ((num (gensym "num-")) (then (gensym "then-")) (else (gensym "else-")))
     `(let* ((,num ,expresion) (,then ,then-do) (,else ,else-do)) 
	(cond ((AND (numberp ,num) (plusp ,num)) ,then)
	      (T ,else)
	)
     )
  )
)
