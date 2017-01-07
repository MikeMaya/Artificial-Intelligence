(load "package.lisp")
(load "tables.lisp")
(load "ddr.lisp")
(in-package ddr)

(defparameter *Datos*
  '(
  	;;Si el mono esta en la misma posicion que la caja y el palo 
  	;;y es la posicion de los platanos, es verdadero que puede comer
  	(<- (Puede-comer (Estado Mono ?pos Caja ?pos Palo ?pos MonoSobre caja) Terminado)
  		(en platanos ?pos))

 	;;Para poder realizar la transicion del Estado 1
 	;; al Estado 2 es necesario que ocurra una accion
  	(<- (Puede-comer ?Estado1 (hacer ?accion ?pasos))
        (transicion ?Estado1 ?accion ?Estado2)
        (Puede-comer ?Estado2 ?pasos))

  	;;ACCION CON LA CAJA: Trepar
  	;;Subirse a la caja si es que el mono esta en la posicion de la caja
  	;;Solo podra subir si tiene el palo en la misma posicion
  	(<- (transicion (Estado Mono ?pos Caja ?pos Palo ?pos MonoSobre piso)
                 trepar
                 (Estado Mono ?pos Caja ?pos Palo ?pos MonoSobre caja)))

    ;;ACCION CON LA CAJA: Empujar
  	;;El mono puede empujar la caja hacia los platanos
  	;;si esta en la misma posicion que la caja 
    (<- (transicion (Estado Mono ?pos1 Caja ?pos1 Palo ?pos3 MonoSobre piso)
                 (Empujar-caja ?pos1 ?pos2)
                 (Estado Mono ?pos2 Caja ?pos2 Palo ?pos3 MonoSobre piso))
        (en platanos ?pos2)
        (diferente ?pos1 ?pos2)
        (diferente ?pos1 ?pos3)
        (diferente ?pos2 ?pos3))

    ;; ACCION DEL MONO: Caminar a la caja
    (<- (transicion (Estado Mono ?pos1 Caja ?pos2 Palo ?pos3 MonoSobre piso)
                 (Caminar-De ?pos1 ?pos2)
                 (Estado Mono ?pos2 Caja ?pos2 Palo ?pos3 MonoSobre piso))
        (diferente ?pos1 ?pos2)
        (diferente ?pos1 ?pos3)
        (diferente ?pos2 ?pos3))
    
    ;;ACCION CON EL PALO: Tomar
    ;;El mono puede tomar el palo para llevarlo a los platanos
    ;;El mono debe estar en la misma posicion que el palo 
    ;;Se debe hacer despues de empujar la caja
    (<- (transicion (Estado Mono ?pos1 Caja ?pos2 Palo ?pos1 MonoSobre piso)
                 (Tomar-Palo ?pos1 ?pos2)
                 (Estado Mono ?pos2 Caja ?pos2 Palo ?pos2 MonoSobre piso))
        (en platanos ?pos2)
        (diferente ?pos1 ?pos2))

	;; ACCION DEL MONO: Caminar al palo
    (<- (transicion (Estado Mono ?pos1 Caja ?pos1 Palo ?pos2 MonoSobre piso)
                 (Caminar-De ?pos1 ?pos2)
                 (Estado Mono ?pos2 Caja ?pos1 Palo ?pos2 MonoSobre piso))
        (diferente ?pos1 ?pos2))

    ;;Conocimiento del entorno
    (diferente ventana centro)
    (diferente ventana puerta)

    (diferente centro ventana)
    (diferente centro puerta)

    (diferente puerta ventana)
    (diferente puerta centro)

    (diferente caja piso)
    (diferente piso caja)
 
    (en platanos centro)
))

#|
(init-kb *Datos*)
(ask '(Puede-comer (Estado Mono centro Caja ventana Palo puerta MonoSobre piso) ?pasos))
(ask-trace '(Puede-comer (Estado Mono centro Caja ventana Palo puerta MonoSobre piso) ?pasos))
|#
