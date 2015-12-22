;Estas funciones son para reducir el número de rasgos de los patrones del conteo
;de número de notas.

(defun suma_cola (arreglo indice)
  ;Suma los últimos elementos de un arreglo, a partir del índice especificado.
  ;Regresa un nuevo arreglo en donde los últimos elementos son reemplazados con
  ;dicha suma.
  (let ((num_elementos (length arreglo))(total 0) (salida (make-array (+ 1 indice) :fill-pointer 0)))
    (setq total (loop for k from indice below num_elementos sum (aref arreglo k)) )
    (loop for k from 0 below indice do (vector-push (aref arreglo k ) salida))
    (vector-push total salida)
    salida))	

(defun suma_cola_grupo (grupo indice)
  ;Lo mismo que suma cola pero para un grupo.
  (let ((salida (make-array (length grupo) :fill-pointer 0)))
    (loop for k across grupo do (vector-push (suma_cola k indice) salida))
    salida))	

(defun suma_cola_todo (todo indice)
  ;Lo mismo que suma_cola pero para todos los grupos.
  (loop for k in todo collect (suma_cola_grupo k indice)))
