;;;Este archivo contiene los scripts que se usarán para convertir de los
;;;archivos de frases a uno solo por pieza. Como no todos los rasgos se procesan
;;;de la misma forma, va a ser necesario hacer una función distinta para cada
;;;tipo de rasgo.

(defun procesa_armadura (arreglo)
  ;Procesa el archivo de 'Armadura_nuevo.txt'. En este caso sólo es necesario
  ;tomar el valor del primer objeto del arreglo.
  (print (aref arreglo 0)))

(defun suma_meta_arreglos (meta_arreglo)
  ;Suma los arreglos uno a uno
  (let ((ceros (make-array (length (aref meta_arreglo 0)))))
    (loop for arreglo across meta_arreglo do
          (setq ceros (map 'vector #'+ ceros arreglo)))
    ceros))

(defun columna_a_lista (arreglo indice)
  ;De un arreglo de arreglos crea una lista tomando todos los elementos de un
  ;cierto índice. No es una "columna" verdadera por que no es un arreglo de dos
  ;dimensiones, pero pues ya que.
  (loop for elemento across arreglo append
        (list (aref elemento indice))))

;;Esta función convertirá la información del archivo Ámbito_recortado de por
;;frases a por piezas. Entonces es necesario tomar en cuenta lo siguiente para
;;cada índice:
;;0: Escoger el menor
;;1: Escoger el mayor
;;2: Escoger el menor
;;3: Escoger el mayor
;;4: Resta, 1 menos 0
;;5: Resta, 3 menos 2
;;6: Resta, 1 menos 2
(defun procesa_ambito (arreglo)
  ;Procesa el archivo de Ámbito_Recortado
  (let ((salida (make-array 7 :initial-element 0)))
    (setf (aref salida 0) (apply #'min (columna_a_lista arreglo 0)))
    (setf (aref salida 1) (apply #'max (columna_a_lista arreglo 1)))
    (setf (aref salida 2) (apply #'min (columna_a_lista arreglo 2)))
    (setf (aref salida 3) (apply #'max (columna_a_lista arreglo 3)))
    (setf (aref salida 4) (- (aref salida 1) (aref salida 0)))
    (setf (aref salida 5) (- (aref salida 3) (aref salida 2)))
    (setf (aref salida 6) (- (aref salida 1) (aref salida 2)))
    salida))

(defun average (lista)
  ;Regresa el promedio de los números dentro de la lista
  (/ (apply #'+ lista) (length lista)))

(defun round-to (number precision &optional (what #'round))
 ;Para redondear a un número específico de dígitos.
  (let ((div (expt 10 precision)))
    (/ (funcall what (* number div)) div 1.0)))


(defun procesa_relaciones (arreglo)
  ;Procesa las relaciones. En los primeros 6 valores sólo es necesario calcular el
  ;promedio.
  (let ((salida (make-array 7 :initial-element 0))
        (rh (apply #'+ (columna_a_lista arreglo 0)))
        (lh (apply #'+ (columna_a_lista arreglo 1))))
    (setf (aref salida 0) (round-to (average (columna_a_lista arreglo 0)) 4))
    (setf (aref salida 1) (round-to (average (columna_a_lista arreglo 1)) 4))
    (setf (aref salida 2) (round-to (average (columna_a_lista arreglo 2)) 4))
    (setf (aref salida 3) (round-to (average (columna_a_lista arreglo 3)) 4))
    (setf (aref salida 4) (round-to (average (columna_a_lista arreglo 4)) 4))
    (setf (aref salida 5) (round-to (average (columna_a_lista arreglo 5)) 4))
    (setf (aref salida 6) 
          (cond
            ((= rh lh) 'B)
            ((> rh lh) 'R) 
            (t 'L) )) 
    salida))
