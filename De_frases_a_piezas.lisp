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
