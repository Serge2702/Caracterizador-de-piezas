;;; Este archivo contiene la información necesaria para calcular el "índice de
;;; caracterización" de un archivo de propiedades.

(load "Caracterizador_nuevo.lisp")

(defun normaliza_arreglo (arreglo) 
  ;Normaliza un arreglo dividiéndolo entre su valor máximo
  (coerce (let ((maximo (apply #'max (coerce arreglo 'list))))
     (mapcar #'(lambda (x) (/ x maximo 1.0))(coerce arreglo 'list))) 'vector))

(defun contar_tipos_propiedades (conjunto)
  ;Cuenta cuantas propiedades positivas y negativas hay en el archivo. Esto lo
  ;hace por clase. La salida es un arreglo de dos dimensiones. Cada "fila" es
  ;una clase, el primer elemento son las positivas y el segundo las negativas.
  (let ((sumas (make-array '(6 2)))
        (tipo nil))
    (loop for propiedad across conjunto do
          (setq tipo (nth 2 (nth 1 propiedad)))
          (cond
            ((equal '+ (second tipo)) (incf (aref sumas (first tipo) 0)))
            (t (incf (aref sumas (first tipo) 1))))) 
    sumas))

(defun obtener_pares_+ (conjunto nclase cantidad)
  ;De un archivo de características obtiene los pares positivos de la clase
  ;indicada.
  (let ((salida)
        (contador 0)
        (tipo)
        (ordenados))
    (setq salida (list (make-array cantidad)(make-array cantidad)))
    (loop for propiedad across conjunto until (= cantidad contador) do
          (setq tipo (nth 2 (nth 1 propiedad)))
          (cond
            ((equal tipo (list nclase '+))
             (setq ordenados (sort (copy-list (coerce (nth 1 (nth 1 propiedad)) 'list)) #'>))
             (setf (aref (nth 0 salida) contador) (nth 0 ordenados))
             (setf (aref (nth 1 salida) contador) (nth 1 ordenados))
             (incf contador))
            (t nil)))
    salida))

(defun obtener_pares_- (conjunto nclase cantidad)
  ;De un archivo de características obtiene los pares negativos de la clase
  ;indicada.
  (let ((salida)
        (contador 0)
        (tipo)
        (ordenados))
    (setq salida (list (make-array cantidad)(make-array cantidad)))
    (loop for propiedad across conjunto until (= cantidad contador) do
          (setq tipo (nth 2 (nth 1 propiedad)))
          (cond
            ((equal tipo (list nclase '-))
             (setq ordenados (sort (copy-list (coerce (nth 1 (nth 1 propiedad)) 'list )) #'<))
             (setf (aref (nth 0 salida) contador) (nth 0 ordenados ))
             (setf (aref (nth 1 salida) contador) (nth 1 ordenados ))
             (incf contador))
            (t nil)))
    salida))

(defun rasgos_empleados (archivo indice)
  ;Del archivo de características, revisa que rasgos se están considerando en una
  ;clase.
  (let ((num_total_propiedades) 
        (renglon) 
        (conjunto_indices (make-array 40)) ;Hard-coded para este conjunto de datos
        (tipo))
    (with-open-file (stream archivo)
      (setq num_total_propiedades (read stream nil nil))
      (read-line stream nil nil)                   ;Para saltarse la línea de comentarios
      (loop for i from 0 below num_total_propiedades do
            (setq renglon (read-from-string (read-line stream nil nil)))
            (setq tipo (nth 2 (nth 1 renglon)))
            (cond
              ((and (equal '+ (second tipo)) (= indice (first tipo))) ;Si es una propiedad positiva
               (loop for k across (first renglon) do
                     (incrementa_indice conjunto_indices k)))
              (t ))))
    (normaliza_arreglo conjunto_indices)))

(defun suma_conteos_ocurrencias_una_clase (archivo indice)
  ;Toma el archivo de propiedades y suma los conteos de ocurrencia de cada
  ;propiedad en una clase.
  (let ((num_total_propiedades) 
        (renglon) 
        (tipo)
        (suma_total 0))
    (with-open-file (stream archivo)
      (setq num_total_propiedades (read stream nil nil))
      (read-line stream nil nil)                   ;Para saltarse la línea de comentarios
      (loop for i from 0 below num_total_propiedades do
            (setq renglon (read-from-string (read-line stream nil nil)))
            (setq tipo (nth 2 (nth 1 renglon)))
            (cond
              ((and (equal '+ (second tipo)) (= indice (first tipo))) ;Si es una propiedad positiva
               (setq suma_total (+ suma_total (aref (nth 1 (nth 1 renglon)) indice))))
              (t ))))
    suma_total))

(defun average (&rest numeros)
;Calcula el promedio de una seria de números
  (/ (apply #'+ numeros) (length numeros) 1.0))

(defparameter *patrones_por_clase* #(127.0 1635.0 281.0 255.0 209.0 1011.0))

(defun suma_conteos_ocurrencias (archivo)
  ;Toma el archivo de propiedades y suma los conteos de ocurrencia de cada
  ;propiedad en una clase.
  (let ((num_total_propiedades) 
        (renglon) 
        (tipo)
        (suma_total_+ (make-array 6))
        (suma_total_- (make-array 6))) ;Hard-coded para este conjunto de datos
    (with-open-file (stream archivo)
      (setq num_total_propiedades (read stream nil nil))
      (read-line stream nil nil)                   ;Para saltarse la línea de comentarios
      (loop for i from 0 below num_total_propiedades do
            (setq renglon (read-from-string (read-line stream nil nil)))
            (setq tipo (nth 2 (nth 1 renglon)))
            (cond
              ((equal '+ (second tipo)) ;Si es una propiedad positiva
               (incf (aref suma_total_+ (first tipo)) (aref (nth 1 (nth 1 renglon)) (first tipo))))
              ((equal '- (second tipo)) ;Si es una propiedad positiva
               (incf (aref suma_total_- (first tipo)) (apply #'max (coerce (nth 1 (nth 1 renglon)) 'list ) )))
              (t ))))
    (list suma_total_+ suma_total_-)))

(defun indice_de_caracterizacion (archivo)
  ;Regresa el índice de caracterización de cada clase
  (loop for k in (suma_conteos_ocurrencias archivo) collect 
        (map 'vector #'/ k *patrones_por_clase*)))

;;;Esto es para calcular automáticamente el indice de caracterización en forma
;;;de script.

(indice_de_caracterizacion (nth 1 *posix-argv*))
