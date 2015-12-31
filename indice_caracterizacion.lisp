;;; Este archivo contiene la información necesaria para calcular el "índice de
;;; caracterización" de un archivo de propiedades.

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

(defun resta_ (argumentos)
;Descripción
  
  )
