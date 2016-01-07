;;;Estas líneas es para el script de coberturas. Será necesario comentarlas
;;;cuando ya no se usen.
(load "Caracterizador_nuevo.lisp")
(load "Script_caracterizador_de_piezas.lisp")
(defvar *archivo_propiedades* (nth 1 *posix-argv*))
(defvar *ap* (read-from-string (nth 2 *posix-argv*)))
(defvar *bp* (read-from-string (nth 3 *posix-argv*)))
(defvar *bn* (read-from-string (nth 2 *posix-argv*)))
(defvar *an* (read-from-string (nth 3 *posix-argv*)))
;;;

;En este archivo se incluyen las funciones para determinar la cobertura de las
;características encontradas.

;;Empecemos con los parámetros:
(defparameter *g* 100)

(defun comparacion_con_mascara (patron mascara segmento)
  ;Compara si el patrón es igual al segmento después de haberle aplicado la
  ;máscara.
  (let ((num_total (length mascara)) (diferentes nil))
    (loop for k from 0 below num_total until diferentes do
          (cond
            ((not (equal (aref segmento k) (aref patron (aref mascara k)))) (setq diferentes t))))
    (if diferentes nil t)))

(defun indices_patrones (grupo mascara segmento &optional verbose)
  ;Llama a la función de comparacion_con_mascara para regresar una lista
  ;conteniendo los indices de los patrones a los que se les cumplió dicha
  ;característica.
  (let ((salida)(num_total (length grupo)))
    (loop for indice from 0 below num_total do
          (cond
            ((comparacion_con_mascara (aref grupo indice) mascara segmento) (setq salida (append salida (list indice))))
            (t nil)))
    (if verbose (format t "Son ~S patrones, cubren ~S%~%" (length salida)(* 1.0 (/ (length salida) (length grupo)))))
    salida))

(defun es_dura (arreglo)
  ;Indica si el vector corresponde a una característica dura
  (let ((bandera nil)(no_es nil))
    (loop for k from 0 below (length arreglo) until no_es do
          (cond
            ((and (> (aref arreglo k) 0) (null bandera)) (setq bandera k))
            ((and (> (aref arreglo k) 0) bandera) (setq no_es t))))
    (if no_es nil bandera)))

(defun revisa_lista_es_dura (lista)
  ;Revisa si la lista tiene características duras.
  (let ((mascara (first lista))(carac (rest lista)) (salida nil))
    (loop for k in carac do
          (cond
            ((es_dura (second k)) (setq salida (append salida (list k))))))
    (cond
      ((null salida) nil)
      (t (append (list mascara) salida)))))

(defun caracteristicas_duras (archivo)
  ;Recibe el archivo, y regresa las características duras que ha encontrado.
  (let ((contador 0)(num_patrones)(renglon))
    (with-open-file (stream archivo)
      (setq num_patrones (read stream nil nil))
      (read-line stream nil nil)                   ;Para saltarse la línea de comentarios
      (loop for k from 0 below num_patrones do
            (setq renglon (revisa_lista_es_dura (read-from-string (read-line stream nil nil))))
            (cond
              (renglon (setq contador (+ 1 contador))(print renglon)))))
    contador)) 

(defun revisa_lista_tiene_positivas (lista)
  ;Revisa si la lista tiene características positivas.
  (let ((mascara (first lista))(carac (rest lista)) (salida nil)(tipo_carac))
    (loop for k in carac do
          (setq tipo_carac (determina_tipo_caracterizacion (second k) *ap* *bp* *an* *bn* *g*))
          (cond
            ((equal '+ (second tipo_carac)) 
             (setq salida (append salida (list (list (first k) (first tipo_carac))))))))
    (cond
      ((null salida) nil)
      (t (append (list mascara) salida)))))


(defun cobertura_positivas (grupos archivo)
  ;Determina la cobertura de las características positivas que se encuentren.
  (let ((num_patrones)(renglon)(conjuntos (make-array (length grupos) :initial-element nil))(patrones_encontrados) (indice_grupo))
    (with-open-file (stream archivo)
      (setq num_patrones (read stream nil nil))
      (read-line stream nil nil)                   ;Para saltarse la línea de comentarios
      (loop for k from 0 below num_patrones do
            (setq renglon (revisa_lista_tiene_positivas (read-from-string (read-line stream nil nil))))
            (cond
              (renglon 
                (loop for elemento in (rest renglon) do
                      (setq indice_grupo (second elemento))
                      (setq patrones_encontrados (indices_patrones (nth indice_grupo grupos) (first renglon) (first elemento)))
                      (setf (aref conjuntos indice_grupo) (union (aref conjuntos indice_grupo) patrones_encontrados)))))))
    (loop for indice from 0 below (length conjuntos) do 
          (format t "~%Cubre ~S patrones de ~S. Es ~S%~%" 
                  (length (aref conjuntos indice)) 
                  (length (nth indice grupos)) 
                  (/ (length (aref conjuntos indice)) (length (nth indice grupos)) 0.01)))))

(defun cobertura_positivas_csv (grupos archivo)
  ;Determina la cobertura de las características positivas que se encuentren.
  (let ((num_patrones)(renglon)(conjuntos (make-array (length grupos) :initial-element nil))(patrones_encontrados) (indice_grupo))
    (with-open-file (stream archivo)
      (setq num_patrones (read stream nil nil))
      (read-line stream nil nil)                   ;Para saltarse la línea de comentarios
      (loop for k from 0 below num_patrones do
            (setq renglon (revisa_lista_tiene_positivas (read-from-string (read-line stream nil nil))))
            (cond
              (renglon 
                (loop for elemento in (rest renglon) do
                      (setq indice_grupo (second elemento))
                      (setq patrones_encontrados (indices_patrones (nth indice_grupo grupos) (first renglon) (first elemento)))
                      (setf (aref conjuntos indice_grupo) (union (aref conjuntos indice_grupo) patrones_encontrados)))))))
    (loop for indice from 0 below (length conjuntos) do 
          (format t "~S," (length (aref conjuntos indice))))
    (format t "~%")))

(cobertura_positivas_csv *supervision* *archivo_propiedades*)
