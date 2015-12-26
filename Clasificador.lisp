;;;Algoritmo del clasificador.
;;;En este archivo se definirán las funciones necesarias para realizar la
;;;clasificación de los patrones usando las características obtenidas
;;;anteriormente
(load "Caracterizador_nuevo.lisp")
(load "Script_caracterizador_de_piezas.lisp")
(load "Cobertura.lisp")

(defun caracteristicas_del_patron (archivo patron)
  ;Esta función compara un patrón con el archivo de características. 
  ;TODO: Ver que información regresará. ¿La suma total de los conteos? ¿Lista de
  ;las pertenencias?
  (let ((num_patrones)(lista_valores) (renglon) (mascara)(salida nil))
    (with-open-file (stream archivo :direction :input)
      (setq num_patrones (read stream nil nil))
      (read-line stream nil nil)                   ;Para saltarse la línea de comentarios
      (loop for linea_archivo from 0 below num_patrones do
            (setq renglon (read-from-string (read-line stream nil nil)))
            (print renglon)
            (setq mascara (first renglon)) 
            (setq lista_valores (second renglon))
            (cond
              ((comparacion_con_mascara patron mascara (first lista_valores))
               (setq salida (append salida (list (third lista_valores)))))
              (t ))))
    salida))

(defun tabla_caracteristicas_del_patron (archivo patron)
  ;Esta función compara un patrón con el archivo de características. 
  ;TODO: Ver que información regresará. ¿La suma total de los conteos? ¿Lista de
  ;las pertenencias?
  (let ((tabla_salida (make-hash-table :test #'equal)) ;tabla hash de salida 
        (llave_existe) ;Donde se guarda la llave
        (aux)
        (elemento)
        (num_patrones)(lista_valores) (renglon) (mascara))
    (with-open-file (stream archivo :direction :input)
      (setq num_patrones (read stream nil nil))
      (read-line stream nil nil)                   ;Para saltarse la línea de comentarios
      (loop for linea_archivo from 0 below num_patrones do
            (setq renglon (read-from-string (read-line stream nil nil)))
            ;(print renglon)
            (setq mascara (first renglon)) 
            (setq lista_valores (second renglon))
            (cond
              ((comparacion_con_mascara patron mascara (first lista_valores))
               (setq elemento (third lista_valores))
               (setq llave_existe (gethash elemento tabla_salida))
               ;Ahora las condicionales de si existe o no:
               (cond
                 ((null llave_existe) ;Si la llave no existe
                  (setf (gethash elemento tabla_salida) 1 ))
                 (t ;Si la llave existe.
                   (setq aux (gethash elemento tabla_salida))
                   (setf (gethash elemento tabla_salida) (+ 1 aux))))))))
    tabla_salida))

(defun pertenencias_del_patron (archivo patron)
  ;Obtiene la tabla de características del patrón, y después calcula las
  ;pertenencias.
  (let ((tabla_caracteristicas)(salida (make-array 6 :initial-element 0))(numero 0)(total 0))
    (setq tabla_caracteristicas (tabla_caracteristicas_del_patron archivo patron))
    (loop for llave being the hash-keys in tabla_caracteristicas do
          (setq numero (gethash llave tabla_caracteristicas))
          (cond
            ((equal (second llave) '+) 
             (setq total (first (nth (first llave) *conteo_caracteristicas*))))
            ((equal (second llave) '-) 
             (setq total (* -1 (second (nth (first llave) *conteo_caracteristicas*))) )))
          (cond
            ((> total 0) (setf (aref salida (first llave)) (+ (aref salida (first llave)) (/ numero total 1.0))))
            (t )))
    salida))

(defun separa_caracteristicas (archivo)
  ;Separa las características de un archivo en positivas duras, positivas y
  ;negativas.
  (let ((num_patrones)(renglon)(lista_valores)
        (mascara)(valores)(repeticiones)
        (tipo)
        (positivas)(positivas_duras)(negativas)(negativas_duras))
    (with-open-file (stream archivo :direction :input)
      (setq num_patrones (read stream nil nil))
      (read-line stream nil nil)                   ;Para saltarse la línea de comentarios
      (loop for linea_archivo from 0 below num_patrones do
            (setq renglon (read-from-string (read-line stream nil nil)))
            (setq mascara (first renglon)) 
            (setq lista_valores (rest renglon))
            (loop for valor in lista_valores do
                  (setq valores (first valor))
                  (setq repeticiones (second valor))
                  (setq tipo (determina_tipo_caracterizacion repeticiones *ap* *bp* *an* *bn* *g*))
                  (cond
                    ((equal (second tipo) '+) 
                     (cond
                       ((es_dura repeticiones) (append_to_list (list mascara (list valores repeticiones)) positivas_duras))
                       (t (append_to_list (list mascara (list valores repeticiones)) positivas))))
                    ((equal (second tipo) '-) 
                     (cond
                       ((member 0 (coerce repeticiones 'list)) (append_to_list (list mascara (list valores repeticiones)) negativas_duras))
                       (t (append_to_list (list mascara (list valores repeticiones)) negativas))))
                    (t nil)))))
    (list positivas positivas_duras negativas negativas_duras)))

(defun separa_caracteristicas_archivos (archivo prefijo)
  ;Separa las características de un archivo en positivas duras, positivas y
  ;negativas.
  (let ((num_patrones)(renglon)(lista_valores)
        (mascara)(valores)(repeticiones)
        (tipo))
    (with-open-file (arch_neg_hard (concatenate 'string prefijo "_negativas_duras.txt") :direction :output :if-exists :supersede)
     (with-open-file (arch_neg (concatenate 'string prefijo "_negativas.txt") :direction :output :if-exists :supersede)
       (with-open-file (arch_pos_hard (concatenate 'string prefijo "_positivas_duras.txt") :direction :output :if-exists :supersede)
         (with-open-file (arch_pos (concatenate 'string prefijo "_positivas.txt") :direction :output :if-exists :supersede)
           (with-open-file (stream archivo :direction :input)
             (setq num_patrones (read stream nil nil))
             (read-line stream nil nil)                   ;Para saltarse la línea de comentarios
             (loop for linea_archivo from 0 below num_patrones do
                   (setq renglon (read-from-string (read-line stream nil nil)))
                   (setq mascara (first renglon)) 
                   (setq lista_valores (rest renglon))
                   (loop for valor in lista_valores do
                         (setq valores (first valor))
                         (setq repeticiones (second valor))
                         (setq tipo (determina_tipo_caracterizacion repeticiones *ap* *bp* *an* *bn* *g*))
                         (cond
                           ((equal (second tipo) '+) 
                            (cond
                              ((es_dura repeticiones) (format arch_pos_hard "~S~%" (list mascara (list valores repeticiones tipo))))
                              (t (format arch_pos "~S~%" (list mascara (list valores repeticiones tipo))))))
                           ((equal (second tipo) '-) 
                            (cond
                              ((member 0 (coerce repeticiones 'list)) (format arch_neg_hard "~S~%" (list mascara (list valores repeticiones tipo))))
                              (t (format arch_neg "~S~%" (list mascara (list valores repeticiones tipo))))))
                           (t nil)))))))))))

(defun conteo_caracteristicas_por_compositor (archivo)
  ;Crea una tabla hash con las repeticiones encontradas en los elementos de una
  ;lista.
  (let ((tabla_salida (make-hash-table :test #'equal)) ;tabla hash de salida 
        (llave_existe) ;Donde se guarda la llave
        (aux)
        (renglon)(repeticiones)(num_patrones)(elemento))
    (with-open-file (stream archivo :direction :input)
      (setq num_patrones (read stream nil nil))
      (read-line stream nil nil)                   ;Para saltarse la línea de comentarios
      (loop for linea_archivo from 0 below num_patrones do
            (setq renglon (read-from-string (read-line stream nil nil)))
            (setq repeticiones (second (second renglon)))
            (setq elemento (determina_tipo_caracterizacion repeticiones *ap* *bp* *an* *bn* *g*))
            ;Primero vemos si está el elemento en la tabla hash.
            (setq llave_existe (gethash elemento tabla_salida))
            ;Ahora las condicionales de si existe o no:
            (cond
              ((null llave_existe) ;Si la llave no existe
               (setf (gethash elemento tabla_salida) 1 ))
              (t ;Si la llave existe.
                (setq aux (gethash elemento tabla_salida))
                (setf (gethash elemento tabla_salida) (+ 1 aux))))))
    tabla_salida))

(defun separa_en_tipos (lista)
  ;La lista de características encontradas es procesada para determinar cuáles son
  ;positivas y cuales negativas.
  (let ((tipo)(lista_positivas) (lista_negativas) )
    (loop for k in lista do
          (setq tipo (determina_tipo_caracterizacion k *ap* *bp* *an* *bn* *g*))
          (cond
            ((equal (second tipo) '+) (append_to_list (first tipo) lista_positivas))
            ((equal (second tipo) '-) (append_to_list (first tipo) lista_negativas))
            (t nil)) )
    (list lista_positivas lista_negativas)))

;Idea para clasificar: para el patrón ver que porcentaje de patrones cubre tiene
;de cada grupo.

(defun tabla_repeticiones_general (lista_grupos)
  ;Crea una tabla hash con las repeticiones encontradas en los elementos de una
  ;lista.
  (let ((tabla_salida (make-hash-table)) ;tabla hash de salida 
        (llave_existe) ;Donde se guarda la llave
        (aux))
    ;Iremos grupo por grupo 
    (loop for elemento in lista_grupos do
          ;Primero vemos si está el elemento en la tabla hash.
          (setq llave_existe (gethash elemento tabla_salida))
          ;Ahora las condicionales de si existe o no:
          (cond
            ((null llave_existe) ;Si la llave no existe
             (setf (gethash elemento tabla_salida) 1 ))
            (t ;Si la llave existe.
             (setq aux (gethash elemento tabla_salida))
              (setf (gethash elemento tabla_salida) (+ 1 aux)))))
    tabla_salida))

(defun hash_table_to_list (tabla_hash)
  ;Convierte la tabla hash a una lista de repeticiones
  (let ((salida nil))
    (loop for k being the hash-keys of tabla_hash do
          (append_to_list (list k (gethash k tabla_hash)) salida)) 
    salida))

(defun pertenencias_patron (patron archivo)
  ;De un solo patrón indica cuantas características tiene de cada grupo.
  ;Esto viene en dos listas, la primera es la de las positivas y la segunda la
  ;de las negativas.
  (let ((lista_repeticiones)(lista_pertenencias)(positivas)(negativas))
    (setq lista_repeticiones (separa_en_tipos (caracteristicas_del_patron archivo patron)))
    (setq positivas (tabla_repeticiones_general (first lista_repeticiones)))
    (setq negativas (tabla_repeticiones_general (second lista_repeticiones)))
    (setq lista_pertenencias (list (hash_table_to_list positivas)(hash_table_to_list negativas)))
    lista_pertenencias))

;(defparameter *considerar_por_pieza* #("Ambito_recortado.txt" "Armadura_nuevo.txt" "Pitch_transpuesto.txt" "Conteo_duraciones_recortado.txt" "Relaciones_nuevo.txt" "Octavas_recortado.txt" "Repeticiones_Num_Notas.txt"))
(defparameter *considerar_por_pieza* #("Armadura_nuevo.txt" "Pitch_transpuesto.txt" "Repeticiones_Num_Notas.txt" "Relaciones_nuevo.txt" "Octavas_recortado.txt" "Ambito_recortado.txt" ))

;Ok, entonces es necesario cargar todos los archivos de la pieza. Para esto es
;necesario cargar todos los archivos del folder.

(defun cargar_pieza (ruta)
  ;Cargar los patrones de una pieza en memoria.
  (let ((salida nil)(nombre_archivo)(rasgo))
    ;Ok, primero recorramos todos los archivos de la carpeta uno por uno.
    (loop for tipo_archivo across *considerar_por_pieza* do
          (setq nombre_archivo (concatenate 'string ruta tipo_archivo ))
          (setq rasgo nil)
          (with-open-file (archivo_actual nombre_archivo)
            (loop for linea = (read-line archivo_actual nil 'eof) until (eq linea 'eof) do
                  (append_to_list (read-from-string linea) rasgo)) 
            (append_to_list rasgo salida)))
    salida))

(defun procesa_pieza (pieza)
  ;Después de que la pieza ha sido cargada en memoria es necesario procesarla.
  (let ((salida nil)(num_patrones (length (first pieza)))(patron_individual nil) )
    (loop for indice from 0 below num_patrones do
          ;Esto va a quedar "Hard-coded."
          (setq patron_individual nil)
          (setq patron_individual (concatenate 'vector 
                                                (nth indice ( nth 0 pieza ))
                                                (patron_con_intervalos (nth indice ( nth 1 pieza)) *lista_intervalos_tonos*)
                                                (patron_con_intervalos ( suma_cola (nth indice (nth 2 pieza) ) 4 ) *lista_intervalos_numnotas*)
                                                (patron_con_intervalos (nth indice ( nth 3 pieza )) *lista_intervalos_relaciones*)
                                                (patron_con_intervalos (nth indice ( nth 4 pieza )) *lista_intervalos_octavas*)
                                                (patron_con_intervalos (nth indice ( nth 5 pieza )) *lista_intervalos_ambito*) ))
          (append_to_list patron_individual salida))
    salida))

(defun procesa_pieza_sin (pieza)
  ;Después de que la pieza ha sido cargada en memoria es necesario procesarla.
  (let ((salida nil)(num_patrones (length (first pieza)))(patron_individual nil) )
    (loop for indice from 0 below num_patrones do
          ;Esto va a quedar "Hard-coded."
          (setq patron_individual nil)
          (setq patron_individual (concatenate 'vector 
                                               (nth indice ( nth 0 pieza ))
                                               (nth indice ( nth 1 pieza))
                                               ( suma_cola (nth indice (nth 2 pieza) ) 4 )
                                               (nth indice ( nth 3 pieza ))
                                               (nth indice ( nth 4 pieza ))
                                               (nth indice ( nth 5 pieza ))))
          (append_to_list patron_individual salida))
    salida))

(defun indice_max (arreglo)
  ;Regresa el índice del elemento más grande
  (let ((contador 0)(salida 0)(maximo (aref arreglo 0)))
    (loop for k across arreglo do
          (cond
            ((> k maximo) 
             (setq maximo k)
             (setq salida contador)))
          (setq contador (+ contador 1)))
    salida))

(defun tabla_a_lista (tabla)
;Transforma una tabla a un arreglo.
  (loop for llave being the hash-keys of tabla collect
   (list llave (gethash llave tabla))))

(defun caracteristicas_pieza (ruta archivo_caracteristicas)
  ;Carga los archivos de la pieza, y regesa la lista de las características que
  ;tiene la pieza.
  (let ((pieza) (pert_individual)(max_individual)(maximos)(pertenencias (make-array 0 :adjustable t :fill-pointer 0))(num_patrones)(contador 1))
    (setq pieza (procesa_pieza (cargar_pieza ruta)))
    (setq num_patrones (length pieza))
    (print ruta)
    (format t "Son ~S patrones.~%" num_patrones)
    (loop for patron in pieza do
          (setq pert_individual (pertenencias_del_patron archivo_caracteristicas patron))
          (setq max_individual (indice_max pert_individual))
          (append_to_list max_individual maximos)
          (format t "~S - ~S: ~S~%" contador pert_individual max_individual)
          (vector-push-extend pert_individual pertenencias)
          (setq contador (+ 1 contador)))
    (print (indice_max (sumatoria_+p pertenencias)))
    (print (sort (tabla_a_lista (tabla_repeticiones_general maximos)) #'> :key #'second))))

(defun caracteristicas_pieza_sin_intervalos (ruta archivo_caracteristicas)
  ;Carga los archivos de la pieza, y regesa la lista de las características que
  ;tiene la pieza.
  (let ((pieza) (pert_individual)(max_individual)(maximos)(pertenencias (make-array 0 :adjustable t :fill-pointer 0))(num_patrones)(contador 1))
    (setq pieza (procesa_pieza_sin (cargar_pieza ruta)))
    (setq num_patrones (length pieza))
    (print ruta)
    (format t "Son ~S patrones.~%" num_patrones)
    (loop for patron in pieza do
          (setq pert_individual (pertenencias_del_patron archivo_caracteristicas patron))
          (setq max_individual (indice_max pert_individual))
          (append_to_list max_individual maximos)
          (format t "~S - ~S: ~S~%" contador pert_individual max_individual)
          (vector-push-extend pert_individual pertenencias)
          (setq contador (+ 1 contador)))
    (print (indice_max (sumatoria_+p pertenencias)))
    (print (sort (tabla_a_lista (tabla_repeticiones_general maximos)) #'> :key #'second))))
