;;;En este documento acomodo los comandos del Caracterizador, pero acomodadas
;;;por categoría.
;;;

;================================================================================
;Variables y constantes de los archivos;{{{
;================================================================================

(if (not (boundp 'dir_patrones))  (defconstant dir_patrones  "Patrones por Pieza/"))
(if (not (boundp 'dir_bach))      (defconstant dir_bach      "Patrones por Pieza/Bach/"))
(if (not (boundp 'dir_mozart))    (defconstant dir_mozart    "Patrones por Pieza/Mozart/"))
(if (not (boundp 'dir_muzio))     (defconstant dir_muzio     "Patrones por Pieza/Muzio/"))
(if (not (boundp 'dir_beethoven)) (defconstant dir_beethoven "Patrones por Pieza/Beethoven/"))
(if (not (boundp 'dir_chopin))    (defconstant dir_chopin    "Patrones por Pieza/Chopin/"))
(if (not (boundp 'dir_debussy))   (defconstant dir_debussy   "Patrones por Pieza/Debussy/"))
(if (not (boundp 'dir_joplin))    (defconstant dir_joplin    "Patrones por Pieza/Joplin/"))

(defparameter *considerar* (list dir_bach dir_mozart dir_muzio dir_beethoven dir_debussy dir_joplin))
;}}}

;================================================================================
;Variables y constantes para el algoritmo de caracterización;{{{
;================================================================================

;Aquí se guardan los grupos de supervisión. Es una lista de listas, en la que 
;cada una contiene los patrones que componen a un grupo.
(defvar *supervision*)
;}}}

;================================================================================
;Funciones para la carga de archivos.;{{{
;================================================================================


(defun ruta (dir archivo)
  "Regresa la ruta completa del archivo"
  (concatenate 'string dir archivo))

(defun read-file (path) 
  ;Lee un archivo que contenga datos, y regresa un arreglo conteniendo arreglos con la información de cada patrón. 
  (let ((num_patrones) (patrones (make-array 0 :adjustable t :fill-pointer 0)))
    (with-open-file (stream path)
      (setq num_patrones (read stream nil nil))
      (adjust-array patrones num_patrones)
      (read-line stream nil nil)                   ;Para saltarse la línea de comentarios
      (loop for i from 0 below num_patrones do
            ;(setq patron (read-from-string (read-line stream nil nil)))
            ;(setq patrones (append patrones (list patron)))
            (vector-push (read-from-string (read-line stream nil nil)) patrones)))
    patrones))

(defun carga_tipo_archivo (nombre)
  "Carga el archivo de patrones de todos los compositores"
  (loop for k in *considerar* collect
        (read-file (ruta k nombre))))
;}}}

;================================================================================
;Funciones del algoritmo de caracterización.;{{{
;================================================================================

(defun subconjuntos (conjunto  &optional (cardMin 0) (cardMax (length conjunto)) ) 
;Esta función recibe una lista, y entrega los subconjuntos de esa lista que
;contengan la cardinalidad especificada. Se usa para obtener parte del conjunto
;potencia de los rasgos.
  (cond ((< cardMax cardMin) nil) 
        ((= cardMax cardMin)
         (if (zerop cardMin) (list '()) 
           (loop for elementos on conjunto nconc 
                 (mapcar #'(lambda (conj) (cons (first elementos) conj)) 
                         (subconjuntos (rest elementos) (- cardMin 1) (- cardMax 1)))))) 
        (t (loop for i :from cardMin :to cardMax nconc (Subconjuntos conjunto i i)))))

(defun listas_rasgos (inferior superior num)
  "Primero crea una lista del 0 al número de rasgos que tienen los patrones.
  Después usa la función subconjuntos para crear subconjuntos de cardinalidad
  inferior a superior."
  (let ((lista (loop for i from 0 below num collect i)))
    (subconjuntos lista inferior superior)))

(defun máscara_rasgos (patron mask)
;De un patrón, aplica una máscara para sólo tomar en cuenta dichos rasgos.
    (loop for i in mask collect (aref patron i)))


(defun mascara_arreglo (arreglo mascara)
  "Aplica una máscara, siendo la máscara un arreglo" 
  (let ((salida (make-array (length mascara) :fill-pointer 0))) 
    (loop for k across mascara do (vector-push (aref arreglo k) salida)) salida))

;Con esto comienza el código con la tabla hash.
;Cada elemento de la tabla va a ser una lista de dos objetos:
;    - El primero es el que más nos interesa: una lista indicando cuantas veces
;    se repite el patrón en cada grupo.
;    - El seguno es un valor auxiliar. Si es nil indica que sólo una vez se ha
;    escrito en dicha entrada, indicándonos que ese patrón no se repite. Si es T
;    es que han habido mínimo dos repeticiones.
;Con esto después se iterará sobre las llaves de la tabla para obtener las
;repeticiones.

(defmacro incrementa_indice (arreglo indice)
  "De un arreglo incrementa en uno el valor de un índice."
  `(setf (aref ,arreglo ,indice) (+ 1  (aref ,arreglo ,indice))))

(defun tabla_repeticiones (lista_grupos mascara)
  ;Crea una tabla hash con las repeticiones encontradas hasta ahora. Esta tabla
  ;es única para dicha máscara.
  (let ((tabla_salida (make-hash-table :test #'equal)) ;tabla hash de salida 
        (num_grupos (length lista_grupos)) ;Número de grupos en total
        (patrones_grupo)  ;Lista conteniendo los patrones del grupo
        (subpatron) ;Subpatrón obtenido después de aplicar máscara
        (llave_existe) ;Donde se guarda la llave
        (arreglo_repeticiones))
    ;Iremos grupo por grupo 
    (loop for indice_grupo from 0 below num_grupos do
          ;Luego iremos patrón por patrón
          (setq patrones_grupo (nth indice_grupo lista_grupos))
          (loop for patron across patrones_grupo do
                (setq subpatron (máscara_rasgos patron mascara))
                ;Primero vemos si está el patrón en la tabla hash.
                (setq llave_existe (gethash subpatron tabla_salida))
                ;Ahora las condicionales de si existe o no:
                (cond
                  ((null llave_existe) ;Si la llave no existe
                   (setq arreglo_repeticiones (make-array num_grupos :initial-element 0))
                   (setf (aref arreglo_repeticiones indice_grupo) 1) 
                   (setf (gethash subpatron tabla_salida) arreglo_repeticiones ))
                  (t ;Si la llave existe.
                    (incrementa_indice (gethash subpatron tabla_salida) indice_grupo)))))
    tabla_salida))

(defun imprime_tabla (tabla)
  "Imprime una tabla hash"
  (maphash #'(lambda (llave valor) (format t "~S:    ~S~%" llave valor)) tabla))

(defmacro limpia_tabla (tabla alpha+ beta+ alpha- beta- gamma)
  "De una tabla hash elimina los elementos que no cumplen los criterios."
  `(loop for key being the hash-keys of ,tabla do
         (cond
           ((null (determina_tipo_caracterizacion (gethash key ,tabla) ,alpha+ ,beta+ ,alpha- ,beta- ,gamma)) (remhash key ,tabla)))))

(defun busca_repeticiones_hash (lista_grupos minimo maximo alpha+ beta+ alpha- beta- gamma)
  "Recibe la lista de grupos, la cardinalidad mínima y la máxima.  Con ello
  determina las repeticiones."
  (let ((salida)
        (tabla)
        (lista_mascaras (listas_rasgos minimo maximo (length (aref (first lista_grupos) 0)))))
    (setq salida (make-hash-table :test #'equal))
    (loop for k from 0 below (length lista_mascaras) do
          (setq tabla (tabla_repeticiones lista_grupos (nth k lista_mascaras)))
          (limpia_tabla tabla alpha+ beta+ alpha- beta- gamma)
          (cond
            ((< 0 (hash-table-count tabla)) (setf (gethash (nth k lista_mascaras) salida) tabla))
            (t nil)))
    salida))

(defun normaliza_arreglo (repeticiones totales)
;Normaliza el conteo de las repeticiones al dividr el arreglo de las
;repeticiones entre la lista que contiene las 
  (let ((salida (make-array (length repeticiones))))
    (loop for indice from 0 below (length repeticiones) do
        (setf (aref salida indice) (round (/ (aref repeticiones indice) (nth indice totales) 0.001 ))))
    salida))

(defmacro normaliza_tabla (tabla totales)
  ;Normaliza todas las salidas de una tabla hash
  `(loop for key being the hash-keys in ,tabla do
         (setf (gethash key ,tabla) (normaliza_arreglo (gethash key ,tabla) ,totales))))

(defun busca_repeticiones_normalizada (lista_grupos minimo maximo alpha+ beta+ alpha- beta- gamma)
  "Recibe la lista de grupos, la cardinalidad mínima y la máxima.
  Con ello determina las repeticiones."
  (let ((salida)
        (tabla)
        (sizes (loop for k in lista_grupos collect (length k)))
        (lista_mascaras (listas_rasgos minimo maximo (length (aref (first lista_grupos) 0)))))
   (print sizes)
    (setq salida (make-hash-table :test #'equal))
    (loop for k from 0 below (length lista_mascaras) do
          (setq tabla (tabla_repeticiones lista_grupos (nth k lista_mascaras)))
          (normaliza_tabla tabla sizes)
          (limpia_tabla tabla alpha+ beta+ alpha- beta- gamma)
          (cond
            ((< 0 (hash-table-count tabla)) (setf (gethash (nth k lista_mascaras) salida) tabla))
            (t nil)))
    salida))

(defun resultados_a_archivo (lista_grupos minimo maximo alpha+ beta+ alpha- beta- gamma archivo)
  ;Realiza la búsqueda de repeticiones y al final imprime todo lo relevante a un
  ;archivo.
  (let ((tabla)(subtabla))
    (time (setq tabla (busca_repeticiones_normalizada lista_grupos minimo maximo alpha+ beta+ alpha- beta- gamma)))
    (with-open-file (stream archivo :direction :output :if-exists :supersede)
      (format stream "Resultados:~%")
      (format stream "Valores:~%  Alpha+: ~S~%" alpha+)
      (format stream "  Beta+:  ~S~%" beta+)
      (format stream "  Alpha-: ~S~%" alpha-)
      (format stream "  Beta-:  ~S~%" beta-)
      (format stream "  Gamma:  ~S~%~%~%" gamma)
      (loop for key being the hash-keys in tabla do 
            (setq subtabla (gethash key tabla))
            (format stream "~%Máscara: ~S~%" key)
            (maphash #'(lambda (llave valor) (format stream "~S:    ~S ~%" llave valor)) subtabla)))))

(defun busca_repeticiones_a_archivo (lista_grupos minimo maximo alpha+ beta+ alpha- beta- gamma archivo)
  "Recibe la lista de grupos, la cardinalidad mínima y la máxima.
  Con ello determina las repeticiones."
  (let ( (tabla)
         (contador 0)
         (decimo)
         (fraccion)
         (num_mascaras)
        (sizes (loop for k in lista_grupos collect (length k)))
        (lista_mascaras (listas_rasgos minimo maximo (length (aref (first lista_grupos) 0)))))
    (print sizes)
    (setq num_mascaras (length lista_mascaras))
    (setq decimo (/ num_mascaras 100))
    (setq fraccion decimo)
    (with-open-file (stream archivo :direction :output :if-exists :supersede)
      (format stream "Resultados:~%")
      (format stream "Valores:~%  Alpha+: ~S~%" alpha+)
      (format stream "  Beta+:  ~S~%" beta+)
      (format stream "  Alpha-: ~S~%" alpha-)
      (format stream "  Beta-:  ~S~%" beta-)
      (format stream "  Gamma:  ~S~%~%~%" gamma)
      (loop for mascara in lista_mascaras do
       (setq contador (+ 1 contador))
       (cond
         ((> contador fraccion) (format t "~S%  ~S~%" (* 100 ( / fraccion num_mascaras )) contador ) (setq fraccion (+ fraccion decimo)))
         (t ))
            (setq tabla (tabla_repeticiones lista_grupos mascara))
            (normaliza_tabla tabla sizes)
            (limpia_tabla tabla alpha+ beta+ alpha- beta- gamma)
            (cond
              ((< 0 (hash-table-count tabla)) 
               (format stream "~%Máscara: ~S~%" mascara)
               (maphash #'(lambda (llave valor) (format stream "~S:    ~S ~%" llave valor)) tabla))
              (t nil))))))

;}}}

;================================================================================
;Funciones del caracterizador, leyendo máscaras de un archivo;{{{
;================================================================================

(defun caracteriza_con_archivo_mascaras (lista_grupos archivo_mascaras alpha+ beta+ alpha- beta- gamma archivo)
  "Recibe la lista de grupos, el archivo de máscaras.
  Con ello determina las repeticiones."
  (let ((tabla)
        (contador 0)
        (decimo)
        (porcentaje)
        (fraccion)
        (num_mascaras)
        (mascara)
        (sizes (loop for k in lista_grupos collect (length k))))
    (with-open-file (mascaras archivo_mascaras :direction :input)
      (setq num_mascaras (read mascaras nil nil))
      (setq decimo (/ num_mascaras 1000))
      (setq fraccion decimo)
      (read-line mascaras nil nil)              ;Para saltarse la línea de comentarios
      (with-open-file (stream archivo :direction :output :if-exists :supersede)
        (format stream "Resultados:~%")
        (format stream "Valores:~%  Alpha+: ~S~%" alpha+)
        (format stream "  Beta+:  ~S~%" beta+)
        (format stream "  Alpha-: ~S~%" alpha-)
        (format stream "  Beta-:  ~S~%" beta-)
        (format stream "  Gamma:  ~S~%~%~%" gamma)
        (format t "0%")
        (loop 
          for linea = (read-line mascaras nil 'eof)  
          until (eq linea 'eof) 
          do
          (setq mascara (read-from-string linea))
          (setq contador (+ 1 contador))
          (cond
            ((> contador fraccion) 
             (setq porcentaje (* 100.0 ( / fraccion num_mascaras )))
             (format t " ~A - ~A%~%" contador porcentaje)
             ;(loop for numerito from 0 to porcentaje by 5 do (format t "#"))
             ;(format t "~A -> ~A%" contador porcentaje) 
             (setq fraccion (+ fraccion decimo)))
            (t ))
          (setq tabla (tabla_repeticiones lista_grupos mascara))
          (normaliza_tabla tabla sizes)
          (limpia_tabla tabla alpha+ beta+ alpha- beta- gamma)
          (cond
            ((< 0 (hash-table-count tabla)) 
             (format stream "~%Máscara: ~S~%" mascara)
             (maphash #'(lambda (llave valor) (format stream "~S:    ~S ~%" llave valor)) tabla))
            (t nil)))))))

(defmacro limpia_tabla_nuevo (tabla alpha+ beta+ alpha- beta- gamma)
  "De una tabla hash elimina los elementos que no cumplen los criterios."
  (let ((elemento (gensym "elemento-")) (caracteristica (gensym "carac-")))
    `(let ((,elemento) (,caracteristica) )
       (loop for key being the hash-keys of ,tabla do
             (setq ,elemento (gethash key ,tabla))
             (setq ,caracteristica (determina_tipo_caracterizacion ,elemento ,alpha+ ,beta+ ,alpha- ,beta- ,gamma))
             (cond
               ((null ,caracteristica) (remhash key ,tabla))
               (t (setf (gethash key ,tabla) (list ,elemento ,caracteristica ))))))))

(defun caracteriza_nuevo (lista_grupos archivo_mascaras alpha+ beta+ alpha- beta- gamma archivo)
  "Recibe la lista de grupos, el archivo de máscaras.  Con ello determina las repeticiones."
  (let ((tabla)
        (contador 0)
        (decimo)
        (porcentaje)
        (fraccion)
        (num_mascaras)
        (mascara)
        (sizes (loop for k in lista_grupos collect (length k))))
    (with-open-file (mascaras archivo_mascaras :direction :input)
      (setq num_mascaras (read mascaras nil nil))
      (setq decimo (/ num_mascaras 100))
      (setq fraccion decimo)
      (read-line mascaras nil nil)              ;Para saltarse la línea de comentarios
      (with-open-file (stream archivo :direction :output :if-exists :supersede)
        (format t "~0%")
        (loop 
          for linea = (read-line mascaras nil 'eof)  
          until (eq linea 'eof) 
          do
          (setq mascara (read-from-string linea))
          (setq contador (+ 1 contador))
          (cond
            ((> contador fraccion) 
             (setq porcentaje (* 100.0 ( / fraccion num_mascaras )))
             (format t " ~A - ~A%~%" contador porcentaje)
             (setq fraccion (+ fraccion decimo)))
            (t ))
          (setq tabla (tabla_repeticiones lista_grupos mascara))
          (normaliza_tabla tabla sizes)
          (limpia_tabla_nuevo tabla alpha+ beta+ alpha- beta- gamma)
          (cond
            ((< 0 (hash-table-count tabla)) 
             (loop for llave being the hash-keys of tabla do
                   (format stream "~S~%" (list (coerce mascara 'vector) (append (list (coerce llave 'vector)) (gethash llave tabla))))))
            (t nil)))))))

;}}}

;================================================================================
;Variables para determinar el tipo de caracterización;{{{
;================================================================================

;Gamma es para las caracterizaciones neutras, si una característica se repite en
;todos los grupo más de *gamma* veces, entonces es una característica neutra.
(defparameter *gamma* 10)

;Alpha1 y beta1 son para las caracterizaciones positivas. Si una característica 
;se repite más de *alpha1* veces en ese grupo y menos de *beta1* en los demás,
;entonces es una característica positiva para dicho grupo.
(defparameter *alpha+* 50)
(defparameter *beta+* 40)

;Alpha2 y beta2 son para las caracterizaciones negativas. Si una característica 
;se repite menos de *alpha2* veces en ese grupo y más de *beta2* en los demás,
;entonces es una característica negativa para dicho grupo.
(defparameter *alpha-* 20)
(defparameter *beta-* 30)
;}}}

;================================================================================
;Funciones para determinar el tipo de caracterización;{{{
;================================================================================
(defun enlista_elemento_indice (arreglo)
  ;Regresa una lista nueva, en donde cada elemento es una lista en donde el
  ;primer elemento es el índice del grupo el segundo el valor original del
  ;arreglo.
  (loop for k from 0 below (length arreglo) collect (list k (aref arreglo k))))

(defun es_positiva (lista alfa+ beta+)
  ;Recibe la lista e indica si corresponde a una caracterización positiva.
  (cond
    ((>= (second (first lista)) alfa+) 
     (cond
       ((<= (second (second lista)) beta+) (first (first lista)))
       (t nil))) 
    (t nil)))

(defun es_negativa (lista alfa- beta-)
  ;Indica si la lista representa una caracterización negativa.
  (let ((num (- (length lista) 1)))
    (cond
      ((<= (second (nth num lista)) alfa-) 
       (cond
         ((>= (second (nth (- num 1) lista)) beta-) (first (nth num lista)))
         (t nil)))
      (t nil))))

(defun es_neutra (lista gamma)
  ;Indica si la lista representa una caracterización neutra
  (let ((num (- (length lista) 1)))
    (cond
      ((>= (second (nth num lista)) gamma) t)
      (t nil))))

(defun determina_tipo_caracterizacion (arreglo alfa+ beta+ alfa- beta- gamma)
  ;Descripción
  (let* ((salida) (lista (sort (enlista_elemento_indice arreglo) #'> :key #'second)))
    (setq salida (es_positiva lista alfa+ beta+))
    (cond
      (salida (setq salida (list salida '+)))
      (t 
        (setq salida (es_negativa lista alfa- beta-))
        (cond
          (salida (setq salida (list salida '-)))
          (t 
            (setq salida (es_neutra lista gamma))
            (cond
              (salida (setq salida (list salida 'n)))
              (t (setq salida nil)))))))
    salida))
;}}}

;================================================================================
;Funciones de diferencia;{{{
;================================================================================

(defgeneric diferencia_numerica (A B)
            (:documentation "El valor absoluto de la resta de un número y el otro"))

(defmethod diferencia_numerica ((A array) (B array))
  (let ((salida 0) (num (length A)))
    (cond
      ((= num (length B) ) 
       (loop for k from 0 below num do
             (setq salida (+ salida (abs (- (aref A k) (aref B k)))))))
      (t (error "Los arreglos no son de la misma longitud.")))
    salida))

(defmethod diferencia_numerica ((A list) (B list))
  (reduce #'+ (mapcar #'(lambda (x y) (abs (- x y))) A B)) )

(defun dif_casi_booleana (a b)
  "Regresa T si ambos elementos son mayores a 0 o iguales a 0, regresa nil en caso de que uno sea 0 y el otro no."
  (cond
    ((or (and (= a 0) (= b 0)) (and (> a 0) (> b 0))) 0)
    (t 1 )))

(defgeneric diferencia_miembros (A B)
 (:documentation "Compara dos listas. Si hay un elemento que si tenga una lista y el otro no, se suma uno a las diferencias. Si ambos lo tienen o ambos carecen de el, se ignora"))

(defmethod diferencia_miembros ((A array) (B array))
  (let ((salida 0) (num (length A)))
    (cond
      ((= num (length B) ) 
       (loop for k from 0 below num do
             (setq salida (+ salida (dif_casi_booleana (aref A k)(aref B k))))))
      (t (error "Los arreglos no son de la misma longitud.")))
    salida))

(defun matriz_diferencias (patrones funcion)
  "Calcula la matriz de distancia en una lista de patrones"
  (let* ((num (length patrones))(matriz (make-array num )) (renglon))
    (loop for fila from 0 below num do
          (setq renglon (make-array (+ fila 1) :fill-pointer 0))
          (loop for col from 0 to fila do
                (vector-push (funcall funcion (aref patrones fila) (aref patrones col)) renglon))
          (setf (aref matriz fila) renglon))
    matriz))

 (defun sumatoria (arreglo)
   "Suma todo"
   (let ((salida (make-array (length (aref arreglo 0)) :initial-element 0)))
     (loop for k across arreglo do
           (setq salida (suma_arreglos salida k)))
     salida)) 

(defun suma_arreglos (a b)
  "suma arreglos"
  (let ((salida (make-array (length a) :fill-pointer 0)))
    (loop for k from 0 below (length a) do
          (vector-push (+ (aref a k) (aref b k)) salida))
    salida))

(defun sumatoria_porcentaje (arreglo)
   "Suma todo"
   (let ((salida (make-array (length (aref arreglo 0)) :initial-element 0)))
     (loop for k across arreglo do
           (setq salida (suma_arreglos salida k)))
     (loop for k across salida collect (/ k (length arreglo) 1.0))))

(defun +p ( &rest num)
  ;Suma números, pero si el resultado es negativo regresa 0.
  (let ((salida (apply #'+ num)))
    (cond
      ((< salida 0) 0)
      (t salida))) )

(defun +p_arreglos (a b)
  "suma arreglos"
  (let ((salida (make-array (length a) :fill-pointer 0)))
    (loop for k from 0 below (length a) do
          (vector-push (+p (aref a k) (aref b k)) salida))
    salida))

 (defun sumatoria_+p (arreglo)
   "Suma todo"
   (let ((salida (make-array (length (aref arreglo 0)) :initial-element 0)))
     (loop for k across arreglo do
           (setq salida (+p_arreglos salida k)))
     salida)) 

;}}}

;================================================================================
;Funciones para verificar repeticiones;{{{
;================================================================================

(defun elementos_rasgo (arreglo indice)
  ;Regresa una tabla hash en donde las llaves son los elementos que se han
  ;encontrado en los patrones, mientras que el destino son el número de veces que
  ;ha aparecido cada elemento.
  (let ((tabla (make-hash-table :test #'equal )) (valor) (llave_existe))
    (loop for patron across arreglo do
          (setq valor (aref patron indice))
          (setq llave_existe (gethash valor tabla))
          (cond
            (llave_existe (setf (gethash valor tabla) (+ llave_existe 1)))
            (t (setf (gethash valor tabla) 1)))
          )
    tabla))

(defun unir_en_arreglote (lista)
  ;Une una lista en un arreglote
  (let ((salida (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for grupo in lista do
          (loop for arreglo across grupo do
                (vector-push-extend arreglo salida))) 
    salida))
;}}}

;================================================================================
;Funciones para imprimir a archivo;{{{
;================================================================================

(defun tabla_a_csv (lista nombre)
  "Crea un archivo csv para que los arreglos de la lista queden como una tabla"
  (with-open-file (stream nombre :direction :output :if-exists :supersede)
    (loop for fila from 0 below (length (first lista)) do
          (loop for col in lista do
                (format stream "~S, " (aref col fila)))
          (format stream "~%"))))

;Con el comando de elementos_rasgo obtenemos la tabla hash de los elementos del
;rasgo con sus repeticiones. Ok, hagamos una función para imprimir en un archivo
;csv todas esas tablas, para cada uno de los rasgos.
(defun crear_archivo_repeticiones (lista_patrones nombre_archivo)
  ;De la lista de patrones crea un archivo csv para poder graficar las
  ;repeticiones de los elementos
  (let ((num_rasgos (length (aref lista_patrones 0))) (tabla))
    (with-open-file (stream nombre_archivo :direction :output :if-exists :supersede)
      (format stream "Repeticiones de los rasgos: ~%")
      (loop for k from 0 below num_rasgos do
            (setq tabla (elementos_rasgo lista_patrones k))
            (format stream "~%Rasgo ~S~%" k)
            (loop for key being the hash-keys of tabla do
                  (format stream "~S,~S~%" key (gethash key tabla)))))))

(defun crear_archivo_repeticiones_mat (lista_patrones nombre_archivo)
  ;De la lista de patrones crea un archivo csv para poder graficar las
  ;repeticiones de los elementos
  (let ((num_rasgos (length (aref lista_patrones 0))) (tabla))
    (with-open-file (stream (concatenate 'string nombre_archivo ".m") :direction :output :if-exists :supersede)
      (loop for k from 0 below num_rasgos do
            (setq tabla (elementos_rasgo lista_patrones k))
            (format stream "~%~S_~S=[" nombre_archivo k)
            (loop for key being the hash-keys of tabla do
                  (format stream "~S,~S;" key (gethash key tabla)))
            (format stream "]~%")))))
;}}}

;================================================================================
;Funciones para dividir en intervalos los datos.;{{{
;================================================================================

(defun pertenece_a_intervalo (valor arreglo_intervalos)
  ;Regresa a que intervalo pertenece el valor. Si es menor a todos los valores,
  ;entonces pertenece al intervalo 0. 
  (let ((salida -1)(fin))
    (loop for k across arreglo_intervalos until fin do 
          (cond
            ((< valor k) (setq fin t))
            (t (setq salida (+ 1 salida)))))
    salida))

(defun patron_con_intervalos (patron arreglo_intervalos)
  ;Recibe un patrón,y convierte los rasgos para que representen a que intervalo
  ;pertenecen
  (let* ((num (length patron))(salida (make-array num :fill-pointer 0)) (intervalos))
    (loop for k from 0 below num do
          (setq intervalos (aref arreglo_intervalos k))
          (cond
            (intervalos (vector-push (pertenece_a_intervalo (aref patron k) intervalos) salida))
            (t (vector-push (aref patron k) salida))))
    salida))

(defun intervaliza_grupo (grupo arreglo_intervalos)
  ;Toma un grupo de patrones y regresa un grupo nuevo en donde los grupos ya están
  ;intervalizados
  (let* ((num (length grupo))(salida (make-array num :fill-pointer 0)))
    (loop for patron across grupo do
          (vector-push (patron_con_intervalos patron arreglo_intervalos) salida))
    salida))

(defun intervaliza_todos_los_datos (datos arreglo_intervalos)
  ;Toma un grupo de patrones y regresa un grupo nuevo en donde los grupos ya están
  ;intervalizados
  (loop for k in datos collect (intervaliza_grupo k arreglo_intervalos)))
;}}}

;================================================================================
;Funciones para pegar patrones en uno solo.;{{{
;================================================================================
;(defun append_array (arreglo1 arreglo2)
  ;;Anexa un arreglo al final del otro,como la función append.
  ;(let ((lista1 (coerce arreglo1 'list))(lista2 (coerce arreglo2 'list)))
    ;(coerce (append lista1 lista2) 'vector)))

(defun append_arrays (&rest arreglos)
  ;Anexa un arreglo al final del otro,como la función append.
  (coerce (loop for arreglo in arreglos append (coerce arreglo 'list)) 'vector))

(defun concatena_grupos (&rest grupos)
  ;Pega los patrones de un grupo uno a uno.
  (let* ((num (length (first grupos)))(salida (make-array num :fill-pointer 0))(lista))
    (loop for k from 0 below num do
     (setq lista (loop for grupo in grupos append (coerce (aref grupo k) 'list)))
          (vector-push (coerce lista 'vector) salida))
    salida))


;}}}

;================================================================================
;Funciones misceláneas;{{{
;================================================================================
(defun range (max &key (min 0) (step 1))
  (loop for n from min below max by step
        collect n))

(defun range_array (max &key (min 0) (step 1))
  (let ((salida (make-array (round (/ (- max min) step)):fill-pointer 0)))
    (loop for n from min below max by step do 
          (vector-push n salida))
    salida))

(defun combinaciones (m list fn)
  (labels ((comb1 (l c m)
                  (when (>= (length l) m)
                    (if (zerop m) (return-from comb1 (funcall fn c)))
                    (comb1 (cdr l) c m)
                    (comb1 (cdr l) (cons (first l) c) (1- m)))))
    (comb1 list nil m)))

(defun next-combination (n a)
    (let ((k (length a)) m)
    (loop for i from 1 do
        (when (> i k) (return nil))
        (when (< (aref a (- k i)) (- n i))
            (setf m (aref a (- k i)))
            (loop for j from i downto 1 do
                (incf m)
                (setf (aref a (- k j)) m))
            (return t)))))

(defun all-combinations (n k)
    (if (or (< k 0) (< n k)) '()
        (let ((a (make-array k)))
            (loop for i below k do (setf (aref a i) i))
            (loop collect (coerce a 'list) while (next-combination n a)))))

(defun map-combinations (n k fun)
    (if (and (>= k 0) (>= n k))
        (let ((a (make-array k)))
            (loop for i below k do (setf (aref a i) i))
            (loop do (funcall fun (coerce a 'list)) while (next-combination n a)))))

(defun archivo_combinaciones (n k archivo)
  ;Imprime en un archivo las combinaciones.
  (with-open-file (stream archivo :direction :output :if-exists :supersede)
    (if (and (>= k 0) (>= n k))
      (let ((a (make-array k)))
        (loop for i below k do (setf (aref a i) i))
        (loop do (format stream "~S~%" (coerce a 'list)) while (next-combination n a))))))

(defmacro append_to_list (elemento lista)
  ;Añade de forma destructiva un elemento a una lista
  `(setq ,lista (append ,lista (list ,elemento) )) )

;}}}

