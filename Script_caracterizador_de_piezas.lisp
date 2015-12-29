(load "Caracterizador_nuevo.lisp")
(load "Funciones_varias.lisp")

(defvar *p_bach* (read-file "Patrones_por_pieza/Patrones_piezas_bach.txt"))
(defvar *p_beethoven* (read-file "Patrones_por_pieza/Patrones_piezas_beethoven.txt"))
(defvar *p_joplin* (read-file "Patrones_por_pieza/Patrones_piezas_joplin.txt"))
(defvar *p_debussy* (read-file "Patrones_por_pieza/Patrones_piezas_debussy.txt"))
(defvar *p_mozart* (read-file "Patrones_por_pieza/Patrones_piezas_mozart.txt"))
(defvar *p_muzio* (read-file "Patrones_por_pieza/Patrones_piezas_muzio.txt"))

(setq *supervision* (list *p_bach* *p_mozart* *p_muzio* *p_beethoven* *p_debussy* *p_joplin*))
(print *supervision*)

;(defvar mask-file (second *posix-argv*))
;(defvar alpha (read-from-string (third *posix-argv*)))
;(defvar beta (read-from-string (fourth *posix-argv*)))
;(defvar nombre_archivo_salida (concatenate 'string (third *posix-argv*) "_" (fourth *posix-argv*) "-" (second *posix-argv*)".txt"))

;;;;Esta es la sección que se usó para la caracterización empleando muchos pares;{{{
;;;;de valores para alfa y beta
;(defvar alpha (read-from-string (second *posix-argv*)))
;(defvar beta (read-from-string (third *posix-argv*)))
;(defvar nombre_archivo_salida (concatenate 'string (second *posix-argv*) "-" (third *posix-argv*)".txt"))

;(caracteriza_nuevo 
  ;*supervision* 
  ;(concatenate 'string "Archivos_mascaras/" "40_3.lisp") 
  ;alpha beta beta alpha 500 
  ;(concatenate 'string "Resultados/" nombre_archivo_salida) )
;;;;;}}}

;;;;Esta sección está "hardcodeada" por que es para pruebas;{{{
;(caracteriza_nuevo 
  ;*supervision* 
  ;(concatenate 'string "Archivos_mascaras/" "40_3.lisp") 
  ;100 10 10 100 500 
  ;(concatenate 'string "Resultados/" "Resultados_de_prueba.txt"));}}}

;;;Para este archivo, los rasgos están indicados de la siguiente forma:
;0 - 1 .- Armadura y ritmo
;2 - 13 tonos
;14 - 18 .- Notas en acordes.
;19 - 25 .- Relaciones
;26 - 32 .- Octavas
;33 -39 .- Ámbito
