(load "Caracterizador_nuevo.lisp")
(load "Funciones_varias.lisp")
(defvar *ambito* )
(defvar *armadura* )
(defvar *tonos* )
(defvar *duraciones* )
(defvar *relaciones* )
(defvar *num_notas* )
(defvar *octavas* )

(defvar *ambito2* )
(defvar *armadura* )
(defvar *tonos2* )
(defvar *duraciones2* )
(defvar *relaciones2* )
(defvar *num_notas2* )
(defvar *octavas2* )

(setq *ambito* (carga_tipo_archivo "Ambito_recortado.txt"))
(setq *armadura* (carga_tipo_archivo "Armadura_nuevo.txt"))
(setq *tonos* (carga_tipo_archivo "Pitch_transpuesto.txt"))
(setq *duraciones* (carga_tipo_archivo "Conteo_duraciones_recortado.txt"))
(setq *relaciones* (carga_tipo_archivo "Relaciones_nuevo.txt"))
(setq *octavas* (carga_tipo_archivo "Octavas_recortado.txt"))
(setq *num_notas* (suma_cola_todo (carga_tipo_archivo "Repeticiones_Num_Notas.txt") 4))

(defparameter *lista_intervalos_ambito* 
  (vector 
    #(0 57 60 62 63 65 66 68 70 72 77 85)
    #(0 75 78 80 82 83 85 87 89 103)
    #(0 34 37 39 42 44 47 49 52 55 60 80)
    #(0 57 61 63 64 65 66 68 71 76 91)
    #(-1 11 13 15 17 18 20 22 25 30 62)
    #(-1 11 13 16 18 20 23 25 29 32 56)
    #(-30 26 30 33 35 37 40 42 45 49 56 93)))


;(defparameter *lista_intervalos_duraciones* 
  ;(vector
    ;#(0 1 15 )
    ;#(0 1 3 14)
    ;#(0 2 6)
    ;#(0 1 3 9 13 17 20 23 27 38 67)
    ;#(0 1 3 6 10 18 40)
    ;#(0 1 73)
    ;#(0 3 7)
    ;#(0 1 2 3 6 18)
    ;#(0 1 13)
    ;#(0 1 134)
    ;#(0 1 9 17 25 47 133)
    ;#(0 1 2 4 12)
    ;#(0 1 3 22)
    ;#(0 1 3 17)
    ;#(0 1 105)
    ;#(0 1 35)))

;(defparameter *lista_intervalos_duraciones* 
  ;(vector
    ;#(0 1 15 )
    ;#(0 1 14)
    ;#(0 2 6)
    ;#(0 1 67)
    ;#(0 1 40)
    ;#(0 1 73)
    ;#(0 3 7)
    ;#(0 1 18)
    ;#(0 1 13)
    ;#(0 1 134)
    ;#(0 1 133)
    ;#(0 1 12)
    ;#(0 1 22)
    ;#(0 1 17)
    ;#(0 1 105)
    ;#(0 1 35)))

(defparameter *lista_intervalos_relaciones* 
  (vector 
    #(0 0.25 0.5 0.75 1.0 3.0)
    #(0 0.25 0.5 0.75 1.0 3.0)
    #(0 0.25 0.5 0.75 1.0 3.0)
    #(0 0.25 0.5 0.75 1.0 3.0)
    #(0 0.25 0.5 0.75 1.0 3.0)
    #(0 0.25 0.5 0.75 1.0 3.0)
    nil))

(defparameter *lista_intervalos_tonos* 
  (vector 
    #(0 1 7 11 18 53)
    #(0 1 6 29)
    #(0 1 5 8 15 53)
    #(0 1 5 36)
    #(0 1 4 8 13 35)
    #(0 1 3 7 13 43)
    #(0 1 4 25)
    #(0 1 7 12 20 74)
    #(0 1 5 36)
    #(0 1 3 7 16 43)
    #(0 1 6 37)
    #(0 1 3 6 13 35)
    nil))

(defparameter *lista_intervalos_numnotas* 
  (vector 
    #(0 1 7 14 26 97)
    #(0 1 3 7 14 73)
    #(0 1 4 8 28)
    #(0 1 4 12 25)
    #(0 1 3 7 32)))

(defparameter *lista_intervalos_octavas* 
  (vector 
    #(0 1 3 33)
    #(0 1 3 6 10 52)
    #(0 1 8 14 20 91)
    #(0 1 12 19 27 110)
    #(0 1 9 16 24 38 73)
    #(0 1 3 8 53)
    #(0 1 2 4 11)))

(setq *ambito2* (intervaliza_todos_los_datos *ambito* *lista_intervalos_ambito*))
;(setq *duraciones2* (intervaliza_todos_los_datos *duraciones* *lista_intervalos_duraciones*))
(setq *relaciones2* (intervaliza_todos_los_datos *relaciones* *lista_intervalos_relaciones*))
(setq *tonos2* (intervaliza_todos_los_datos *tonos* *lista_intervalos_tonos*))

(setq *num_notas2* (intervaliza_todos_los_datos *num_notas* *lista_intervalos_numnotas*))
(setq *octavas2* (intervaliza_todos_los_datos *octavas* *lista_intervalos_octavas*))
(setq *supervision* (mapcar #'concatena_grupos *armadura* *tonos2* *num_notas2* *relaciones2* *octavas2* *ambito2*))
;(busca_repeticiones_a_archivo *supervision* 7 7 100 15 15 100 100 "~/Documentos/Maestría/4to_Semestre/Resultados/Características_7.txt")
(defvar mask-file (second *posix-argv*))
(defvar alpha (read-from-string (third *posix-argv*)))
(defvar beta (read-from-string (fourth *posix-argv*)))
(defvar nombre_archivo_salida (concatenate 'string (third *posix-argv*) "_" (fourth *posix-argv*) "-" (second *posix-argv*)".txt"))

(caracteriza_nuevo 
  *supervision* 
  (concatenate 'string "~/Documentos/Maestría/5to_Semestre/Máscaras/" mask-file) 
  alpha beta beta alpha 700 
  (concatenate 'string "~/Documentos/Maestría/5to_Semestre/Resultados/Corregidos/" nombre_archivo_salida) )

(print *supervision*)
;;;Para este archivo, los rasgos están indicados de la siguiente forma:
;0 - 1 .- Armadura y ritmo
;2 - 13 tonos
;14 - 18 .- Notas en acordes.
;19 - 25 .- Relaciones
;26 - 32 .- Octavas
;33 -39 .- Ámbito
