(defvar nombre_archivo (nth 1 *posix-argv*))
(load "Caracterizador_nuevo.lisp")
(load "Script_caracterizador_de_piezas.lisp")
(load "Clasificador.lisp")
(defvar archivo_caracteristicas (concatenate 'string "Resultados_de_prueba/" nombre_archivo))

(cond
  ((equal nombre_archivo "Resultados/100-21.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2377 0)(list 4 0)(list 1095 0)(list 88303 0)(list 98526 1)(list 13 1))))
  ((equal nombre_archivo "Resultados/100-24.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2383 0)(list 12 0)(list 1133 0)(list 88391 0)(list 98612 1)(list 13 2))))
  ((equal nombre_archivo "Resultados/100-42.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2388 0)(list 12 0)(list 1152 0)(list 88409 0)(list 98630 1)(list 15 2))))
  ((equal nombre_archivo "Resultados/100-48.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2388 0)(list 12 0)(list 1174 0)(list 88420 0)(list 98647 1)(list 15 2))))
  ((equal nombre_archivo "Resultados/110-24.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2383 0)(list 7 0)(list 1133 0)(list 88391 0)(list 11 1)(list 13 0))))
  ((equal nombre_archivo "Resultados/110-42.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2388 0)(list 7 0)(list 1152 0)(list 88409 0)(list 11 1)(list 15 0))))
  ((equal nombre_archivo "Resultados/110-48.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2388 0)(list 7 0)(list 1174 0)(list 88420 0)(list 12 1)(list 15 0))))
  ((equal nombre_archivo "Resultados/120-24.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2383 0)(list 7 0)(list 155 0)(list 21 0)(list 11 0)(list 8 0))))
  ((equal nombre_archivo "Resultados/120-42.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2388 0)(list 7 0)(list 156 0)(list 21 0)(list 11 0)(list 8 0))))
  ((equal nombre_archivo "Resultados/120-48.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2388 0)(list 7 0)(list 159 0)(list 23 0)(list 12 0)(list 8 0))))
  ((equal nombre_archivo "Resultados/120-56.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2612 0)(list 11 0)(list 159 0)(list 27 0)(list 13 0)(list 12 0))))
  ((equal nombre_archivo "Resultados/130-24.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2383 0)(list 4 0)(list 155 0)(list 21 0)(list 11 0)(list 8 0))))
  ((equal nombre_archivo "Resultados/130-42.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2388 0)(list 4 0)(list 156 0)(list 21 0)(list 11 0)(list 8 0))))
  ((equal nombre_archivo "Resultados/130-48.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2388 0)(list 4 0)(list 159 0)(list 23 0)(list 12 0)(list 8 0))))
  ((equal nombre_archivo "Resultados/130-56.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2612 0)(list 7 0)(list 159 0)(list 27 0)(list 13 0)(list 12 0))))
  ((equal nombre_archivo "Resultados/130-62.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2618 0)(list 7 0)(list 160 0)(list 27 0)(list 14 0)(list 12 0))))
  ((equal nombre_archivo "Resultados/140-24.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 541 0)(list 4 0)(list 155 0)(list 21 0)(list 11 0)(list 8 0))))
  ((equal nombre_archivo "Resultados/140-42.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 542 0)(list 4 0)(list 156 0)(list 21 0)(list 11 0)(list 8 0))))
  ((equal nombre_archivo "Resultados/140-48.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 542 0)(list 4 0)(list 159 0)(list 23 0)(list 12 0)(list 8 0))))
  ((equal nombre_archivo "Resultados/140-56.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 619 0)(list 7 0)(list 159 0)(list 27 0)(list 13 0)(list 12 0))))
  ((equal nombre_archivo "Resultados/140-62.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 622 0)(list 7 0)(list 160 0)(list 27 0)(list 14 0)(list 12 0))))
  ((equal nombre_archivo "Resultados/140-67.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 622 0)(list 7 0)(list 191 0)(list 30 0)(list 14 0)(list 12 0))))
  ((equal nombre_archivo "Resultados/50-0.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 141753 1)(list 116 0)(list 172691 0)(list 88155 1)(list 98423 1)(list 138 1))))
  ((equal nombre_archivo "Resultados/50-21.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 142044 1)(list 116 0)(list 173372 0)(list 88303 1)(list 98526 1)(list 172 1))))
  ((equal nombre_archivo "Resultados/50-24.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 142158 1)(list 149 0)(list 173775 0)(list 88391 1)(list 98612 1)(list 172 2))))
  ((equal nombre_archivo "Resultados/60-0.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 141753 1)(list 116 0)(list 1022 0)(list 88155 0)(list 98423 1)(list 138 1))))
  ((equal nombre_archivo "Resultados/60-21.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 142044 1)(list 116 0)(list 1095 0)(list 88303 0)(list 98526 1)(list 172 1))))
  ((equal nombre_archivo "Resultados/60-24.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 142158 1)(list 149 0)(list 1133 0)(list 88391 0)(list 98612 1)(list 172 2))))
  ((equal nombre_archivo "Resultados/70-0.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2323 1)(list 17 0)(list 1022 0)(list 88155 0)(list 98423 1)(list 138 1))))
  ((equal nombre_archivo "Resultados/70-21.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2377 1)(list 17 0)(list 1095 0)(list 88303 0)(list 98526 1)(list 172 1))))
  ((equal nombre_archivo "Resultados/70-24.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2383 1)(list 31 0)(list 1133 0)(list 88391 0)(list 98612 1)(list 172 2))))
  ((equal nombre_archivo "Resultados/80-0.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2323 1)(list 17 0)(list 1022 0)(list 88155 0)(list 98423 1)(list 26 1))))
  ((equal nombre_archivo "Resultados/80-21.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2377 1)(list 17 0)(list 1095 0)(list 88303 0)(list 98526 1)(list 38 1))))
  ((equal nombre_archivo "Resultados/80-24.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2383 1)(list 31 0)(list 1133 0)(list 88391 0)(list 98612 1)(list 38 2))))
  ((equal nombre_archivo "Resultados/90-0.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2323 1)(list 4 0)(list 1022 0)(list 88155 0)(list 98423 1)(list 26 1))))
  ((equal nombre_archivo "Resultados/90-21.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2377 1)(list 4 0)(list 1095 0)(list 88303 0)(list 98526 1)(list 38 1))))
  ((equal nombre_archivo "Resultados/90-24.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2383 1)(list 12 0)(list 1133 0)(list 88391 0)(list 98612 1)(list 38 2))))
  ((equal nombre_archivo "Resultados/90-42.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 2388 1)(list 12 0)(list 1152 0)(list 88409 0)(list 98630 1)(list 41 2))))
  ((equal nombre_archivo "Resultados/Calculo_coberturas.txt")
   (defparameter *conteo_caracteristicas*
     (list (list 0 0)(list 0 0)(list 0 0)(list 0 0)(list 0 0)(list 0 0))))
  ((equal nombre_archivo "Resultados/Resultados_de_prueba.lisp")
   (defparameter *conteo_caracteristicas*
     (list (list 2323 0)(list 4 0)(list 1022 0)(list 88155 0)(list 98423 1)(list 9 1))))
  (t (print "Escribiste mal el nombre...")))

(let ((temporal nil)(contador 0))
  (loop for grupo in *supervision* do
        (format t "~%Grupo ~S~%" contador)
        (loop for pieza across grupo do
              (setq temporal (pertenencias_del_patron nombre_archivo pieza))
              (format t "~S - ~S~%" temporal (indice_max temporal)))))
