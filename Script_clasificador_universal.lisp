(defvar nombre_archivo (nth 1 *posix-argv*))
(defvar numero_script (nth 2 *posix-argv*))
(defvar nombre_script (concatenate 'string "script_38_" numero_script))
(load "Clasificador.lisp")
(defvar archivo_caracteristicas (concatenate 'string "Resultados_de_prueba/" nombre_archivo))

(cond
  ((equal nombre_archivo "70-24-all.txt")
   (defparameter *conteo_caracteristicas* (list (list 846594 1029)(list 586 243)(list 303305 31)(list 35784 239)(list 19929 146)(list 239162 5343))))
  ((equal nombre_archivo "80-24-all.txt")
   (defparameter *conteo_caracteristicas* (list (list 430769 561)(list 233 148)(list 143620 2)(list 13214 99)(list 9548 46)(list 144778 3038))))
  ((equal nombre_archivo "90-36-all.txt")
   (defparameter *conteo_caracteristicas* (list (list 420433 458)(list 248 334)(list 148727 22)(list 10876 82)(list 8341 48)(list 119941 2924))))
  ((equal nombre_archivo "100-36-all.txt")
   (defparameter *conteo_caracteristicas* (list (list 719786 266)(list 110 394)(list 237292 16)(list 5634 16)(list 5284 29)(list 113421 1989))))
  ((equal nombre_archivo "110-39-all.txt")
   (defparameter *conteo_caracteristicas* (list (list 259324 184)(list 62 201)(list 71099 27)(list 3343 24)(list 3105 25)(list 57531 1419))))
  ((equal nombre_archivo "100-36_3.txt")
   (defparameter *conteo_caracteristicas* (list (list 1957 97)(list 11 4)(list 394 1)(list 460 1)(list 407 12)(list 2095 302))))
  ((equal nombre_archivo "100-36_3_a_4.txt")
   (defparameter *conteo_caracteristicas* (list (list 19450 204)(list 41 34)(list 4735 5)(list 1715 6)(list 1626 23)(list 12760 920))))
  ((equal nombre_archivo "100-36_3_a_5.txt")
   (defparameter *conteo_caracteristicas* (list (list 98733 256)(list 80 124)(list 27413 11)(list 3485 13)(list 3380 28)(list 39351 1546))))
  ((equal nombre_archivo "100-36_3_a_8.txt")
   (defparameter *conteo_caracteristicas* (list (list 1242394 266)(list 110 460)(list 436058 16)(list 5842 16)(list 5399 29)(list 134416 1999))))
  (t (error "Escribiste mal el nombre...")))
;Original
;(defparameter *conteo_caracteristicas* (list (list cp0 cn0) (list 0 0) (list cp2 0) (list cp3 0) (list cp4 0) (list cp5 cn5)))
;Con intervalos:
;100_36
;(defparameter *conteo_caracteristicas* (list (list 317235 266)(list 104 266)(list 96912 15)(list 4928 16)(list 4729 29)(list 78038 1891)))
;;90_36
;(defparameter *conteo_caracteristicas* (list (list 420433 458)(list 248 334)(list 148727 22)(list 10876 82)(list 8341 48)(list 119941 2924)))
;;80_24
;(defparameter *conteo_caracteristicas* (list (list 430769 561)(list 233 148)(list 143620 2)(list 13214 99)(list 9548 46)(list 144778 3038)))
;;70_24
;(defparameter *conteo_caracteristicas* (list (list 846594 1029)(list 586 243)(list 303305 31)(list 35784 239)(list 19929 146)(list 239162 5343)))

(load (concatenate 'string "./Scripts_clasificadores/" nombre_script))
