## Namespaces

- Un simbolo puede referirse a:

  - Una variable
  - Una clase
  - Una función
  - Un operador especial
  - Un macro

- Hay dos namespaces en LISP:

  - Function namespace: funciones, operadores especiales, macros

  - Variable namespace: variables, clases

```lisp
;; Defino una variable y una función con el mismo nombre
(setq X 1)
(defun X (arg) (+ 10 arg))

; Un simbolo por defecto evalua al variable namespace
X ; => 3
; Para referirme explicitamente al function namespace, llamo a `(function symbol)`
(function X) ; => #<FUNCTION X>
; #' es syntactic sugar de function
(#'X) ; => #<FUNCTION X>

; Hay que tener cuidado con lo que quiero
(funcall X 3) ; Explota. Estoy llamando a la variable X
(funcall #'X 3) ; => 13
```

https://wiki.c2.com/?SingleNamespaceLisp
