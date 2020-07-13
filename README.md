---
title: "(Common) LISP"
author: |
  | del Mazo, Federico - 100029
  | Di Santo, Javier - 101696
  | Dvorkin, Camila - 101109
  | Secchi, Anita - 99131
---

[![](img/byte.png)](https://github.com/FdelMazo/7531-TDL)

# Historia

[Early LISP History (1956 - 1959) ~ Herbert Stoyan](https://campus.hesge.ch/Daehne/2004-2005/Langages/Lisp.htm)

[History of Lisp ~ John McCarthy](http://www-formal.stanford.edu/jmc/history/lisp/lisp.html)

[Revenge of the Nerds ~ Paul Graham](http://www.paulgraham.com/icad.html)

[Lets LISP like it's 1959](https://youtu.be/hGY3uBHVVr4) // [LISP and the foundations of computing](https://lwn.net/Articles/778550/)

##

![](img/mccarthy.png)

> "Programming is the problem of describing procedures or algorithms to an electronic calculator."

~ John McCarthy, The Programming Problem

## ¿Por qué nace LISP?

- Un lenguaje de programación para Inteligencia Artificial Simbólica.

  - ¿Cómo se representa el conocimiento humano en términos computacionales?

- McCarthy busca un lenguaje: Explicito, universal, conciso.

- Una respuesta al modelo secuencial (la máquina de Turing) y al paradigma imperativo (FORTRAN).

## ¿Cómo nace LISP?

**Idea**

- En los 50 se empiezan a desarrollar los primeros lenguajes de alto nivel (FLOW-MATIC, FORTRAN)

- En 1956, en una conferencia de AI, McCarthy se inspira para comenzar a diseñar LISP (LISt Proccessing) usando:

  - Las ideas de procesamiento de listas y recursión de IPL-II

  - El alto nivel de FORTRAN (1957) y su expresión algebraica.

  - La notación del cálculo Lambda de Alonzo Church.

## ¿Cómo nace LISP?

**Definición**

- 1958: "An Algebraic Language for the Manipulation of Symbolic Expression"

  - Primer diseño (incompleto) de LISP.

  - Comienzos de la programación funcional.

- 1960: "Recursive Functions of Symbolic Expressionsand Their Computation by Machine, Part I"

  - Especificación completa de LISP, un lenguaje _teórico_, que no estaba pensado para ser implementado.

  - Solución más comprensible a la máquina de Turing.

  - Para demostrar que es Turing-Completo, define una "función universal de LISP", que luego llegaría a ser `eval`.

## ¿Cómo nace LISP?

**Implementación**

- 1960: Steve Russell, un alumno de McCarthy, decide probar e implementar en código máquina `eval`

  - Primer interprete de LISP, para la IBM 704.

- 1962: Tim Hart y Mike Levin hacen el primer compilador de LISP.

  - Lo más cercano al LISP que conocemos hoy en día.

  - Funciones compiladas e interpretadas pueden ser intercambiadas libremente.

## Cálculo Lambda

https://youtu.be/eis11j_iGMs

?x. x?x.

(lambda (x) (x\*x))

The power of the lambda notation is in its generality. The lambda notation will handle the case in which the value of a function is a function. In many computer languages the value of a function must be an element of a set, such as a number or a string or the label of a function. In the lambda notation the value can be a function, not the name or label of a function but a function itself.

https://www.sjsu.edu/faculty/watkins/lambda.htm

- Calculo Lambda no tipado: expresa _mas_ que el calculo lambda tipado

## Pionero

[What Made Lisp Different ~ Paul Graham](http://www.paulgraham.com/diff.html)

[Influential Programming Languages, Lisp ~ David Chisnall](https://www.informit.com/articles/article.aspx?p=1671639)

- **if-then-else**: Las expresiones condicionales fueron definidas en el paper de 1960, con `cond`. Un condicional es un una construcción if-then-else; hoy en día los damos por hecho. Fueron inventados por McCarthy en el transcurso de desarrollo de Lisp.

- **Funciones**: Las funciones son objectos de primera clase, son un tipo de dato como lo son los enteros, cadenas, etc. Tienen una representación literal, pueden ser asignadas a variables, pasadas como argumentos (parámetros)...

- **Recursión**: Ya existía matemáticamente, pero nunca en un lenguaje de programación

- **Un nuevo concepto en variables**: Todas las variables son efectivamente punteros. Los valores son aquellos que _tienen tipos_, no variables. Asignar variables significa copiar punteros, y no aquello a lo que apuntan.

- **Garbarge Collection**: Con un diseño primitivo (no era concurrente), LISP fue el primer lenguaje en utilizar garbage collection automático.

- **Tipado dinámico**: No hay que explicitar si algo es un átomo o una lista.

- **Interactividad**: Gracias al interprete REPL se tiene feedback inmediato y se puede programar desde abajo para arriba, compilando incrementalmente.

- **El lenguaje completo está siempre disponible**: No hay una distinción real entre tiempo de lectura, tiempo de compilación y tiempo de ejecución. Uno puede compilar o ejecutar mientras lee, leer o ejecutar código mientras compila, leer o compilar mientras se ejecuta el código.

- **Map y Reduce**: LISP fue el primer lenguaje en implementar dos funciones muy importantes en la programación funcional.

# Sintaxis

[LISP 1.5 Programmer's Manual](http://web.cse.ohio-state.edu/~rountev.1/6341/pdf/Manual.pdf)

[Common Lisp HyperSpec](http://clhs.lisp.se/Front/index.htm)

[Learn X in Y minutes, Where X=Common Lisp](https://learnxinyminutes.com/docs/common-lisp/)

## Expresiones -> átomos y listas

```lisp
;; Todo en LISP se compone de symbolic expressions
1 ; Una s-expression puede ser un átomo -> irreducible
(+ 1 2) ; Una s-expression puede ser una lista -> partible

;; Las s-expressions evaluan a valores
2 ; evalua a 2
(+ 2 3) ; evalua a 5
(+ (+ 2 3) 2) ; (+ 2 3) evalua a 5 -> todo evalua a 7

;; Acá comienza la dualidad entre código (la lista) y data (el átomo)
;; Ambos son S-expressions
```

## Code & Data -> `eval`, `quote`

```lisp
;; El operador quote toma una s-expression y devuelve el código
(+ 1 1) ; evalua a 2
(quote (+ 1 1)) ; evalua a (+ 1 1)
('(+ 1 1)) ; quote se abrevia a '

;; El operador eval toma una s-expresion y devuelve su valor
(eval (+ 1 1)) ; evalua a 2
(eval '(+ 1 1)) ; evalua a 2
(eval ''(+ 1 1)) ; evalua a (+ 1 1)

;; 1 + 1 es 2
;; "1 + 1" es el código 1 + 1
```

Puedo hacer un programa entero, ponerle un `'` adelante, y estoy tratando con el **código** de mi programa.

## Chequeando Valores -> `atom`, `listp`

¿Es código o data?

```lisp
;; atom devuelve si algo es un átomo o no
(atom 1) ; True (el valor de la expresion 1 es un átomo)
(atom (+ 1 2)) ; True (el valor de la expresion (+ 1 2) es un átomo)
(atom '(+ 1 2)) ; Nil (la expresion (+ 1 2) es una lista)

;; listp devuelve si algo es una lista o no
(listp 1) ; Nil (1 no es una lista)
(listp (+ 1 2)) ; Nil (la expresion evalua a 3, no es una lista)
(listp '(+ 1 2)) ; True (estoy hablando del código de la expresion, la lista)
```

## Procesar listas -> `car`, `cdr`, `cons` y `list`

- Las listas en LISP son listas enlazadas

- Los "nodos" (dato y prox) se llaman **cons cells**

  - El dato actual es el **car**

  - El resto es otra lista, el **cdr**

```lisp
;; car recibe una lista y devuelve su primer elemento
(car (+ 1 2)) ; explota, no recibio una lista
(car '(+ 1 2)) ; devuelve +

;; cdr recibe una lista y devuelve el resto (todo menos el primer elemento)
(cdr '(+ 1 2)) ; devuelve (1 2)

;; cons crea un cons de un valor seguido de una lista
;; AKA agrega un valor al principio de la lista
(cons '1 '(2 3)) ; devuelve (1 2 3)
(cons '+ '(2 3)) ; devuelve (+ 2 3)

;; list compone una lista de sus argumentos
(list 1 2 3) ; devuelve (1 2 3)
(list '+ 2 3) ; devuelve (+ 2 3)
;; si tan solo hubiese una manera de ejecutar esta expresion!
```

## Procesar una s-expression -> Programming Bottom Up

```lisp
;; Partimos de la expresion (+ 1 2)
(+ 1 2)
;; La convertimos en el código de la expresion
'(+ 1 2)
;; Sacamos su operador -> el +
(car '(+ 1 2))
;; Sacamos los operandos -> el (1 2)
(cdr '(+ 1 2))
;; Empaquetamos esto nuevamente -> (+ 1 2)
(cons (car '(+ 1 2)) (cdr '(+ 1 2)))
;; Evaluamos la expresion
(eval (cons (car '(+ 1 2)) (cdr '(+ 1 2))))
```

# Ejemplo de TDA: Tablas de hash

## Crear una tabla de hash en Common Lisp

- función `make-hash-table`
- No requiere ningún argumento.

Ejemplo:

```lisp
(defvar tabla)
(setq tabla (make-hash-table))
```

Sin embargo, el argumento opcional más usado es `:TEST`, que especifica la función utilizada para testear claves iguales.

```lisp
(defvar tabla)
(setq tabla (make-hash-table :test 'equal))
```

## Agregar un elemento a la tabla

- función `gethash` en conjunto con la función `setf`

```lisp
* (setf (gethash "clave1" tabla) 3) ; gethash recibe dos argumentos: la clave y el hash
3
```

## Obtener un valor

- función `gethash` toma dos argumentos obligatorios: una clave y una tabla de hash

Ejemplo:

```lisp
* (defvar tabla)
TABLA
* (setq tabla (make-hash-table :test 'equal))
#<HASH-TABLE :TEST EQUAL :COUNT 0 {10058B8553}>
* (setf (gethash "clave1" tabla) 3)
3
* (gethash "clave1" tabla)
3
T
```

En el siguiente ejemplo, guardamos NIL en el hash:

```lisp
* (setf (gethash "clave2" tabla) nil)
NIL
* (gethash "clave2" tabla)
NIL
T ; T indica True, existe la clave.
```

## Borrar de la tabla de hash

- función `remhash` para eliminar el par clave-valor

```lisp
* (remhash "clave1" tabla) ; elimino el par: "clave"-3
T
* (remhash "clave2" tabla) ; elimino el par: "clave2"-nil
T
* (gethash "clave1" tabla) ; trato de obtener el valor de algo que fue eliminado
NIL
NIL
* (remhash "clave3" tabla) ; trato de borrar una clave que no existe
NIL
```

## Contar entradas

- función `hash-table-count`

```lisp
* (setf (gethash "clave1" tabla) 3)
3
* (setf (gethash "clave2" tabla) "estoesunstring")
"estoesunstring"
* (setf (gethash "clave3" tabla) 2)
2
* (hash-table-count tabla)
3 ; 3 elementos en mi hash.
```

## El tamaño del hash

- función `make-hash-table`

```lisp
* (defvar tabla)
TABLA
* (setq tabla (make-hash-table :test 'equal))
#<HASH-TABLE :TEST EQUAL :COUNT 0 {10058B8553}>
*(hash-table-size tabla)
16 ; por default
*(hash-table-rehash-size tabla)
1.5 ; indica que la tabla se agrandará en un 50% cada vez que necesite crecer.
```

- Los valores para `hash-table-size` y `hash-table-rehash-size` dependen de la implementación. En este caso, la implementación de Common Lisp con la cual contamos, elige un tamaño inicial de 16, y aumentará el tamaño en un 50% (1.5) cada vez que el hash necesite crecer.

- Ejemplo: agregar un total de un millón\* de pares clave-valor al hash:

```lisp
* (time (dotimes (n 1000000) (setf (gethash n tabla) n))) ; le tomo el tiempo que tarda
Evaluation took:
  0.162 seconds of real time
  0.161954 seconds of total run time (0.137696 user, 0.024258 system)
  [ Run times consist of 0.015 seconds GC time, and 0.147 seconds non-GC time. ]
  100.00% CPU
  355,501,132 processor cycles
  83,836,896 bytes consed
NIL
* (hash-table-count tabla)
1000000
* (hash-table-size tabla)
1048576
```

\*_Se eligió un millón para resaltar los tiempos que tardan_

- Y si piso todas las claves y tomo el tiempo nuevamente:

```lisp
* (time (dotimes (n 1000000) (setf (gethash n tabla) n)))
Evaluation took:
  0.088 seconds of real time
  0.088449 seconds of total run time (0.088449 user, 0.000000 system)
  100.00% CPU
  194,161,741 processor cycles
  0 bytes consed
NIL
```

- Veamos cuantas veces temenos que redimensionar para llegar al tamaño final:

```lisp
* (log (/ 1000000 16) 1.5)
27.235197
* (let ((size 16)) (dotimes (n 29) (print (list n size)) (setq size (* 1.5 size))))
(0 16)
(1 24.0)
(2 36.0)
(3 54.0)
(4 81.0)
(5 121.5)
(6 182.25)
(7 273.375)
(8 410.0625)
(9 615.09375)
(10 922.6406)
(11 1383.9609)
(12 2075.9414)
(13 3113.912)
(14 4670.868)
(15 7006.3022)
(16 10509.453)
(17 15764.18)
(18 23646.27)
(19 35469.406)
(20 53204.11)
(21 79806.164)
(22 119709.25)
(23 179563.88)
(24 269345.8)
(25 404018.72)
(26 606028.06)
(27 909042.1)
(28 1363563.3)
NIL
```

- Se redimensiona 28 veces hasta que sea lo suficientemente grande para contener 1,000,000 de claves con sus respectivos valores.

- Una manera de hacerlo más rápido: Si ya sabemos con anticipación que tan grande nuestro hash será, podemos comenzar con el tamaño correcto desde el vamos:

```lisp
* (defvar tabla)
TABLA
* (setq tabla (make-hash-table :test 'equal :size 1000000))
#<HASH-TABLE :TEST EQUAL :COUNT 0 {10039B0043}>
* (hash-table-size tabla)
1000000
* (time (dotimes (n 1000000) (setf (gethash n tabla) n)))
Evaluation took:
  0.086 seconds of real time
  0.085881 seconds of total run time (0.085881 user, 0.000000 system)
  100.00% CPU
  188,651,959 processor cycles
  0 bytes consed
NIL
```

- Se prueba que tarda considerablemente menos tiempo.
- No hubo alocamientos involucrados ya que no hubo que redimensionar en absoluto
- Se puede anticipar el comportamiento de crecimiento que tendrá el hash: parámetro `:rehash-size` en la función `make-hash-table`

```lisp
* (defvar tabla)
TABLA
* (setq tabla (make-hash-table :test 'equal :rehash-size 1000000))
#<HASH-TABLE :TEST EQUAL :COUNT 0 {100589D563}>
* (hash-table-size tabla)
16
* (hash-table-rehash-size tabla)
1000000
* (time (dotimes (n 1000000) (setf (gethash n tabla) n)))
Evaluation took:
  0.120 seconds of real time
  0.120026 seconds of total run time (0.116221 user, 0.003805 system)
  [ Run times consist of 0.017 seconds GC time, and 0.104 seconds non-GC time. ]
  100.00% CPU
  263,851,583 processor cycles
  41,943,104 bytes consed
NIL
```

- Solamente necesitamos una redimensión, pero mucho màs realocamiento (41,943,107 bytes consed) porque casi toda la tabla (menos los 16 elementos iniciales) tuvieron que se construídos durante la iteración.

## Fun stuff e iteradores del hash

Si se quiere realizar una acción sobre cada par clave-valor en la tabla de hash, existen múltiples opciones:

- `maphash`: itera sobre todas las claves de la tabla. Su primer argumento debe ser una función que acepte dos parámetros: la clave y el valor. Muy importante notar y recordar que, dado la naturaleza de las tablas de hash, uno no puede controlar el orden en el cual las claves son devueltas. `maphash` devuelve siemple `NIL`.

```lisp
* (defvar tabla)
TABLA
* (setq tabla (make-hash-table :test 'equal))
#<HASH-TABLE :TEST EQUAL :COUNT 0 {100589D0E3}>
* (setf (gethash "clave1" tabla) 1)
1
* (setf (gethash "clave2" tabla) 2)
2
* (setf (gethash "clave3" tabla) 3)
3
* (defun imprimir-entrada (clave valor) (format t "El valor asociado a la clave ~S es ~S~%" clave valor))
IMPRIMIR-ENTRADA
* (maphash #'imprimir-entrada tabla)
El valor asociado a la clave "clave1" es 1
El valor asociado a la clave "clave2" es 2
El valor asociado a la clave "clave3" es 3
NIL
*
```

- `with-hash-table-iterator`: es una macro que convierte el primer argumento en un iterador que en cada invocación devuelve tres valores cada clave-valor del hash: un booleano generalizado que es `true` si alguna entrada es devuelta, la clave, y el valor. Si no encuentra más claves, devuelve `NIL`

```lisp
* (with-hash-table-iterator (iterador tabla)
    (loop
        (multiple-value-bind (entrada clave valor)
            (iterador)
        (if entrada
            (imprimir-entrada clave valor)
            (return)))))
El valor asociado a la clave "clave1" es 1
El valor asociado a la clave "clave2" es 2
El valor asociado a la clave "clave3" es 3
NIL
```

- `loop`: ~~la vieja confiable~~

```lisp
  * (loop for clave being the hash-keys of tabla do (print clave))
"clave1"
"clave2"
"clave3"
NIL
```

Formateado clave-valor:

```lisp
* (loop for clave being the hash-keys of tabla using (hash-value valor)
    do (format t "El valor asociado a la clave ~S es ~S~%" clave valor))
El valor asociado a la clave "clave1" es 1
El valor asociado a la clave "clave2" es 2
El valor asociado a la clave "clave3" es 3
NIL
```

Solo el valor:

```lisp
* (loop for valor being the hash-values of tabla do (print valor))
1
2
3
NIL
```

Clave y valor:

```lisp
* (loop for valor being the hash-values of tabla using (hash-key clave) do (format t "~&~S -> ~S" clave valor))
"clave1" -> 1
"clave2" -> 2
"clave3" -> 3
NIL
```

HASH TABLE: http://cl-cookbook.sourceforge.net/hashes.html - https://www.tutorialspoint.com/lisp/lisp_hash_table.htm

# Fractales, en Common LISP

[cl-aristid](https://github.com/FdelMazo/cl-aristid)

- En las implementaciones más comunes, es compilado (pero no JIT necesariamente)

- LISP es un lenguaje interactivo y dinámico

- Tipado fuerte y dinámico

- Manifest typing

- Funciones y estructuras: `defun`, `defstruct`, `let`

- Control de flujo: `if`, `loop`

- Static (lexical) Scoping

- Closures

- Namespaces

- Manejo de memoria

- Manejo de errores

- Macros

# Metaprogramming

## Expression oriented / Simbolico

https://beautifulracket.com/appendix/why-racket-why-lisp.html

Usa S-Expressions: Simbolic expression

Lisp is an expression oriented language. Unlike most other languages, no distinction is made between "expressions" and "statements";[dubious – discuss] all code and data are written as expressions. When an expression is evaluated, it produces a value (in Common Lisp, possibly multiple values), which can then be embedded into other expressions. Each value can be any data type.

https://en.wikipedia.org/wiki/Expression-oriented_programming_language

Transparencia referencial: equals can be replaced by equals

## Extensibilidad del lenguaje

https://sep.yimg.com/ty/cdn/paulgraham/onlisp.pdf?t=1564708198&

cahpter 4 de practical common lisp

Further, because Lisp code has the same structure as lists, macros can be built with any of the list-processing functions in the language. In short, anything that Lisp can do to a data structure, Lisp macros can do to code. In contrast, in most other languages, the parser's output is purely internal to the language implementation and cannot be manipulated by the programmer.

https://youtu.be/dw-y3vNDRWk

The Lisp feature that makes this trivially easy is its macro system. I can't emphasize enough that the Common Lisp macro shares essentially nothing but the name with the text-based macros found in C and C++.

https://beautifulracket.com/appendix/why-racket-why-lisp.html#a_vmsLq

https://beautifulracket.com/appendix/why-racket-why-lisp.html#a_pwJR1

"language-oriented program­ming"

- comparar como se extiende python o C normalmente https://stevelosh.com/blog/2018/08/a-road-to-common-lisp/#s6-extensibility y http://www.gigamonkeys.com/book/macros-standard-control-constructs.html

- Hablar de extensiones "populares" (importantes) del lenguaje

  - Darle mucha bola a CLOS! que es importantisimo en la historia de lisp (chusmear relacion con smalltalk!!!) (chusmear como CLOS es de lo mas "puro" en cuanto a Object Oriented)

  - Concurrencia

  - String interpolation

Solo en librerías.

- [Bordeaux Threads](https://common-lisp.net/project/bordeaux-threads/) para la creación de hilos.

- [lparallel](https://github.com/lmj/lparallel) para una implementación más compleja que incluye comunicación entre hilos (colas, promesas, etc).

- [Blackbird](https://github.com/orthecreedence/blackbird) implementación de promesas (la dejo porque no la encontré en cl-awesole).

* "los macros son **parte** de lo que es la extensibilidad de list" --> pie para entrar a la siguiente seccion

## Macros

- Despues de la teoria, Volver a cl-aristid y al REPL. Expandir macros, compararlos con las versiones canonicas, mostrar los macros definidos, como se definen, como cambian la sintaxis

- Esta seccion que sea bien teorica con codigo (pero poco codigo). Los ejemplos ejemplos van en sintaxis

- En Lisp, una macro es una función que genera código de Lisp. La forma más sencilla de pensarlo sería como una transformación de código. Cuando se llama a una macro en el código:

1. Se arma el código en base a la definición `defmacro` de la misma.
2. Se evalúa el nuevo código en el lugar de la llamada a la macro.

A partir de esto, se pueden usar macros para simplificar y reutilizar código, o hasta manipular la sintaxis del lenguaje.

- Algunos operadores:
  - `` Backquote ` ``: Funciona similar a `quote`. (se explica antes?)

```lisp
`(a, b, c)
; equivalente a escribir
'(a b c)
(list 'a 'b 'c)
```

- `Comma ,`: Combinado con `backquote` sirve para "activar y desactivar" el efecto de `backquote`. Es útil al escribir macros:

```lisp
`(a ,b c ,d)
; equivalente a escribir
(list 'a b 'c d)
```

- `Comma-at ,@`: Dada una expresión que resuelve una lista, se puede utilizar `,@` para reemplazar esta lista por la secuencia de sus mismos elementos (elimina el paréntesis):

```lisp
`(a b c)    ->  (A (1 2 3) C)
`(a ,@b c)  ->  (A 1 2 3 C)
```

- `(nil! x)`: Cambiar el valor de la variable `x` a `nil`.
  En el ejemplo se puede observar que `var` se expande al valor que corresponde (por el operador `,`), mientras que `setq` y `nil` no se evalúan.

```lisp
(defmacro nil! (var)
  `(setq ,var nil))

; se llama de la forma
(nil! x)

; genera el código
(setq x nil)
```

- `(if test then else)`: Ya se encuentra definida en Lisp. Tiene que ser una macro para evaluar la expresión solo cuando corresponda. Una posible implementación utilizaría la macro `cond`, que evalúa solo la primer expresión cuya condición sea true:

```lisp
(defmacro if (condition then else)
  `(cond (,condition ,then)
         (t ,else)))
```

- `(when test do1 do2 ...)`: Cuando la expresión `test` devuelve `true`, se ejecutan todas las expresiones `do`, devolviendo el valor de la última. Para que se expandan todas las expresiones `do` se las combina en una lista `&rest body` y luego se utiliza el operador `,@`:

```lisp
(defmacro our-when (test &rest body)
  `(if ,test
    (progn
      ,@body)))

; se llama de la forma
(when test do1 do2 do3)

; genera el código
(if (eligible obj)
  (progn do1
    do2
    do3
    obj))
```

- `infix`: Las macros permiten cambiar el orden de las expresiones sin evaluarlas. Entonces, se podría hacer una macro `infix` para tener operadores matemáticos en notación de infijo en vez de la notación polaca de Lisp. Existen implementaciones completas de esta macro para que funcione con más de una operación, pero para mostrar la más simple:

```lisp
(defmacro infix (arg1 op arg2)
  `(,op ,arg1 ,arg2))

; se llama de la forma
(infix 2 + 3)

; genera el código
(+ 2 3)
```

- `lcomp`: Replicar la sintaxis de compresión de listas de Python.

```lisp
(defmacro lcomp (expression for var in list conditional conditional-test)
  (let ((result (gensym)))
    `(let ((,result nil))
       (loop for ,var in ,list
            ,conditional ,conditional-test
            do (setq ,result (append ,result (list ,expression))))
       ,result)))

; se llama de la forma
(lcomp x for x in (1 2 3 4 5 6 7) if (= (mod x 2) 0))

; una vez generado y ejecutado el código devuelve
(2 4 6)
```

- `let`:

```lisp
(defmacro let (binds &body body)
  '((lambda ,(mapcar #'(lambda (x)
                         (if (consp x) (car x) x))
                        binds)
      ,@body)
    ,@(mapcar #'(lambda (x)
                  (if (consp x) (cadr x) nil))
              binds)))
```

- `while`:

```lisp
(defmacro while (test &body body)
  '(do ()
       ((not ,test))
       ,@body))
```

- `till`:

```lisp
(defmacro till (test &body body)
  '(do ()
       (,test)
       ,@body))
```

- `for`:

```lisp
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    '(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))
```

## Homoiconicidad ("Code as Data")

"This means that the way you think about a program­ming problem can be quite close to the way you actu­ally program it."

- Gracias a las S-Expressions

- Codigo y estructuras de datos intercambiable

- La representación interna del programa es la que se lee

- Todo es una lista -> Code and Data are made out of the same data structures, and the quote operator is the way we distinguish between them

- Reflexion mucho mas sencilla

- Lisp functions can be manipulated, altered or even created within a Lisp program without lower-level manipulations

- Extensibilidad mucho mas facil

- List of lists ??? Tree!

- And how do we deal with trees? Recursion!

- El abstract syntax tree de una funcion se puede manipualr como una estructura de datos y luego evaluar

- s-expressions can represent arbitrary tree expressions, much like the usual abstract syntax tree

- anything that Lisp can do to a data structure, Lisp macros can do to code

This feature makes it easy to develop efficient languages within languages. For example, the Common Lisp Object System can be implemented cleanly as a language extension using macros. This means that if an application needs a different inheritance mechanism, it can use a different object system. This is in stark contrast to most other languages; for example, Java does not support multiple inheritance and there is no reasonable way to add it.

- Las s-expresiones son binary trees porque son o atomos o (x y) siendo (x . y) un dotted pair (x hijo izq, y hijo der)

- (hoy por hoy se usa el sintacic sugar LIST, que es nada mas concatenar mil dotted pairs. (x y z) es (x . (y . (z . null))))

- foto binary tree

- Dualidad entre el codigo y la data.

- Todo es una lista por ende tanto el código, como la data se escriben de la misma forma

- Toda expresión se puede interpretar de las dos maneras. - Se interpreta como data usando `quote` - Se interpreta como code usando `eval`

- Misma expresión que se puede leer de ambas formas y permite swapear dependiendo que necesite.

[Code vs Data (Metaprogramming) ~ Computerphile](https://youtu.be/dw-y3vNDRWk)

# Desglosando `eval`

[The Roots of LISP ~ Paul Graham](http://www.paulgraham.com/rootsoflisp.html)

[The Most Beautiful Program Ever Written ~ William Byrd](https://youtu.be/OyfBQmvr2Hc)

- La magía de LISP es el **read–eval–print loop**: un entorno donde se toma lo escrito por el programador, se lee, se evalua, se imprime, y luego se vuelve a pedir input

- Este self-interpreter es un **evaluador meta-circular**, esta escrito en LISP y puede evaluar código de LISP.

- ¿Cómo? Gracias a la función `eval`, definida por McCarthy en base a 7 operadores que toma como axiomas: `quote`, `atom`, `eq`, `car`, `cdr`, `cons` y `cond`

##

> That was the big revelation to me when I was in graduate school—when I finally understood that the half page of code on the bottom of page 13 of the Lisp 1.5 manual was Lisp in itself. These were "Maxwell's Equations of Software!" This is the whole world of programming in a few lines that I can put my hand over.

~ Alan Kay, [A Conversation with Alan Kay](https://queue.acm.org/detail.cfm?id=1039523)

![](img/eval.png)

##

```lisp
;; Anotaciones sobre el código de Paul Graham en Roots of Lisp
; The Lisp defined in McCarthy's 1960 paper, translated into Common Lisp.

; eval recibe una expresion `e` y una lista de argumentos `a` -> El "entorno"
; Básicamente, recibe todo el scope donde estoy parado
(defun eval (e a)
  ; Es todo un if grande de 4 condiciones que chequean el tipo de la expresion
  (cond
    ; Si es un atomo -> Devuelvo su valor en el entorno
    ((atom e) (assoc e a))
    ; Si no es un atomo tiene que ser una lista
    ; Si es una lista del tipo (atomo...resto) -> Es una función!
    ; (car e) es el operador
    ; (cadr e) es (car (cdr e)) que es el primero de los argumentos
    ((atom (car e))
     ; Si Es una funcion, ¿que funcion es?
     (cond
       ; Si es quote, solo devuelvo los argumentos de la funcion
       ((eq (car e) 'quote) (cadr e))
       ; Para el resto de los axiomas, llamo a esa funcion contra los argumentos
       ; Como quiero llamar a la operacion contra los valores de los argumentos, llamo a eval
       ((eq (car e) 'atom)  (atom   (eval (cadr e) a)))
       ((eq (car e) 'eq)    (eq     (eval (cadr e) a)
                                    (eval (caddr e) a)))
       ((eq (car e) 'car)   (car    (eval (cadr e) a)))
       ((eq (car e) 'cdr)   (cdr    (eval (cadr e) a)))
       ((eq (car e) 'cons)  (cons   (eval (cadr e) a)
                                    (eval (caddr e) a)))
       ; cond tiene que evaluar recursivamente todas las condiciones, hasta encontrar el primer true
       ; para eso, se define una funcion auxiliar, `evcon` que recorre la lista de parametros y los evalua
       ((eq (car e) 'cond)  (evcon (cdr e) a))
       ; el caso final es recibir una funcion definida por el usuario
       ('t (eval (cons (assoc (car e) a)
                        (cdr e))
                  a))))
    ; Si no es un atomo ni una lista que comienza por un atomo, entonces es una lista que comienza por otra cosa
    ; Si es una lista que comienza con label, evaluo la funcion a la que refiere
    ((eq (caar e) 'label)
     (eval (cons (caddar e) (cdr e))
            (cons (list (cadar e) (car e)) a)))
    ; Si es una lista que comienza con lambda, evaluo sus parametros
    ((eq (caar e) 'lambda)
     (eval (caddar e)
            (append (pair (cadar e) (evlis  (cdr e) a))
                     a)))))
```

# LISP en la practica

## Estadisticas

- repos de github

_Se incluyen estadísticas de uso del lenguaje, frameworks y la evolución en los últimos años. Para lenguajes antiguos se incluye información sobre qué lenguajes o técnicas se vieron influenciadas por este lenguaje_

http://blockml.awwapps.com/example/example/document.html#sec-6

## Comparaciones

- repetir tema de la influencia y de como todo lenguaje se ve tocado por lisp
- comparar con python
- con C performance
- con oz, scala, haskell
- con dialectos de lisp como scheme y racket
- con javascript para decir como se influencia por shcem

## Familias de Lisp

- Más usados: Scheme (1975),Emacs Lisp (1985) y Clojure (2007).

Hoy, los dialectos de Lisp más ampliamente usados, ademas de Common Lisp, son Scheme (1975),Emacs Lisp (1985) y Clojure (2007).

### Scheme

- Minimalista, solamente define el inner core del lenguaje
- Sistema de macros limpio y trnasparente
- Lexical scoping
- Garbage collection.
- Ocupa mucha menos memoria

### Clojure

- Sistema de macros similar al de Common Lisp
- Secuencias lazy (`seq`)
- El sistema de macros de Clojure es muy similar al de Common Lisp con la excepción de que la versión de Clojure de la comilla inversa (llamada "comilla sintáctica") cualifica los símbolos con el espacio de nombres al que pertenece.
- Sistema integrado de estructuras de datos persistentes e inmutables.
- Compilado con Java

### Emacs Lisp

- Emacs Lisp trabaja con dynamic scoping por default.
- Sintaxis para el compilado de un archivo `M-x byte-compile-file`
- No tiene closures

## Otros lenguajes

### Python

- Python admite todas las caracteristicas esenciales de Lisp, exeptuando las macros.
- Sintaxis más sencilla de leer,
- Tiempo de compilacion más rápido
- Tiempo de ejecución mucho más lento
- Más dinámico, python realiza menos chequeos de erorres.
- Similitudes:
  - Lenguaje de alto nivel interpretado y compiladom orientado a objetos con semántica dinámica.
  - Admiten modulos y paquetes, fomentando la modularidad del programa.
  - Debuggear es sencillo

### C++

- Lisp es una o dos veces más lento que C++
- Manejo de memoria con punteros
- Sintaxis más detallada y restrictiva

## Casos de estudio

_Se mencionan casos reales indicando el motivo por el cual se sabe o se cree que se usa el lenguaje_

emacs

crash bandicoot

beating the averages!!!! (http://www.paulgraham.com/avg.html)

Reddit

https://github.com/CodyReichert/awesome-cl
