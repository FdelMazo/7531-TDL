---
title: "LISP"
author: |
  | del Mazo, Federico - 100029
  | di Santo, Javier - 101696
  | Dvorkin, Camila - 101109
  | Secchi, Anita - 99131
---

[![](img/lisp.png)](https://github.com/FdelMazo/7531-TDL)

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

# Para qué sirve (y para qué no)

_Se explica en qué casos es bueno y malo el lenguaje con los motivos_

# Caracteristicas del Lenguaje

## Pionero

[What Made Lisp Different ~ Paul Graham](http://www.paulgraham.com/diff.html)

[Influential Programming Languages, Lisp ~ David Chisnall](https://www.informit.com/articles/article.aspx?p=1671639)

- **if-then-else**: Las expresiones condicionales fueron definidas en el paper de 1960, con `cond`. Un condicional es un una construcción if-then-else; hoy en día los damos por hecho. Fueron inventados por McCarthy en el transcurso de desarrollo de Lisp.

- **Funciones**: Las funciones son objectos de primera clase, son un tipo de dato como lo son los enteros, cadenas, etc. Tienen una representación literal, pueden ser asignadas a variables, pasadas como argumentos (parámetros)...

- **Recursión**: Ya existía matemáticamente, pero nunca en un lenguaje de programación

- **Un nuevo concepto en variables**: Todas las variables son efectivamente punteros. Los valores son aquellos que _tienen tipos_, no variables. Asignar variables significa copiar punteros, y no aquello a lo que apuntan.

- **Garbarge Collection**: Con un diseño primitivo (no era concurrente), LISP fue el primer lenguaje en utilizar garbage collection automático.

- **Programas compuestos por expresiones**: Los programas en Lisp son árboles de expresiones, cada uno devuelve un valor. (En otras expresiones de Lisp, puede devolver múltiples valores). Esto es en contraste con los lenguajes más exitosos, que distinguen entre _expresión_ y _declaración_. Cuando un lenguaje está hecho enteramente de expresiones, uno puede componer expresiones como uno quiera:
  Puede ser (en syntaxis de _Arc_):

```lisp
  (if foo (= x 1) (= x 2))
```

o

```lisp
(= x (if foo 1 2))
```

- **Tipado dinámico**: No hay que explicitar si algo es un átomo o una lista.

- **Interactividad**: Gracias al interprete REPL se tiene feedback inmediato y se puede programar desde abajo para arriba, compilando incrementalmente.

- **El lenguaje completo está siempre disponible**: No hay una distinción real entre tiempo de lectura, tiempo de compilación y tiempo de ejecución. Uno puede compilar o ejecutar mientras lee, leer o ejecutar código mientras compila, leer o compilar mientras se ejecuta el código.

- **Map y Reduce**: LISP fue el primer lenguaje en implementar dos funciones muy importantes en la programación funcional.

## Paradigma

- Programación declarativa: definir el _qué_ sin explicar el _cómo_

  - Programación funcional: los componentes se definen como funciones matemáticas

    - Determinístico

    - Misma entrada garantiza misma salida

    - No hay estado

    - No hay efectos secundarios

    - Programación de alto orden:

      - Funciones como ciudadanos de primera clase: se pueden usar en cualquier contexto

      - Funciones de alto orden: poder recibir como parametro o retornar funciones

- Lisp es un lenguaje de programacón de tipo multiparadigma: soporta más de un paradigma de programación
- Lisp es orientado a objetos, reflexivo, imperativo y funcional: el programador será capaz de crear programas usando más de un estilo de programación, sin estar forzado a tomar un estilo en particular.
- Existen otros lenguajes multiparadigma como - Python: éste además de programación orientada a objetos, programación imperativa y programación funcional, acepta otros paradigmas soportados mediante el uso de extensiones - Oz: incluye la idea de programación lógica, funcional(tanto lazy como eager), impertativa, orientada a objetos, con restricciones, distribuida y concurrente.

In func­tional program­ming, func­tions avoid two habits common in other languages: muta­tion (= changing data in-place rather than returning a value) and relying on state (= extra context that's not provided as input, for instance global vari­ables).

- ¿¿dataflow vs logicflow??

## Compilado/interpretado [Cami]

[How is Lisp dynamic and compiled? - StackOverflow](https://stackoverflow.com/questions/12593768/how-is-lisp-dynamic-and-compiled/12595700#12595700)

- Las funciones pueden ser compiladas de forma individual o por el archivo.
- Funciones compiladas o interpretadas se comportan de la misma forma, excepto con el comando `compiled-f unction-p` que verifica si la función pasada por parámetro fue compilada.

- Common List no es un compilador en tiempo de ejecución, sino que es necesario invocar al compilador medicante las funciones COMPILE, para las funciones individuales y COMPILE-FILE, para los archivos.
- El compilador puede recibir instrucciones sobre qué tan dinámico debe ser el código compilado
- Intérprete REPL(Read-Eval-Print-Loop): se tiene feedback inmediato y se puede programar desde abajo para arriba, compilando incrementalmente
- La función _eval_, va a toma las entradas individuales del usuario(s-expression pre parseada), las evalúa y devuelve el resultado al usuario
- No existe una distinción entre el tiempo de compilación, tiempo de ejecución y el tiempo de lectura:

- Ejecutar código en tiempo de lectura permite al usuario reprogramar la sintáxis de Lisp.
- Ejecutar código en tiempo de compilación es la base de las macros.
- Compilar en tiempo de ejecución es la base del uso de Lisp como un lenguaje de extensión en programas como lo es Emacs.
- Leer en tiempo de ejecución permite a los programas comunicarse utilizando _s-expressions_, una idea recientemente reinventada como _XML_.

## Tipado

Lisp es un lenguaje de tipado dinámico porque las verificaciones de tipo se realizan en tiempo de ejecución y las variables se pueden configurar de forma predeterminada para todo tipo de objetos.

_Dato_: Ademas de ser de tipado dinámico, Lisp es dinámico, porque tanto el lenguaje de programación Lisp como el programa en sí se pueden cambiar en tiempo de ejecución: se le prermite al usuario agregar, cambiar y eliminar funciones, construcciones sintácticas, tipos de datos, se podrá cambiar la sintaxis de superficie de Lisp de varias maneras. Esto facilite a que Lisp se tipee dinámicamente para proporcionar algunas de estas características.

- Lisp es dinámico: tanto el lenguaje de programación Lisp como el programa en sí se pueden cambiar en tiempo de ejecución, se le permite al usuario agregar, cambiar y eliminar - funciones - construcciones sintácticas - tipos de datos - sintáxis

- Tipado dinámico: las verificaciones de tipo se realizan en tiempo de ejecución y las variables se pueden configurar de forma predeterminada para todo tipo de objetos.

* Explota en runtime

  - Tipado fuerte: explota

  - Tipado dinamico: en runtime

## Closures [Sacarlo? o darle mejor forma y ponerlo como comparacion a OZ? o agregar mas cosas tipicas de programacion funcional (y te queda un popurri de 3 o 4 features de FP)]

- La variable debe persistir mientras la función lo haga.
- Variables léxicas válidas dentro del contexto en donde son definidas.
- Variable libre: Se continua haciendo referencia a una variable por fuera (mientras se continue usando el mismo contexto del cual fue definida).
- Lisp permite devolver una función como valor como cualquier otro objeto.

A continuación se muestra un ejemplo. Por un lado, la función combine toma argumentos de cualquier tipo y los combina de forma apropiada. combiner toma un argumento y devuelve una función para combinar argumentos de cualquier tipo.

```
(defun combiner (x)
    (typecase x
        (number #'+)
        (list #'append)
        (t #'list)))

(defun combine (&rest args)
    (apply (combiner (car args))
    args))
```

[How is Lisp dynamic and compiled? - StackOverflow](https://stackoverflow.com/questions/12593768/how-is-lisp-dynamic-and-compiled/12595700#12595700)

## Lexical/Static Scoping

## Dynamic scoping

Cuando se habla de variables de ambito léxico, se habla de un nombre que siempre refiere a su entorno léxico local, es decir que si yo defino una variable _X_ dentro de una función, esa será su definción adentro sin importar cualquier valor que podría tener por fuera.
En cambio, con dynamic scoping, se refiere al identificador asociado con el entorno más reciente. En otras palabras, se busca a la variable en el ambiente en el que la función se llama y no en dónde se define. Para que esto suceda es necesario declararla con `special`:

```
(le t ((x 20))
    (declare (specia l x))
    (foo))
```

## Recursión

La recursión en este lenguaje es muy importante por diversas razones:

- Evita errores por efectos secundarios.
- La estructura de datos de Lisp es más sencilla de utilizar con recursión. Las listas son o `nil` o `cons`.
- Código más elegante y limpio.

Sin embargo, tener en cuenta que las solución recursiva más obvio no necesariamente es la más eficiente. Por ejemplo, la función de Fibonacci, que se define recursivamente de la siguiente forma:

1. Fib(0) = Fib(l)=l.
2. Fib(n) = Fib(n-1) + Fib(rc-2)
   Al traducir esta idea a Lisp, nos encontramos con una idea poco eficiente(repite constantemente instrucciones que ya se resolvieron):

```
(defun fi b (n)
    (if « = n 1)
        1
        (+ (fib (- n 1))
            (fib (- n 2)))))
```

Por otro lado, se puede resolver de forma iterativa de esta forma:

```
(defun fi b (n)
    (do ((i n (- i 1))
        (fl 1 (+ fl f2))
        (f2 1 fl))
        ( « = i 1) fl)))
```

## Metaprogramming / Extensibilidad / Macros

## Expression oriented / Simbolico

Usa S-Expressions: Simbolic expression

Lisp is an expression oriented language. Unlike most other languages, no distinction is made between "expressions" and "statements";[dubious – discuss] all code and data are written as expressions. When an expression is evaluated, it produces a value (in Common Lisp, possibly multiple values), which can then be embedded into other expressions. Each value can be any data type.

https://en.wikipedia.org/wiki/Expression-oriented_programming_language

## Homoiconicidad ("Code as Data")

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

## s-exprs as trees

- Las s-expresiones son binary trees porque son o atomos o (x y) siendo (x . y) un dotted pair (x hijo izq, y hijo der)

- (hoy por hoy se usa el sintacic sugar LIST, que es nada mas concatenar mil dotted pairs. (x y z) es (x . (y . (z . null))))

- foto binary tree


## Macros

- Despues de la teoria, Volver a cl-aristid y al REPL. Expandir macros, compararlos con las versiones canonicas, mostrar los macros definidos, como se definen, como cambian la sintaxis

- En Lisp, una macro genera y devuelve código. La forma más sencilla de pensarlo sería como una transformación de código. Cuando se llama a una macro:

1. Se arma el código en base a la definición `defmacro` de la misma.
2. Se evalúa el nuevo código en el lugar de la llamada a la macro.

Si queremos analizar el código que generaría la macro, existe la función `macroexpand` que devuelve el código generado.

A partir de esto, se pueden usar macros para simplificar y reutilizar código, cambiar el orden de evaluación, agregar más argumentos en el medio o hasta manipular la sintaxis del lenguaje.

Las macros de Lisp son macros sintácticas, y funciona al nivel del árbol de sintaxis abstracta, preservando la estructura léxica del programa original. En otras palabras, las macros en Lisp se escriben en el mismo lenguaje de Lisp, donde está disponible toda la funcionalidad del lenguaje.

En contraste, las macros de _C_ se expanden en el preprocesador y funciona como substitución de texto.

- Algunos operadores:
  - `` Backquote ` ``: Funciona similar a `quote`.

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
  `(let ((result nil))
     (loop for ,var in ,list
          ,conditional ,conditional-test
          do (setq result (append result (list ,expression))))
     result))

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


## Extensibilidad del lenguaje

Macros llevan a la extensibilidad del lenguaje.

Hacer que un programa sea extensible significa que el programa permite a usuarios avanzados agregar funcionalidad extra si el programa no la tiene. Es una opción a que desarrollar un software que satisfaga todas las necesidades de todos los usuarios.

Lisp es un muy buen lenguaje para crear software extensible porque el lenguaje mismo es extensible: _permite escribir código que genera código_, como vimos con las macros. Con otros lenguajes no tenemos esta libertad, y cualquier extensión cambio en la sintaxis debería ser implementado de forma oficial por los desarrolladores del lenguaje.

¿Qué podemos hacer?

- Crear módulos completos para extender la funcionalidad del lenguaje:

  - Common Lisp Object System [CLOS]: agrega un sistema de objetos a Lisp, completo con enlaces dinámicos múltiples y herencia múltiple. Provee las macros `defclass`, `defgeneric`, `defmethod`.
  - bordeaux-threads: agrega funcionalidad para la creación de hilos.
  - lparallel: extiende funcionalidad de bordeaux-threads, con comunicación entre hilos, promesas, implementaciones de `map`, `reduce`, `sort` y `remove` que corren de forma concurrente, y más.
  - quicklisp: sistema de gestión de paquetes.
  - awesome-cl: Colección de librerías destacadas que incluye pero no está limitado a: conexión a base de datos, sistemas gráficos, sistemas matemático, unit testing.

- Crear lenguajes de dominio específico cambiando la sintaxis:

  - `CL-INTERPOL`: para interpolación de strings.
  - `infix`: para escribir ecuaciones matemáticas en notación de infijo.

- ¿Aún más? Racket, undialecto de Scheme y parte de la familia de Lisp, está orientado específicamente a crear lenguajes nuevos agregando funcionalidad para convertir código fuente en S-Expressions.


## Manejo de memoria

- Las variables de Lisp apuntan a sus valores.

- Todos los valores son conceptualmente un puntero.

- Manejo de memoria automática-> Consing.

- Sistema de garbage collection.

- Para tener una representación más inmediata, Lisp podría devolver un pequeño integer en vez de un puntero.

- Excepto que se declare lo contrario, se podrá almacenar cualquier tipo de objeto en cualquier estructura de datos (incluyendo la estructura misma).

- Garbage collector


- Las variables de Lisp apuntan a sus valores.
- La razon por la cual Lisp no tiene punteros es que todos los valores son conceptualmente un puntero.
- Manejo de memoria automática.
- Lisp cuenta con un sistema de garbage collection.
- Para tener una representación más inmediata, Lisp podría devolver un pequeño integer en vez de un puntero.
- Excepto que se declare lo contrario, se podrá almacenar cualquier tipo de objeto en cualquier estructura de datos (incluyendo la estructura misma).

Las variables tienen valores de la misma manera que las listas tienen elementos, es decir que las variables tienen apuntan al valor. Lisp se va a ocupar de manipular estos punteros, el usuario no debe preocuparse por ello.
Si el programador setea una variable _X_ con un valor _Y_, en la ubicación de memoria asociada al valor _Y_ se encuentra un puntero al mismo y Lisp va a copiar ese puntero en la variable _X_.
La razon por la cual Lisp no tiene punteros es que todos los valores son conceptualmente un puntero.

Las listas son una estructura de datos un poco lenta cuando se trata de recuperar un valor en especifico, ya que la busqueda es secuencial. Sin embargo este costo pueden ser pequeño en comparación con el costo
de asignación y reciclaje de _cons cell_.
Lisp cuenta con un manejo de memoria automatico compuesto por un heap en el que lleva un seguimiento de la memoria que no esta en uso y lo distribuye a medida que se crean nuevos objetos. El sistema esta buscando constantemente de la memoria que ya no se necesita, _garabage_ para poder reutilizar los _cons cells_. La alocación de memoria en el heap se conoce como _consing_ y la busqueda de basura, _garbage collection_.
Como ventaja, el programador no va a tener que encargarse jamas de alocar y desalocar memoria, y todos los problemas que en general conllevan esas tareas. Como desventaja, el costo asociado a usar, reciclar los espacios de memoria y constantemente recorrer en busca de basura podría ser costoso.

[Ansi common lisp cap 3 (link de descarga pdf)](https://mafiadoc.com/ansi-common-lisp-paul-grahampdf_59b625c51723dddcc6daf94d.html)

## Control de flujo

Common Lisp proporciona una variedad de estructuras especiales para organizar programas. Las mismas se implementan como formas especiales o como macros. En general, los programas Lisp se escriben como una gran colección de pequeñas funciones, cada una de las cuales implementa una operación simple. Estas funciones funcionan llamándose entre sí, por lo que las operaciones más grandes se definen en términos de las más pequeñas. Las funciones de Lisp pueden recurrir a sí mismas de forma recursiva, ya sea directa o indirectamente.

Las funciones definidas localmente (flet, etiquetas) y macros (macrolet) son bastante versátiles. La nueva función de macro de símbolos permite una flexibilidad aún más sintáctica.

Si bien el lenguaje Lisp tiene un estilo más aplicativo que orientado a declaraciones, proporciona muchas operaciones que producen efectos secundarios y, en consecuencia, requiere construcciones para controlar la secuencia de los efectos secundarios. `progn`, es más o menos equivalente a un bloque de inicio-fin con todos sus puntos y comas, ejecuta varias formas secuencialmente, descartando los valores de todos menos el último. Muchas construcciones de control de Lisp incluyen secuenciación implícita, en cuyo caso se dice que proporcionan un `implicit progn`.

Para los ciclos, Common Lisp proporciona la facilidad de iteración general y nos facilita la iteración y mapeo especificamente sobre varias estructuras de datos.

Common Lisp proporciona los condicionales unidireccionales simples `when` y `unless`, el condicional bidireccional simple `if`, y los condicionales multidireccionales más generales como `cond` y `case`.

Se proporcionan construcciones para realizar salidas no locales con diversas disciplinas de alcance: `block`, `return`, `return-from`, `catch`, y `throw`.

[Common Lisp the Language - Control Structure](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node76.html)
[Data and Control Flow (ejemplos de funciones y macros)](https://mr.gy/ansi-common-lisp/Data-and-Control-Flow.html#Data-and-Control-Flow)

## TDA

### Tablas de hash

#### Introducción

Las tablas de hash son una importante estructura de datos, que asocian claves con valores de una manera muy eficiente. Los hashes son preferibles por sobre listas cuando se le da importancia a los tiempos de búsqueda, pero son más complejos lo cual hace que las listas sean las elegidas cuando solamente hay unos pocos pares clave-valor a mantener.

#### Crear una tabla de hash en Common Lisp

Las tablas de hash son creadas usando la función `make-hash-table`. No requiere ningún argumento.
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

#### Agregar un elemento a la tabla

Se utiliza la función `gethash`, que es la función que se encarga de devolver el elemento, en conjunto con la función `setf`

```lisp
* (setf (gethash "clave1" tabla) 3) ; gethash recibe dos argumentos: la clave y el hash
3
```

#### Obtener un valor

La función `gethash` toma dos argumentos obligatorios: una clave y una tabla de hash. Devuelve dos valores: el valor que corresponde a la clave en la tabla de hash (ó `NIL` en caso de que no se encuentre), y un booleano que indica si la clave fue encontrada en la tabla.
El booleano es necesario ya que `NIL` es un valor válido en in par clave-valor. Es decir que obtener un `NIL` como primer valor de `gethash` no significa necesariamente que la clave no se encuentra en la tabla.

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

#### Borrar de la tabla de hash

Se utiliza la función `remhash` para eliminar el par clave-valor. Es decir, la clave y su valor asociado serán eliminados por completo de la tabla. `remhash` devuelve `T` si dicho par existe, `NIL` en otro caso.

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

#### Contar entradas

No hay necesidad de usar tus dedos! Common Lisp posee una función que lo hace por vos: `hash-table-count`. Recibe la tabla por parámetro.

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

#### El tamaño del hash

La función `make-hash-table` tiene algunos parámetros opcionales que controlan el tamaño inicial del hash y como crecerá en caso de que necesite hacerlo. Esto puede ser un gran problema de performance si se trabajo con tablas muy grandes.

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

Los valores para `hash-table-size` y `hash-table-rehash-size` dependen de la implementación. En este caso, la implementación de Common Lisp con la cual contamos, elige un tamaño inicial de 16, y aumentará el tamaño en un 50% (1.5) cada vez que el hash necesite crecer.

Veamos que sucede cuando agregamos un total de un millón\* de pares clave-valor al hash:

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

Y si piso todas las claves y tomo el tiempo nuevamente:

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

Veamos cuantas veces temenos que redimensionar para llegar al tamaño final:

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

El hash se redimensiona 28 veces hasta que sea lo suficientemente grande para contener 1,000,000 de claves con sus respectivos valores. Esto explica por qué tardó bastante en rellenar la tabla. También explica por qué la segunda guardada tardó considerablemente menos: el hash ya poseía el tamaño correcto.

Acá está la manera de hacerlo más rápido: Si ya sabemos con anticipación que tan grande nuestro hash será, podemos comenzar con el tamaño correcto desde el vamos:

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

Allí se prueba que tarda considerablemente menos tiempo. Tampoco hubo alocamientos involucrados ya que no hubo que redimensionar en absoluto. Si no sabemos el tamañop final de antemano, pero podemos anticipar el comportamiento de crecimiento que tendrá el hash, podemos utilizar sin duda el parámetro `:rehash-size` en la función `make-hash-table`. Usamos un entero para especificar cremiento absoluto, o un float para especificar crecimiento relativo.

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

Solamente necesitamos una redimensión, pero mucho màs realocamiento (41,943,107 bytes consed) porque casi toda la tabla (menos los 16 elementos iniciales) tuvieron que se construídos durante la iteración.

#### Fun stuff e iteradores del hash

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

## Cálculo Lambda

https://youtu.be/eis11j_iGMs

?x. x?x.

(lambda (x) (x\*x))

The power of the lambda notation is in its generality. The lambda notation will handle the case in which the value of a function is a function. In many computer languages the value of a function must be an element of a set, such as a number or a string or the label of a function. In the lambda notation the value can be a function, not the name or label of a function but a function itself.

https://www.sjsu.edu/faculty/watkins/lambda.htm

- Calculo Lambda no tipado: expresa _mas_ que el calculo lambda tipado

## Evaluacion

- Eager / Data-driven evaluation

- Todo se evalua

Lisp is usually evaluated eagerly. In Common Lisp, arguments are evaluated in applicative order ('leftmost innermost').

## Manejo de errores

Los errores puede ser señalizados por una amplia variedad de razones. Muchas funciones intregradas en Common Lisp, dan señal de error cuando se le da un parámetro incorrecto. Otras funciones, son llamadas por programas del usuario con el propósito de señalizar el error.


[Common Lisp the Language - Errors](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node219.html#SECTION002800000000000000000)

## Paralelismo

## Code vs Data

[Code vs Data (Metaprogramming) ~ Computerphile](https://youtu.be/dw-y3vNDRWk)

# Sintáxis

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

## Control de flujo

- Estructuras para organizar programas: formas especiales(flet, etiquetas) o macros(macrolet).

- Versatilidad en funciones definidas localmente y macros.

- Facilidad de iteración general.

- Facilidad de iteración y mapeo en estructura de datos.

- Condicionales unidireccionales simples `when` y `unless`.

- Condicional bidireccional simple `if`.

- Condicionales multidireccionales `cond` y `case`.

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

- Lisp-1 (scheme) vs. Lisp-2 (clisp) debate

```lisp6
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

## Sistemas

https://stevelosh.com/blog/2018/08/a-road-to-common-lisp/#s30-modern-common-lisp

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

# Estadísticas

# LISP en la práctica

[Who Cares About Functional Programming?](https://thomasbandt.com/who-cares-about-functional-programming)

## Stack Overflow

[Stack Overflow Trends](https://insights.stackoverflow.com/trends?tags=lisp%2Chaskell)

[Popularity of LISP](http://blockml.awwapps.com/example/example/document.html#id-DIALECTS)

<div class="container">
<div class="col">
![](img/lispvsdialects.svg)

</div>
<div class="col">

![](img/lispvslanguages.svg)

</div>
</div>

> En cuanto a popularidad de LISP en preguntas dentro de stackoverflow, vemos que Common Lisp compite contra otros dialectos, pero Clojure es el más utilizado.

> De todas formas, toda la familia de LISP es un porcentaje muy chico, en comparación a otros lenguajes.

## GitHub

[GitHut 2.0: A Small Place To Discover Languages In Github](https://madnight.github.io/githut/#/pull_requests/2020/2)

![](img/githut.png)

> Podemos ver que la cantidad de pull requests de Common Lisp anuales son muy bajos, incluso contra otros dialectos y lenguajes

## Hacker News

[Hacker News Front Page Trends](https://toddwschneider.com/dashboards/hacker-news-trends/?q=lisp%2C+haskell%2C+java&f=title&s=text&m=items_count&t=year)

![](img/hackernews.png)

> Aunque Lisp no es un lenguaje que se usa mucho en la práctica, si hay un gran interés teórico por este (hay que tener en cuenta que este foro nos da una idea de que _blogs_ o _artículos_ se comparten de una tecnología, en vez de usos practicos y código productivo).

# Comparaciones

## Familias de Lisp

Hoy, los dialectos de Lisp más ampliamente usados, ademas de Common Lisp, son Scheme (1975),Emacs Lisp (1985) y Clojure (2007).

### Scheme

Scheme es un lenguaje de programación muy corto, cuenta con solo 50 páginas, es más corto que el índice del libro de Guy Steele Common Lisp: The Language. Conocido por ser limpio y minimalista, Scheme solamente define el inner core del lenguaje, proporciona el mínimo número posible de nociones primitivas, contruyendo todo lo demas a partir de un reducido numero de abstracciones.
Cuenta con ciertas características de implementación (tales como optimización de llamada de cola y continuación completa), un sistema de macros limpio y transparente basado en reglas de reencritura.
Scheme se usa a menudo en ciencias de computación o investigación debido a su capacidad de representar muchas abstracciones de programación con sus primitivas simples. Common Lisp, en cambio, se usa para la programación del mundo real debido a su gran biblioteca de funciones de utilidad, CLOS y su sistema de manejo de condiciones.
Si se tuviera que crear una aplicación, probablemente es mejor ir por Common Lisp. Pero si se tuviera que enseñar en un curso, probablemente la mejor opción sería Scheme (pero con macros de Common Lisp)

### Clojure

Clojure es de los lenguajes de programación pertenecientes a la familia de Lisp más recientes.
El sistema de macros de Clojure es muy similar al de Common Lisp con la excepción de que la versión de Clojure de la comilla inversa (llamada "comilla sintáctica") cualifica los símbolos con el espacio de nombres al que pertenece.
A diferencia de Common Lisp que se maneja con lista, Clojure usa secuencias con evaluación perezosa, es decir que los elementos de la secuencia no se computan hasta que son necesarios, lo que permite representar conjuntos infinitos en potencia.
Las aplicaciones escritas en Clojure pueden ser fácilmente integradas en servidores de aplicaciones u otros entornos Java con escasa complejidad adicional. Este puede ser ejecutado sobre la Máquina Virtual de Java y la máquina virtual de la plataforma .NET, así como compilado a JavaScript.

### Emacs Lisp

- Emacs Lisp trabaja con dynamic scoping por default.
- No tiene closures, lo cual hace la composicion de funciones bastante dificil.

## Otros lenguajes

### Python

Python admite todas las caracteristicas esenciales de Lisp, exeptuando las macros.
La sintaxis de este lenguaje de programación es bastante más sencilla de leer, Lisp proporciona un core más potente y consistente, pero más engorroso a la hora de leer. En consecuencia, Lisp, en general, es más dificil para aprende ya que se opera en un alto nivel de abstracción desde un principio. Sin embargo, luego sera mas sencillo agregar niveles de abstracción y complejidad. Esto hace que Python sea más sencillo de programar para problemas de dificultad básica pero Lisp este más preparado para facilitar las tareas más complejas.
En cuanto al tiempo de compilacion, Python más rápido(tiempo de compilación, de analisis de errores y dedeclaración de tipos baja). En cuanto a tiempo de ejecución python es mucho más lento.
Por otro lado, es más dinámico, ya que realiza menos chequeos de erorres.
Más allá de estas diferencias, son lenguajes de alto nivel interpretado y compilado orientado a objetos muy similares. Ambos admiten módulos y paquetes, fomrantando la modularidad del programa y en ambos lenguajes es muy sencillo de debuggear, un bug no puede producir un Segmentation Fault.

### C++

- Lisp es una o dos veces más lento que C++
- Manejo de memoria con punteros
- Sintaxis más detallada y restrictiva

# Casos de estudio

### Crash Bandicoot

El siguiente caso de estudio y como es que tener buenos fundamentos de la programación es una muy buena ventaja:
Crash Bandicoot fue originalmente creado en 1996 por la desarrolladora de videojuegos Naughty Dog, fundada en 1984 por Andy Gavin y Jason Rubin. 
Gavin explica como, por casi dos décadas, era un fan devoto de Lisp. Incluso admite como forzó a sus programadores a codear Crash, en un dialecto de Lisp personlizado, GOOL
(Game Oriented Object LISP). 
Gavin diseño GOOL usando Common Lisp, lo cual facilita mucho porque desde el comienzo, ya contaba con el garbage collector, listas, arboles, tabla de hash y macros. Usando GOOL, el equipo pudo producir cientos de objetos de juegos diferentes con comportamiento y animación sofisticada, en tiempo real.
Eventualmente Lisp perdió su impulso y  Gavin le empezaró a soltar lentamente la mano. Comenzó a desarrollar en otros lenguajes, como Ruby, que venia con mejores librerias y la rompia con el sistema de macros que tenia. Pero siempre teniendo en cuenta a Lisp, que te da una buena calidad técnica y eso se transfiere a la modernidad: Codear Lisp, pero en Ruby.

