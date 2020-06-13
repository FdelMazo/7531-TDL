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

## Lisp FAQ (yes, it exists)

- ¿Qué es Lisp?

  - Familia de lenguajes de programación que desciende de un lenguaje inventado por John McCarthy a fines de los 50.
  - Los dos dialectos principales son Common Lisp y Scheme.

- ¿Cómo puedo aprender Lisp?

  - La manera de aprender un lenguaje es escribir programas en él
  - Los dos dialectos principales a elegir son Common Lisp y Scheme, ambos tienen ventajas y desventajas, pero las diferencias entre ellos sigue siendo chica que comparando contra otros lenguajes, así que no importa cual elejas para empezar.
  - Get a book! Y empezá a escribir programas.
  - Leer código ya existente para tener una idea del estilo Lisp
  - Still a newbie? Googleá comp.lang.lisp

- ¿Cuál debería aprender, Common Lisp o Scheme? Cuál es la diferencia?

  - Common Lisp: Es poderoso pero feo
  - Scheme: Es chiquito y limpio, pero el estándar solamente define es inner core del lenguaje.
  - Si tuviera que crear una aplicación, probablemente es mejor ir por Common Lisp.
  - Si tuviera que enseniar (no tengo enie) en un curso, usaria Scheme (pero con macros de Common Lisp)
  
  
- Si Lisp es tan genial como dicen (**cough** Fede **cough**), ¿por qué no lo usan más personas?

  - La causa principal probablemente sea porque _se ve_ complicado, pero ayudaría si hubiera un dialecto con librerías extensas

Source: http://www.paulgraham.com/lispfaq1.html

## Que hizo a Lisp algo diferente

  Lisp incluyó 9 ideas novedosas: 

### 1. _Condicionales_
  
  - Un condicional es un una construcción if-then-else; hoy en día los damos por hecho. 
  - Fueron inventados por McCarthy en el transcurso de desarrollo de Lisp.

### 2. _Un tipo función_

  - En Lisp, las funciones son objectos de primera clase
  - Son un tipo de dato como lo son los enteros, cadenas, ert. 
  - Tienen una representación literal, pueden ser asignadas a variables, pasadas como argumentos (parámetros)...

### 3. _Recursión_

  - Solía existir como un concepto matemático antes de Lisp (por supuesto), pero Lisp fue **el primer lenguaje de programación que lo soportaba**

### 4. _Un nuevo concepto en variables_

  - En Lisp, todas las variables son efectivamente punteros.
  - Los valores son aquellos que _tienen tipos_, no variables
  - Asignar variables significa copiar punteros, y no aquello a lo que apuntan.
  
### 5. _Recolector de basura automático_
  
  Sí, también comenzó con Lisp.

### 6. _Programas compuestos por expresiones_

  - Los programas en Lisp son árboles de expresiones, cada uno devuelve un valor. (En otras expresiones de Lisp, puede devolver múltiples valores.)
  - Esto es en contraste con los lenguajes más exitosos, que distinguen entre _expresión_ y _declaración_
  - Cuando un lenguaje está hecho enteramente de expresiones, uno puede componer expresiones como uno quiera:
  Puede ser (en syntaxis de _Arc_):
  
```lisp
  (if foo (= x 1) (= x 2))
```
o

```lisp
(= x (if foo 1 2))
```
  
### 7. _Un tipo de símbolo_

  - Los símbolos difieren de las cadenas en que uno puede testear igualidad comparando un puntero.

### 8. _Una notación de código usando árboles de símbolos_

### 9. _El lenguaje completo está siempre disponible_

  - No hay una distinción real entre tiempo de lectura, tiempo de compilación y tiempo de ejecución. 
  - Uno puede compilar o ejecutar mientras lee, leer o ejecutar código mientras compila, leer o compilar mientras se ejecuta el código.
 
  - Ejecutar código en tiempo de lectura permite al usuario reprogramar la sintáxis de Lisp
  - Ejecutar código en tiempo de compilación es la base de las macros
  - Compilar en tiempo de ejecución es la base del uso de Lisp como un lenguaje de extensión en programas como lo es **Emacs**
  - Leer en tiempo de ejecución permite a los programas comunicarse utilizando _s-expressions_, una idea recientemente reinventada como _XML_

Source: http://www.paulgraham.com/diff.html
