---
title: |
  | (Common) LISP
  | Caso de Estudio: Reddit
author: del Mazo, Federico - 100029
header-includes: \hypersetup{colorlinks=true}
---

Reddit, en grandes rasgos, es una red social dedicada a la discusión de noticias. Fundado en junio de 2005, este sitio fue programado originalmente en Common Lisp. Inesperadamente, al muy poco tiempo, en diciembre de ese mismo año, el sitio entero fuera re-escrito en Python.

Entonces, ¿por qué los creadores decidieron inicialmente programar en Lisp? ¿por qué fue tan pronta esta migración?

\

**Desarrollo**

El trayecto que recorrieron los fundadores de Reddit es muy similar a lo que le sucede a varios productos que intentan ser escritos en Lisp: Primero se enamoran del lenguaje[^1], luego, después de mucha programación en Lisp, se agarran todas las buenas prácticas de programación que este brinda, y finalmente se dan cuenta que el lenguaje, para niveles prácticos, es mucho menos potente de lo que imaginaron, y pasado el desenamoramiento migran a otro lenguaje.

Esto pasa en varios casos, y termina produciendo el mismo efecto, programadores apasionados por todo lo que aprendieron, pero que van y lo utilizan _en otros lenguajes_. Esto es porque, como el tiempo va demostrando, los lenguajes funcionales no se llevan muy bien con el mundo real, y en su mayoría se ven relegados al mundo académico.

En particular, algo que destacan mucho los programadores originales de Reddit, es que Lisp funciona mucho para proyectos donde no sabes el objetivo final. Lisp es un gran lenguaje para la _exploración_ y el _descubrimiento_ de una idea. Cuando comenzó, Reddit no sabía en que se iba a convertir, no había un final del camino super claro, solo se sabía que iba a ser una página web de noticias, "y después vemos que pasa".

Lisp es una gran herramienta para esto gracias a su dinamicidad. Esta característica es parte de la filosofía del lenguaje, y lleva a que cambiar de ideas en el medio del tipeo de código se haga muy sencillo y hasta este alentado en pleno desarrollo.

¿Dónde se ve esta filosofía del lenguaje? Por empezar, en el REPL, que permite probar código en vivo y ver que sirve y que no. También, en el debugger interactivo, que permite probar distintos valores para las variables, distintos órdenes de ejecución y demás pruebas que llevan el código original a lugares que no eran parte de su concepción. Por último, también se ve en las bases de este lenguaje, que por ser funcional, permite que toda función se considere un bloque independiente de código, y modificarlo no altera el trayecto del producto. Es así como se puede ir programando desde el piso hasta ver a donde se llega. Al ser un lenguaje tan flexible, se alienta la idea del prototipado, haciendo que si hay una idea para probar, se pueda programar en poco tiempo, y este código hecho termine siendo parte del producto final, por su funcionalidad.

\

**Migración**

> `Since we’re building a site largely by standing on the shoulders of others, this made things a little tougher. There just aren’t as many shoulders on which to stand.'

La migración finalmente sucede por los motivos que siempre escuchamos: Lisp no es bueno para el mundo real. En particular, los creadores de Reddit le atribuyen la migración a la falta de bibliotecas en Lisp: dicen que siempre existe exactamente _una_ biblioteca para cada tarea, pero que no hay mucho lugar a tener alternativas, y que las existentes no suelen estar mantenidas o bien documentadas.

También, muestran que hay problemas del desarrollo en distintas plataformas, de networking y de threading. Un lenguaje que no puede ser desarrollado fácilmente en distintas plataformas (el creador tenía problemas con MacOS) ya habla de una gran inestabilidad práctica.

Es por todo esto que finalmente los creadores se pasaron a Python, donde la comunidad ya estaba bastante crecida y de todo se puede encontrar algun desarrollador apasionado que hizo justo la biblioteca necesaria. Por suerte, no vieron nada compleja la traducción de código Lisp a Python, comentando que es mu sencilla por la flexibilidad de ambos lenguajes.

---

[on lisp ~ Reddit](https://redditblog.com/2005/12/05/on-lisp/)

[Rewriting Reddit ~ Aaron Swartz](http://www.aaronsw.com/weblog/rewritingreddit)

[Reddit and Lisp psychosis ~ Dave Roberts](http://www.findinglisp.com/blog/2005/12/reddit-and-lisp-psychosis.html)

[We Are the Nerds: The Birth and Tumultuous Life of Reddit, the Internet's Culture Laboratory ~ Christine Lagorio-Chafkin](https://books.google.com.ar/books?id=WNpKDwAAQBAJ)

[^1]: Incluso es gracioso que los fundadores de Reddit comparan el cambiar de lenguaje de programación a cortar con una novia.
