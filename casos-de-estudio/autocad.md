% Caso de uso de Lisp
% 75.31 - Teoría de Lenguaje
  Javier Di Santo - 101696

# AutoCAD

AutoCAD es un software de modelado de diseño 2D y 3D lanzado en 1982. Actualmente es desarrollado y comercializado por la empresa Autodesk. Es reconocido por sus amplias capacidades de edición, que hacen posible el dibujo digital de planos de edificios o la recreación de imágenes en 3D; es uno de los programas más usados por arquitectos, ingenieros, diseñadores industriales y otros.

Se puede controlar mediante las interfaces de menús y línea de comandos. Además proporciona APIs para la automatización de tareas, y uno de los lenguajes soportados es AutoLISP.

# AutoLISP

AutoLISP es un dialecto de LISP hecho para la API inicial de AutoCAD. Fue lanzado en 1986 con actualizaciones hasta 1995, donde su desarrollo fue abandonado a favor de otros lenguajes y entornos de desarrollo.

En su momento, LISP fue elegido para esta tarea por ser _"adecuado al proceso de diseño desestructurado de los proyectos AutoCAD, que involucra intentar distintas soluciones ante problemas de diseño"_, a diferencia de otros lenguajes como C que obligan a plantear todo un problema antes de comenzar a programar. En otras palabras, coincide con el estilo de programación _bottom-up_ que es muy común en Lisp.

En comparación a Lisp, AutoLISP tiene funcionalidad limitada, como por ejemplo no tiene sistema de macros, funciones con cantidad de parámetros variable, creación de variables con `let`, etcétera.

El objetivo de AutoLISP es extender la funcionalidad de AutoCAD mediante scripts para automatizar tareas, los cuales se pueden agregar a la misma interfaz del programa.
Hoy en día tiene APIs para .NET y ObjectARX (librería de C++), pero AutoLISP sigue siendo compatible y es incluído en las versiones completas de AutoCAD y sus derivados.

A pesar de tener alternativas más modernas para desarrollar extensiones, AutoLISP sigue siendo una opción viable:

- El foro oficial dedicado a AutoLISP sigue activo con múltiples publicaciones nuevas por día.

- Su funcionalidad "acotada" y sintaxis simple lo hace más fácil de entender que C++ o .NET. Ideal para los usuarios de AutoCAD, que no suelen ser programadores.

- El lenguaje fue implementado en otros programas de diseño similares (como BricsCAD e IntelliCAD) para competir con la funcionalidad de AutoCAD.


### Bibliografía

_"What is AutoLISP?"_
	
--- http://www.caddsoftsolutions.com/AutoLISP.htm


_"Why Lisp?"_ por John Walker, 5 de febrero 1985 
	
--- https://www.fourmilab.ch/autofile/e5/chapter2_35.html


Página Wikipedia de AutoLISP 
	
--- https://en.wikipedia.org/wiki/AutoLISP