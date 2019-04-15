## Lab de Programacion Funcional

En este lab queremos diseñar un lenguaje funcional que nos permita manejar
dibujos "básicos" o "simples" para poder aplicarles funciones que los
modifiquen y producir así figuras mas complejas compuestas por estas básicas.

El objetivo final es ser capaces de, con este lenguaje, producir una de las
figuras clásicas de Escher :

![Square Limit, Escher][image1]

### Experiencia

Al principio del lab nos llevó tiempo entender como deberíamos implementar los
tipos de las funciones y como estas deberían trabajar con nuestras figuras
básicas. Con ayuda de los profes pudimos empezar a pensar que teníamos que
hacer.

Esta tarea también nos llevó tiempo porque tuvimos que recordar como programar
en Haskell.

Lo que más nos costó fue entender cuál es la conexión entre el lenguaje, la
interpretación geométrica y el uso de Gloss.

Las conceptos como `Vacio` que estaban implementadas a medias y los typos en la
consigna no fueron de ayuda a la hora de implementar las funciones. Además,
muchos de los comentarios de ayuda en las funciones generaban más confusión.


### Extras

Pudimos implementar como item extra que las figuras de escher se vieran
dibujadas con colores distintos, se puede elegir entre 14 colores reemplazando
en el archivo `escher.hs` el atributo `colorbas` dentro de `myBC` por cualquiera
del datatype `Colores`.

En la rama llamada `fish` intentamos también ser capaces de cargar imágenes bmp
y así dibujar Escher pero en vez de triángulos con pescados. Creamos una nueva
función `interpBasbmp` para ser capaces de admitir un bmp y poder generar una
figura para representar en `Escher.hs`. Esto quedó incompleto porque solo
pudimos cargar el bmp sin poder componerlo y generarlo correctamente.

<!-- # referencias -->

[image1]:http://www.tess-elation.co.uk/_/rsrc/1472862680265/self-similar-tessellations/400px--square-limit.jpg
