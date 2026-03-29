# Lógica Computacional 2026-2

## Práctica 3

Para trabajar sobre esta base, tienen que hacer un fork de este repositorio y trabajar sobre él.

Únicamente modifiquen el archivo Practica03.hs que se encuentra en el directorio src. Si quieren modificar las pruebas o agregar más, pueden preguntarme con confianza y les explico como modificarlas.

Deben tener instalado el compilador de Haskell para poder probar su práctica. Para ello deben colocarse en el directorio src y ejecutar el comando `ghci Practica03.hs`.

Si quieren probar su práctica haciendo uso de las pruebas unitarias que les estoy pasando, tienen que ejecutar los siguientes comandos desde el directorio donde se encuentra este ReadMe:
```
cabal build
cabal test
```

El primero es para compilar y el segundo es para ejecutar las pruebas unitarias.

Si no les llegan a funcionar, es posible que el problema es que tengan una versión diferente de cabal y de ghc. Si ese es el caso, pueden ejecutar el comando `ghc-pkg list base` para reemplazar la versión base que viene en el archivo .cabal en las líneas 70 y 102.

En este caso particular es posible que también necesiten ejecutar `ghc-pkg list deepseq` para igualmente reemplazar la versión de ese paquete que viene en el archivo .cabal en la línea 104.

## Integrantes

En esta sección deben eliminar esta línea de texto, borrar la leyenda "Integrante n" y escribir su nombre empezando por apellidos y su número de cuenta.

+ Olivares Martínez Andrea Danae
    - No. de Cuenta: 322247279
+ Ramirez Palacios Miguel
    - No. de Cuenta: 322216376


## Comentarios
-Tuve que ocupar "v" e "y" para Or y And porque mi compu me mandaba error y no podia ver los que tu pusiste sjajs 
-En pares2 tuve que añadir un error porque haskell me botaba si no añadia el caso de la lista vacia, apesar de que tenemos que asumir que siempre se devuelve una literal 
-El test 2 de la formula normal conjuntiva no lo pasa por que no q y no p aparecen invertidas, pero realmente no afecta su posición en el Or donde estan
-En el ultimo test de saturación si intento quitar algo de eliminar duplicados en la lista para que el programa se logre enciclar y pasar el ultimo test, no puede pasar los tests, lo intente y ya llevaba 10 minutos sin lograr pasar el primero :c
