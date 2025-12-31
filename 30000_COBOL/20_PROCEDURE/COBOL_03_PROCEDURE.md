# COBOL_03: PROCEDURE DIVISION

> "En COBOL, la PROCEDURE DIVISION es donde la lógica de negocio cobra vida. Cada párrafo cuenta una historia."

---

## Estructura de PROCEDURE DIVISION

```cobol
       PROCEDURE DIVISION.
      *-----------------------------------------
      * SECCIÓN: Agrupación lógica de párrafos
      *-----------------------------------------
       0000-MAIN SECTION.

       0100-INICIO.
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESO
           PERFORM 3000-FINALIZAR
           STOP RUN.

       1000-INICIALIZAR SECTION.
       1100-ABRIR-ARCHIVOS.
           OPEN INPUT ARCHIVO-ENTRADA
           OPEN OUTPUT ARCHIVO-SALIDA.

       2000-PROCESO SECTION.
       2100-LEER-REGISTRO.
           READ ARCHIVO-ENTRADA
               AT END SET FIN-ARCHIVO TO TRUE
           END-READ.
```

### Párrafos vs Secciones

| Elemento | Uso | Scope |
|----------|-----|-------|
| **Párrafo** | Unidad básica de código | Termina en siguiente párrafo |
| **Sección** | Agrupa párrafos relacionados | Contiene múltiples párrafos |

```cobol
      * Solo Párrafos (más común en código moderno)
       PROCEDURE DIVISION.

       MAIN-PARA.
           PERFORM INIT-PARA
           PERFORM PROCESS-PARA
           PERFORM END-PARA
           STOP RUN.

       INIT-PARA.
           INITIALIZE WS-WORK-AREAS.

       PROCESS-PARA.
           PERFORM UNTIL END-OF-FILE
               READ INPUT-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END PERFORM PROCESS-RECORD
               END-READ
           END-PERFORM.

       END-PARA.
           CLOSE INPUT-FILE OUTPUT-FILE.
```

---

## MOVE - Movimiento de Datos

### MOVE Básico

```cobol
      * MOVE simple
           MOVE WS-ORIGEN TO WS-DESTINO

      * MOVE a múltiples destinos
           MOVE SPACES TO WS-CAMPO1
                          WS-CAMPO2
                          WS-CAMPO3

      * MOVE con valores especiales
           MOVE ZEROS TO WS-CONTADOR
           MOVE SPACES TO WS-NOMBRE
           MOVE HIGH-VALUES TO WS-CLAVE-BUSQUEDA
           MOVE LOW-VALUES TO WS-CLAVE-INICIAL
           MOVE ALL "*" TO WS-LINEA
```

### Reglas de MOVE

```cobol
      * Numérico a Numérico: Alineación por punto decimal
       01  WS-NUM1    PIC 9(5)V99.
       01  WS-NUM2    PIC 9(3)V9999.

           MOVE 12345.67 TO WS-NUM1      *> 12345.67
           MOVE WS-NUM1 TO WS-NUM2       *> 345.6700 (trunca izq)

      * Alfanumérico: Alineación izquierda, relleno espacios
       01  WS-TEXTO1  PIC X(10).
       01  WS-TEXTO2  PIC X(5).

           MOVE "HOLA" TO WS-TEXTO1      *> "HOLA      "
           MOVE WS-TEXTO1 TO WS-TEXTO2   *> "HOLA " (trunca der)

      * Grupo a Grupo: Copia byte por byte
       01  WS-GRUPO1.
           05 FILLER  PIC X(10) VALUE "ABCDEFGHIJ".
       01  WS-GRUPO2.
           05 G2-PARTE1 PIC X(5).
           05 G2-PARTE2 PIC X(5).

           MOVE WS-GRUPO1 TO WS-GRUPO2
      *    G2-PARTE1 = "ABCDE", G2-PARTE2 = "FGHIJ"
```

### MOVE CORRESPONDING

```cobol
       01  WS-ENTRADA.
           05  ENT-NOMBRE    PIC X(20).
           05  ENT-EDAD      PIC 9(3).
           05  ENT-SUELDO    PIC 9(7)V99.

       01  WS-SALIDA.
           05  SAL-CODIGO    PIC X(10).
           05  ENT-NOMBRE    PIC X(20).
           05  ENT-EDAD      PIC 9(3).
           05  ENT-SUELDO    PIC 9(7)V99.

           MOVE CORRESPONDING WS-ENTRADA TO WS-SALIDA
      *    Mueve campos con el mismo nombre
```

---

## Aritmética

### ADD

```cobol
      * ADD simple
           ADD WS-A TO WS-B
      *    WS-B = WS-B + WS-A

      * ADD múltiples
           ADD WS-A WS-B WS-C TO WS-TOTAL
      *    WS-TOTAL = WS-TOTAL + WS-A + WS-B + WS-C

      * ADD GIVING (no modifica operandos)
           ADD WS-A TO WS-B GIVING WS-RESULTADO
      *    WS-RESULTADO = WS-A + WS-B

      * ADD con manejo de overflow
           ADD WS-A TO WS-B
               ON SIZE ERROR
                   DISPLAY "OVERFLOW EN SUMA"
                   MOVE 9999999 TO WS-B
               NOT ON SIZE ERROR
                   CONTINUE
           END-ADD

      * ADD CORRESPONDING
           ADD CORRESPONDING WS-VALORES TO WS-TOTALES
```

### SUBTRACT

```cobol
      * SUBTRACT simple
           SUBTRACT WS-A FROM WS-B
      *    WS-B = WS-B - WS-A

      * SUBTRACT múltiples
           SUBTRACT WS-A WS-B FROM WS-C
      *    WS-C = WS-C - WS-A - WS-B

      * SUBTRACT GIVING
           SUBTRACT WS-DESCUENTO FROM WS-BRUTO
               GIVING WS-NETO
      *    WS-NETO = WS-BRUTO - WS-DESCUENTO

      * SUBTRACT con SIZE ERROR
           SUBTRACT WS-PAGO FROM WS-SALDO
               ON SIZE ERROR
                   DISPLAY "ERROR: SALDO INSUFICIENTE"
                   SET ERROR-SALDO TO TRUE
           END-SUBTRACT
```

### MULTIPLY

```cobol
      * MULTIPLY simple
           MULTIPLY WS-CANTIDAD BY WS-PRECIO
      *    WS-PRECIO = WS-PRECIO * WS-CANTIDAD

      * MULTIPLY GIVING
           MULTIPLY WS-CANTIDAD BY WS-PRECIO
               GIVING WS-SUBTOTAL
      *    WS-SUBTOTAL = WS-CANTIDAD * WS-PRECIO

      * MULTIPLY con ROUNDED
           MULTIPLY WS-MONTO BY WS-TASA
               GIVING WS-INTERES ROUNDED

      * MULTIPLY con SIZE ERROR
           MULTIPLY WS-A BY WS-B GIVING WS-C
               ON SIZE ERROR
                   DISPLAY "OVERFLOW EN MULTIPLICACION"
           END-MULTIPLY
```

### DIVIDE

```cobol
      * DIVIDE simple
           DIVIDE WS-DIVISOR INTO WS-DIVIDENDO
      *    WS-DIVIDENDO = WS-DIVIDENDO / WS-DIVISOR

      * DIVIDE GIVING
           DIVIDE WS-TOTAL BY WS-CANTIDAD
               GIVING WS-PROMEDIO

      * DIVIDE con REMAINDER
           DIVIDE WS-DIVIDENDO BY WS-DIVISOR
               GIVING WS-COCIENTE
               REMAINDER WS-RESIDUO
      *    Ejemplo: 17 / 5 = 3, residuo 2

      * DIVIDE con validación
           IF WS-DIVISOR = ZERO
               DISPLAY "ERROR: DIVISION POR CERO"
           ELSE
               DIVIDE WS-DIVIDENDO BY WS-DIVISOR
                   GIVING WS-RESULTADO ROUNDED
                   ON SIZE ERROR
                       DISPLAY "OVERFLOW EN DIVISION"
               END-DIVIDE
           END-IF
```

### COMPUTE

```cobol
      * COMPUTE permite expresiones complejas
           COMPUTE WS-RESULTADO = WS-A + WS-B * WS-C
      *    Respeta precedencia: * antes de +

      * Paréntesis para control de precedencia
           COMPUTE WS-RESULTADO = (WS-A + WS-B) * WS-C

      * Operadores disponibles
      *    +   Suma
      *    -   Resta
      *    *   Multiplicación
      *    /   División
      *    **  Exponenciación

      * Fórmula compleja
           COMPUTE WS-PAGO-MENSUAL =
               (WS-CAPITAL * WS-TASA-MENSUAL) /
               (1 - (1 + WS-TASA-MENSUAL) ** (- WS-MESES))

      * Con ROUNDED y SIZE ERROR
           COMPUTE WS-IMPUESTO ROUNDED =
               WS-BASE * 0.16
               ON SIZE ERROR
                   PERFORM ERROR-CALCULO
           END-COMPUTE

      * Múltiples destinos
           COMPUTE WS-TOTAL WS-BACKUP =
               WS-SUBTOTAL + WS-IVA - WS-DESCUENTO
```

---

## Condicionales

### IF-ELSE

```cobol
      * IF simple
           IF WS-EDAD >= 18
               DISPLAY "MAYOR DE EDAD"
           END-IF

      * IF-ELSE
           IF WS-SALDO >= WS-MONTO-RETIRO
               SUBTRACT WS-MONTO-RETIRO FROM WS-SALDO
               DISPLAY "RETIRO EXITOSO"
           ELSE
               DISPLAY "FONDOS INSUFICIENTES"
           END-IF

      * IF anidados
           IF WS-TIPO-CLIENTE = "PREMIUM"
               IF WS-ANTIGUEDAD > 5
                   COMPUTE WS-DESCUENTO = WS-TOTAL * 0.20
               ELSE
                   COMPUTE WS-DESCUENTO = WS-TOTAL * 0.10
               END-IF
           ELSE
               IF WS-ANTIGUEDAD > 10
                   COMPUTE WS-DESCUENTO = WS-TOTAL * 0.05
               ELSE
                   MOVE ZERO TO WS-DESCUENTO
               END-IF
           END-IF
```

### Operadores de Comparación

```cobol
      * Operadores relacionales
           IF WS-A = WS-B          *> Igual
           IF WS-A NOT = WS-B      *> Diferente
           IF WS-A > WS-B          *> Mayor que
           IF WS-A >= WS-B         *> Mayor o igual
           IF WS-A < WS-B          *> Menor que
           IF WS-A <= WS-B         *> Menor o igual

      * También se pueden escribir como:
           IF WS-A IS EQUAL TO WS-B
           IF WS-A IS NOT EQUAL TO WS-B
           IF WS-A IS GREATER THAN WS-B
           IF WS-A IS GREATER THAN OR EQUAL TO WS-B
           IF WS-A IS LESS THAN WS-B
           IF WS-A IS LESS THAN OR EQUAL TO WS-B
```

### Operadores Lógicos

```cobol
      * AND
           IF WS-EDAD >= 18 AND WS-EDAD <= 65
               DISPLAY "EN EDAD LABORAL"
           END-IF

      * OR
           IF WS-ESTADO = "A" OR WS-ESTADO = "P"
               PERFORM PROCESAR-ACTIVO
           END-IF

      * NOT
           IF NOT WS-FIN-ARCHIVO
               PERFORM LEER-SIGUIENTE
           END-IF

      * Combinados
           IF (WS-TIPO = "A" OR WS-TIPO = "B")
              AND WS-MONTO > 1000
              AND NOT WS-BLOQUEADO
               PERFORM APROBAR-TRANSACCION
           END-IF
```

### Condiciones Especiales

```cobol
      * Condiciones de clase
           IF WS-CAMPO IS NUMERIC
               COMPUTE WS-RESULTADO = WS-CAMPO + 1
           END-IF

           IF WS-CAMPO IS ALPHABETIC
               DISPLAY "SOLO LETRAS"
           END-IF

           IF WS-CAMPO IS ALPHABETIC-LOWER
           IF WS-CAMPO IS ALPHABETIC-UPPER

      * Condiciones de signo
           IF WS-NUMERO IS POSITIVE
           IF WS-NUMERO IS NEGATIVE
           IF WS-NUMERO IS ZERO

      * Condition Names (88)
       01  WS-ESTADO-CIVIL   PIC X(1).
           88  SOLTERO       VALUE "S".
           88  CASADO        VALUE "C".
           88  DIVORCIADO    VALUE "D".
           88  VIUDO         VALUE "V".
           88  VALIDO        VALUE "S" "C" "D" "V".

           IF CASADO
               PERFORM CALCULAR-BENEFICIO-CONYUGAL
           END-IF

           IF NOT VALIDO
               DISPLAY "ESTADO CIVIL INVALIDO"
           END-IF

      * SET para cambiar valores 88
           SET CASADO TO TRUE
      *    Equivale a: MOVE "C" TO WS-ESTADO-CIVIL
```

### EVALUATE (Switch/Case)

```cobol
      * EVALUATE simple
           EVALUATE WS-OPCION
               WHEN "1"
                   PERFORM OPCION-ALTA
               WHEN "2"
                   PERFORM OPCION-BAJA
               WHEN "3"
                   PERFORM OPCION-CONSULTA
               WHEN OTHER
                   DISPLAY "OPCION NO VALIDA"
           END-EVALUATE

      * EVALUATE TRUE (múltiples condiciones)
           EVALUATE TRUE
               WHEN WS-SALDO >= 1000000
                   MOVE "PLATINUM" TO WS-CATEGORIA
               WHEN WS-SALDO >= 500000
                   MOVE "GOLD" TO WS-CATEGORIA
               WHEN WS-SALDO >= 100000
                   MOVE "SILVER" TO WS-CATEGORIA
               WHEN OTHER
                   MOVE "STANDARD" TO WS-CATEGORIA
           END-EVALUATE

      * EVALUATE múltiples sujetos
           EVALUATE WS-TIPO-DOC WS-PAIS
               WHEN "FAC" "MX"
                   PERFORM FACTURA-MEXICO
               WHEN "FAC" "US"
                   PERFORM INVOICE-USA
               WHEN "NC"  ANY
                   PERFORM NOTA-CREDITO
               WHEN OTHER
                   PERFORM ERROR-DOCUMENTO
           END-EVALUATE

      * EVALUATE con rangos
           EVALUATE TRUE ALSO TRUE
               WHEN WS-EDAD < 18 ALSO ANY
                   MOVE "MENOR" TO WS-CLASIFICACION
               WHEN WS-EDAD < 65 ALSO WS-INGRESOS > 50000
                   MOVE "ADULTO-PREMIUM" TO WS-CLASIFICACION
               WHEN WS-EDAD < 65 ALSO OTHER
                   MOVE "ADULTO" TO WS-CLASIFICACION
               WHEN OTHER ALSO ANY
                   MOVE "SENIOR" TO WS-CLASIFICACION
           END-EVALUATE

      * EVALUATE con THRU (rangos)
           EVALUATE WS-CALIFICACION
               WHEN 90 THRU 100
                   MOVE "A" TO WS-LETRA
               WHEN 80 THRU 89
                   MOVE "B" TO WS-LETRA
               WHEN 70 THRU 79
                   MOVE "C" TO WS-LETRA
               WHEN 60 THRU 69
                   MOVE "D" TO WS-LETRA
               WHEN OTHER
                   MOVE "F" TO WS-LETRA
           END-EVALUATE
```

---

## PERFORM - Loops y Llamadas

### PERFORM Simple

```cobol
      * Ejecutar párrafo una vez
           PERFORM INICIALIZAR-VARIABLES

      * Ejecutar sección
           PERFORM 1000-PROCESO THRU 1999-PROCESO-FIN

      * PERFORM inline
           PERFORM
               DISPLAY "LINEA 1"
               DISPLAY "LINEA 2"
           END-PERFORM
```

### PERFORM TIMES

```cobol
      * Repetir N veces
           PERFORM IMPRIMIR-ASTERISCO 80 TIMES

      * Con variable
           PERFORM PROCESAR-ITEM WS-CANTIDAD TIMES

      * Inline con TIMES
           PERFORM 10 TIMES
               ADD 1 TO WS-CONTADOR
               DISPLAY WS-CONTADOR
           END-PERFORM
```

### PERFORM UNTIL

```cobol
      * Loop hasta condición
           PERFORM LEER-REGISTRO
               UNTIL FIN-ARCHIVO

      * Con TEST BEFORE (default)
           PERFORM WITH TEST BEFORE
               UNTIL WS-CONTADOR > 100
               ADD 1 TO WS-CONTADOR
           END-PERFORM

      * Con TEST AFTER (do-while)
           PERFORM WITH TEST AFTER
               UNTIL WS-RESPUESTA = "N"
               DISPLAY "CONTINUAR? (S/N)"
               ACCEPT WS-RESPUESTA
           END-PERFORM

      * Múltiples condiciones
           PERFORM HASTA QUE
               UNTIL FIN-ARCHIVO
               OR WS-ERRORES > 10
               OR WS-REGISTROS > 1000000
```

### PERFORM VARYING (For Loop)

```cobol
      * Loop con contador
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 10
               DISPLAY "ITERACION: " WS-I
           END-PERFORM

      * Countdown
           PERFORM VARYING WS-I FROM 10 BY -1
               UNTIL WS-I < 1
               DISPLAY WS-I
           END-PERFORM

      * Recorrer tabla
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-TOTAL-ELEMENTOS
               DISPLAY WS-ELEMENTO(WS-IDX)
           END-PERFORM

      * Nested loops (matriz)
           PERFORM VARYING WS-FILA FROM 1 BY 1
               UNTIL WS-FILA > 10
               PERFORM VARYING WS-COL FROM 1 BY 1
                   UNTIL WS-COL > 20
                   COMPUTE WS-MATRIZ(WS-FILA, WS-COL) =
                       WS-FILA * WS-COL
               END-PERFORM
           END-PERFORM

      * PERFORM con AFTER (doble loop)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               AFTER WS-J FROM 1 BY 1 UNTIL WS-J > 3
               DISPLAY WS-I ", " WS-J
           END-PERFORM
      *    Produce: 1,1 1,2 1,3 2,1 2,2 2,3 ... 5,3
```

---

## Manejo de Strings

### STRING

```cobol
       01  WS-NOMBRE      PIC X(15) VALUE "JUAN".
       01  WS-APELLIDO    PIC X(15) VALUE "PEREZ".
       01  WS-COMPLETO    PIC X(35).
       01  WS-PUNTERO     PIC 99.

      * Concatenar strings
           MOVE 1 TO WS-PUNTERO
           STRING WS-NOMBRE DELIMITED BY SPACE
                  ", " DELIMITED BY SIZE
                  WS-APELLIDO DELIMITED BY SPACE
               INTO WS-COMPLETO
               WITH POINTER WS-PUNTERO
               ON OVERFLOW
                   DISPLAY "STRING MUY LARGO"
           END-STRING
      *    Resultado: "JUAN, PEREZ"

      * Sin POINTER (desde posición 1)
           STRING "CUENTA: " DELIMITED BY SIZE
                  WS-NUM-CUENTA DELIMITED BY SIZE
               INTO WS-MENSAJE
           END-STRING
```

### UNSTRING

```cobol
       01  WS-LINEA-CSV   PIC X(100)
           VALUE "JUAN,PEREZ,35,INGENIERO".
       01  WS-NOMBRE      PIC X(20).
       01  WS-APELLIDO    PIC X(20).
       01  WS-EDAD        PIC 9(3).
       01  WS-PROFESION   PIC X(20).
       01  WS-CONTADOR    PIC 99.
       01  WS-PUNTERO     PIC 999.

      * Separar por delimitador
           MOVE 1 TO WS-PUNTERO
           UNSTRING WS-LINEA-CSV
               DELIMITED BY ","
               INTO WS-NOMBRE
                    WS-APELLIDO
                    WS-EDAD
                    WS-PROFESION
               WITH POINTER WS-PUNTERO
               TALLYING IN WS-CONTADOR
               ON OVERFLOW
                   DISPLAY "MAS CAMPOS DE LOS ESPERADOS"
           END-UNSTRING

      * Múltiples delimitadores
           UNSTRING WS-DATOS
               DELIMITED BY "," OR ";" OR SPACE
               INTO WS-CAMPO1
                    WS-CAMPO2
                    WS-CAMPO3
           END-UNSTRING

      * Con COUNT (longitud de cada campo)
       01  WS-COUNTS.
           05  WS-CNT1    PIC 99.
           05  WS-CNT2    PIC 99.

           UNSTRING WS-LINEA
               DELIMITED BY "|"
               INTO WS-CAMPO1 COUNT IN WS-CNT1
                    WS-CAMPO2 COUNT IN WS-CNT2
           END-UNSTRING
```

### INSPECT

```cobol
       01  WS-TEXTO       PIC X(50).
       01  WS-CONTADOR    PIC 999.

      * INSPECT TALLYING (contar)
           MOVE "ABRACADABRA" TO WS-TEXTO
           MOVE ZERO TO WS-CONTADOR

           INSPECT WS-TEXTO
               TALLYING WS-CONTADOR FOR ALL "A"
      *    WS-CONTADOR = 5

           INSPECT WS-TEXTO
               TALLYING WS-CONTADOR FOR LEADING SPACES

           INSPECT WS-TEXTO
               TALLYING WS-CONTADOR FOR CHARACTERS
                   BEFORE INITIAL SPACE

      * INSPECT REPLACING (reemplazar)
           INSPECT WS-TEXTO
               REPLACING ALL "A" BY "X"
      *    "XBRXCXDXBRX"

           INSPECT WS-TEXTO
               REPLACING LEADING ZEROS BY SPACES

           INSPECT WS-TEXTO
               REPLACING FIRST "ERROR" BY "AVISO"

      * INSPECT CONVERTING (traducir caracteres)
           INSPECT WS-TEXTO
               CONVERTING "abcdefghijklmnopqrstuvwxyz"
                   TO     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      *    Convierte a mayúsculas

      * Combinado
           INSPECT WS-TEXTO
               TALLYING WS-CONTADOR FOR ALL "X"
               REPLACING ALL "X" BY "Y"
```

### REFERENCE MODIFICATION

```cobol
       01  WS-CADENA      PIC X(20) VALUE "ABCDEFGHIJ".
       01  WS-SUBCADENA   PIC X(5).

      * Sintaxis: identificador(inicio:longitud)
           MOVE WS-CADENA(1:5) TO WS-SUBCADENA
      *    WS-SUBCADENA = "ABCDE"

           MOVE WS-CADENA(3:4) TO WS-SUBCADENA
      *    WS-SUBCADENA = "CDEF "

      * Con variables
           MOVE WS-CADENA(WS-INICIO:WS-LONGITUD)
               TO WS-RESULTADO

      * En condiciones
           IF WS-CADENA(1:2) = "AB"
               DISPLAY "EMPIEZA CON AB"
           END-IF

      * Modificar parte de string
           MOVE "XX" TO WS-CADENA(5:2)
      *    WS-CADENA = "ABCDXXGHIJ"
```

---

## CALL - Subprogramas

### CALL Básico

```cobol
      * Llamada simple
           CALL "SUBPROG1"

      * Con parámetros
           CALL "CALCULADORA" USING
               WS-OPERANDO1
               WS-OPERANDO2
               WS-RESULTADO

      * Manejo de errores
           CALL "PROGRAMA-EXTERNO" USING WS-DATOS
               ON EXCEPTION
                   DISPLAY "ERROR: PROGRAMA NO ENCONTRADO"
                   MOVE 99 TO WS-RETURN-CODE
               NOT ON EXCEPTION
                   MOVE 0 TO WS-RETURN-CODE
           END-CALL
```

### Modos de Paso de Parámetros

```cobol
      * BY REFERENCE (default) - Se pasa la dirección
           CALL "SUBPROG" USING BY REFERENCE WS-DATO
      *    El subprograma puede modificar WS-DATO

      * BY CONTENT - Se pasa una copia
           CALL "SUBPROG" USING BY CONTENT WS-DATO
      *    El subprograma NO puede modificar el original

      * BY VALUE - Pasa el valor (para interoperabilidad C)
           CALL "CFUNC" USING BY VALUE WS-ENTERO

      * Combinados
           CALL "PROCESAR" USING
               BY REFERENCE WS-REGISTRO
               BY CONTENT WS-PARAMETRO-FIJO
               BY REFERENCE WS-RESULTADO
```

### RETURN-CODE

```cobol
      * En el programa llamador
           CALL "VALIDAR-DATOS" USING WS-CLIENTE

           EVALUATE RETURN-CODE
               WHEN 0
                   DISPLAY "VALIDACION EXITOSA"
               WHEN 1
                   DISPLAY "ERROR: DATOS INCOMPLETOS"
               WHEN 2
                   DISPLAY "ERROR: RFC INVALIDO"
               WHEN OTHER
                   DISPLAY "ERROR DESCONOCIDO: " RETURN-CODE
           END-EVALUATE

      * En el subprograma
           IF WS-DATOS-VALIDOS
               MOVE 0 TO RETURN-CODE
           ELSE
               MOVE 1 TO RETURN-CODE
           END-IF
           GOBACK.
```

### Ejemplo Completo: Llamador

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAMA-PRINCIPAL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NUMERO1    PIC S9(9)V99 COMP-3.
       01  WS-NUMERO2    PIC S9(9)V99 COMP-3.
       01  WS-RESULTADO  PIC S9(9)V99 COMP-3.
       01  WS-OPERACION  PIC X(1).
           88  OP-SUMA   VALUE "+".
           88  OP-RESTA  VALUE "-".

       PROCEDURE DIVISION.
           MOVE 100.50 TO WS-NUMERO1
           MOVE 25.25 TO WS-NUMERO2
           SET OP-SUMA TO TRUE

           CALL "CALCULADORA" USING
               WS-NUMERO1
               WS-NUMERO2
               WS-OPERACION
               WS-RESULTADO

           DISPLAY "RESULTADO: " WS-RESULTADO
           STOP RUN.
```

### Ejemplo Completo: Subprograma

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULADORA.

       DATA DIVISION.
       LINKAGE SECTION.
       01  LS-NUM1       PIC S9(9)V99 COMP-3.
       01  LS-NUM2       PIC S9(9)V99 COMP-3.
       01  LS-OPER       PIC X(1).
       01  LS-RESULT     PIC S9(9)V99 COMP-3.

       PROCEDURE DIVISION USING
           LS-NUM1
           LS-NUM2
           LS-OPER
           LS-RESULT.

           EVALUATE LS-OPER
               WHEN "+"
                   ADD LS-NUM1 TO LS-NUM2 GIVING LS-RESULT
               WHEN "-"
                   SUBTRACT LS-NUM2 FROM LS-NUM1
                       GIVING LS-RESULT
               WHEN "*"
                   MULTIPLY LS-NUM1 BY LS-NUM2
                       GIVING LS-RESULT
               WHEN "/"
                   IF LS-NUM2 NOT = ZERO
                       DIVIDE LS-NUM1 BY LS-NUM2
                           GIVING LS-RESULT
                   ELSE
                       MOVE 0 TO RETURN-CODE
                       MOVE ZERO TO LS-RESULT
                   END-IF
               WHEN OTHER
                   MOVE 99 TO RETURN-CODE
           END-EVALUATE

           GOBACK.
```

---

## Tablas y SEARCH

### SEARCH (Búsqueda Lineal)

```cobol
       01  WS-TABLA-CODIGOS.
           05  WS-CODIGO OCCURS 100 TIMES
               INDEXED BY WS-IDX.
               10  COD-CLAVE    PIC X(5).
               10  COD-DESCRIP  PIC X(30).

       01  WS-BUSCAR       PIC X(5).
       01  WS-ENCONTRADO   PIC X(30).
       01  WS-HALLADO      PIC X VALUE "N".
           88  ENCONTRADO  VALUE "S".
           88  NO-ENCONTRADO VALUE "N".

       PROCEDURE DIVISION.
           MOVE "A0123" TO WS-BUSCAR
           SET NO-ENCONTRADO TO TRUE

           SET WS-IDX TO 1
           SEARCH WS-CODIGO
               AT END
                   DISPLAY "CODIGO NO ENCONTRADO"
               WHEN COD-CLAVE(WS-IDX) = WS-BUSCAR
                   MOVE COD-DESCRIP(WS-IDX) TO WS-ENCONTRADO
                   SET ENCONTRADO TO TRUE
           END-SEARCH
```

### SEARCH ALL (Búsqueda Binaria)

```cobol
       01  WS-TABLA-ORDENADA.
           05  WS-ENTRADA OCCURS 1000 TIMES
               ASCENDING KEY IS ENT-CLAVE
               INDEXED BY WS-IX.
               10  ENT-CLAVE    PIC X(10).
               10  ENT-NOMBRE   PIC X(30).
               10  ENT-VALOR    PIC 9(9)V99.

       PROCEDURE DIVISION.
      *    NOTA: La tabla DEBE estar ordenada por ENT-CLAVE

           SEARCH ALL WS-ENTRADA
               AT END
                   DISPLAY "NO EXISTE EN TABLA"
                   MOVE ZERO TO WS-RESULTADO
               WHEN ENT-CLAVE(WS-IX) = WS-CLAVE-BUSCAR
                   MOVE ENT-NOMBRE(WS-IX) TO WS-NOMBRE-RESULT
                   MOVE ENT-VALOR(WS-IX) TO WS-VALOR-RESULT
           END-SEARCH
```

### SET con Índices

```cobol
      * Inicializar índice
           SET WS-IDX TO 1

      * Incrementar
           SET WS-IDX UP BY 1

      * Decrementar
           SET WS-IDX DOWN BY 1

      * Asignar de variable
           SET WS-IDX TO WS-POSICION

      * Asignar a variable
           SET WS-POSICION TO WS-IDX

      * Comparar
           IF WS-IDX > 100
               DISPLAY "FUERA DE RANGO"
           END-IF
```

---

## Otras Instrucciones

### ACCEPT

```cobol
      * Desde consola
           DISPLAY "INGRESE NOMBRE: "
           ACCEPT WS-NOMBRE

      * Datos del sistema
           ACCEPT WS-FECHA FROM DATE         *> YYMMDD
           ACCEPT WS-FECHA FROM DATE YYYYMMDD *> YYYYMMDD
           ACCEPT WS-HORA FROM TIME          *> HHMMSSCC
           ACCEPT WS-DIA FROM DAY            *> YYDDD
           ACCEPT WS-DIA-SEMANA FROM DAY-OF-WEEK  *> 1-7

      * Variables de entorno (IBM)
           ACCEPT WS-VARIABLE FROM ENVIRONMENT "NOMBRE_VAR"
```

### DISPLAY

```cobol
      * Display simple
           DISPLAY "MENSAJE DE TEXTO"

      * Con variables
           DISPLAY "CLIENTE: " WS-NOMBRE " SALDO: " WS-SALDO

      * A dispositivo específico
           DISPLAY "ERROR CRITICO" UPON CONSOLE
           DISPLAY WS-REGISTRO UPON SYSOUT

      * Sin avance de línea
           DISPLAY "PROCESANDO..." WITH NO ADVANCING
```

### INITIALIZE

```cobol
      * Inicializa a valores por defecto
      * Numéricos a ZEROS, Alfanuméricos a SPACES
           INITIALIZE WS-REGISTRO

      * Solo ciertos tipos
           INITIALIZE WS-REGISTRO
               REPLACING NUMERIC DATA BY ZEROS

           INITIALIZE WS-REGISTRO
               REPLACING ALPHANUMERIC DATA BY SPACES

      * Múltiples campos
           INITIALIZE WS-CAMPO1
                      WS-CAMPO2
                      WS-CAMPO3

      * Con valor específico
           INITIALIZE WS-REGISTRO
               REPLACING ALPHANUMERIC DATA BY ALL "*"
```

### CONTINUE y NEXT SENTENCE

```cobol
      * CONTINUE - No hace nada (placeholder)
           IF WS-CONDICION
               CONTINUE
           ELSE
               PERFORM PROCESO-ALTERNATIVO
           END-IF

      * NEXT SENTENCE - Salta al siguiente punto
      * (EVITAR - estilo antiguo, propenso a errores)
           IF WS-CONDICION
               NEXT SENTENCE
           ELSE
               PERFORM PROCESO.
           PERFORM SIGUIENTE-PASO.
```

### EXIT

```cobol
      * EXIT PARAGRAPH
           IF WS-ERROR
               EXIT PARAGRAPH
           END-IF

      * EXIT SECTION
           IF WS-CRITICO
               EXIT SECTION
           END-IF

      * EXIT PERFORM (salir de loop)
           PERFORM UNTIL FIN-ARCHIVO
               READ ARCHIVO INTO WS-REG
                   AT END SET FIN-ARCHIVO TO TRUE
               END-READ

               IF WS-REG-TIPO = "X"
                   EXIT PERFORM
               END-IF

               PERFORM PROCESAR-REG
           END-PERFORM

      * EXIT PERFORM CYCLE (continue)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 100
               IF WS-ELEMENTO(WS-I) = SPACES
                   EXIT PERFORM CYCLE
               END-IF
               PERFORM PROCESAR-ELEMENTO
           END-PERFORM
```

### GOBACK vs STOP RUN

```cobol
      * STOP RUN - Termina TODO el run-unit
      * Usar solo en programa principal
           STOP RUN

      * GOBACK - Retorna al llamador
      * Usar en subprogramas
           GOBACK

      * En programa principal, GOBACK = STOP RUN
      * En subprograma, GOBACK retorna al CALL
```

---

## Patrones Comunes

### Patrón: Lectura de Archivo

```cobol
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO UNTIL FIN-ARCHIVO
           PERFORM 3000-FIN
           STOP RUN.

       1000-INICIO.
           OPEN INPUT ARCHIVO-ENTRADA
           OPEN OUTPUT ARCHIVO-SALIDA
           PERFORM 2100-LEER-REGISTRO.

       2000-PROCESO.
           PERFORM 2200-PROCESAR-REGISTRO
           PERFORM 2100-LEER-REGISTRO.

       2100-LEER-REGISTRO.
           READ ARCHIVO-ENTRADA INTO WS-REGISTRO
               AT END SET FIN-ARCHIVO TO TRUE
           END-READ.

       2200-PROCESAR-REGISTRO.
           ADD 1 TO WS-CONTADOR
           WRITE REG-SALIDA FROM WS-REGISTRO.

       3000-FIN.
           DISPLAY "REGISTROS PROCESADOS: " WS-CONTADOR
           CLOSE ARCHIVO-ENTRADA
           CLOSE ARCHIVO-SALIDA.
```

### Patrón: Validación con Códigos de Error

```cobol
       01  WS-ERRORES.
           05  WS-ERR-COUNT    PIC 99 VALUE 0.
           05  WS-ERR-MSG      PIC X(50) OCCURS 10 TIMES.

       PROCEDURE DIVISION.
           PERFORM VALIDAR-CLIENTE

           IF WS-ERR-COUNT > 0
               PERFORM MOSTRAR-ERRORES
           ELSE
               PERFORM GUARDAR-CLIENTE
           END-IF.

       VALIDAR-CLIENTE.
           INITIALIZE WS-ERRORES

           IF WS-CLI-NOMBRE = SPACES
               ADD 1 TO WS-ERR-COUNT
               MOVE "NOMBRE ES REQUERIDO"
                   TO WS-ERR-MSG(WS-ERR-COUNT)
           END-IF

           IF WS-CLI-RFC NOT NUMERIC
               ADD 1 TO WS-ERR-COUNT
               MOVE "RFC DEBE SER NUMERICO"
                   TO WS-ERR-MSG(WS-ERR-COUNT)
           END-IF

           IF WS-CLI-SALDO < 0
               ADD 1 TO WS-ERR-COUNT
               MOVE "SALDO NO PUEDE SER NEGATIVO"
                   TO WS-ERR-MSG(WS-ERR-COUNT)
           END-IF.

       MOSTRAR-ERRORES.
           DISPLAY "ERRORES ENCONTRADOS:"
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-ERR-COUNT
               DISPLAY "  - " WS-ERR-MSG(WS-I)
           END-PERFORM.
```

---

## Próximos Documentos

- **COBOL_04_FILES.md**: Manejo de archivos (Sequential, Indexed, Relative)
- **COBOL_05_MAINFRAME.md**: JCL, CICS, DB2

---

*"La PROCEDURE DIVISION es donde los datos cobran vida. Cada PERFORM es una historia que se cuenta."*

