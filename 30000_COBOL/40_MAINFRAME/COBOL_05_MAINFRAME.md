# COBOL_05: Entorno Mainframe

> "El mainframe no es una computadora, es un ecosistema. JCL es su shell, COBOL es su alma."

---

## Arquitectura del Mainframe

```
┌─────────────────────────────────────────────────────────────────┐
│                         z/OS                                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   ┌───────────────┐    ┌───────────────┐    ┌───────────────┐  │
│   │     BATCH     │    │     CICS      │    │      IMS      │  │
│   │   (JES2/3)    │    │   (Online)    │    │   (DB/TM)     │  │
│   └───────────────┘    └───────────────┘    └───────────────┘  │
│           │                    │                    │           │
│   ┌───────┴────────────────────┴────────────────────┴───────┐  │
│   │                      COBOL Programs                      │  │
│   └───────┬────────────────────┬────────────────────┬───────┘  │
│           │                    │                    │           │
│   ┌───────────────┐    ┌───────────────┐    ┌───────────────┐  │
│   │     VSAM      │    │      DB2      │    │    IMS DB     │  │
│   │    Files      │    │   (SQL/RDB)   │    │  (Hierarchical│  │
│   └───────────────┘    └───────────────┘    └───────────────┘  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## JCL (Job Control Language)

### Estructura Básica de un Job

```jcl
//JOBNAME  JOB (ACCOUNT),'DESCRIPCION',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,TIME=(5,0)
//*
//* COMENTARIOS EMPIEZAN CON //*
//*
//STEP1    EXEC PGM=PROGRAMA1
//STEPLIB  DD DSN=LIB.LOAD,DISP=SHR
//INPUT    DD DSN=PROD.DATA.INPUT,DISP=SHR
//OUTPUT   DD DSN=PROD.DATA.OUTPUT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=0)
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//
```

### Sentencias Principales

#### JOB Statement

```jcl
//MYJOB    JOB (ACCOUNTING),'PROGRAMMER NAME',
//             CLASS=A,              CLASE DE JOB (A-Z)
//             MSGCLASS=X,           CLASE DE MENSAJES
//             MSGLEVEL=(1,1),       NIVEL DE MENSAJES
//             NOTIFY=&SYSUID,       NOTIFICAR AL USUARIO
//             TIME=(5,0),           TIEMPO MAX (MIN,SEG)
//             REGION=0M,            MEMORIA (0=ILIMITADA)
//             TYPRUN=SCAN           SCAN/HOLD/COPY
```

#### EXEC Statement

```jcl
//* Ejecutar programa
//STEP1    EXEC PGM=MIPROG

//* Ejecutar procedimiento catalogado
//STEP2    EXEC PROC=COMPLINK

//* Con parámetros
//STEP3    EXEC PGM=MIPROG,PARM='PARAM1,PARAM2'

//* Con condiciones
//STEP4    EXEC PGM=MIPROG,COND=(0,NE,STEP1)

//* Control de tiempo y región
//STEP5    EXEC PGM=MIPROG,TIME=(2,30),REGION=256M
```

#### DD Statement (Data Definition)

```jcl
//* Archivo en disco
//INPUT    DD DSN=PROD.DATA.FILE,DISP=SHR

//* Crear archivo nuevo
//OUTPUT   DD DSN=PROD.NEW.FILE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(100,50),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=27800)

//* Archivo temporal
//TEMP     DD DSN=&&TEMPFILE,
//            DISP=(NEW,PASS),
//            SPACE=(TRK,(50,10))

//* SYSOUT (salida a spool)
//SYSPRINT DD SYSOUT=*

//* Dummy (archivo vacío)
//EMPTY    DD DUMMY

//* Concatenación
//INPUT    DD DSN=FILE1,DISP=SHR
//         DD DSN=FILE2,DISP=SHR
//         DD DSN=FILE3,DISP=SHR

//* In-stream data
//SYSIN    DD *
DATO1
DATO2
DATO3
/*
```

### DISP (Disposition)

```jcl
DISP=(status,normal_end,abnormal_end)

Status:
  NEW     - Crear archivo nuevo
  OLD     - Exclusivo (solo este job)
  SHR     - Compartido
  MOD     - Agregar al final

Normal/Abnormal:
  DELETE  - Borrar
  KEEP    - Mantener
  PASS    - Pasar al siguiente step
  CATLG   - Catalogar
  UNCATLG - Descatalogar

Ejemplos:
  DISP=SHR                      Solo lectura compartida
  DISP=(NEW,CATLG,DELETE)       Crear, catalogar o borrar si falla
  DISP=(MOD,CATLG)              Append al final
  DISP=(OLD,KEEP)               Exclusivo, mantener
```

### SPACE (Asignación de Espacio)

```jcl
SPACE=(unit,(primary,secondary,directory),RLSE)

Units:
  TRK     - Tracks (56,664 bytes)
  CYL     - Cylinders (849,960 bytes)
  bytes   - SPACE=(200,(100,50))
  blocks  - Con BLKSIZE

Ejemplos:
  SPACE=(CYL,(10,5))            10 cyl primarios, 5 secundarios
  SPACE=(TRK,(100,50),RLSE)     Liberar espacio no usado
  SPACE=(CYL,(5,2,10))          Con 10 directory blocks (PDS)
```

### DCB (Data Control Block)

```jcl
DCB=(RECFM=FB,LRECL=100,BLKSIZE=27800)

RECFM (Record Format):
  F   - Fixed length
  FB  - Fixed Blocked
  V   - Variable length
  VB  - Variable Blocked
  FBA - Fixed Blocked ASA (print control)
  U   - Undefined

LRECL - Logical Record Length
BLKSIZE - Block Size (0 = system optimal)
```

### Condiciones (COND)

```jcl
//* COND code: Si condición es TRUE, NO ejecutar

//* No ejecutar si STEP1 terminó con RC > 0
//STEP2    EXEC PGM=PROG2,COND=(0,LT,STEP1)

//* No ejecutar si RC de cualquier step previo > 4
//STEP3    EXEC PGM=PROG3,COND=(4,LT)

//* Múltiples condiciones (OR)
//STEP4    EXEC PGM=PROG4,COND=((0,NE,STEP1),(4,LT,STEP2))

//* Operadores: GT, GE, LT, LE, EQ, NE
```

### IF/THEN/ELSE (Moderno)

```jcl
//STEP1    EXEC PGM=PROG1
//*
//         IF (STEP1.RC = 0) THEN
//STEP2    EXEC PGM=PROG2
//         ENDIF
//*
//         IF (STEP1.RC > 4) THEN
//ERROR    EXEC PGM=ERRORPGM
//         ELSE
//OK       EXEC PGM=OKPGM
//         ENDIF
//*
//         IF (STEP1.ABEND) THEN
//ABEND    EXEC PGM=ABENDPGM
//         ENDIF
```

### Procedimientos

```jcl
//* Procedimiento inline
//MYPROC   PROC
//STEP1    EXEC PGM=PROG1
//INPUT    DD DSN=&INFILE,DISP=SHR
//OUTPUT   DD DSN=&OUTFILE,DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5))
//         PEND
//*
//* Invocar procedimiento
//RUNJOB   EXEC MYPROC,INFILE='PROD.INPUT',
//              OUTFILE='PROD.OUTPUT'
```

### Variables Simbólicas

```jcl
//         SET ENV=PROD
//         SET DATE=&SYSDATE
//*
//STEP1    EXEC PGM=MYPROG
//INPUT    DD DSN=&ENV..DATA.FILE,DISP=SHR
//OUTPUT   DD DSN=&ENV..OUTPUT.&DATE,
//            DISP=(NEW,CATLG,DELETE)
```

---

## VSAM (Virtual Storage Access Method)

### Tipos de VSAM

| Tipo | Descripción | Uso |
|------|-------------|-----|
| KSDS | Key Sequenced | Acceso por clave (más común) |
| ESDS | Entry Sequenced | Secuencial (como plano) |
| RRDS | Relative Record | Por número de registro |
| LDS | Linear Data Set | Datos sin estructura |

### IDCAMS - Definir VSAM

```jcl
//DEFVSAM  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER (                               -
         NAME(PROD.CLIENTES.KSDS)                -
         INDEXED                                  -
         RECORDS(100000 50000)                   -
         RECORDSIZE(200 200)                     -
         KEYS(10 0)                              -
         FREESPACE(20 10)                        -
         SHAREOPTIONS(2 3)                       -
         )                                        -
         DATA (                                   -
              NAME(PROD.CLIENTES.KSDS.DATA)      -
              CONTROLINTERVALSIZE(4096)          -
              )                                   -
         INDEX (                                  -
              NAME(PROD.CLIENTES.KSDS.INDEX)     -
              CONTROLINTERVALSIZE(2048)          -
              )
/*
```

### IDCAMS - Otros Comandos

```jcl
//VSAM     EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  /* BORRAR CLUSTER */
  DELETE PROD.CLIENTES.KSDS CLUSTER PURGE

  /* LISTAR CATÁLOGO */
  LISTCAT ENTRIES(PROD.CLIENTES.*) ALL

  /* COPIAR VSAM */
  REPRO INFILE(INPUT) OUTFILE(OUTPUT)

  /* DEFINIR ALTERNATE INDEX */
  DEFINE AIX (                                   -
         NAME(PROD.CLIENTES.AIX.RFC)             -
         RELATE(PROD.CLIENTES.KSDS)              -
         KEYS(13 10)                             -
         NONUNIQUEKEY                            -
         UPGRADE                                 -
         )

  /* CONSTRUIR PATH */
  DEFINE PATH (                                  -
         NAME(PROD.CLIENTES.PATH.RFC)            -
         PATHENTRY(PROD.CLIENTES.AIX.RFC)        -
         )

  /* CONSTRUIR AIX */
  BLDINDEX INDATASET(PROD.CLIENTES.KSDS)        -
           OUTDATASET(PROD.CLIENTES.AIX.RFC)
/*
```

### JCL para VSAM

```jcl
//STEP1    EXEC PGM=MIPROG
//CLIENTES DD DSN=PROD.CLIENTES.KSDS,DISP=SHR
//CLIPATH  DD DSN=PROD.CLIENTES.PATH.RFC,DISP=SHR
```

---

## DB2 - SQL Embebido en COBOL

### Estructura de Programa DB2

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB2PROG.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *-------------------------------------------------
      * SQLCA - SQL Communication Area
      *-------------------------------------------------
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *-------------------------------------------------
      * DCLGEN - Declaración de tabla generada
      *-------------------------------------------------
           EXEC SQL
               INCLUDE DCLCLIENTE
           END-EXEC.

      * Variables host para comunicación con DB2
       01  WS-CLIENTE-ID         PIC 9(10).
       01  WS-CLIENTE-NOMBRE     PIC X(40).
       01  WS-CLIENTE-SALDO      PIC S9(11)V99 COMP-3.

      * Indicadores de NULL
       01  WS-IND-SALDO          PIC S9(4) COMP.

       PROCEDURE DIVISION.
```

### DCLGEN (Declaration Generator)

```cobol
      * Generado por DCLGEN para tabla CLIENTE
           EXEC SQL DECLARE CLIENTE TABLE
               ( CLIENTE_ID        DECIMAL(10, 0)  NOT NULL,
                 NOMBRE            CHAR(40)        NOT NULL,
                 RFC               CHAR(13),
                 SALDO             DECIMAL(13, 2),
                 FECHA_ALTA        DATE,
                 ESTADO            CHAR(1)         NOT NULL
               )
           END-EXEC.

      * Variables COBOL correspondientes
       01  DCLCLIENTE.
           05  CLIENTE-ID         PIC S9(10) COMP-3.
           05  NOMBRE             PIC X(40).
           05  RFC                PIC X(13).
           05  SALDO              PIC S9(11)V99 COMP-3.
           05  FECHA-ALTA         PIC X(10).
           05  ESTADO             PIC X(1).

      * Indicadores de NULL
       01  CLIENTE-IND.
           05  CLIENTE-ID-IND     PIC S9(4) COMP.
           05  NOMBRE-IND         PIC S9(4) COMP.
           05  RFC-IND            PIC S9(4) COMP.
           05  SALDO-IND          PIC S9(4) COMP.
           05  FECHA-ALTA-IND     PIC S9(4) COMP.
           05  ESTADO-IND         PIC S9(4) COMP.
```

### SELECT

```cobol
      *-------------------------------------------------
      * SELECT de un registro
      *-------------------------------------------------
           MOVE 12345 TO WS-CLIENTE-ID

           EXEC SQL
               SELECT NOMBRE, SALDO
               INTO :WS-CLIENTE-NOMBRE,
                    :WS-CLIENTE-SALDO :WS-IND-SALDO
               FROM CLIENTE
               WHERE CLIENTE_ID = :WS-CLIENTE-ID
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   IF WS-IND-SALDO < 0
                       DISPLAY "SALDO ES NULL"
                   ELSE
                       DISPLAY "SALDO: " WS-CLIENTE-SALDO
                   END-IF
               WHEN 100
                   DISPLAY "CLIENTE NO ENCONTRADO"
               WHEN OTHER
                   DISPLAY "ERROR DB2: " SQLCODE
                   PERFORM MANEJAR-ERROR-DB2
           END-EVALUATE
```

### CURSOR (SELECT múltiples filas)

```cobol
      *-------------------------------------------------
      * Declarar CURSOR
      *-------------------------------------------------
           EXEC SQL
               DECLARE CURSOR-CLIENTES CURSOR FOR
               SELECT CLIENTE_ID, NOMBRE, SALDO
               FROM CLIENTE
               WHERE ESTADO = 'A'
               ORDER BY NOMBRE
           END-EXEC.

      *-------------------------------------------------
      * Usar CURSOR
      *-------------------------------------------------
       PROCESAR-CLIENTES.
      * Abrir cursor
           EXEC SQL
               OPEN CURSOR-CLIENTES
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY "ERROR ABRIENDO CURSOR"
               PERFORM MANEJAR-ERROR-DB2
           END-IF.

      * Leer registros
           PERFORM UNTIL SQLCODE = 100
               EXEC SQL
                   FETCH CURSOR-CLIENTES
                   INTO :CLIENTE-ID,
                        :NOMBRE,
                        :SALDO :SALDO-IND
               END-EXEC

               IF SQLCODE = 0
                   PERFORM PROCESAR-REGISTRO
               END-IF
           END-PERFORM.

      * Cerrar cursor
           EXEC SQL
               CLOSE CURSOR-CLIENTES
           END-EXEC.
```

### INSERT

```cobol
      *-------------------------------------------------
      * INSERT simple
      *-------------------------------------------------
           MOVE 99999 TO CLIENTE-ID
           MOVE "NUEVO CLIENTE" TO NOMBRE
           MOVE "RFC1234567890" TO RFC
           MOVE 1000.00 TO SALDO
           MOVE FUNCTION CURRENT-DATE(1:10) TO FECHA-ALTA
           MOVE "A" TO ESTADO

           EXEC SQL
               INSERT INTO CLIENTE
               (CLIENTE_ID, NOMBRE, RFC, SALDO, FECHA_ALTA, ESTADO)
               VALUES
               (:CLIENTE-ID, :NOMBRE, :RFC, :SALDO,
                :FECHA-ALTA, :ESTADO)
           END-EXEC

           IF SQLCODE = 0
               DISPLAY "CLIENTE INSERTADO"
           ELSE
               IF SQLCODE = -803
                   DISPLAY "ERROR: CLAVE DUPLICADA"
               ELSE
                   DISPLAY "ERROR INSERT: " SQLCODE
               END-IF
           END-IF
```

### UPDATE

```cobol
      *-------------------------------------------------
      * UPDATE por clave
      *-------------------------------------------------
           MOVE 12345 TO WS-CLIENTE-ID
           MOVE 5000.00 TO WS-NUEVO-SALDO

           EXEC SQL
               UPDATE CLIENTE
               SET SALDO = :WS-NUEVO-SALDO,
                   FECHA_ULT_MOV = CURRENT DATE
               WHERE CLIENTE_ID = :WS-CLIENTE-ID
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   DISPLAY "FILAS ACTUALIZADAS: " SQLERRD(3)
               WHEN 100
                   DISPLAY "CLIENTE NO ENCONTRADO"
               WHEN OTHER
                   DISPLAY "ERROR UPDATE: " SQLCODE
           END-EVALUATE

      *-------------------------------------------------
      * UPDATE con CURSOR (FOR UPDATE)
      *-------------------------------------------------
           EXEC SQL
               DECLARE CURSOR-UPD CURSOR FOR
               SELECT CLIENTE_ID, SALDO
               FROM CLIENTE
               WHERE ESTADO = 'A'
               FOR UPDATE OF SALDO
           END-EXEC.

           EXEC SQL OPEN CURSOR-UPD END-EXEC.

           PERFORM UNTIL SQLCODE = 100
               EXEC SQL
                   FETCH CURSOR-UPD
                   INTO :CLIENTE-ID, :SALDO
               END-EXEC

               IF SQLCODE = 0
                   COMPUTE WS-NUEVO-SALDO = SALDO * 1.05
                   EXEC SQL
                       UPDATE CLIENTE
                       SET SALDO = :WS-NUEVO-SALDO
                       WHERE CURRENT OF CURSOR-UPD
                   END-EXEC
               END-IF
           END-PERFORM.

           EXEC SQL CLOSE CURSOR-UPD END-EXEC.
```

### DELETE

```cobol
      *-------------------------------------------------
      * DELETE por condición
      *-------------------------------------------------
           EXEC SQL
               DELETE FROM CLIENTE
               WHERE ESTADO = 'I'
               AND FECHA_ULT_MOV < CURRENT DATE - 2 YEARS
           END-EXEC

           DISPLAY "CLIENTES ELIMINADOS: " SQLERRD(3)
```

### COMMIT y ROLLBACK

```cobol
      *-------------------------------------------------
      * Control de transacciones
      *-------------------------------------------------

      * COMMIT - confirmar cambios
           EXEC SQL
               COMMIT
           END-EXEC

      * ROLLBACK - deshacer cambios
           EXEC SQL
               ROLLBACK
           END-EXEC

      * Patrón de transacción
       PROCESAR-TRANSACCION.
           PERFORM ACTUALIZAR-TABLA-1
           IF SQLCODE NOT = 0
               EXEC SQL ROLLBACK END-EXEC
               GO TO TRANS-ERROR
           END-IF

           PERFORM ACTUALIZAR-TABLA-2
           IF SQLCODE NOT = 0
               EXEC SQL ROLLBACK END-EXEC
               GO TO TRANS-ERROR
           END-IF

           EXEC SQL COMMIT END-EXEC
           GO TO TRANS-EXIT.

       TRANS-ERROR.
           DISPLAY "ERROR EN TRANSACCION".

       TRANS-EXIT.
           EXIT.
```

### SQLCA y Manejo de Errores

```cobol
       01  SQLCA.
           05  SQLCAID      PIC X(8).
           05  SQLCABC      PIC S9(9) COMP-4.
           05  SQLCODE      PIC S9(9) COMP-4.
           05  SQLERRM.
               10 SQLERRML  PIC S9(4) COMP-4.
               10 SQLERRMC  PIC X(70).
           05  SQLERRP      PIC X(8).
           05  SQLERRD      OCCURS 6 TIMES PIC S9(9) COMP-4.
           05  SQLWARN.
               10 SQLWARN0  PIC X(1).
               10 SQLWARN1  PIC X(1).
               10 SQLWARN2  PIC X(1).
               10 SQLWARN3  PIC X(1).
               10 SQLWARN4  PIC X(1).
               10 SQLWARN5  PIC X(1).
               10 SQLWARN6  PIC X(1).
               10 SQLWARN7  PIC X(1).
           05  SQLSTATE     PIC X(5).

      * SQLCODE comunes:
      *  0     = Éxito
      *  100   = No encontrado / Fin de cursor
      * -803   = Clave duplicada
      * -811   = SELECT devolvió múltiples filas
      * -818   = Timestamp mismatch (recompilar)
      * -904   = Recurso no disponible
      * -911   = Deadlock o timeout
      * -913   = Deadlock

       MANEJAR-ERROR-DB2.
           DISPLAY "========================================="
           DISPLAY "ERROR DB2"
           DISPLAY "SQLCODE:  " SQLCODE
           DISPLAY "SQLSTATE: " SQLSTATE
           DISPLAY "MENSAJE:  " SQLERRMC
           DISPLAY "========================================="

           IF SQLCODE = -911 OR SQLCODE = -913
               DISPLAY "DEADLOCK DETECTADO - REINTENTANDO"
               EXEC SQL ROLLBACK END-EXEC
               ADD 1 TO WS-REINTENTOS
               IF WS-REINTENTOS < 3
                   GO TO REINTENTAR-TRANSACCION
               END-IF
           END-IF.
```

### JCL para DB2

```jcl
//DB2JOB   JOB (ACCT),'DB2 PROGRAM',CLASS=A,MSGCLASS=X
//*
//* PRECOMPILE COBOL
//PRECOMP  EXEC PGM=DSNHPC,
//             PARM='HOST(COB2),APOST,APOSTSQL'
//STEPLIB  DD DSN=DSN.V12R1.SDSNLOAD,DISP=SHR
//DBRMLIB  DD DSN=MY.DBRM.LIB(MYPROG),DISP=SHR
//SYSCIN   DD DSN=MY.COBOL.LIB(MYPROG),DISP=SHR
//SYSLIB   DD DSN=MY.COPY.LIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSTERM  DD SYSOUT=*
//SYSUT1   DD UNIT=SYSDA,SPACE=(4000,(20,20))
//SYSUT2   DD UNIT=SYSDA,SPACE=(4000,(20,20))
//SYSCOUT  DD DSN=&&COBOL,DISP=(,PASS),UNIT=SYSDA,
//            SPACE=(4000,(100,100))
//*
//* COMPILE COBOL
//COMPILE  EXEC PGM=IGYCRCTL,PARM='LIB,APOST'
//SYSLIB   DD DSN=MY.COPY.LIB,DISP=SHR
//SYSLIN   DD DSN=&&OBJ,DISP=(,PASS),UNIT=SYSDA,
//            SPACE=(3000,(50,50))
//SYSIN    DD DSN=&&COBOL,DISP=(OLD,DELETE)
//SYSPRINT DD SYSOUT=*
//*
//* BIND
//BIND     EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD DSN=DSN.V12R1.SDSNLOAD,DISP=SHR
//DBRMLIB  DD DSN=MY.DBRM.LIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
 DSN SYSTEM(DSN1)
 BIND PLAN(MYPLAN) MEMBER(MYPROG) ACTION(REP) VALIDATE(BIND)
 END
/*
//*
//* LINK
//LINK     EXEC PGM=IEWL,PARM='LIST,MAP,XREF'
//SYSLIB   DD DSN=CEE.SCEELKED,DISP=SHR
//         DD DSN=DSN.V12R1.SDSNLOAD,DISP=SHR
//SYSLMOD  DD DSN=MY.LOAD.LIB(MYPROG),DISP=SHR
//SYSLIN   DD DSN=&&OBJ,DISP=(OLD,DELETE)
//         DD *
 INCLUDE SYSLIB(DSNELI)
 NAME MYPROG(R)
/*
//SYSPRINT DD SYSOUT=*
```

---

## CICS (Customer Information Control System)

### Estructura de Programa CICS

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSPROG.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * DFHCOMMAREA - Communication Area
       01  WS-COMMAREA.
           05  WS-ESTADO          PIC X(1).
           05  WS-CLIENTE-ID      PIC 9(10).
           05  WS-MENSAJE         PIC X(50).

      * DFHEIBLK - Execute Interface Block
       01  DFHEIBLK.
           05  EIBTIME            PIC S9(7) COMP-3.
           05  EIBDATE            PIC S9(7) COMP-3.
           05  EIBTRNID           PIC X(4).
           05  EIBTASKN           PIC S9(7) COMP-3.
           05  EIBTRMID           PIC X(4).
           05  EIBCPOSN           PIC S9(4) COMP.
           05  EIBCALEN           PIC S9(4) COMP.
           05  EIBAID             PIC X(1).
           05  EIBFN              PIC X(2).
           05  EIBRCODE           PIC X(6).
           05  EIBDS              PIC X(8).
           05  EIBREQID           PIC X(8).
           05  EIBRSRCE           PIC X(8).
           05  EIBSYNC            PIC X(1).
           05  EIBFREE            PIC X(1).
           05  EIBRECV            PIC X(1).
           05  EIBSEND            PIC X(1).
           05  EIBATT             PIC X(1).
           05  EIBEOC             PIC X(1).
           05  EIBFMH             PIC X(1).
           05  EIBCOMPL           PIC X(1).
           05  EIBSIG             PIC X(1).
           05  EIBCONF            PIC X(1).
           05  EIBERR             PIC X(1).
           05  EIBERRCD           PIC X(4).
           05  EIBSYNRB           PIC X(1).
           05  EIBNODAT           PIC X(1).
           05  EIBRESP            PIC S9(8) COMP.
           05  EIBRESP2           PIC S9(8) COMP.
           05  EIBRLDBK           PIC X(1).

       LINKAGE SECTION.
       01  DFHCOMMAREA            PIC X(100).

       PROCEDURE DIVISION.
           EXEC CICS HANDLE ABEND
               LABEL(ABEND-HANDLER)
           END-EXEC.

           EVALUATE TRUE
               WHEN EIBCALEN = 0
                   PERFORM PRIMERA-VEZ
               WHEN OTHER
                   MOVE DFHCOMMAREA TO WS-COMMAREA
                   PERFORM PROCESAR-ENTRADA
           END-EVALUATE.

           EXEC CICS RETURN
               TRANSID('MYTR')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.
```

### Comandos CICS Básicos

```cobol
      *-------------------------------------------------
      * ENVIAR/RECIBIR MAPAS (BMS)
      *-------------------------------------------------

      * Enviar mapa (pantalla)
           EXEC CICS SEND
               MAP('MAPA001')
               MAPSET('MAPSET1')
               FROM(WS-MAPA)
               ERASE
               CURSOR
           END-EXEC.

      * Recibir mapa (entrada de usuario)
           EXEC CICS RECEIVE
               MAP('MAPA001')
               MAPSET('MAPSET1')
               INTO(WS-MAPA)
           END-EXEC.

      *-------------------------------------------------
      * LEER TECLAS DE FUNCIÓN
      *-------------------------------------------------
           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM PROCESAR-ENTER
               WHEN DFHPF1
                   PERFORM MOSTRAR-AYUDA
               WHEN DFHPF3
                   PERFORM SALIR-PROGRAMA
               WHEN DFHPF7
                   PERFORM PAGINA-ANTERIOR
               WHEN DFHPF8
                   PERFORM PAGINA-SIGUIENTE
               WHEN DFHCLEAR
                   PERFORM LIMPIAR-PANTALLA
           END-EVALUATE

      *-------------------------------------------------
      * ACCESO A ARCHIVOS VSAM
      *-------------------------------------------------

      * READ
           EXEC CICS READ
               FILE('CLIENTES')
               INTO(WS-REG-CLIENTE)
               RIDFLD(WS-CLIENTE-ID)
               LENGTH(WS-LENGTH)
               RESP(WS-RESP)
               RESP2(WS-RESP2)
           END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
               PERFORM MOSTRAR-CLIENTE
           ELSE
               IF WS-RESP = DFHRESP(NOTFND)
                   MOVE "CLIENTE NO ENCONTRADO" TO WS-MENSAJE
               END-IF
           END-IF.

      * WRITE
           EXEC CICS WRITE
               FILE('CLIENTES')
               FROM(WS-REG-CLIENTE)
               RIDFLD(WS-CLIENTE-ID)
               LENGTH(WS-LENGTH)
               RESP(WS-RESP)
           END-EXEC.

      * REWRITE (requiere READ UPDATE previo)
           EXEC CICS READ
               FILE('CLIENTES')
               INTO(WS-REG-CLIENTE)
               RIDFLD(WS-CLIENTE-ID)
               UPDATE
           END-EXEC.

           MOVE WS-NUEVO-SALDO TO CLI-SALDO

           EXEC CICS REWRITE
               FILE('CLIENTES')
               FROM(WS-REG-CLIENTE)
           END-EXEC.

      * DELETE
           EXEC CICS DELETE
               FILE('CLIENTES')
               RIDFLD(WS-CLIENTE-ID)
               RESP(WS-RESP)
           END-EXEC.

      * BROWSE (lectura secuencial)
           EXEC CICS STARTBR
               FILE('CLIENTES')
               RIDFLD(WS-INICIO)
               GTEQ
           END-EXEC.

           PERFORM UNTIL WS-FIN-BROWSE
               EXEC CICS READNEXT
                   FILE('CLIENTES')
                   INTO(WS-REG-CLIENTE)
                   RIDFLD(WS-CLIENTE-ID)
                   RESP(WS-RESP)
               END-EXEC

               IF WS-RESP = DFHRESP(ENDFILE)
                   SET WS-FIN-BROWSE TO TRUE
               ELSE
                   PERFORM PROCESAR-CLIENTE
               END-IF
           END-PERFORM.

           EXEC CICS ENDBR FILE('CLIENTES') END-EXEC.

      *-------------------------------------------------
      * CONTROL DE PROGRAMA
      *-------------------------------------------------

      * Llamar otro programa
           EXEC CICS LINK
               PROGRAM('SUBPROG1')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      * Transferir control (no retorna)
           EXEC CICS XCTL
               PROGRAM('OTHERPROG')
               COMMAREA(WS-COMMAREA)
           END-EXEC.

      * Retornar a CICS
           EXEC CICS RETURN
               TRANSID('NEXT')
               COMMAREA(WS-COMMAREA)
           END-EXEC.

      * Terminar sin transacción siguiente
           EXEC CICS RETURN END-EXEC.
```

### BMS (Basic Mapping Support)

```
MAPSET1  DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,             X
               TIOAPFX=YES,CTRL=FREEKB

MAPA001  DFHMDI SIZE=(24,80),LINE=1,COLUMN=1

         DFHMDF POS=(1,1),LENGTH=30,                              X
               ATTRB=(ASKIP,BRT),                                  X
               INITIAL='=== CONSULTA DE CLIENTES ==='

         DFHMDF POS=(3,1),LENGTH=10,                              X
               ATTRB=ASKIP,                                        X
               INITIAL='CLIENTE: '

CLIPID   DFHMDF POS=(3,11),LENGTH=10,                             X
               ATTRB=(UNPROT,IC,FSET)

         DFHMDF POS=(3,22),LENGTH=1,ATTRB=ASKIP

         DFHMDF POS=(5,1),LENGTH=10,                              X
               ATTRB=ASKIP,                                        X
               INITIAL='NOMBRE: '

CLINOMB  DFHMDF POS=(5,11),LENGTH=40,                             X
               ATTRB=(PROT,BRT)

         DFHMDF POS=(7,1),LENGTH=10,                              X
               ATTRB=ASKIP,                                        X
               INITIAL='SALDO: '

CLISALD  DFHMDF POS=(7,11),LENGTH=15,                             X
               ATTRB=(PROT,BRT),                                   X
               PICOUT='$$$,$$$,$$9.99'

MENSAJE  DFHMDF POS=(23,1),LENGTH=78,                             X
               ATTRB=(ASKIP,BRT)

         DFHMDF POS=(24,1),LENGTH=50,                             X
               ATTRB=ASKIP,                                        X
               INITIAL='PF1=AYUDA  PF3=SALIR  ENTER=CONSULTAR'

         DFHMSD TYPE=FINAL
         END
```

---

## Utilities Comunes

### IEBGENER - Copiar archivos

```jcl
//COPY     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=INPUT.FILE,DISP=SHR
//SYSUT2   DD DSN=OUTPUT.FILE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5)),
//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=27800)
//SYSIN    DD DUMMY
```

### IEBCOPY - Copiar PDS

```jcl
//COPYLIB  EXEC PGM=IEBCOPY
//SYSPRINT DD SYSOUT=*
//INLIB    DD DSN=SOURCE.LOAD.LIB,DISP=SHR
//OUTLIB   DD DSN=TARGET.LOAD.LIB,DISP=SHR
//SYSIN    DD *
  COPY INDD=INLIB,OUTDD=OUTLIB
  SELECT MEMBER=(PROG1,PROG2,PROG3)
/*
```

### SORT

```jcl
//SORT     EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=INPUT.FILE,DISP=SHR
//SORTOUT  DD DSN=OUTPUT.FILE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5))
//SYSIN    DD *
  SORT FIELDS=(1,10,CH,A,11,5,ZD,D)
  INCLUDE COND=(21,1,CH,EQ,C'A')
  OUTREC FIELDS=(1,10,21,50)
/*
```

### ICETOOL

```jcl
//ICETOOL  EXEC PGM=ICETOOL
//TOOLMSG  DD SYSOUT=*
//DFSMSG   DD SYSOUT=*
//IN1      DD DSN=INPUT.FILE,DISP=SHR
//OUT1     DD DSN=OUTPUT.FILE,DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5))
//TOOLIN   DD *
  SORT FROM(IN1) TO(OUT1) USING(CTL1)
  COUNT FROM(IN1) TEXT('TOTAL REGISTROS:')
/*
//CTL1CNTL DD *
  SORT FIELDS=(1,10,CH,A)
  SUM FIELDS=(21,9,ZD)
/*
```

---

## Programa Batch Completo

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCHPGM.
       AUTHOR. ARCHAEON.
      *-------------------------------------------------
      * PROGRAMA: BATCHPGM
      * FUNCION:  Procesar movimientos y actualizar saldos
      * FECHA:    2024-12-31
      *-------------------------------------------------

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MOVIMIENTOS
               ASSIGN TO MOVTOS
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FS-MOV.

           SELECT CLIENTES
               ASSIGN TO CLIENTES
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CLI-NUMERO
               FILE STATUS IS WS-FS-CLI.

           SELECT ERRORES
               ASSIGN TO ERRORES
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FS-ERR.

           SELECT REPORTE
               ASSIGN TO REPORTE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FS-REP.

       DATA DIVISION.
       FILE SECTION.

       FD  MOVIMIENTOS.
       01  REG-MOV.
           05  MOV-CLIENTE        PIC 9(10).
           05  MOV-TIPO           PIC X(1).
               88  MOV-DEPOSITO   VALUE "D".
               88  MOV-RETIRO     VALUE "R".
           05  MOV-MONTO          PIC S9(11)V99 COMP-3.
           05  MOV-FECHA          PIC 9(8).
           05  FILLER             PIC X(50).

       FD  CLIENTES.
       01  REG-CLI.
           05  CLI-NUMERO         PIC 9(10).
           05  CLI-NOMBRE         PIC X(40).
           05  CLI-SALDO          PIC S9(11)V99 COMP-3.
           05  CLI-FECHA-ULT      PIC 9(8).
           05  FILLER             PIC X(50).

       FD  ERRORES.
       01  REG-ERROR              PIC X(150).

       FD  REPORTE.
       01  REG-REPORTE            PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-FS-MOV          PIC X(2).
           05  WS-FS-CLI          PIC X(2).
           05  WS-FS-ERR          PIC X(2).
           05  WS-FS-REP          PIC X(2).

       01  WS-FLAGS.
           05  WS-FIN-MOV         PIC X VALUE "N".
               88  FIN-MOVIMIENTOS VALUE "S".

       01  WS-CONTADORES.
           05  WS-LEIDOS          PIC 9(9) VALUE 0.
           05  WS-DEPOSITOS       PIC 9(9) VALUE 0.
           05  WS-RETIROS         PIC 9(9) VALUE 0.
           05  WS-ERRORES         PIC 9(9) VALUE 0.

       01  WS-TOTALES.
           05  WS-TOT-DEPOSITOS   PIC S9(13)V99 VALUE 0.
           05  WS-TOT-RETIROS     PIC S9(13)V99 VALUE 0.

       01  WS-FECHA-HOY           PIC 9(8).

       01  WS-LINEA-ERROR.
           05  FILLER             PIC X(10) VALUE "CLIENTE: ".
           05  WS-ERR-CLI         PIC 9(10).
           05  FILLER             PIC X(10) VALUE " ERROR: ".
           05  WS-ERR-MSG         PIC X(50).
           05  FILLER             PIC X(10) VALUE " MONTO: ".
           05  WS-ERR-MONTO       PIC Z,ZZZ,ZZZ,ZZ9.99-.

       01  WS-ENCABEZADO-1.
           05  FILLER             PIC X(50)
               VALUE "========== REPORTE DE MOVIMIENTOS ==========".
           05  FILLER             PIC X(30) VALUE SPACES.
           05  FILLER             PIC X(7) VALUE "FECHA: ".
           05  WS-REP-FECHA       PIC 9999/99/99.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO UNTIL FIN-MOVIMIENTOS
           PERFORM 3000-FIN
           STOP RUN.

       1000-INICIO.
           ACCEPT WS-FECHA-HOY FROM DATE YYYYMMDD
           MOVE WS-FECHA-HOY TO WS-REP-FECHA

           OPEN INPUT MOVIMIENTOS
           IF WS-FS-MOV NOT = "00"
               DISPLAY "ERROR ABRIENDO MOVIMIENTOS: " WS-FS-MOV
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF

           OPEN I-O CLIENTES
           IF WS-FS-CLI NOT = "00"
               DISPLAY "ERROR ABRIENDO CLIENTES: " WS-FS-CLI
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF

           OPEN OUTPUT ERRORES
           OPEN OUTPUT REPORTE

           WRITE REG-REPORTE FROM WS-ENCABEZADO-1
           PERFORM 2100-LEER-MOVIMIENTO.

       2000-PROCESO.
           ADD 1 TO WS-LEIDOS
           PERFORM 2200-BUSCAR-CLIENTE
           IF WS-FS-CLI = "00"
               PERFORM 2300-APLICAR-MOVIMIENTO
           ELSE
               PERFORM 2400-REGISTRAR-ERROR
           END-IF
           PERFORM 2100-LEER-MOVIMIENTO.

       2100-LEER-MOVIMIENTO.
           READ MOVIMIENTOS
               AT END SET FIN-MOVIMIENTOS TO TRUE
           END-READ.

       2200-BUSCAR-CLIENTE.
           MOVE MOV-CLIENTE TO CLI-NUMERO
           READ CLIENTES
               INVALID KEY CONTINUE
           END-READ.

       2300-APLICAR-MOVIMIENTO.
           EVALUATE TRUE
               WHEN MOV-DEPOSITO
                   ADD MOV-MONTO TO CLI-SALDO
                   ADD MOV-MONTO TO WS-TOT-DEPOSITOS
                   ADD 1 TO WS-DEPOSITOS
               WHEN MOV-RETIRO
                   IF CLI-SALDO >= MOV-MONTO
                       SUBTRACT MOV-MONTO FROM CLI-SALDO
                       ADD MOV-MONTO TO WS-TOT-RETIROS
                       ADD 1 TO WS-RETIROS
                   ELSE
                       MOVE "SALDO INSUFICIENTE" TO WS-ERR-MSG
                       PERFORM 2400-REGISTRAR-ERROR
                       GO TO 2300-EXIT
                   END-IF
               WHEN OTHER
                   MOVE "TIPO MOVIMIENTO INVALIDO" TO WS-ERR-MSG
                   PERFORM 2400-REGISTRAR-ERROR
                   GO TO 2300-EXIT
           END-EVALUATE

           MOVE WS-FECHA-HOY TO CLI-FECHA-ULT
           REWRITE REG-CLI
               INVALID KEY
                   MOVE "ERROR EN REWRITE" TO WS-ERR-MSG
                   PERFORM 2400-REGISTRAR-ERROR
           END-REWRITE.

       2300-EXIT.
           EXIT.

       2400-REGISTRAR-ERROR.
           IF WS-FS-CLI = "23"
               MOVE "CLIENTE NO EXISTE" TO WS-ERR-MSG
           END-IF
           MOVE MOV-CLIENTE TO WS-ERR-CLI
           MOVE MOV-MONTO TO WS-ERR-MONTO
           WRITE REG-ERROR FROM WS-LINEA-ERROR
           ADD 1 TO WS-ERRORES.

       3000-FIN.
           DISPLAY "========================================="
           DISPLAY "        RESUMEN DE PROCESO"
           DISPLAY "========================================="
           DISPLAY "MOVIMIENTOS LEIDOS:    " WS-LEIDOS
           DISPLAY "DEPOSITOS APLICADOS:   " WS-DEPOSITOS
           DISPLAY "RETIROS APLICADOS:     " WS-RETIROS
           DISPLAY "ERRORES:               " WS-ERRORES
           DISPLAY "-----------------------------------------"
           DISPLAY "TOTAL DEPOSITOS: " WS-TOT-DEPOSITOS
           DISPLAY "TOTAL RETIROS:   " WS-TOT-RETIROS
           DISPLAY "========================================="

           CLOSE MOVIMIENTOS
                 CLIENTES
                 ERRORES
                 REPORTE

           IF WS-ERRORES > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
```

---

## Próximos Documentos

- **COBOL_06_DB2_CICS.md**: Patrones avanzados DB2/CICS
- **C_01_FUNDAMENTOS.md**: Inicio de Fase 3 - Lenguaje C

---

*"El mainframe es la catedral del procesamiento. JCL es su liturgia, COBOL es su oración."*

