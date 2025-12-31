---
título: "Java 17+ Fundamentos para Migración COBOL"
código: JAVA-01-FUND
versión: 1.0.0
fecha_creación: 2025-12-31
última_actualización: 2025-12-31
autor: ARCHAEON_CORE
dominio: LENGUAJES_MODERNOS
especialización: JAVA_ENTERPRISE
dependencias:
  - JDK 17+
  - Maven/Gradle
tags:
  - java
  - fundamentos
  - cobol-migration
  - enterprise
  - records
  - streams
nivel_complejidad: INTERMEDIO
estado: ACTIVO
---

# ═══════════════════════════════════════════════════════════════════════════════
# JAVA 01: FUNDAMENTOS MODERNOS PARA MIGRACIÓN COBOL
# ARCHAEON_CORE - Sistema de Documentación de Lenguajes Legacy
# ═══════════════════════════════════════════════════════════════════════════════

## 1. INTRODUCCIÓN A JAVA MODERNO

### 1.1 Contexto de Migración COBOL a Java

La migración de sistemas COBOL a Java representa uno de los desafíos más significativos
en la modernización de aplicaciones empresariales. Este documento establece los
fundamentos de Java 17+ necesarios para comprender las técnicas de migración.

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                    EVOLUCIÓN COBOL → JAVA                                   │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│   COBOL (1959)                              JAVA (1995+)                    │
│   ┌─────────────────┐                       ┌─────────────────┐             │
│   │ DIVISION        │                       │ Package         │             │
│   │ SECTION         │         ═══▶          │ Class           │             │
│   │ PARAGRAPH       │                       │ Method          │             │
│   │ DATA ITEM       │                       │ Field           │             │
│   └─────────────────┘                       └─────────────────┘             │
│                                                                             │
│   Paradigma: Procedural                     Paradigma: OOP + Functional     │
│   Tipado: Estático/Declarativo              Tipado: Estático/Inferido       │
│   Datos: PICTURE Clauses                    Datos: Tipos Primitivos/Objects │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### 1.2 Versiones Java y Características Clave

```java
// Java version timeline for enterprise migration
public class JavaVersionTimeline {

    /*
     * Java 8 (2014)  - Lambdas, Streams, Optional
     * Java 11 (2018) - HTTP Client, var keyword, String methods
     * Java 17 (2021) - LTS, Records, Sealed Classes, Pattern Matching
     * Java 21 (2023) - LTS, Virtual Threads, Pattern Matching for switch
     */

    public static void main(String[] args) {
        // Recommended: Use latest LTS version (17 or 21)
        System.out.println("Java Version: " + Runtime.version());
    }
}
```

---

## 2. RECORDS: MODERNIZACIÓN DE ESTRUCTURAS DE DATOS

### 2.1 De COBOL Working-Storage a Java Records

Los Records de Java 17+ representan la evolución ideal para mapear estructuras
de datos COBOL, proporcionando inmutabilidad y reducción de código boilerplate.

**Estructura COBOL Original:**

```cobol
       01  CLIENTE-RECORD.
           05  CLIENTE-ID          PIC 9(8).
           05  CLIENTE-NOMBRE      PIC X(50).
           05  CLIENTE-DIRECCION.
               10  CALLE           PIC X(40).
               10  CIUDAD          PIC X(30).
               10  CODIGO-POSTAL   PIC 9(5).
           05  CLIENTE-SALDO       PIC S9(9)V99.
           05  FECHA-ALTA          PIC 9(8).
```

**Equivalente Java Record:**

```java
package com.enterprise.migration.model;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Cliente record - Direct mapping from COBOL CLIENTE-RECORD
 * Immutable data carrier replacing COBOL copybook structure
 */
public record Cliente(
    long clienteId,           // CLIENTE-ID PIC 9(8)
    String nombre,            // CLIENTE-NOMBRE PIC X(50)
    Direccion direccion,      // CLIENTE-DIRECCION (nested)
    BigDecimal saldo,         // CLIENTE-SALDO PIC S9(9)V99
    LocalDate fechaAlta       // FECHA-ALTA PIC 9(8)
) {

    // Compact constructor for validation (like COBOL 88 level conditions)
    public Cliente {
        if (clienteId < 0 || clienteId > 99999999) {
            throw new IllegalArgumentException("CLIENTE-ID out of range");
        }
        if (nombre == null || nombre.isBlank()) {
            throw new IllegalArgumentException("CLIENTE-NOMBRE required");
        }
        // Normalize string to COBOL fixed-length behavior
        nombre = normalizeString(nombre, 50);
    }

    // Static factory method for COBOL data conversion
    public static Cliente fromCobolRecord(String cobolLine) {
        // Parse fixed-position COBOL record
        long id = Long.parseLong(cobolLine.substring(0, 8).trim());
        String nombre = cobolLine.substring(8, 58).trim();
        String calle = cobolLine.substring(58, 98).trim();
        String ciudad = cobolLine.substring(98, 128).trim();
        String codigoPostal = cobolLine.substring(128, 133).trim();
        BigDecimal saldo = parseCobolDecimal(cobolLine.substring(133, 145));
        LocalDate fecha = parseCobolDate(cobolLine.substring(145, 153));

        Direccion dir = new Direccion(calle, ciudad, codigoPostal);
        return new Cliente(id, nombre, dir, saldo, fecha);
    }

    private static String normalizeString(String value, int length) {
        if (value.length() > length) {
            return value.substring(0, length);
        }
        return value;
    }

    private static BigDecimal parseCobolDecimal(String cobolValue) {
        // Handle COBOL S9(9)V99 format with implied decimal
        String cleaned = cobolValue.replace(" ", "0");
        boolean negative = cleaned.endsWith("-") || cleaned.startsWith("-");
        cleaned = cleaned.replace("-", "").replace("+", "");
        BigDecimal value = new BigDecimal(cleaned).movePointLeft(2);
        return negative ? value.negate() : value;
    }

    private static LocalDate parseCobolDate(String cobolDate) {
        // COBOL date format: YYYYMMDD
        int year = Integer.parseInt(cobolDate.substring(0, 4));
        int month = Integer.parseInt(cobolDate.substring(4, 6));
        int day = Integer.parseInt(cobolDate.substring(6, 8));
        return LocalDate.of(year, month, day);
    }
}

/**
 * Nested address record - Maps to COBOL CLIENTE-DIRECCION group
 */
public record Direccion(
    String calle,         // CALLE PIC X(40)
    String ciudad,        // CIUDAD PIC X(30)
    String codigoPostal   // CODIGO-POSTAL PIC 9(5)
) {
    public Direccion {
        calle = calle != null ? calle : "";
        ciudad = ciudad != null ? ciudad : "";
        codigoPostal = codigoPostal != null ? codigoPostal : "00000";
    }
}
```

### 2.2 Records con Comportamiento Extendido

```java
package com.enterprise.migration.model;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Cuenta record with business logic - COBOL CUENTA-REGISTRO migration
 * Demonstrates adding methods to immutable records
 */
public record Cuenta(
    String numeroCuenta,      // NUMERO-CUENTA PIC X(20)
    String tipoCuenta,        // TIPO-CUENTA PIC X(2) - 'CA' or 'CC'
    BigDecimal saldo,         // SALDO-ACTUAL PIC S9(13)V99
    BigDecimal limiteCredito, // LIMITE-CREDITO PIC S9(13)V99
    boolean activa            // CUENTA-ACTIVA PIC 9 (88 level)
) {
    // COBOL 88-level conditions translated to Java methods

    // 88 CUENTA-AHORRO VALUE 'CA'.
    public boolean isCuentaAhorro() {
        return "CA".equals(tipoCuenta);
    }

    // 88 CUENTA-CORRIENTE VALUE 'CC'.
    public boolean isCuentaCorriente() {
        return "CC".equals(tipoCuenta);
    }

    // 88 SALDO-POSITIVO VALUE > 0.
    public boolean isSaldoPositivo() {
        return saldo.compareTo(BigDecimal.ZERO) > 0;
    }

    // 88 SALDO-NEGATIVO VALUE < 0.
    public boolean isSaldoNegativo() {
        return saldo.compareTo(BigDecimal.ZERO) < 0;
    }

    // 88 TIENE-CREDITO-DISPONIBLE based on calculation
    public boolean tieneCreditoDisponible() {
        return getCreditoDisponible().compareTo(BigDecimal.ZERO) > 0;
    }

    // Business calculation - COMPUTE CREDITO-DISPONIBLE
    public BigDecimal getCreditoDisponible() {
        if (!activa || limiteCredito == null) {
            return BigDecimal.ZERO;
        }
        BigDecimal disponible = limiteCredito.add(saldo);
        return disponible.max(BigDecimal.ZERO);
    }

    // COBOL ROUNDED equivalent
    public BigDecimal getSaldoRedondeado() {
        return saldo.setScale(2, RoundingMode.HALF_UP);
    }

    // Create modified copy - MOVE CORRESPONDING equivalent
    public Cuenta withNuevoSaldo(BigDecimal nuevoSaldo) {
        return new Cuenta(numeroCuenta, tipoCuenta, nuevoSaldo,
                         limiteCredito, activa);
    }

    public Cuenta desactivar() {
        return new Cuenta(numeroCuenta, tipoCuenta, saldo,
                         limiteCredito, false);
    }
}
```

---

## 3. SEALED CLASSES: JERARQUÍAS CONTROLADAS

### 3.1 Mapeo de Estructuras COBOL Variantes

Las clases selladas permiten modelar estructuras COBOL con REDEFINES o variantes.

**COBOL con REDEFINES:**

```cobol
       01  TRANSACCION-REGISTRO.
           05  TIPO-TRANS              PIC X(2).
               88  DEPOSITO            VALUE 'DE'.
               88  RETIRO              VALUE 'RE'.
               88  TRANSFERENCIA       VALUE 'TR'.
           05  DATOS-COMUNES.
               10  FECHA-TRANS         PIC 9(8).
               10  MONTO               PIC S9(11)V99.
               10  NUMERO-CUENTA       PIC X(20).
           05  DATOS-ESPECIFICOS       PIC X(100).
           05  DATOS-DEPOSITO REDEFINES DATOS-ESPECIFICOS.
               10  ORIGEN-FONDOS       PIC X(30).
               10  REFERENCIA-DEP      PIC X(20).
               10  FILLER              PIC X(50).
           05  DATOS-RETIRO REDEFINES DATOS-ESPECIFICOS.
               10  MOTIVO-RETIRO       PIC X(50).
               10  CAJERO-ID           PIC 9(6).
               10  FILLER              PIC X(44).
           05  DATOS-TRANSFER REDEFINES DATOS-ESPECIFICOS.
               10  CUENTA-DESTINO      PIC X(20).
               10  BANCO-DESTINO       PIC X(30).
               10  CONCEPTO            PIC X(50).
```

**Java Sealed Classes Equivalente:**

```java
package com.enterprise.migration.model;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Sealed class hierarchy for COBOL TRANSACCION-REGISTRO with REDEFINES
 * Provides type-safe variant handling at compile time
 */
public sealed abstract class Transaccion
    permits Deposito, Retiro, Transferencia {

    // Common fields - DATOS-COMUNES
    protected final LocalDate fechaTrans;
    protected final BigDecimal monto;
    protected final String numeroCuenta;

    protected Transaccion(LocalDate fechaTrans, BigDecimal monto,
                         String numeroCuenta) {
        this.fechaTrans = fechaTrans;
        this.monto = monto;
        this.numeroCuenta = numeroCuenta;
    }

    // Common getters
    public LocalDate getFechaTrans() { return fechaTrans; }
    public BigDecimal getMonto() { return monto; }
    public String getNumeroCuenta() { return numeroCuenta; }

    // Abstract method for polymorphic behavior
    public abstract String getTipoTransaccion();
    public abstract BigDecimal calcularImpactoSaldo();

    // Factory method to parse COBOL record
    public static Transaccion fromCobolRecord(String record) {
        String tipo = record.substring(0, 2);
        LocalDate fecha = parseDate(record.substring(2, 10));
        BigDecimal monto = parseMonto(record.substring(10, 24));
        String cuenta = record.substring(24, 44).trim();
        String datosEspecificos = record.substring(44, 144);

        return switch (tipo) {
            case "DE" -> Deposito.fromDatosEspecificos(
                fecha, monto, cuenta, datosEspecificos);
            case "RE" -> Retiro.fromDatosEspecificos(
                fecha, monto, cuenta, datosEspecificos);
            case "TR" -> Transferencia.fromDatosEspecificos(
                fecha, monto, cuenta, datosEspecificos);
            default -> throw new IllegalArgumentException(
                "Unknown transaction type: " + tipo);
        };
    }

    private static LocalDate parseDate(String cobolDate) {
        return LocalDate.of(
            Integer.parseInt(cobolDate.substring(0, 4)),
            Integer.parseInt(cobolDate.substring(4, 6)),
            Integer.parseInt(cobolDate.substring(6, 8))
        );
    }

    private static BigDecimal parseMonto(String cobolMonto) {
        return new BigDecimal(cobolMonto.trim()).movePointLeft(2);
    }
}

/**
 * Deposito - DATOS-DEPOSITO variant
 */
public final class Deposito extends Transaccion {
    private final String origenFondos;
    private final String referenciaDep;

    public Deposito(LocalDate fechaTrans, BigDecimal monto,
                   String numeroCuenta, String origenFondos,
                   String referenciaDep) {
        super(fechaTrans, monto, numeroCuenta);
        this.origenFondos = origenFondos;
        this.referenciaDep = referenciaDep;
    }

    public static Deposito fromDatosEspecificos(LocalDate fecha,
            BigDecimal monto, String cuenta, String datos) {
        String origen = datos.substring(0, 30).trim();
        String ref = datos.substring(30, 50).trim();
        return new Deposito(fecha, monto, cuenta, origen, ref);
    }

    @Override
    public String getTipoTransaccion() { return "DE"; }

    @Override
    public BigDecimal calcularImpactoSaldo() {
        return monto; // Positive impact
    }

    public String getOrigenFondos() { return origenFondos; }
    public String getReferenciaDep() { return referenciaDep; }
}

/**
 * Retiro - DATOS-RETIRO variant
 */
public final class Retiro extends Transaccion {
    private final String motivoRetiro;
    private final int cajeroId;

    public Retiro(LocalDate fechaTrans, BigDecimal monto,
                 String numeroCuenta, String motivoRetiro, int cajeroId) {
        super(fechaTrans, monto, numeroCuenta);
        this.motivoRetiro = motivoRetiro;
        this.cajeroId = cajeroId;
    }

    public static Retiro fromDatosEspecificos(LocalDate fecha,
            BigDecimal monto, String cuenta, String datos) {
        String motivo = datos.substring(0, 50).trim();
        int cajero = Integer.parseInt(datos.substring(50, 56).trim());
        return new Retiro(fecha, monto, cuenta, motivo, cajero);
    }

    @Override
    public String getTipoTransaccion() { return "RE"; }

    @Override
    public BigDecimal calcularImpactoSaldo() {
        return monto.negate(); // Negative impact
    }

    public String getMotivoRetiro() { return motivoRetiro; }
    public int getCajeroId() { return cajeroId; }
}

/**
 * Transferencia - DATOS-TRANSFER variant
 */
public final class Transferencia extends Transaccion {
    private final String cuentaDestino;
    private final String bancoDestino;
    private final String concepto;

    public Transferencia(LocalDate fechaTrans, BigDecimal monto,
                        String numeroCuenta, String cuentaDestino,
                        String bancoDestino, String concepto) {
        super(fechaTrans, monto, numeroCuenta);
        this.cuentaDestino = cuentaDestino;
        this.bancoDestino = bancoDestino;
        this.concepto = concepto;
    }

    public static Transferencia fromDatosEspecificos(LocalDate fecha,
            BigDecimal monto, String cuenta, String datos) {
        String destino = datos.substring(0, 20).trim();
        String banco = datos.substring(20, 50).trim();
        String concepto = datos.substring(50, 100).trim();
        return new Transferencia(fecha, monto, cuenta, destino, banco, concepto);
    }

    @Override
    public String getTipoTransaccion() { return "TR"; }

    @Override
    public BigDecimal calcularImpactoSaldo() {
        return monto.negate(); // Negative impact for sender
    }

    public String getCuentaDestino() { return cuentaDestino; }
    public String getBancoDestino() { return bancoDestino; }
    public String getConcepto() { return concepto; }
}
```

---

## 4. PATTERN MATCHING: EVALUACIONES TIPO COBOL

### 4.1 Pattern Matching para instanceof

```java
package com.enterprise.migration.processing;

import com.enterprise.migration.model.*;
import java.math.BigDecimal;

/**
 * Transaction processor using pattern matching
 * Replaces COBOL EVALUATE TRUE statements
 */
public class TransactionProcessor {

    /**
     * Process transaction with pattern matching
     * Equivalent to COBOL EVALUATE TIPO-TRANS
     */
    public ProcessingResult procesarTransaccion(Transaccion trans) {
        // Pattern matching with type checking and casting
        if (trans instanceof Deposito dep) {
            return procesarDeposito(dep);
        } else if (trans instanceof Retiro ret) {
            return procesarRetiro(ret);
        } else if (trans instanceof Transferencia trf) {
            return procesarTransferencia(trf);
        }
        throw new IllegalStateException("Unknown transaction type");
    }

    private ProcessingResult procesarDeposito(Deposito dep) {
        // Business logic for deposits
        BigDecimal impuesto = calcularImpuesto(dep.getMonto());
        BigDecimal montoNeto = dep.getMonto().subtract(impuesto);

        return new ProcessingResult(
            dep.getNumeroCuenta(),
            montoNeto,
            "DEPOSITO_PROCESADO",
            "Origen: " + dep.getOrigenFondos()
        );
    }

    private ProcessingResult procesarRetiro(Retiro ret) {
        // Business logic for withdrawals
        if (ret.getMonto().compareTo(new BigDecimal("10000")) > 0) {
            return new ProcessingResult(
                ret.getNumeroCuenta(),
                BigDecimal.ZERO,
                "RETIRO_PENDIENTE_AUTORIZACION",
                "Monto excede límite diario"
            );
        }

        return new ProcessingResult(
            ret.getNumeroCuenta(),
            ret.getMonto().negate(),
            "RETIRO_PROCESADO",
            "Cajero: " + ret.getCajeroId()
        );
    }

    private ProcessingResult procesarTransferencia(Transferencia trf) {
        // Business logic for transfers
        BigDecimal comision = trf.getBancoDestino().equals("INTERNO")
            ? BigDecimal.ZERO
            : new BigDecimal("5.00");

        return new ProcessingResult(
            trf.getNumeroCuenta(),
            trf.getMonto().add(comision).negate(),
            "TRANSFERENCIA_PROCESADA",
            "Destino: " + trf.getCuentaDestino()
        );
    }

    private BigDecimal calcularImpuesto(BigDecimal monto) {
        // ITF calculation
        return monto.multiply(new BigDecimal("0.00005"));
    }
}

record ProcessingResult(
    String cuenta,
    BigDecimal impactoSaldo,
    String codigoResultado,
    String mensaje
) {}
```

### 4.2 Pattern Matching en Switch (Java 21+)

```java
package com.enterprise.migration.processing;

import com.enterprise.migration.model.*;
import java.math.BigDecimal;

/**
 * Advanced pattern matching with switch expressions
 * Replaces complex COBOL EVALUATE statements
 */
public class AdvancedTransactionProcessor {

    /**
     * Process using switch with pattern matching
     *
     * COBOL equivalent:
     * EVALUATE TRUE
     *     WHEN DEPOSITO
     *         PERFORM PROCESO-DEPOSITO
     *     WHEN RETIRO
     *         WHEN MONTO > 10000
     *             PERFORM PROCESO-RETIRO-GRANDE
     *         WHEN OTHER
     *             PERFORM PROCESO-RETIRO-NORMAL
     *     WHEN TRANSFERENCIA
     *         PERFORM PROCESO-TRANSFERENCIA
     * END-EVALUATE
     */
    public String procesarConPatrones(Transaccion trans) {
        return switch (trans) {
            case Deposito dep when dep.getMonto()
                    .compareTo(new BigDecimal("100000")) > 0 ->
                procesarDepositoGrande(dep);

            case Deposito dep ->
                procesarDepositoNormal(dep);

            case Retiro ret when ret.getMonto()
                    .compareTo(new BigDecimal("10000")) > 0 ->
                procesarRetiroGrande(ret);

            case Retiro ret ->
                procesarRetiroNormal(ret);

            case Transferencia trf when
                    trf.getBancoDestino().equals("INTERNO") ->
                procesarTransferenciaInterna(trf);

            case Transferencia trf ->
                procesarTransferenciaExterna(trf);
        };
    }

    /**
     * Deconstruction patterns for records
     * Extract values directly in pattern
     */
    public BigDecimal calcularComision(Transaccion trans) {
        return switch (trans) {
            case Deposito(var fecha, var monto, var cuenta,
                         var origen, var ref)
                    when monto.compareTo(new BigDecimal("50000")) > 0 ->
                monto.multiply(new BigDecimal("0.001")); // 0.1%

            case Deposito _ ->
                BigDecimal.ZERO;

            case Retiro(var fecha, var monto, var cuenta,
                       var motivo, var cajero) ->
                new BigDecimal("2.50"); // Fixed fee

            case Transferencia(var fecha, var monto, var cuenta,
                              var destino, var banco, var concepto)
                    when banco.startsWith("SWIFT") ->
                monto.multiply(new BigDecimal("0.005")); // 0.5%

            case Transferencia _ ->
                new BigDecimal("5.00"); // Standard fee
        };
    }

    // Processing methods
    private String procesarDepositoGrande(Deposito dep) {
        return "DEP-GRANDE-" + dep.getReferenciaDep();
    }

    private String procesarDepositoNormal(Deposito dep) {
        return "DEP-NORMAL-" + dep.getReferenciaDep();
    }

    private String procesarRetiroGrande(Retiro ret) {
        return "RET-GRANDE-PENDIENTE-" + ret.getCajeroId();
    }

    private String procesarRetiroNormal(Retiro ret) {
        return "RET-NORMAL-" + ret.getCajeroId();
    }

    private String procesarTransferenciaInterna(Transferencia trf) {
        return "TRF-INTERNA-" + trf.getCuentaDestino();
    }

    private String procesarTransferenciaExterna(Transferencia trf) {
        return "TRF-EXTERNA-" + trf.getBancoDestino();
    }
}
```

---

## 5. COLLECTIONS FRAMEWORK

### 5.1 Mapeo de Tablas COBOL a Collections

```cobol
       * COBOL table definition
       01  TABLA-PRODUCTOS.
           05  NUM-PRODUCTOS           PIC 9(4) VALUE 0.
           05  PRODUCTO-ENTRY OCCURS 100 TIMES
               INDEXED BY IDX-PROD.
               10  PROD-CODIGO         PIC X(10).
               10  PROD-NOMBRE         PIC X(50).
               10  PROD-PRECIO         PIC 9(7)V99.
               10  PROD-STOCK          PIC 9(5).
```

```java
package com.enterprise.migration.collections;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

/**
 * COBOL table migration to Java Collections
 * Demonstrates ArrayList, HashMap, and modern collection operations
 */
public class ProductoManager {

    // Record for PRODUCTO-ENTRY
    public record Producto(
        String codigo,      // PROD-CODIGO
        String nombre,      // PROD-NOMBRE
        BigDecimal precio,  // PROD-PRECIO
        int stock           // PROD-STOCK
    ) {
        // 88-level equivalent conditions
        public boolean sinStock() { return stock == 0; }
        public boolean stockBajo() { return stock > 0 && stock < 10; }
        public boolean stockNormal() { return stock >= 10; }
    }

    // ArrayList equivalent to OCCURS clause
    private final List<Producto> productos = new ArrayList<>();

    // HashMap for indexed access (like INDEXED BY)
    private final Map<String, Producto> indicePorCodigo = new HashMap<>();

    // COBOL: ADD 1 TO NUM-PRODUCTOS, MOVE data TO PRODUCTO-ENTRY(NUM-PRODUCTOS)
    public void agregarProducto(Producto producto) {
        productos.add(producto);
        indicePorCodigo.put(producto.codigo(), producto);
    }

    // COBOL: SEARCH PRODUCTO-ENTRY
    public Optional<Producto> buscarPorCodigo(String codigo) {
        return Optional.ofNullable(indicePorCodigo.get(codigo));
    }

    // COBOL: SEARCH ALL with ASCENDING KEY
    public Optional<Producto> buscarBinario(String codigo) {
        // Requires sorted list
        List<Producto> sorted = productos.stream()
            .sorted(Comparator.comparing(Producto::codigo))
            .toList();

        int index = Collections.binarySearch(
            sorted,
            new Producto(codigo, "", BigDecimal.ZERO, 0),
            Comparator.comparing(Producto::codigo)
        );

        return index >= 0 ? Optional.of(sorted.get(index)) : Optional.empty();
    }

    // COBOL: PERFORM VARYING with condition
    public List<Producto> obtenerProductosSinStock() {
        List<Producto> sinStock = new ArrayList<>();
        for (Producto p : productos) {
            if (p.sinStock()) {
                sinStock.add(p);
            }
        }
        return sinStock;
    }

    // Modern Stream API equivalent
    public List<Producto> obtenerProductosSinStockStream() {
        return productos.stream()
            .filter(Producto::sinStock)
            .toList();
    }

    // COBOL: COMPUTE TOTAL-VALOR = SUM OF (PRECIO * STOCK)
    public BigDecimal calcularValorInventario() {
        return productos.stream()
            .map(p -> p.precio().multiply(BigDecimal.valueOf(p.stock())))
            .reduce(BigDecimal.ZERO, BigDecimal::add);
    }

    // COBOL: SORT with multiple keys
    public List<Producto> ordenarPorPrecioYNombre() {
        return productos.stream()
            .sorted(Comparator
                .comparing(Producto::precio).reversed()
                .thenComparing(Producto::nombre))
            .toList();
    }

    // COBOL: Group by category using PERFORM VARYING
    public Map<String, List<Producto>> agruparPorRangoPrecio() {
        return productos.stream()
            .collect(Collectors.groupingBy(this::categorizarPorPrecio));
    }

    private String categorizarPorPrecio(Producto p) {
        if (p.precio().compareTo(new BigDecimal("100")) < 0) {
            return "ECONOMICO";
        } else if (p.precio().compareTo(new BigDecimal("1000")) < 0) {
            return "ESTANDAR";
        } else {
            return "PREMIUM";
        }
    }
}
```

### 5.2 Colecciones Inmutables y Thread-Safe

```java
package com.enterprise.migration.collections;

import java.util.*;
import java.util.concurrent.*;

/**
 * Thread-safe collections for concurrent COBOL batch processing migration
 */
public class ConcurrentCollectionsDemo {

    // Thread-safe map for shared access (like COBOL shared storage)
    private final ConcurrentHashMap<String, Cuenta> cuentasActivas =
        new ConcurrentHashMap<>();

    // Thread-safe queue for batch processing
    private final BlockingQueue<Transaccion> colaTransacciones =
        new LinkedBlockingQueue<>(10000);

    // Immutable list for reference data (like COBOL CONSTANT sections)
    private final List<String> codigosValidos = List.of(
        "DEP", "RET", "TRF", "PAG", "COB"
    );

    // Immutable map for code descriptions
    private final Map<String, String> descripcionesCodigos = Map.of(
        "DEP", "DEPOSITO",
        "RET", "RETIRO",
        "TRF", "TRANSFERENCIA",
        "PAG", "PAGO",
        "COB", "COBRO"
    );

    // Atomic updates - like COBOL file locking
    public void actualizarCuenta(String numeroCuenta,
                                  java.util.function.Function<Cuenta, Cuenta> actualizador) {
        cuentasActivas.compute(numeroCuenta, (key, cuenta) -> {
            if (cuenta == null) {
                throw new IllegalArgumentException("Cuenta no encontrada");
            }
            return actualizador.apply(cuenta);
        });
    }

    // Producer pattern for batch input
    public void encolarTransaccion(Transaccion trans) throws InterruptedException {
        colaTransacciones.put(trans); // Blocks if queue is full
    }

    // Consumer pattern for batch processing
    public Transaccion obtenerSiguienteTransaccion() throws InterruptedException {
        return colaTransacciones.take(); // Blocks if queue is empty
    }

    // Defensive copy for external access
    public List<String> getCodigosValidos() {
        return codigosValidos; // Already immutable
    }

    // Create modifiable copy when needed
    public List<String> getCodigosValidosModificable() {
        return new ArrayList<>(codigosValidos);
    }
}
```

---

## 6. STREAM API Y PROGRAMACIÓN FUNCIONAL

### 6.1 Sustitución de PERFORM VARYING

```java
package com.enterprise.migration.functional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.stream.*;

/**
 * Stream API patterns replacing COBOL PERFORM loops
 */
public class StreamPatterns {

    // Sample data structure
    record MovimientoCuenta(
        String numeroCuenta,
        String tipoMovimiento,
        BigDecimal monto,
        String fecha
    ) {}

    /**
     * COBOL:
     * PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > NUM-MOVIMIENTOS
     *     IF TIPO-MOVIMIENTO(IDX) = 'DEP'
     *         ADD MONTO(IDX) TO TOTAL-DEPOSITOS
     *     END-IF
     * END-PERFORM
     */
    public BigDecimal sumarDepositos(List<MovimientoCuenta> movimientos) {
        return movimientos.stream()
            .filter(m -> "DEP".equals(m.tipoMovimiento()))
            .map(MovimientoCuenta::monto)
            .reduce(BigDecimal.ZERO, BigDecimal::add);
    }

    /**
     * COBOL with multiple aggregations:
     * Multiple PERFORM with different accumulations
     */
    public record ResumenMovimientos(
        BigDecimal totalDepositos,
        BigDecimal totalRetiros,
        BigDecimal saldoNeto,
        long cantidadMovimientos,
        BigDecimal promedioMonto
    ) {}

    public ResumenMovimientos generarResumen(List<MovimientoCuenta> movimientos) {
        // Using collectors for multiple aggregations
        DoubleSummaryStatistics stats = movimientos.stream()
            .mapToDouble(m -> m.monto().doubleValue())
            .summaryStatistics();

        BigDecimal depositos = movimientos.stream()
            .filter(m -> "DEP".equals(m.tipoMovimiento()))
            .map(MovimientoCuenta::monto)
            .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal retiros = movimientos.stream()
            .filter(m -> "RET".equals(m.tipoMovimiento()))
            .map(MovimientoCuenta::monto)
            .reduce(BigDecimal.ZERO, BigDecimal::add);

        return new ResumenMovimientos(
            depositos,
            retiros,
            depositos.subtract(retiros),
            stats.getCount(),
            BigDecimal.valueOf(stats.getAverage())
                .setScale(2, RoundingMode.HALF_UP)
        );
    }

    /**
     * COBOL SORT verb equivalent using streams
     * SORT WORK-FILE ON ASCENDING KEY FECHA DESCENDING KEY MONTO
     */
    public List<MovimientoCuenta> ordenarMovimientos(
            List<MovimientoCuenta> movimientos) {
        return movimientos.stream()
            .sorted(Comparator
                .comparing(MovimientoCuenta::fecha)
                .thenComparing(MovimientoCuenta::monto, Comparator.reverseOrder()))
            .toList();
    }

    /**
     * COBOL MERGE equivalent - combining sorted streams
     */
    public List<MovimientoCuenta> fusionarMovimientos(
            List<MovimientoCuenta> lista1,
            List<MovimientoCuenta> lista2) {
        return Stream.concat(lista1.stream(), lista2.stream())
            .sorted(Comparator.comparing(MovimientoCuenta::fecha))
            .toList();
    }

    /**
     * COBOL PERFORM UNTIL with break condition
     * PERFORM UNTIL SALDO-ACUMULADO > LIMITE OR FIN-ARCHIVO
     */
    public List<MovimientoCuenta> obtenerHastaLimite(
            List<MovimientoCuenta> movimientos, BigDecimal limite) {
        List<MovimientoCuenta> resultado = new ArrayList<>();
        BigDecimal acumulado = BigDecimal.ZERO;

        for (MovimientoCuenta mov : movimientos) {
            if (acumulado.compareTo(limite) > 0) {
                break;
            }
            resultado.add(mov);
            acumulado = acumulado.add(mov.monto());
        }
        return resultado;
    }

    // Stream version with takeWhile (Java 9+)
    public List<MovimientoCuenta> obtenerHastaLimiteStream(
            List<MovimientoCuenta> movimientos, BigDecimal limite) {
        // Note: takeWhile doesn't maintain state, so we use a holder
        var acumulador = new Object() { BigDecimal total = BigDecimal.ZERO; };

        return movimientos.stream()
            .takeWhile(mov -> {
                boolean continuar = acumulador.total.compareTo(limite) <= 0;
                acumulador.total = acumulador.total.add(mov.monto());
                return continuar;
            })
            .toList();
    }

    /**
     * Grouping - COBOL control break logic
     * PERFORM UNTIL FIN-ARCHIVO
     *     IF CUENTA-ACTUAL NOT = CUENTA-ANTERIOR
     *         PERFORM CONTROL-BREAK
     *     END-IF
     * END-PERFORM
     */
    public Map<String, List<MovimientoCuenta>> agruparPorCuenta(
            List<MovimientoCuenta> movimientos) {
        return movimientos.stream()
            .collect(Collectors.groupingBy(MovimientoCuenta::numeroCuenta));
    }

    /**
     * Control break with totals
     */
    public Map<String, BigDecimal> totalesPorCuenta(
            List<MovimientoCuenta> movimientos) {
        return movimientos.stream()
            .collect(Collectors.groupingBy(
                MovimientoCuenta::numeroCuenta,
                Collectors.reducing(
                    BigDecimal.ZERO,
                    MovimientoCuenta::monto,
                    BigDecimal::add
                )
            ));
    }
}
```

### 6.2 Optional: Manejo de Valores Nulos

```java
package com.enterprise.migration.functional;

import java.math.BigDecimal;
import java.util.Optional;

/**
 * Optional patterns replacing COBOL IF field = SPACES checks
 */
public class OptionalPatterns {

    record ClienteInfo(
        String id,
        String nombre,
        String email,          // May be null/spaces in COBOL
        String telefono,       // May be null/spaces in COBOL
        BigDecimal limiteCredito  // May be zero/null
    ) {}

    /**
     * COBOL:
     * IF EMAIL NOT = SPACES
     *     PERFORM ENVIAR-NOTIFICACION
     * END-IF
     */
    public void notificarCliente(ClienteInfo cliente) {
        Optional.ofNullable(cliente.email())
            .filter(email -> !email.isBlank())
            .ifPresent(this::enviarEmail);

        Optional.ofNullable(cliente.telefono())
            .filter(tel -> !tel.isBlank())
            .ifPresent(this::enviarSMS);
    }

    /**
     * COBOL:
     * IF LIMITE-CREDITO > 0
     *     COMPUTE CREDITO-DISPONIBLE = LIMITE-CREDITO - SALDO-DEUDOR
     * ELSE
     *     MOVE 0 TO CREDITO-DISPONIBLE
     * END-IF
     */
    public BigDecimal calcularCreditoDisponible(
            ClienteInfo cliente, BigDecimal saldoDeudor) {
        return Optional.ofNullable(cliente.limiteCredito())
            .filter(limite -> limite.compareTo(BigDecimal.ZERO) > 0)
            .map(limite -> limite.subtract(saldoDeudor))
            .map(credito -> credito.max(BigDecimal.ZERO))
            .orElse(BigDecimal.ZERO);
    }

    /**
     * Chaining optionals - multiple COBOL field checks
     */
    public String obtenerContactoPrincipal(ClienteInfo cliente) {
        return Optional.ofNullable(cliente.email())
            .filter(e -> !e.isBlank())
            .or(() -> Optional.ofNullable(cliente.telefono())
                .filter(t -> !t.isBlank()))
            .orElse("SIN-CONTACTO");
    }

    /**
     * Optional with exception - COBOL error handling
     * IF CLIENTE-NO-ENCONTRADO
     *     MOVE 'ERROR' TO RETURN-CODE
     *     PERFORM ERROR-HANDLING
     * END-IF
     */
    public ClienteInfo buscarClienteRequerido(String id) {
        return buscarCliente(id)
            .orElseThrow(() -> new ClienteNoEncontradoException(
                "Cliente no encontrado: " + id));
    }

    private Optional<ClienteInfo> buscarCliente(String id) {
        // Simulated database lookup
        return Optional.empty();
    }

    private void enviarEmail(String email) {
        System.out.println("Enviando email a: " + email);
    }

    private void enviarSMS(String telefono) {
        System.out.println("Enviando SMS a: " + telefono);
    }
}

class ClienteNoEncontradoException extends RuntimeException {
    public ClienteNoEncontradoException(String message) {
        super(message);
    }
}
```

---

## 7. MANEJO DE EXCEPCIONES

### 7.1 Patrones de Excepción para Migración COBOL

```java
package com.enterprise.migration.exceptions;

import java.io.*;
import java.math.BigDecimal;

/**
 * Exception patterns replacing COBOL FILE STATUS and error handling
 */
public class ExceptionPatterns {

    /**
     * COBOL FILE STATUS codes mapped to exceptions
     */
    public enum FileStatus {
        STATUS_00("Successful completion"),
        STATUS_10("End of file"),
        STATUS_21("Sequence error"),
        STATUS_22("Duplicate key"),
        STATUS_23("Record not found"),
        STATUS_30("Permanent error"),
        STATUS_34("Boundary violation"),
        STATUS_35("File not found"),
        STATUS_37("Permission denied"),
        STATUS_39("Fixed length mismatch"),
        STATUS_41("File already open"),
        STATUS_42("File not open"),
        STATUS_43("Delete without prior read"),
        STATUS_44("Boundary violation"),
        STATUS_46("Read without positioning"),
        STATUS_47("Read on file not open for input"),
        STATUS_48("Write on file not open for output"),
        STATUS_49("Delete/Rewrite on file not open for I/O");

        private final String description;

        FileStatus(String description) {
            this.description = description;
        }

        public String getDescription() {
            return description;
        }
    }

    /**
     * Custom exception hierarchy for COBOL migration
     */
    public static class CobolMigrationException extends RuntimeException {
        private final String cobolParagraph;
        private final FileStatus fileStatus;

        public CobolMigrationException(String message, String cobolParagraph,
                                       FileStatus fileStatus) {
            super(message);
            this.cobolParagraph = cobolParagraph;
            this.fileStatus = fileStatus;
        }

        public CobolMigrationException(String message, String cobolParagraph,
                                       FileStatus fileStatus, Throwable cause) {
            super(message, cause);
            this.cobolParagraph = cobolParagraph;
            this.fileStatus = fileStatus;
        }

        public String getCobolParagraph() { return cobolParagraph; }
        public FileStatus getFileStatus() { return fileStatus; }
    }

    public static class RecordNotFoundException extends CobolMigrationException {
        public RecordNotFoundException(String key) {
            super("Record not found: " + key, "READ-RECORD", FileStatus.STATUS_23);
        }
    }

    public static class DuplicateKeyException extends CobolMigrationException {
        public DuplicateKeyException(String key) {
            super("Duplicate key: " + key, "WRITE-RECORD", FileStatus.STATUS_22);
        }
    }

    public static class DataValidationException extends CobolMigrationException {
        private final String fieldName;
        private final Object invalidValue;

        public DataValidationException(String fieldName, Object invalidValue) {
            super("Invalid value for " + fieldName + ": " + invalidValue,
                  "VALIDATE-DATA", null);
            this.fieldName = fieldName;
            this.invalidValue = invalidValue;
        }

        public String getFieldName() { return fieldName; }
        public Object getInvalidValue() { return invalidValue; }
    }

    /**
     * Try-with-resources pattern replacing COBOL OPEN/CLOSE
     *
     * COBOL:
     * OPEN INPUT INPUT-FILE
     * PERFORM PROCESS-FILE UNTIL END-OF-FILE
     * CLOSE INPUT-FILE
     */
    public void procesarArchivo(String rutaArchivo) {
        try (BufferedReader reader = new BufferedReader(
                new FileReader(rutaArchivo))) {
            String linea;
            while ((linea = reader.readLine()) != null) {
                procesarLinea(linea);
            }
        } catch (FileNotFoundException e) {
            throw new CobolMigrationException(
                "Archivo no encontrado: " + rutaArchivo,
                "OPEN-INPUT-FILE",
                FileStatus.STATUS_35,
                e
            );
        } catch (IOException e) {
            throw new CobolMigrationException(
                "Error leyendo archivo: " + rutaArchivo,
                "READ-INPUT-FILE",
                FileStatus.STATUS_30,
                e
            );
        }
    }

    private void procesarLinea(String linea) {
        // Processing logic
    }

    /**
     * Multi-catch for COBOL error handling consolidation
     *
     * COBOL:
     * EVALUATE FILE-STATUS
     *     WHEN '35' PERFORM ERROR-FILE-NOT-FOUND
     *     WHEN '37' PERFORM ERROR-PERMISSION
     *     WHEN '39' PERFORM ERROR-FORMAT
     *     WHEN OTHER PERFORM ERROR-GENERAL
     * END-EVALUATE
     */
    public void procesarConMultiCatch() {
        try {
            ejecutarOperacion();
        } catch (RecordNotFoundException | DuplicateKeyException e) {
            // Handle data-related errors
            registrarErrorDatos(e);
        } catch (DataValidationException e) {
            // Handle validation errors
            registrarErrorValidacion(e);
        } catch (CobolMigrationException e) {
            // Handle other migration errors
            registrarErrorGeneral(e);
        }
    }

    private void ejecutarOperacion() {
        // Business logic
    }

    private void registrarErrorDatos(CobolMigrationException e) {
        System.err.println("Data error: " + e.getMessage());
    }

    private void registrarErrorValidacion(DataValidationException e) {
        System.err.println("Validation error: " + e.getFieldName());
    }

    private void registrarErrorGeneral(CobolMigrationException e) {
        System.err.println("General error: " + e.getFileStatus());
    }
}
```

---

## 8. TABLA DE MAPEO: COBOL A JAVA

### 8.1 Tipos de Datos

| COBOL PICTURE | Java Type | Notas |
|---------------|-----------|-------|
| `PIC 9(n)` | `int` / `long` | Use `long` for n > 9 |
| `PIC 9(n)V9(m)` | `BigDecimal` | Always use BigDecimal for money |
| `PIC S9(n)` | `int` / `long` | Signed integer |
| `PIC S9(n)V9(m)` | `BigDecimal` | Signed decimal |
| `PIC X(n)` | `String` | Fixed-length string |
| `PIC A(n)` | `String` | Alphabetic only |
| `PIC 9(8)` (date) | `LocalDate` | YYYYMMDD format |
| `PIC 9(6)` (time) | `LocalTime` | HHMMSS format |
| `PIC 9(14)` (timestamp) | `LocalDateTime` | Combined date/time |
| `COMP` | `int` / `long` | Binary format |
| `COMP-3` | `BigDecimal` | Packed decimal |
| `POINTER` | Object reference | Memory reference |

### 8.2 Estructuras de Control

| COBOL | Java |
|-------|------|
| `IF ... END-IF` | `if (...) { }` |
| `EVALUATE ... END-EVALUATE` | `switch` expression |
| `PERFORM UNTIL` | `while` / `do-while` |
| `PERFORM VARYING` | `for` loop |
| `PERFORM ... THRU ...` | Method calls |
| `GO TO` | (Avoid) structured control |
| `STOP RUN` | `System.exit()` |

### 8.3 Operaciones de Datos

| COBOL | Java |
|-------|------|
| `MOVE` | Assignment `=` |
| `MOVE CORRESPONDING` | Manual field copy / Record.withX() |
| `ADD TO` | `+=` operator |
| `SUBTRACT FROM` | `-=` operator |
| `MULTIPLY BY` | `*=` operator |
| `DIVIDE INTO` | `/=` operator |
| `COMPUTE` | Expression assignment |
| `INITIALIZE` | Constructor / factory method |
| `STRING ... DELIMITED` | `String.join()` / StringBuilder |
| `UNSTRING` | `String.split()` |
| `INSPECT REPLACING` | `String.replace()` |
| `INSPECT TALLYING` | Stream counting |

---

## 9. IDIOMAS MODERNOS DE JAVA

### 9.1 Patrones Recomendados

```java
package com.enterprise.migration.idioms;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.*;

/**
 * Modern Java idioms for COBOL migration
 */
public class ModernJavaIdioms {

    // ═══════════════════════════════════════════════════════════════════════
    // PATTERN 1: Immutable Data Objects (Records)
    // ═══════════════════════════════════════════════════════════════════════

    // PREFER: Records for data transfer
    public record ClienteDTO(
        String id,
        String nombre,
        BigDecimal saldo
    ) {}

    // AVOID: Mutable POJOs with getters/setters for simple data

    // ═══════════════════════════════════════════════════════════════════════
    // PATTERN 2: var for Local Variables
    // ═══════════════════════════════════════════════════════════════════════

    public void procesoConVar() {
        // PREFER: var when type is obvious from right side
        var clientes = new ArrayList<ClienteDTO>();
        var total = BigDecimal.ZERO;
        var fecha = LocalDate.now();

        // AVOID: var when type is not obvious
        // var resultado = servicio.procesar(datos); // Type unclear
    }

    // ═══════════════════════════════════════════════════════════════════════
    // PATTERN 3: Text Blocks for SQL and Templates
    // ═══════════════════════════════════════════════════════════════════════

    // PREFER: Text blocks for multi-line strings
    private static final String SQL_QUERY = """
        SELECT c.cliente_id,
               c.nombre,
               c.saldo
        FROM clientes c
        WHERE c.fecha_alta >= :fechaDesde
          AND c.estado = 'ACTIVO'
        ORDER BY c.nombre
        """;

    // AVOID: String concatenation for SQL

    // ═══════════════════════════════════════════════════════════════════════
    // PATTERN 4: Switch Expressions
    // ═══════════════════════════════════════════════════════════════════════

    // PREFER: Switch expressions returning values
    public String obtenerDescripcion(String codigo) {
        return switch (codigo) {
            case "A" -> "Activo";
            case "I" -> "Inactivo";
            case "P" -> "Pendiente";
            case "C" -> "Cancelado";
            default -> "Desconocido";
        };
    }

    // AVOID: Switch statements with break and mutable variables

    // ═══════════════════════════════════════════════════════════════════════
    // PATTERN 5: Stream API for Collections
    // ═══════════════════════════════════════════════════════════════════════

    public List<ClienteDTO> filtrarClientesActivos(List<ClienteDTO> clientes) {
        // PREFER: Stream operations
        return clientes.stream()
            .filter(c -> c.saldo().compareTo(BigDecimal.ZERO) > 0)
            .sorted(Comparator.comparing(ClienteDTO::nombre))
            .toList();
    }

    // AVOID: Manual loops for filter/map/sort operations

    // ═══════════════════════════════════════════════════════════════════════
    // PATTERN 6: Optional for Nullable Returns
    // ═══════════════════════════════════════════════════════════════════════

    private Map<String, ClienteDTO> clientes = new HashMap<>();

    // PREFER: Optional for nullable returns
    public Optional<ClienteDTO> buscarCliente(String id) {
        return Optional.ofNullable(clientes.get(id));
    }

    // Usage
    public void usarOptional() {
        buscarCliente("001")
            .ifPresentOrElse(
                cliente -> System.out.println("Found: " + cliente.nombre()),
                () -> System.out.println("Not found")
            );
    }

    // AVOID: Returning null for "not found" scenarios

    // ═══════════════════════════════════════════════════════════════════════
    // PATTERN 7: Factory Methods
    // ═══════════════════════════════════════════════════════════════════════

    // PREFER: Static factory methods
    public static ClienteDTO crearClienteNuevo(String nombre) {
        String id = UUID.randomUUID().toString().substring(0, 8);
        return new ClienteDTO(id, nombre, BigDecimal.ZERO);
    }

    // PREFER: Immutable collections
    public List<String> getCodigosValidos() {
        return List.of("A", "I", "P", "C");
    }

    public Map<String, Integer> getLimitesDefault() {
        return Map.of(
            "STANDARD", 1000,
            "PREMIUM", 5000,
            "VIP", 10000
        );
    }
}
```

---

## 10. EJERCICIOS DE MIGRACIÓN

### 10.1 Ejercicio: Convertir Programa COBOL Simple

**COBOL Original:**

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALC-SALDO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CUENTA.
           05  WS-NUMERO        PIC X(10).
           05  WS-SALDO         PIC S9(11)V99.
           05  WS-LIMITE        PIC S9(11)V99.
       01  WS-RESULTADO         PIC S9(11)V99.
       01  WS-MENSAJE           PIC X(50).

       PROCEDURE DIVISION.
           PERFORM CALCULAR-DISPONIBLE
           PERFORM EVALUAR-RESULTADO
           STOP RUN.

       CALCULAR-DISPONIBLE.
           COMPUTE WS-RESULTADO = WS-LIMITE + WS-SALDO.
           IF WS-RESULTADO < 0
               MOVE 0 TO WS-RESULTADO
           END-IF.

       EVALUAR-RESULTADO.
           EVALUATE TRUE
               WHEN WS-RESULTADO = 0
                   MOVE 'SIN CREDITO DISPONIBLE' TO WS-MENSAJE
               WHEN WS-RESULTADO < 1000
                   MOVE 'CREDITO BAJO' TO WS-MENSAJE
               WHEN WS-RESULTADO < 5000
                   MOVE 'CREDITO MEDIO' TO WS-MENSAJE
               WHEN OTHER
                   MOVE 'CREDITO ALTO' TO WS-MENSAJE
           END-EVALUATE.
```

**Java Migrado:**

```java
package com.enterprise.migration.ejercicio;

import java.math.BigDecimal;

public class CalculadorSaldo {

    // WS-CUENTA structure as record
    public record Cuenta(
        String numero,           // WS-NUMERO PIC X(10)
        BigDecimal saldo,        // WS-SALDO PIC S9(11)V99
        BigDecimal limite        // WS-LIMITE PIC S9(11)V99
    ) {}

    // Result structure
    public record ResultadoCredito(
        BigDecimal disponible,   // WS-RESULTADO
        String mensaje           // WS-MENSAJE
    ) {}

    // Main program entry - PROCEDURE DIVISION
    public ResultadoCredito calcularCredito(Cuenta cuenta) {
        // PERFORM CALCULAR-DISPONIBLE
        BigDecimal disponible = calcularDisponible(cuenta);

        // PERFORM EVALUAR-RESULTADO
        String mensaje = evaluarResultado(disponible);

        return new ResultadoCredito(disponible, mensaje);
    }

    // CALCULAR-DISPONIBLE paragraph
    private BigDecimal calcularDisponible(Cuenta cuenta) {
        // COMPUTE WS-RESULTADO = WS-LIMITE + WS-SALDO
        BigDecimal resultado = cuenta.limite().add(cuenta.saldo());

        // IF WS-RESULTADO < 0 MOVE 0 TO WS-RESULTADO
        return resultado.max(BigDecimal.ZERO);
    }

    // EVALUAR-RESULTADO paragraph
    private String evaluarResultado(BigDecimal resultado) {
        // EVALUATE TRUE ... END-EVALUATE
        return switch (categorizar(resultado)) {
            case CERO -> "SIN CREDITO DISPONIBLE";
            case BAJO -> "CREDITO BAJO";
            case MEDIO -> "CREDITO MEDIO";
            case ALTO -> "CREDITO ALTO";
        };
    }

    private enum CategoriaCredito { CERO, BAJO, MEDIO, ALTO }

    private CategoriaCredito categorizar(BigDecimal resultado) {
        if (resultado.compareTo(BigDecimal.ZERO) == 0) {
            return CategoriaCredito.CERO;
        } else if (resultado.compareTo(new BigDecimal("1000")) < 0) {
            return CategoriaCredito.BAJO;
        } else if (resultado.compareTo(new BigDecimal("5000")) < 0) {
            return CategoriaCredito.MEDIO;
        } else {
            return CategoriaCredito.ALTO;
        }
    }

    // Main method for testing - replaces STOP RUN
    public static void main(String[] args) {
        CalculadorSaldo calculador = new CalculadorSaldo();

        Cuenta cuenta = new Cuenta(
            "1234567890",
            new BigDecimal("-500.00"),
            new BigDecimal("2000.00")
        );

        ResultadoCredito resultado = calculador.calcularCredito(cuenta);

        System.out.println("Disponible: " + resultado.disponible());
        System.out.println("Mensaje: " + resultado.mensaje());
    }
}
```

---

## 11. REFERENCIAS Y RECURSOS

### 11.1 Documentación Oficial

- [Java 17 Documentation](https://docs.oracle.com/en/java/javase/17/)
- [Java Language Specification](https://docs.oracle.com/javase/specs/)
- [Java Records](https://docs.oracle.com/en/java/javase/17/language/records.html)
- [Pattern Matching](https://docs.oracle.com/en/java/javase/17/language/pattern-matching.html)
- [Sealed Classes](https://docs.oracle.com/en/java/javase/17/language/sealed-classes-and-interfaces.html)

### 11.2 Herramientas de Migración

| Herramienta | Propósito |
|-------------|-----------|
| Micro Focus Enterprise Developer | COBOL development and migration |
| Raincode | COBOL to .NET/Java compilation |
| LzLabs | COBOL rehosting platform |
| Blu Age | Automated COBOL modernization |
| AWS Mainframe Modernization | Cloud-based migration |

### 11.3 Patrones de Diseño Recomendados

- **Adapter Pattern**: Para integrar código COBOL existente
- **Factory Pattern**: Para crear objetos desde registros COBOL
- **Strategy Pattern**: Para variar algoritmos de procesamiento
- **Template Method**: Para mantener estructura de paragraphs COBOL

---

```
═══════════════════════════════════════════════════════════════════════════════
                    FIN DEL DOCUMENTO JAVA_01_FUNDAMENTOS
                         ARCHAEON_CORE - v1.0.0
═══════════════════════════════════════════════════════════════════════════════
```
