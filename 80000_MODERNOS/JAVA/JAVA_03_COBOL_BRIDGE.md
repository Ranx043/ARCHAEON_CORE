---
título: "Java-COBOL Bridge - Patrones de Migración"
código: JAVA-03-BRIDGE
versión: 1.0.0
fecha_creación: 2025-12-31
última_actualización: 2025-12-31
autor: ARCHAEON_CORE
dominio: LENGUAJES_MODERNOS
especialización: COBOL_MIGRATION
dependencias:
  - JDK 17+
  - Micro Focus Visual COBOL
  - JNI
tags:
  - java
  - cobol
  - migration
  - jni
  - interoperability
  - legacy
nivel_complejidad: AVANZADO
estado: ACTIVO
---

# ═══════════════════════════════════════════════════════════════════════════════
# JAVA 03: COBOL BRIDGE - PATRONES DE MIGRACIÓN
# ARCHAEON_CORE - Sistema de Documentación de Lenguajes Legacy
# ═══════════════════════════════════════════════════════════════════════════════

## 1. INTRODUCCIÓN A LA MIGRACIÓN COBOL-JAVA

### 1.1 Estrategias de Migración

La migración de COBOL a Java puede seguir diferentes estrategias según las
necesidades del negocio, complejidad del sistema y restricciones de tiempo.

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                    ESTRATEGIAS DE MIGRACIÓN COBOL                           │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  1. REHOSTING (Lift & Shift)                                               │
│     ┌──────────┐    ┌──────────┐                                           │
│     │ COBOL    │ ══▶│ COBOL    │  Same code, new platform                  │
│     │ Mainframe│    │ Linux/x86│  (Micro Focus, Raincode)                  │
│     └──────────┘    └──────────┘                                           │
│                                                                             │
│  2. REPLATFORMING (COBOL to Java Wrapper)                                  │
│     ┌──────────┐    ┌──────────────────────┐                               │
│     │ COBOL    │ ══▶│ Java ◄─JNI─► COBOL  │  Java wrapper, COBOL core     │
│     │ Programs │    │ Services             │                                │
│     └──────────┘    └──────────────────────┘                               │
│                                                                             │
│  3. REFACTORING (Automated Conversion)                                     │
│     ┌──────────┐    ┌──────────┐                                           │
│     │ COBOL    │ ══▶│ Java     │  Automated code generation                │
│     │ Source   │    │ (Legacy  │  (BluAge, Heirloom, Modern Systems)       │
│     │          │    │ Style)   │                                           │
│     └──────────┘    └──────────┘                                           │
│                                                                             │
│  4. REARCHITECTING (Manual Rewrite)                                        │
│     ┌──────────┐    ┌──────────┐                                           │
│     │ COBOL    │ ══▶│ Modern   │  Complete redesign                        │
│     │ Legacy   │    │ Java/    │  Microservices architecture               │
│     │          │    │ Cloud    │                                           │
│     └──────────┘    └──────────┘                                           │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### 1.2 Factores de Decisión

| Factor | Rehosting | Wrapper | Refactoring | Rearchitecting |
|--------|-----------|---------|-------------|----------------|
| Tiempo | Rápido | Medio | Medio | Largo |
| Costo inicial | Bajo | Medio | Medio | Alto |
| Riesgo | Bajo | Medio | Alto | Alto |
| Deuda técnica | Alta | Media | Media | Baja |
| Flexibilidad | Baja | Media | Media | Alta |
| Skills requeridos | COBOL | COBOL+Java | Tools | Java |

---

## 2. MAPEO DE TIPOS DE DATOS

### 2.1 PICTURE Clauses a Tipos Java

```java
package com.enterprise.migration.datatypes;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;

/**
 * COBOL PICTURE to Java type mapping utilities
 * Comprehensive data type conversion library
 */
public class CobolDataTypeMapper {

    // ═══════════════════════════════════════════════════════════════════════
    // NUMERIC TYPES
    // ═══════════════════════════════════════════════════════════════════════

    /**
     * PIC 9(n) - Unsigned integer
     * n <= 9: int
     * n <= 18: long
     * n > 18: BigDecimal
     */
    public static long parseNumeric(String cobolValue, int totalDigits) {
        String cleaned = cobolValue.replace(" ", "0");
        return Long.parseLong(cleaned);
    }

    /**
     * PIC S9(n) - Signed integer
     * Handle trailing sign (+ or -)
     */
    public static long parseSignedNumeric(String cobolValue) {
        String value = cobolValue.trim();

        // Check for trailing sign
        boolean negative = value.endsWith("-");
        if (negative || value.endsWith("+")) {
            value = value.substring(0, value.length() - 1);
        }

        // Check for leading sign
        if (value.startsWith("-")) {
            negative = true;
            value = value.substring(1);
        } else if (value.startsWith("+")) {
            value = value.substring(1);
        }

        // Handle overpunch (zoned decimal last digit contains sign)
        char lastChar = value.charAt(value.length() - 1);
        if (lastChar >= 'A' && lastChar <= 'R') {
            // Overpunch encoding
            negative = lastChar >= 'J';
            int digit = negative ? (lastChar - 'J') : (lastChar - 'A' + 1);
            value = value.substring(0, value.length() - 1) + digit;
        } else if (lastChar == '{' || lastChar == '}') {
            // { = +0, } = -0
            negative = lastChar == '}';
            value = value.substring(0, value.length() - 1) + "0";
        }

        long result = Long.parseLong(value.replace(" ", "0"));
        return negative ? -result : result;
    }

    /**
     * PIC 9(n)V9(m) - Decimal with implied decimal point
     * Always use BigDecimal for financial calculations
     */
    public static BigDecimal parseDecimal(String cobolValue,
                                          int integerDigits,
                                          int decimalDigits) {
        String cleaned = cobolValue.replace(" ", "0").trim();

        // Remove any explicit decimal point if present
        cleaned = cleaned.replace(".", "").replace(",", "");

        // Create BigDecimal and adjust scale
        BigDecimal value = new BigDecimal(cleaned);
        return value.movePointLeft(decimalDigits);
    }

    /**
     * PIC S9(n)V9(m) - Signed decimal
     */
    public static BigDecimal parseSignedDecimal(String cobolValue,
                                                int integerDigits,
                                                int decimalDigits) {
        String value = cobolValue.trim();
        boolean negative = false;

        // Check for trailing sign
        if (value.endsWith("-")) {
            negative = true;
            value = value.substring(0, value.length() - 1);
        } else if (value.endsWith("+")) {
            value = value.substring(0, value.length() - 1);
        }

        // Check for leading sign
        if (value.startsWith("-")) {
            negative = true;
            value = value.substring(1);
        } else if (value.startsWith("+")) {
            value = value.substring(1);
        }

        BigDecimal result = parseDecimal(value, integerDigits, decimalDigits);
        return negative ? result.negate() : result;
    }

    /**
     * COMP (BINARY) - Binary representation
     */
    public static long parseBinary(byte[] bytes) {
        long result = 0;
        for (byte b : bytes) {
            result = (result << 8) | (b & 0xFF);
        }
        return result;
    }

    /**
     * COMP-3 (PACKED-DECIMAL) - Packed decimal format
     * Each byte contains 2 digits, last nibble is sign
     */
    public static BigDecimal parsePackedDecimal(byte[] bytes, int scale) {
        StringBuilder sb = new StringBuilder();

        for (int i = 0; i < bytes.length - 1; i++) {
            sb.append((bytes[i] >> 4) & 0x0F);
            sb.append(bytes[i] & 0x0F);
        }

        // Last byte: digit + sign
        sb.append((bytes[bytes.length - 1] >> 4) & 0x0F);
        int sign = bytes[bytes.length - 1] & 0x0F;

        BigDecimal value = new BigDecimal(sb.toString());
        value = value.movePointLeft(scale);

        // Sign: D = negative, C/F = positive
        if (sign == 0x0D || sign == 0x0B) {
            value = value.negate();
        }

        return value;
    }

    /**
     * Convert Java BigDecimal to COMP-3 packed decimal
     */
    public static byte[] toPackedDecimal(BigDecimal value, int totalDigits,
                                         int scale) {
        boolean negative = value.compareTo(BigDecimal.ZERO) < 0;
        value = value.abs().setScale(scale, RoundingMode.HALF_UP);

        String digits = value.unscaledValue().toString();

        // Pad with leading zeros
        int requiredDigits = totalDigits;
        while (digits.length() < requiredDigits) {
            digits = "0" + digits;
        }

        // Calculate byte array size: (digits + 1) / 2
        int byteLen = (digits.length() + 1 + 1) / 2;
        byte[] result = new byte[byteLen];

        // If odd number of digits, first nibble is 0
        int digitIdx = 0;
        int byteIdx = 0;

        if (digits.length() % 2 == 0) {
            // Even digits - start with first digit
            result[byteIdx] = (byte) ((digits.charAt(digitIdx) - '0') << 4);
            result[byteIdx] |= (byte) (digits.charAt(digitIdx + 1) - '0');
            digitIdx += 2;
            byteIdx++;
        } else {
            // Odd digits - first nibble is 0
            result[byteIdx] = (byte) (digits.charAt(digitIdx) - '0');
            digitIdx++;
            byteIdx++;
        }

        while (digitIdx < digits.length() - 1) {
            result[byteIdx] = (byte) ((digits.charAt(digitIdx) - '0') << 4);
            result[byteIdx] |= (byte) (digits.charAt(digitIdx + 1) - '0');
            digitIdx += 2;
            byteIdx++;
        }

        // Last digit + sign
        if (digitIdx < digits.length()) {
            result[byteIdx] = (byte) ((digits.charAt(digitIdx) - '0') << 4);
        }
        result[byteIdx] |= negative ? 0x0D : 0x0C;

        return result;
    }

    // ═══════════════════════════════════════════════════════════════════════
    // STRING TYPES
    // ═══════════════════════════════════════════════════════════════════════

    /**
     * PIC X(n) - Alphanumeric string
     */
    public static String parseAlphanumeric(String cobolValue) {
        // COBOL strings are space-padded, trim trailing spaces
        return cobolValue.replaceAll("\\s+$", "");
    }

    /**
     * PIC A(n) - Alphabetic only
     */
    public static String parseAlphabetic(String cobolValue) {
        return cobolValue.replaceAll("\\s+$", "");
    }

    /**
     * Convert Java String to fixed-length COBOL format
     */
    public static String toCobolString(String javaValue, int length) {
        if (javaValue == null) {
            javaValue = "";
        }

        if (javaValue.length() >= length) {
            return javaValue.substring(0, length);
        }

        // Right-pad with spaces
        StringBuilder sb = new StringBuilder(javaValue);
        while (sb.length() < length) {
            sb.append(' ');
        }
        return sb.toString();
    }

    /**
     * Convert numeric to COBOL edited format
     * PIC Z,ZZZ,ZZ9.99
     */
    public static String toEditedNumeric(BigDecimal value, String picture) {
        // Simplified implementation
        return String.format("%,.2f", value);
    }

    // ═══════════════════════════════════════════════════════════════════════
    // DATE/TIME TYPES
    // ═══════════════════════════════════════════════════════════════════════

    /**
     * PIC 9(8) as YYYYMMDD date
     */
    public static LocalDate parseDate8(String cobolDate) {
        if (cobolDate == null || cobolDate.trim().isEmpty() ||
            cobolDate.equals("00000000")) {
            return null;
        }

        int year = Integer.parseInt(cobolDate.substring(0, 4));
        int month = Integer.parseInt(cobolDate.substring(4, 6));
        int day = Integer.parseInt(cobolDate.substring(6, 8));

        return LocalDate.of(year, month, day);
    }

    /**
     * PIC 9(6) as YYYYMM date
     */
    public static LocalDate parseDate6(String cobolDate) {
        if (cobolDate == null || cobolDate.trim().isEmpty()) {
            return null;
        }

        int year = Integer.parseInt(cobolDate.substring(0, 4));
        int month = Integer.parseInt(cobolDate.substring(4, 6));

        return LocalDate.of(year, month, 1);
    }

    /**
     * PIC 9(7) as CYYDDD Julian date (century, year, day of year)
     */
    public static LocalDate parseJulianDate(String cobolDate) {
        int century = Integer.parseInt(cobolDate.substring(0, 1));
        int year = Integer.parseInt(cobolDate.substring(1, 3));
        int dayOfYear = Integer.parseInt(cobolDate.substring(3, 7));

        int fullYear = (century == 0 ? 1900 : 2000) + year;
        return LocalDate.ofYearDay(fullYear, dayOfYear);
    }

    /**
     * PIC 9(6) as HHMMSS time
     */
    public static LocalTime parseTime6(String cobolTime) {
        int hour = Integer.parseInt(cobolTime.substring(0, 2));
        int minute = Integer.parseInt(cobolTime.substring(2, 4));
        int second = Integer.parseInt(cobolTime.substring(4, 6));

        return LocalTime.of(hour, minute, second);
    }

    /**
     * PIC 9(14) as YYYYMMDDHHMMSS timestamp
     */
    public static LocalDateTime parseTimestamp14(String cobolTimestamp) {
        LocalDate date = parseDate8(cobolTimestamp.substring(0, 8));
        LocalTime time = parseTime6(cobolTimestamp.substring(8, 14));

        return LocalDateTime.of(date, time);
    }

    /**
     * Convert Java LocalDate to COBOL format
     */
    public static String toCobolDate8(LocalDate date) {
        if (date == null) {
            return "00000000";
        }
        return date.format(DateTimeFormatter.ofPattern("yyyyMMdd"));
    }

    /**
     * Convert Java LocalDateTime to COBOL timestamp
     */
    public static String toCobolTimestamp14(LocalDateTime dateTime) {
        if (dateTime == null) {
            return "00000000000000";
        }
        return dateTime.format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss"));
    }
}
```

### 2.2 Tabla de Referencia Rápida

| COBOL PICTURE | Java Type | Example COBOL | Example Java |
|---------------|-----------|---------------|--------------|
| `PIC 9(4)` | `int` | `0123` | `123` |
| `PIC 9(9)` | `int` | `000000123` | `123` |
| `PIC 9(10)` | `long` | `0000000123` | `123L` |
| `PIC 9(18)` | `long` | Large number | `Long.MAX_VALUE` |
| `PIC S9(4)` | `int` | `0123-` | `-123` |
| `PIC S9(9)` | `int` | `+00000123` | `123` |
| `PIC 9(7)V99` | `BigDecimal` | `0001234.56` | `new BigDecimal("1234.56")` |
| `PIC S9(9)V99` | `BigDecimal` | `-000123456` | `new BigDecimal("-1234.56")` |
| `PIC S9(9)V99 COMP-3` | `BigDecimal` | Packed bytes | `BigDecimal` |
| `PIC X(20)` | `String` | `JOHN DOE            ` | `"JOHN DOE"` |
| `PIC A(10)` | `String` | `ABCDEFGHIJ` | `"ABCDEFGHIJ"` |
| `PIC 9(8)` (date) | `LocalDate` | `20251231` | `LocalDate.of(2025,12,31)` |
| `PIC 9(6)` (time) | `LocalTime` | `143025` | `LocalTime.of(14,30,25)` |
| `PIC 9(14)` (ts) | `LocalDateTime` | `20251231143025` | `LocalDateTime...` |

---

## 3. PATRONES DE PROCESAMIENTO DE ARCHIVOS

### 3.1 Archivos Secuenciales COBOL a Java I/O

```java
package com.enterprise.migration.fileio;

import java.io.*;
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.nio.file.*;
import java.util.*;
import java.util.function.Function;

/**
 * COBOL Sequential File processing patterns
 * Maps to OPEN, READ, WRITE, CLOSE operations
 */
public class CobolSequentialFileProcessor<T> {

    private final int recordLength;
    private final Charset charset;
    private final Function<String, T> parser;
    private final Function<T, String> formatter;

    /**
     * Constructor for fixed-length record processing
     *
     * @param recordLength COBOL FD record length
     * @param charset Character encoding (usually IBM EBCDIC or ASCII)
     * @param parser Function to parse COBOL record to Java object
     * @param formatter Function to format Java object to COBOL record
     */
    public CobolSequentialFileProcessor(
            int recordLength,
            Charset charset,
            Function<String, T> parser,
            Function<T, String> formatter) {
        this.recordLength = recordLength;
        this.charset = charset;
        this.parser = parser;
        this.formatter = formatter;
    }

    /**
     * Read entire file - COBOL PERFORM UNTIL END-OF-FILE pattern
     *
     * COBOL equivalent:
     * OPEN INPUT ARCHIVO
     * PERFORM READ-ARCHIVO UNTIL FIN-ARCHIVO
     * CLOSE ARCHIVO
     */
    public List<T> leerArchivo(Path archivo) throws IOException {
        List<T> registros = new ArrayList<>();

        try (InputStream is = Files.newInputStream(archivo);
             BufferedReader reader = new BufferedReader(
                 new InputStreamReader(is, charset))) {

            char[] buffer = new char[recordLength];
            int bytesRead;

            // READ ARCHIVO AT END SET FIN-ARCHIVO TO TRUE
            while ((bytesRead = reader.read(buffer)) == recordLength) {
                String registro = new String(buffer);
                T objeto = parser.apply(registro);
                registros.add(objeto);
            }
        }

        return registros;
    }

    /**
     * Write records to file - COBOL WRITE pattern
     *
     * COBOL equivalent:
     * OPEN OUTPUT ARCHIVO
     * PERFORM WRITE-ARCHIVO VARYING IDX
     * CLOSE ARCHIVO
     */
    public void escribirArchivo(Path archivo, List<T> registros)
            throws IOException {

        try (OutputStream os = Files.newOutputStream(archivo);
             BufferedWriter writer = new BufferedWriter(
                 new OutputStreamWriter(os, charset))) {

            for (T registro : registros) {
                String linea = formatter.apply(registro);

                // Ensure fixed length
                if (linea.length() < recordLength) {
                    linea = padRight(linea, recordLength);
                } else if (linea.length() > recordLength) {
                    linea = linea.substring(0, recordLength);
                }

                writer.write(linea);
            }
        }
    }

    /**
     * Process file with callback - Streaming pattern for large files
     *
     * COBOL equivalent:
     * OPEN INPUT ENTRADA
     * OPEN OUTPUT SALIDA
     * PERFORM PROCESO UNTIL FIN-ARCHIVO
     * CLOSE ENTRADA SALIDA
     */
    public ProcessingResult procesarArchivo(
            Path archivoEntrada,
            Path archivoSalida,
            java.util.function.Predicate<T> filtro,
            java.util.function.UnaryOperator<T> transformador) throws IOException {

        long leidos = 0;
        long escritos = 0;
        long filtrados = 0;

        try (InputStream is = Files.newInputStream(archivoEntrada);
             BufferedReader reader = new BufferedReader(
                 new InputStreamReader(is, charset));
             OutputStream os = Files.newOutputStream(archivoSalida);
             BufferedWriter writer = new BufferedWriter(
                 new OutputStreamWriter(os, charset))) {

            char[] buffer = new char[recordLength];

            while (reader.read(buffer) == recordLength) {
                leidos++;

                String registro = new String(buffer);
                T objeto = parser.apply(registro);

                // IF REGISTRO-VALIDO
                if (filtro.test(objeto)) {
                    // PERFORM TRANSFORMAR-REGISTRO
                    T transformado = transformador.apply(objeto);

                    // WRITE REGISTRO-SALIDA
                    String salida = formatter.apply(transformado);
                    salida = padRight(salida, recordLength);
                    writer.write(salida);
                    escritos++;
                } else {
                    filtrados++;
                }
            }
        }

        return new ProcessingResult(leidos, escritos, filtrados);
    }

    private String padRight(String str, int length) {
        StringBuilder sb = new StringBuilder(str);
        while (sb.length() < length) {
            sb.append(' ');
        }
        return sb.toString();
    }

    public record ProcessingResult(long leidos, long escritos, long filtrados) {}
}

/**
 * Example: Customer record processing
 */
class ClienteFileProcessor {

    // COBOL FD record: 200 bytes total
    private static final int RECORD_LENGTH = 200;

    record ClienteRecord(
        long id,           // positions 1-8
        String nombre,     // positions 9-58
        String direccion,  // positions 59-108
        BigDecimal saldo,  // positions 109-121 (S9(11)V99)
        String estado      // position 122
    ) {}

    public static ClienteRecord parseCliente(String cobolRecord) {
        long id = Long.parseLong(cobolRecord.substring(0, 8).trim());
        String nombre = cobolRecord.substring(8, 58).trim();
        String direccion = cobolRecord.substring(58, 108).trim();

        // Parse S9(11)V99 with trailing sign
        String saldoStr = cobolRecord.substring(108, 121);
        BigDecimal saldo = CobolDataTypeMapper.parseSignedDecimal(
            saldoStr, 11, 2);

        String estado = cobolRecord.substring(121, 122);

        return new ClienteRecord(id, nombre, direccion, saldo, estado);
    }

    public static String formatCliente(ClienteRecord cliente) {
        StringBuilder sb = new StringBuilder();

        // ID: PIC 9(8)
        sb.append(String.format("%08d", cliente.id()));

        // Nombre: PIC X(50)
        sb.append(String.format("%-50s", cliente.nombre()));

        // Direccion: PIC X(50)
        sb.append(String.format("%-50s", cliente.direccion()));

        // Saldo: PIC S9(11)V99
        String saldoStr = String.format("%013d",
            cliente.saldo().movePointRight(2).longValue());
        if (cliente.saldo().compareTo(BigDecimal.ZERO) < 0) {
            saldoStr = saldoStr.substring(1) + "-";
        } else {
            saldoStr = saldoStr + "+";
        }
        sb.append(saldoStr);

        // Estado: PIC X(1)
        sb.append(cliente.estado());

        // Pad to record length
        while (sb.length() < RECORD_LENGTH) {
            sb.append(' ');
        }

        return sb.toString();
    }

    public static void main(String[] args) throws IOException {
        var processor = new CobolSequentialFileProcessor<ClienteRecord>(
            RECORD_LENGTH,
            java.nio.charset.StandardCharsets.ISO_8859_1,
            ClienteFileProcessor::parseCliente,
            ClienteFileProcessor::formatCliente
        );

        // Process: filter active clients, update status
        var resultado = processor.procesarArchivo(
            Path.of("CLIENTES.DAT"),
            Path.of("CLIENTES_ACTIVOS.DAT"),
            cliente -> "A".equals(cliente.estado()),
            cliente -> new ClienteRecord(
                cliente.id(),
                cliente.nombre(),
                cliente.direccion(),
                cliente.saldo(),
                "P" // Mark as processed
            )
        );

        System.out.println("Leidos: " + resultado.leidos());
        System.out.println("Escritos: " + resultado.escritos());
        System.out.println("Filtrados: " + resultado.filtrados());
    }
}
```

### 3.2 Archivos Indexados (VSAM KSDS) a Java

```java
package com.enterprise.migration.fileio;

import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.ConcurrentSkipListMap;

/**
 * VSAM KSDS (Key-Sequenced Data Set) simulation in Java
 * Provides indexed access similar to COBOL indexed files
 */
public class IndexedFileSimulator<K extends Comparable<K>, V> {

    private final ConcurrentSkipListMap<K, V> index;
    private final Path dataFile;
    private final Path indexFile;
    private final RecordSerializer<K, V> serializer;

    /**
     * Open indexed file
     *
     * COBOL equivalent:
     * SELECT INDEXED-FILE ASSIGN TO 'DATA.DAT'
     *     ORGANIZATION IS INDEXED
     *     ACCESS MODE IS DYNAMIC
     *     RECORD KEY IS KEY-FIELD
     *     FILE STATUS IS WS-STATUS.
     */
    public IndexedFileSimulator(Path dataFile, Path indexFile,
                                RecordSerializer<K, V> serializer)
            throws IOException {
        this.dataFile = dataFile;
        this.indexFile = indexFile;
        this.serializer = serializer;
        this.index = new ConcurrentSkipListMap<>();

        // Load index if exists
        if (Files.exists(indexFile)) {
            loadIndex();
        }
    }

    /**
     * READ with key - Random access
     *
     * COBOL:
     * MOVE key-value TO KEY-FIELD
     * READ INDEXED-FILE
     *     INVALID KEY
     *         MOVE '23' TO WS-STATUS
     * END-READ
     */
    public Optional<V> read(K key) {
        return Optional.ofNullable(index.get(key));
    }

    /**
     * WRITE - Add new record
     *
     * COBOL:
     * WRITE RECORD-AREA
     *     INVALID KEY
     *         MOVE '22' TO WS-STATUS
     * END-WRITE
     */
    public FileStatus write(K key, V value) {
        if (index.containsKey(key)) {
            return FileStatus.DUPLICATE_KEY; // Status 22
        }

        index.put(key, value);
        return FileStatus.SUCCESS; // Status 00
    }

    /**
     * REWRITE - Update existing record
     *
     * COBOL:
     * REWRITE RECORD-AREA
     *     INVALID KEY
     *         MOVE '23' TO WS-STATUS
     * END-REWRITE
     */
    public FileStatus rewrite(K key, V value) {
        if (!index.containsKey(key)) {
            return FileStatus.RECORD_NOT_FOUND; // Status 23
        }

        index.put(key, value);
        return FileStatus.SUCCESS;
    }

    /**
     * DELETE - Remove record
     *
     * COBOL:
     * DELETE INDEXED-FILE
     *     INVALID KEY
     *         MOVE '23' TO WS-STATUS
     * END-DELETE
     */
    public FileStatus delete(K key) {
        if (index.remove(key) == null) {
            return FileStatus.RECORD_NOT_FOUND;
        }
        return FileStatus.SUCCESS;
    }

    /**
     * START - Position for sequential read
     *
     * COBOL:
     * START INDEXED-FILE KEY >= KEY-FIELD
     *     INVALID KEY
     *         MOVE '23' TO WS-STATUS
     * END-START
     */
    public Iterator<Map.Entry<K, V>> start(K key, StartMode mode) {
        NavigableMap<K, V> tailMap;

        switch (mode) {
            case EQUAL:
                if (index.containsKey(key)) {
                    tailMap = index.tailMap(key, true);
                } else {
                    return Collections.emptyIterator();
                }
                break;
            case GREATER_OR_EQUAL:
                tailMap = index.tailMap(key, true);
                break;
            case GREATER:
                tailMap = index.tailMap(key, false);
                break;
            default:
                tailMap = index;
        }

        return tailMap.entrySet().iterator();
    }

    /**
     * READ NEXT - Sequential read after START
     *
     * COBOL:
     * READ INDEXED-FILE NEXT
     *     AT END
     *         MOVE '10' TO WS-STATUS
     * END-READ
     */
    public Optional<Map.Entry<K, V>> readNext(Iterator<Map.Entry<K, V>> cursor) {
        if (cursor.hasNext()) {
            return Optional.of(cursor.next());
        }
        return Optional.empty(); // Status 10 - End of file
    }

    /**
     * CLOSE - Persist and close file
     */
    public void close() throws IOException {
        saveIndex();
    }

    private void loadIndex() throws IOException {
        try (ObjectInputStream ois = new ObjectInputStream(
                Files.newInputStream(indexFile))) {
            @SuppressWarnings("unchecked")
            Map<K, V> loaded = (Map<K, V>) ois.readObject();
            index.putAll(loaded);
        } catch (ClassNotFoundException e) {
            throw new IOException("Error loading index", e);
        }
    }

    private void saveIndex() throws IOException {
        try (ObjectOutputStream oos = new ObjectOutputStream(
                Files.newOutputStream(indexFile))) {
            oos.writeObject(new HashMap<>(index));
        }
    }

    public enum FileStatus {
        SUCCESS,           // 00
        END_OF_FILE,       // 10
        DUPLICATE_KEY,     // 22
        RECORD_NOT_FOUND,  // 23
        BOUNDARY_ERROR     // 34
    }

    public enum StartMode {
        EQUAL,
        GREATER_OR_EQUAL,
        GREATER
    }

    public interface RecordSerializer<K, V> {
        byte[] serialize(K key, V value);
        Map.Entry<K, V> deserialize(byte[] data);
    }
}

/**
 * Example usage - Customer indexed file
 */
class CustomerIndexedFileExample {

    record Customer(String id, String name, BigDecimal balance) {}

    public static void main(String[] args) throws IOException {
        var file = new IndexedFileSimulator<String, Customer>(
            Path.of("CUSTOMERS.DAT"),
            Path.of("CUSTOMERS.IDX"),
            new IndexedFileSimulator.RecordSerializer<>() {
                public byte[] serialize(String key, Customer value) {
                    return (key + "|" + value.name() + "|" +
                           value.balance()).getBytes();
                }

                public Map.Entry<String, Customer> deserialize(byte[] data) {
                    String[] parts = new String(data).split("\\|");
                    return Map.entry(parts[0],
                        new Customer(parts[0], parts[1],
                            new BigDecimal(parts[2])));
                }
            }
        );

        // WRITE new customer
        file.write("CUST001", new Customer("CUST001", "John Doe",
                                           new BigDecimal("1000.00")));

        // READ by key
        var customer = file.read("CUST001");
        customer.ifPresent(c -> System.out.println("Found: " + c.name()));

        // START and READ NEXT (browse)
        var cursor = file.start("CUST", IndexedFileSimulator.StartMode.GREATER_OR_EQUAL);
        while (true) {
            var entry = file.readNext(cursor);
            if (entry.isEmpty()) break;
            System.out.println("Browse: " + entry.get().getValue().name());
        }

        file.close();
    }
}
```

---

## 4. JNI PARA LLAMAR COBOL NATIVO

### 4.1 Configuración JNI

```java
package com.enterprise.migration.jni;

import java.math.BigDecimal;

/**
 * JNI Bridge for calling native COBOL programs
 * Use when COBOL code cannot be migrated (regulatory, complexity)
 */
public class CobolBridge {

    // Load native library containing COBOL programs
    static {
        try {
            System.loadLibrary("cobol_bridge");
        } catch (UnsatisfiedLinkError e) {
            System.err.println("Failed to load COBOL bridge library");
            throw e;
        }
    }

    /**
     * Call COBOL program with COMMAREA-style interface
     *
     * COBOL program signature:
     * IDENTIFICATION DIVISION.
     * PROGRAM-ID. CALCULAR-INTERES.
     * DATA DIVISION.
     * LINKAGE SECTION.
     * 01  LS-DATOS.
     *     05  LS-CAPITAL       PIC S9(13)V99 COMP-3.
     *     05  LS-TASA          PIC 9(3)V99.
     *     05  LS-PLAZO         PIC 9(4).
     *     05  LS-INTERES       PIC S9(13)V99 COMP-3.
     *     05  LS-RETURN-CODE   PIC 9(4).
     * PROCEDURE DIVISION USING LS-DATOS.
     */
    public native byte[] callCobolProgram(String programName, byte[] commarea);

    /**
     * High-level wrapper for interest calculation
     */
    public InterestResult calcularInteres(BigDecimal capital,
                                          BigDecimal tasa,
                                          int plazo) {
        // Build COMMAREA
        byte[] commarea = buildCommarea(capital, tasa, plazo);

        // Call COBOL
        byte[] resultado = callCobolProgram("CALCULAR-INTERES", commarea);

        // Parse result
        return parseInterestResult(resultado);
    }

    private byte[] buildCommarea(BigDecimal capital, BigDecimal tasa, int plazo) {
        // 8 bytes (COMP-3 S9(13)V99) + 5 bytes (9(3)V99) + 4 bytes (9(4))
        // + 8 bytes (COMP-3) + 4 bytes (return code) = 29 bytes
        byte[] commarea = new byte[29];

        // Pack capital into COMP-3 format
        byte[] capitalPacked = CobolDataTypeMapper.toPackedDecimal(capital, 15, 2);
        System.arraycopy(capitalPacked, 0, commarea, 0, 8);

        // Tasa as zoned decimal
        String tasaStr = String.format("%05d", tasa.movePointRight(2).intValue());
        System.arraycopy(tasaStr.getBytes(), 0, commarea, 8, 5);

        // Plazo
        String plazoStr = String.format("%04d", plazo);
        System.arraycopy(plazoStr.getBytes(), 0, commarea, 13, 4);

        // Interes output (zeroes)
        // Return code (zeroes)

        return commarea;
    }

    private InterestResult parseInterestResult(byte[] resultado) {
        // Extract COMP-3 interest value
        byte[] interesPacked = new byte[8];
        System.arraycopy(resultado, 17, interesPacked, 0, 8);
        BigDecimal interes = CobolDataTypeMapper.parsePackedDecimal(
            interesPacked, 2);

        // Extract return code
        String returnCodeStr = new String(resultado, 25, 4);
        int returnCode = Integer.parseInt(returnCodeStr);

        return new InterestResult(interes, returnCode);
    }

    public record InterestResult(BigDecimal interes, int returnCode) {
        public boolean isSuccess() {
            return returnCode == 0;
        }
    }
}
```

### 4.2 Native C Wrapper para COBOL

```c
/* cobol_bridge.c - JNI native implementation */

#include <jni.h>
#include <string.h>
#include <stdlib.h>
#include "com_enterprise_migration_jni_CobolBridge.h"

/* Micro Focus COBOL runtime headers */
#include "cobcall.h"
#include "cobinit.h"

/*
 * JNI method implementation
 * Calls COBOL program using Micro Focus runtime
 */
JNIEXPORT jbyteArray JNICALL
Java_com_enterprise_migration_jni_CobolBridge_callCobolProgram(
        JNIEnv *env,
        jobject obj,
        jstring programName,
        jbyteArray commarea) {

    /* Initialize COBOL runtime if needed */
    static int initialized = 0;
    if (!initialized) {
        cobinit();
        initialized = 1;
    }

    /* Get program name */
    const char *progName = (*env)->GetStringUTFChars(env, programName, NULL);
    if (progName == NULL) {
        return NULL;
    }

    /* Get COMMAREA bytes */
    jsize commareaLen = (*env)->GetArrayLength(env, commarea);
    jbyte *commareaBytes = (*env)->GetByteArrayElements(env, commarea, NULL);
    if (commareaBytes == NULL) {
        (*env)->ReleaseStringUTFChars(env, programName, progName);
        return NULL;
    }

    /* Allocate working buffer */
    unsigned char *buffer = (unsigned char *)malloc(commareaLen);
    memcpy(buffer, commareaBytes, commareaLen);

    /* Call COBOL program */
    int rc = cobcall(progName, 1, buffer);

    /* Release input resources */
    (*env)->ReleaseStringUTFChars(env, programName, progName);
    (*env)->ReleaseByteArrayElements(env, commarea, commareaBytes, JNI_ABORT);

    if (rc != 0) {
        free(buffer);
        /* Throw exception on COBOL error */
        jclass excClass = (*env)->FindClass(env,
            "com/enterprise/migration/jni/CobolException");
        (*env)->ThrowNew(env, excClass, "COBOL program call failed");
        return NULL;
    }

    /* Create result byte array */
    jbyteArray result = (*env)->NewByteArray(env, commareaLen);
    (*env)->SetByteArrayRegion(env, result, 0, commareaLen, (jbyte *)buffer);

    free(buffer);
    return result;
}
```

---

## 5. MICRO FOCUS HERRAMIENTAS DE MIGRACIÓN

### 5.1 Visual COBOL para Eclipse/IntelliJ

```java
package com.enterprise.migration.microfocus;

/**
 * Integration patterns with Micro Focus Visual COBOL
 * Allows running COBOL as managed code with Java
 */
public class MicroFocusIntegration {

    /**
     * Using Micro Focus COBOL-Java Interoperability
     *
     * COBOL program compiled with Visual COBOL generates:
     * - .dll/.so native library
     * - Java wrapper class
     */
    public void ejemploIntegracion() {
        /*
         * COBOL Source (CALCULATOR.cbl):
         *
         * $SET ILSMARTLINKAGE"ON"
         * $SET ILNAMESPACE"com.legacy.cobol"
         *
         * IDENTIFICATION DIVISION.
         * PROGRAM-ID. Calculator.
         *
         * DATA DIVISION.
         * WORKING-STORAGE SECTION.
         * 01 WS-RESULT PIC S9(9)V99.
         *
         * LINKAGE SECTION.
         * 01 LS-VALUE1 PIC S9(9)V99.
         * 01 LS-VALUE2 PIC S9(9)V99.
         * 01 LS-OPERATION PIC X(1).
         *
         * PROCEDURE DIVISION USING LS-VALUE1 LS-VALUE2 LS-OPERATION.
         *     EVALUATE LS-OPERATION
         *         WHEN 'A' COMPUTE WS-RESULT = LS-VALUE1 + LS-VALUE2
         *         WHEN 'S' COMPUTE WS-RESULT = LS-VALUE1 - LS-VALUE2
         *         WHEN 'M' COMPUTE WS-RESULT = LS-VALUE1 * LS-VALUE2
         *         WHEN 'D' COMPUTE WS-RESULT = LS-VALUE1 / LS-VALUE2
         *     END-EVALUATE
         *     MOVE WS-RESULT TO LS-VALUE1
         *     STOP RUN.
         */

        // Generated Java wrapper usage
        // com.legacy.cobol.Calculator calculator = new Calculator();
        // calculator.calculate(value1, value2, operation);
    }

    /**
     * Configuration for Micro Focus COBOL-Java bridge
     */
    public static class MicroFocusConfig {
        // COBDIR environment variable
        public static final String COBOL_DIR = System.getenv("COBDIR");

        // Runtime library path
        public static final String LIB_PATH = COBOL_DIR + "/lib";

        // License configuration
        public static final String LICENSE_DIR = COBOL_DIR + "/etc";

        public static void initialize() {
            // Set system properties for Micro Focus runtime
            System.setProperty("java.library.path",
                System.getProperty("java.library.path") + ":" + LIB_PATH);

            // Load Micro Focus runtime
            System.loadLibrary("mfcobolrts");
        }
    }
}
```

### 5.2 COBOL-IT y GnuCOBOL Integration

```java
package com.enterprise.migration.gnucobol;

import java.io.*;
import java.nio.file.*;

/**
 * GnuCOBOL/COBOL-IT integration for open source migration
 * Compiles COBOL to C, then to native code
 */
public class GnuCobolIntegration {

    /**
     * Compile COBOL source to shared library
     *
     * Command: cobc -m -o program.so program.cbl
     */
    public void compileCobolProgram(Path sourceFile, Path outputFile)
            throws IOException, InterruptedException {

        ProcessBuilder pb = new ProcessBuilder(
            "cobc",
            "-m",                      // Create module (shared library)
            "-free",                   // Free format source
            "-o", outputFile.toString(),
            sourceFile.toString()
        );

        pb.inheritIO();
        Process process = pb.start();
        int exitCode = process.waitFor();

        if (exitCode != 0) {
            throw new IOException("COBOL compilation failed with exit code: "
                                  + exitCode);
        }
    }

    /**
     * Load and call GnuCOBOL program via JNA
     * Requires JNA library
     */
    /*
    public interface GnuCobolLibrary extends Library {
        GnuCobolLibrary INSTANCE = Native.load("program", GnuCobolLibrary.class);

        // Map COBOL PROGRAM-ID to C function
        int program_name(Pointer commarea);
    }
    */

    /**
     * Alternative: Use Process to call COBOL as external program
     */
    public ProcessResult callCobolExecutable(String programPath,
                                             String... args)
            throws IOException, InterruptedException {

        ProcessBuilder pb = new ProcessBuilder();
        pb.command().add(programPath);
        pb.command().addAll(java.util.Arrays.asList(args));

        pb.redirectErrorStream(true);
        Process process = pb.start();

        StringBuilder output = new StringBuilder();
        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(process.getInputStream()))) {
            String line;
            while ((line = reader.readLine()) != null) {
                output.append(line).append("\n");
            }
        }

        int exitCode = process.waitFor();
        return new ProcessResult(exitCode, output.toString());
    }

    public record ProcessResult(int exitCode, String output) {
        public boolean isSuccess() {
            return exitCode == 0;
        }
    }
}
```

---

## 6. PRESERVACIÓN DE LÓGICA DE NEGOCIO

### 6.1 Patrones de Traducción de Paragraphs

```java
package com.enterprise.migration.patterns;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * COBOL business logic preservation patterns
 * Direct translation maintaining original algorithm structure
 */
public class CobolLogicPreservation {

    /**
     * Original COBOL:
     *
     * CALCULAR-INTERESES.
     *     MOVE ZEROS TO WS-TOTAL-INTERES.
     *     PERFORM VARYING WS-IDX FROM 1 BY 1
     *         UNTIL WS-IDX > WS-NUM-PERIODOS
     *         COMPUTE WS-INTERES-PERIODO =
     *             WS-CAPITAL * WS-TASA / 12 / 100
     *         ADD WS-INTERES-PERIODO TO WS-TOTAL-INTERES
     *         ADD WS-INTERES-PERIODO TO WS-CAPITAL
     *     END-PERFORM.
     *     MOVE WS-TOTAL-INTERES TO WS-RESULTADO.
     *
     * Direct translation maintaining exact COBOL logic:
     */
    public BigDecimal calcularIntereses(BigDecimal capital,
                                        BigDecimal tasa,
                                        int numPeriodos) {
        // MOVE ZEROS TO WS-TOTAL-INTERES
        BigDecimal wsCapital = capital;
        BigDecimal wsTotalInteres = BigDecimal.ZERO;

        // PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > WS-NUM-PERIODOS
        for (int wsIdx = 1; wsIdx <= numPeriodos; wsIdx++) {
            // COMPUTE WS-INTERES-PERIODO = WS-CAPITAL * WS-TASA / 12 / 100
            BigDecimal wsInteresPeriodo = wsCapital
                .multiply(tasa)
                .divide(BigDecimal.valueOf(12), 10, RoundingMode.HALF_UP)
                .divide(BigDecimal.valueOf(100), 10, RoundingMode.HALF_UP);

            // ADD WS-INTERES-PERIODO TO WS-TOTAL-INTERES
            wsTotalInteres = wsTotalInteres.add(wsInteresPeriodo);

            // ADD WS-INTERES-PERIODO TO WS-CAPITAL
            wsCapital = wsCapital.add(wsInteresPeriodo);
        }
        // END-PERFORM

        // MOVE WS-TOTAL-INTERES TO WS-RESULTADO
        return wsTotalInteres.setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * Original COBOL with EVALUATE:
     *
     * DETERMINAR-CATEGORIA-CLIENTE.
     *     EVALUATE TRUE
     *         WHEN CLI-SALDO >= 100000
     *             MOVE 'VIP' TO CLI-CATEGORIA
     *         WHEN CLI-SALDO >= 50000
     *             MOVE 'PREMIUM' TO CLI-CATEGORIA
     *         WHEN CLI-SALDO >= 10000
     *             MOVE 'STANDARD' TO CLI-CATEGORIA
     *         WHEN CLI-SALDO >= 0
     *             MOVE 'BASIC' TO CLI-CATEGORIA
     *         WHEN OTHER
     *             MOVE 'DEUDOR' TO CLI-CATEGORIA
     *     END-EVALUATE.
     */
    public String determinarCategoriaCliente(BigDecimal saldo) {
        // EVALUATE TRUE - translated to if-else chain
        if (saldo.compareTo(new BigDecimal("100000")) >= 0) {
            // WHEN CLI-SALDO >= 100000
            return "VIP";
        } else if (saldo.compareTo(new BigDecimal("50000")) >= 0) {
            // WHEN CLI-SALDO >= 50000
            return "PREMIUM";
        } else if (saldo.compareTo(new BigDecimal("10000")) >= 0) {
            // WHEN CLI-SALDO >= 10000
            return "STANDARD";
        } else if (saldo.compareTo(BigDecimal.ZERO) >= 0) {
            // WHEN CLI-SALDO >= 0
            return "BASIC";
        } else {
            // WHEN OTHER
            return "DEUDOR";
        }
    }

    /**
     * Original COBOL with STRING/UNSTRING:
     *
     * FORMATEAR-NOMBRE-COMPLETO.
     *     STRING CLI-APELLIDO DELIMITED BY SPACE
     *            ', ' DELIMITED BY SIZE
     *            CLI-NOMBRE DELIMITED BY SPACE
     *         INTO WS-NOMBRE-COMPLETO
     *     END-STRING.
     *
     * SEPARAR-NOMBRE-COMPLETO.
     *     UNSTRING WS-NOMBRE-COMPLETO DELIMITED BY ', '
     *         INTO CLI-APELLIDO CLI-NOMBRE
     *     END-UNSTRING.
     */
    public String formatearNombreCompleto(String apellido, String nombre) {
        // STRING ... DELIMITED BY SPACE
        String apellidoTrim = apellido.trim().split("\\s+")[0];
        String nombreTrim = nombre.trim().split("\\s+")[0];

        // STRING INTO WS-NOMBRE-COMPLETO
        return apellidoTrim + ", " + nombreTrim;
    }

    public String[] separarNombreCompleto(String nombreCompleto) {
        // UNSTRING ... DELIMITED BY ', '
        String[] partes = nombreCompleto.split(", ", 2);

        if (partes.length < 2) {
            return new String[] { partes[0], "" };
        }
        return partes;
    }

    /**
     * Original COBOL with INSPECT:
     *
     * LIMPIAR-DATOS.
     *     INSPECT WS-CADENA REPLACING ALL SPACES BY ZEROS.
     *     INSPECT WS-CADENA TALLYING WS-CONTADOR FOR ALL 'A'.
     *     INSPECT WS-CADENA CONVERTING
     *         'abcdefghij' TO 'ABCDEFGHIJ'.
     */
    public String limpiarDatosReplace(String cadena) {
        // INSPECT REPLACING ALL SPACES BY ZEROS
        return cadena.replace(' ', '0');
    }

    public int limpiarDatosTally(String cadena) {
        // INSPECT TALLYING FOR ALL 'A'
        int contador = 0;
        for (char c : cadena.toCharArray()) {
            if (c == 'A') {
                contador++;
            }
        }
        return contador;
    }

    public String limpiarDatosConvert(String cadena) {
        // INSPECT CONVERTING 'abcdefghij' TO 'ABCDEFGHIJ'
        StringBuilder result = new StringBuilder();
        String from = "abcdefghij";
        String to = "ABCDEFGHIJ";

        for (char c : cadena.toCharArray()) {
            int idx = from.indexOf(c);
            result.append(idx >= 0 ? to.charAt(idx) : c);
        }
        return result.toString();
    }
}
```

### 6.2 Documentación de Reglas de Negocio

```java
package com.enterprise.migration.patterns;

/**
 * Business rule documentation pattern
 * Maintains traceability from COBOL to Java
 */
public class BusinessRuleDocumentation {

    /**
     * @cobolSource CALCPREM.CBL
     * @cobolParagraph CALCULAR-PRIMA-ANUAL
     * @cobolLineNumber 1250-1320
     * @businessRule BR-SEGUROS-001
     * @description Calcula la prima anual de seguro basada en edad y cobertura
     * @lastModified 2020-03-15
     * @modifiedBy Juan Pérez
     *
     * Original COBOL Logic:
     * ----------------------
     * CALCULAR-PRIMA-ANUAL.
     *     EVALUATE TRUE
     *         WHEN CLI-EDAD < 25
     *             COMPUTE WS-FACTOR = 1.5
     *         WHEN CLI-EDAD < 40
     *             COMPUTE WS-FACTOR = 1.0
     *         WHEN CLI-EDAD < 60
     *             COMPUTE WS-FACTOR = 1.2
     *         WHEN OTHER
     *             COMPUTE WS-FACTOR = 1.8
     *     END-EVALUATE.
     *
     *     COMPUTE WS-PRIMA-BASE = WS-COBERTURA * WS-TASA-BASE / 1000.
     *     COMPUTE WS-PRIMA-ANUAL ROUNDED =
     *         WS-PRIMA-BASE * WS-FACTOR.
     */
    public record ResultadoPrima(
        java.math.BigDecimal primaBase,
        java.math.BigDecimal factor,
        java.math.BigDecimal primaAnual
    ) {}

    public ResultadoPrima calcularPrimaAnual(int edad,
                                             java.math.BigDecimal cobertura,
                                             java.math.BigDecimal tasaBase) {
        // EVALUATE TRUE - Determine age factor
        java.math.BigDecimal factor;
        if (edad < 25) {
            // WHEN CLI-EDAD < 25: COMPUTE WS-FACTOR = 1.5
            factor = new java.math.BigDecimal("1.5");
        } else if (edad < 40) {
            // WHEN CLI-EDAD < 40: COMPUTE WS-FACTOR = 1.0
            factor = new java.math.BigDecimal("1.0");
        } else if (edad < 60) {
            // WHEN CLI-EDAD < 60: COMPUTE WS-FACTOR = 1.2
            factor = new java.math.BigDecimal("1.2");
        } else {
            // WHEN OTHER: COMPUTE WS-FACTOR = 1.8
            factor = new java.math.BigDecimal("1.8");
        }

        // COMPUTE WS-PRIMA-BASE = WS-COBERTURA * WS-TASA-BASE / 1000
        java.math.BigDecimal primaBase = cobertura
            .multiply(tasaBase)
            .divide(new java.math.BigDecimal("1000"), 2,
                    java.math.RoundingMode.HALF_UP);

        // COMPUTE WS-PRIMA-ANUAL ROUNDED = WS-PRIMA-BASE * WS-FACTOR
        java.math.BigDecimal primaAnual = primaBase
            .multiply(factor)
            .setScale(2, java.math.RoundingMode.HALF_UP);

        return new ResultadoPrima(primaBase, factor, primaAnual);
    }
}
```

---

## 7. TABLA DE REFERENCIA: VERBOS COBOL A JAVA

### 7.1 Verbos de Datos

| COBOL Verb | Java Equivalent | Example |
|------------|-----------------|---------|
| `MOVE` | Assignment `=` | `value = source;` |
| `MOVE CORRESPONDING` | Copy fields | MapStruct, BeanUtils |
| `INITIALIZE` | Constructor/reset | `obj = new Type();` |
| `ADD` | `+` or `+=` | `total += amount;` |
| `SUBTRACT` | `-` or `-=` | `balance -= debit;` |
| `MULTIPLY` | `*` or `*=` | `result *= factor;` |
| `DIVIDE` | `/` or `/=` | `avg = sum / count;` |
| `COMPUTE` | Expression | `result = a * b + c;` |
| `STRING` | `StringBuilder` | `.append()` |
| `UNSTRING` | `String.split()` | `parts = str.split(",");` |
| `INSPECT REPLACING` | `String.replace()` | `str.replace('a','b');` |
| `INSPECT TALLYING` | Loop count | Stream filter + count |
| `INSPECT CONVERTING` | Character map | Custom mapping |

### 7.2 Verbos de Control

| COBOL Verb | Java Equivalent |
|------------|-----------------|
| `PERFORM` | Method call |
| `PERFORM VARYING` | `for` loop |
| `PERFORM UNTIL` | `while` loop |
| `PERFORM TIMES` | `for (int i=0; i<n; i++)` |
| `IF ... END-IF` | `if (...) { }` |
| `EVALUATE` | `switch` expression |
| `GO TO` | (Avoid) method return |
| `STOP RUN` | `System.exit()` |
| `EXIT PROGRAM` | `return` |
| `CALL` | Method invocation |
| `CONTINUE` | Empty statement |

### 7.3 Verbos de Archivo

| COBOL Verb | Java Equivalent |
|------------|-----------------|
| `OPEN INPUT` | `Files.newBufferedReader()` |
| `OPEN OUTPUT` | `Files.newBufferedWriter()` |
| `OPEN I-O` | `RandomAccessFile` |
| `READ` | `reader.readLine()` |
| `READ NEXT` | Iterator pattern |
| `WRITE` | `writer.write()` |
| `REWRITE` | Update in place |
| `DELETE` | Remove record |
| `CLOSE` | `reader/writer.close()` |
| `START` | Position cursor |

---

## 8. REFERENCIAS Y HERRAMIENTAS

### 8.1 Herramientas de Migración Comerciales

| Herramienta | Vendor | Tipo |
|-------------|--------|------|
| Visual COBOL | Micro Focus | Compiler/IDE |
| Enterprise Developer | Micro Focus | Full platform |
| Raincode | Raincode | Compiler |
| TSRI | TSRI | Automated conversion |
| Blu Age | Blu Age | Modernization |
| LzLabs | LzLabs | Rehosting |
| Heirloom | Heirloom Computing | Conversion |
| Modern Systems | Modern Systems | Conversion |

### 8.2 Recursos de Documentación

- [Micro Focus Documentation](https://www.microfocus.com/documentation)
- [GnuCOBOL Manual](https://gnucobol.sourceforge.io/)
- [IBM COBOL for z/OS](https://www.ibm.com/docs/en/cobol-zos)
- [JNI Specification](https://docs.oracle.com/javase/8/docs/technotes/guides/jni/)

---

```
═══════════════════════════════════════════════════════════════════════════════
                    FIN DEL DOCUMENTO JAVA_03_COBOL_BRIDGE
                         ARCHAEON_CORE - v1.0.0
═══════════════════════════════════════════════════════════════════════════════
```
