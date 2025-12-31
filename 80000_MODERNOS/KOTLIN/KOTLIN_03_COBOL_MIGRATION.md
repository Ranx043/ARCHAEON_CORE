---
título: "Migración de COBOL a Kotlin - Patrones y Estrategias"
código: KOTLIN_03_COBOL_MIGRATION
versión: 1.0.0
fecha_creación: 2025-12-31
última_actualización: 2025-12-31
autor: ARCHAEON_CORE
categoría: Migración Legacy
nivel: Avanzado
prerequisitos:
  - KOTLIN_01_FUNDAMENTOS
  - KOTLIN_02_ENTERPRISE
  - Conocimiento de COBOL
  - Experiencia en sistemas mainframe
tags:
  - kotlin
  - cobol
  - migración
  - legacy-modernization
  - batch-processing
  - enterprise
estado: activo
soul_core: ARCHAEON
referencias_cobol:
  - COBOL_REFERENCE_GUIDE
  - COPYBOOK_STANDARDS
  - MAINFRAME_BATCH_PATTERNS
---

# KOTLIN_03_COBOL_MIGRATION

## Índice de Contenidos

1. [Introducción a la Migración](#introduccion)
2. [Mapeo de Tipos COBOL a Kotlin](#mapeo-tipos)
3. [Conversión de COPYBOOK a Data Classes](#copybook-conversion)
4. [Procesamiento de Archivos](#file-processing)
5. [Batch Processing con Coroutines](#batch-coroutines)
6. [Preservación de Lógica de Negocio](#business-logic)
7. [Validaciones y Reglas de Negocio](#validaciones)
8. [Casos de Estudio](#casos-estudio)
9. [Estrategias de Migración](#estrategias)
10. [Testing de Migración](#testing-migration)

---

## Introducción a la Migración {#introduccion}

La migración de COBOL a Kotlin representa la transición de décadas de lógica de negocio
crítica hacia una plataforma moderna manteniendo la integridad operacional.

### Filosofía de Migración

```
┌─────────────────────────────────────────────────────────────────────┐
│                    ESTRATEGIA DE MIGRACIÓN                         │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│   COBOL Legacy          Capa de Traducción          Kotlin Modern  │
│   ────────────          ─────────────────          ──────────────  │
│                                                                     │
│   COPYBOOKS      ──►    Data Classes       ──►    Domain Models    │
│   PERFORM        ──►    Functions          ──►    Coroutines       │
│   PICTURE        ──►    Type Mappers       ──►    Kotlin Types     │
│   FILE-CONTROL   ──►    File Processors    ──►    Streams/Flow     │
│   PROCEDURE DIV  ──►    Business Rules     ──►    Services         │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

### Principios Clave

| Principio | Descripción | Beneficio |
|-----------|-------------|-----------|
| Equivalencia Funcional | Mismo output dado mismo input | Validación automática |
| Preservación de Precisión | BigDecimal para todos los cálculos | Sin pérdida de centavos |
| Trazabilidad | Mapeo 1:1 COBOL→Kotlin documentado | Auditoría facilitada |
| Modularización | SECTION→Función, PARAGRAPH→Método | Testabilidad mejorada |
| Null Safety | Manejo explícito de valores vacíos | Menos errores runtime |

---

## Mapeo de Tipos COBOL a Kotlin {#mapeo-tipos}

### Tabla de Equivalencia PICTURE

| COBOL PICTURE | Descripción | Kotlin Type | Ejemplo |
|---------------|-------------|-------------|---------|
| `PIC 9(n)` | Entero sin signo | `Long` / `Int` | `PIC 9(9)` → `Long` |
| `PIC S9(n)` | Entero con signo | `Long` / `Int` | `PIC S9(5)` → `Int` |
| `PIC 9(n)V9(m)` | Decimal implícito | `BigDecimal` | `PIC 9(7)V99` → `BigDecimal` |
| `PIC S9(n)V9(m)` | Decimal con signo | `BigDecimal` | `PIC S9(13)V99` → `BigDecimal` |
| `PIC X(n)` | Alfanumérico | `String` | `PIC X(30)` → `String` |
| `PIC A(n)` | Solo letras | `String` | `PIC A(20)` → `String` |
| `PIC 9(n) COMP` | Binario | `Long` | `PIC 9(9) COMP` → `Long` |
| `PIC S9(n) COMP-3` | Packed decimal | `BigDecimal` | `PIC S9(15)V99 COMP-3` → `BigDecimal` |
| `PIC 9(8)` (fecha) | Fecha YYYYMMDD | `LocalDate` | → `LocalDate` |
| `PIC 9(6)` (hora) | Hora HHMMSS | `LocalTime` | → `LocalTime` |

### Clase de Conversión de Tipos

```kotlin
import java.math.BigDecimal
import java.math.RoundingMode
import java.time.LocalDate
import java.time.LocalTime
import java.time.format.DateTimeFormatter

/**
 * COBOL to Kotlin type converter
 * Handles PICTURE clause parsing and value conversion
 */
object CobolTypeConverter {

    // Numeric PICTURE patterns
    private val numericPattern = Regex("""S?9\((\d+)\)(V9\((\d+)\))?""")

    /**
     * Convert COBOL numeric string to BigDecimal
     * Handles implied decimal points (V in PICTURE)
     */
    fun toDecimal(
        cobolValue: String,
        integerDigits: Int,
        decimalDigits: Int = 0,
        signed: Boolean = false
    ): BigDecimal {
        val cleanValue = cobolValue.trim()

        // Handle sign
        val isNegative = when {
            signed && cleanValue.endsWith("-") -> true
            signed && cleanValue.endsWith("}") -> true  // EBCDIC negative
            cleanValue.startsWith("-") -> true
            else -> false
        }

        // Remove sign characters
        val numericPart = cleanValue
            .replace(Regex("[^0-9]"), "")
            .padStart(integerDigits + decimalDigits, '0')

        // Insert decimal point
        val withDecimal = if (decimalDigits > 0) {
            val insertPoint = numericPart.length - decimalDigits
            numericPart.substring(0, insertPoint) + "." + numericPart.substring(insertPoint)
        } else {
            numericPart
        }

        val result = BigDecimal(withDecimal).setScale(decimalDigits, RoundingMode.HALF_UP)
        return if (isNegative) result.negate() else result
    }

    /**
     * Convert COBOL date (YYYYMMDD or YYMMDD) to LocalDate
     */
    fun toDate(cobolDate: String, format: String = "yyyyMMdd"): LocalDate? {
        val cleanDate = cobolDate.trim()
        if (cleanDate.isEmpty() || cleanDate.all { it == '0' }) {
            return null  // COBOL low-values or zeros = no date
        }

        return try {
            val formatter = DateTimeFormatter.ofPattern(format)
            LocalDate.parse(cleanDate, formatter)
        } catch (e: Exception) {
            null
        }
    }

    /**
     * Convert COBOL time (HHMMSS) to LocalTime
     */
    fun toTime(cobolTime: String): LocalTime? {
        val cleanTime = cobolTime.trim().padStart(6, '0')
        if (cleanTime.all { it == '0' }) {
            return null
        }

        return try {
            val hour = cleanTime.substring(0, 2).toInt()
            val minute = cleanTime.substring(2, 4).toInt()
            val second = cleanTime.substring(4, 6).toInt()
            LocalTime.of(hour, minute, second)
        } catch (e: Exception) {
            null
        }
    }

    /**
     * Convert COBOL alphanumeric (PIC X) with trailing spaces to trimmed String
     */
    fun toString(cobolString: String): String = cobolString.trimEnd()

    /**
     * Convert COBOL alphanumeric to nullable String
     * Returns null if value is all spaces or low-values
     */
    fun toStringOrNull(cobolString: String): String? {
        val trimmed = cobolString.trimEnd()
        return trimmed.ifEmpty { null }
    }

    /**
     * Convert Kotlin types back to COBOL format
     */
    fun toCobolString(value: String, length: Int): String =
        value.take(length).padEnd(length)

    fun toCobolNumeric(value: BigDecimal, intDigits: Int, decDigits: Int): String {
        val scaled = value.setScale(decDigits, RoundingMode.HALF_UP)
        val strValue = scaled.abs().toPlainString().replace(".", "")
        val totalLength = intDigits + decDigits
        val result = strValue.padStart(totalLength, '0').takeLast(totalLength)
        return if (value < BigDecimal.ZERO) "-$result" else result
    }

    fun toCobolDate(date: LocalDate?): String =
        date?.format(DateTimeFormatter.BASIC_ISO_DATE) ?: "00000000"
}
```

---

## Conversión de COPYBOOK a Data Classes {#copybook-conversion}

### Ejemplo Completo de COPYBOOK

```cobol
      *---------------------------------------------------------
      * CUSTOMER-MASTER COPYBOOK
      * Used in: CUSTMAINT, CUSTRPT, CUSTBILL
      *---------------------------------------------------------
       01 CUSTOMER-MASTER-RECORD.
          05 CM-CUSTOMER-ID        PIC 9(10).
          05 CM-CUSTOMER-TYPE      PIC X(1).
             88 CM-INDIVIDUAL      VALUE 'I'.
             88 CM-CORPORATE       VALUE 'C'.
             88 CM-GOVERNMENT      VALUE 'G'.
          05 CM-NAME-INFO.
             10 CM-LAST-NAME       PIC X(30).
             10 CM-FIRST-NAME      PIC X(20).
             10 CM-MIDDLE-INIT     PIC X(1).
          05 CM-ADDRESS-INFO.
             10 CM-ADDRESS-LINE-1  PIC X(40).
             10 CM-ADDRESS-LINE-2  PIC X(40).
             10 CM-CITY            PIC X(25).
             10 CM-STATE           PIC X(2).
             10 CM-ZIP-CODE        PIC X(10).
             10 CM-COUNTRY-CODE    PIC X(3).
          05 CM-CONTACT-INFO.
             10 CM-PHONE-PRIMARY   PIC X(15).
             10 CM-PHONE-SECONDARY PIC X(15).
             10 CM-EMAIL           PIC X(50).
          05 CM-ACCOUNT-INFO.
             10 CM-ACCOUNT-NUMBER  PIC 9(12).
             10 CM-ACCOUNT-STATUS  PIC X(1).
                88 CM-ACCT-ACTIVE  VALUE 'A'.
                88 CM-ACCT-SUSPEND VALUE 'S'.
                88 CM-ACCT-CLOSED  VALUE 'C'.
             10 CM-OPEN-DATE       PIC 9(8).
             10 CM-LAST-ACTIVITY   PIC 9(8).
             10 CM-CREDIT-LIMIT    PIC S9(11)V99 COMP-3.
             10 CM-CURRENT-BALANCE PIC S9(11)V99 COMP-3.
             10 CM-AVAILABLE-CREDIT PIC S9(11)V99 COMP-3.
          05 CM-STATS.
             10 CM-TOTAL-PURCHASES  PIC S9(13)V99 COMP-3.
             10 CM-TOTAL-PAYMENTS   PIC S9(13)V99 COMP-3.
             10 CM-TRANSACTION-COUNT PIC 9(7) COMP.
          05 CM-FLAGS.
             10 CM-VIP-FLAG         PIC X(1).
             10 CM-PAPERLESS-FLAG   PIC X(1).
             10 CM-MARKETING-OPT-IN PIC X(1).
          05 FILLER                 PIC X(20).
```

### Kotlin Data Class Equivalente

```kotlin
import java.math.BigDecimal
import java.time.LocalDate

/**
 * Kotlin equivalent of CUSTOMER-MASTER-RECORD copybook
 * Preserves all field semantics with proper Kotlin types
 *
 * Original COBOL record length: 350 bytes
 */
data class CustomerMasterRecord(
    // CM-CUSTOMER-ID: PIC 9(10)
    val customerId: Long,

    // CM-CUSTOMER-TYPE: PIC X(1) with 88-levels
    val customerType: CustomerType,

    // CM-NAME-INFO group
    val nameInfo: NameInfo,

    // CM-ADDRESS-INFO group
    val addressInfo: AddressInfo,

    // CM-CONTACT-INFO group
    val contactInfo: ContactInfo,

    // CM-ACCOUNT-INFO group
    val accountInfo: AccountInfo,

    // CM-STATS group
    val stats: CustomerStats,

    // CM-FLAGS group
    val flags: CustomerFlags
) {
    companion object {
        const val COBOL_RECORD_LENGTH = 350

        /**
         * Parse from COBOL fixed-width record
         */
        fun fromCobolRecord(record: String): CustomerMasterRecord {
            require(record.length >= COBOL_RECORD_LENGTH) {
                "Record too short: ${record.length} < $COBOL_RECORD_LENGTH"
            }

            var pos = 0
            fun read(length: Int): String {
                val value = record.substring(pos, pos + length)
                pos += length
                return value
            }

            return CustomerMasterRecord(
                customerId = read(10).trim().toLong(),
                customerType = CustomerType.fromCode(read(1)),
                nameInfo = NameInfo(
                    lastName = read(30).trim(),
                    firstName = read(20).trim(),
                    middleInitial = read(1).trim().firstOrNull()
                ),
                addressInfo = AddressInfo(
                    line1 = read(40).trim(),
                    line2 = read(40).trim().ifEmpty { null },
                    city = read(25).trim(),
                    state = read(2).trim(),
                    zipCode = read(10).trim(),
                    countryCode = read(3).trim()
                ),
                contactInfo = ContactInfo(
                    phonePrimary = read(15).trim().ifEmpty { null },
                    phoneSecondary = read(15).trim().ifEmpty { null },
                    email = read(50).trim().ifEmpty { null }
                ),
                accountInfo = AccountInfo(
                    accountNumber = read(12).trim().toLong(),
                    status = AccountStatus.fromCode(read(1)),
                    openDate = CobolTypeConverter.toDate(read(8)),
                    lastActivity = CobolTypeConverter.toDate(read(8)),
                    creditLimit = CobolTypeConverter.toDecimal(read(8), 11, 2, true),
                    currentBalance = CobolTypeConverter.toDecimal(read(8), 11, 2, true),
                    availableCredit = CobolTypeConverter.toDecimal(read(8), 11, 2, true)
                ),
                stats = CustomerStats(
                    totalPurchases = CobolTypeConverter.toDecimal(read(9), 13, 2, true),
                    totalPayments = CobolTypeConverter.toDecimal(read(9), 13, 2, true),
                    transactionCount = read(4).trim().toInt()
                ),
                flags = CustomerFlags(
                    isVip = read(1) == "Y",
                    isPaperless = read(1) == "Y",
                    marketingOptIn = read(1) == "Y"
                )
            )
        }
    }

    /**
     * Convert back to COBOL fixed-width format
     */
    fun toCobolRecord(): String = buildString {
        append(customerId.toString().padStart(10, '0'))
        append(customerType.code)
        append(nameInfo.lastName.take(30).padEnd(30))
        append(nameInfo.firstName.take(20).padEnd(20))
        append(nameInfo.middleInitial?.toString() ?: " ")
        append(addressInfo.line1.take(40).padEnd(40))
        append((addressInfo.line2 ?: "").take(40).padEnd(40))
        append(addressInfo.city.take(25).padEnd(25))
        append(addressInfo.state.take(2).padEnd(2))
        append(addressInfo.zipCode.take(10).padEnd(10))
        append(addressInfo.countryCode.take(3).padEnd(3))
        append((contactInfo.phonePrimary ?: "").take(15).padEnd(15))
        append((contactInfo.phoneSecondary ?: "").take(15).padEnd(15))
        append((contactInfo.email ?: "").take(50).padEnd(50))
        append(accountInfo.accountNumber.toString().padStart(12, '0'))
        append(accountInfo.status.code)
        append(CobolTypeConverter.toCobolDate(accountInfo.openDate))
        append(CobolTypeConverter.toCobolDate(accountInfo.lastActivity))
        append(CobolTypeConverter.toCobolNumeric(accountInfo.creditLimit, 11, 2))
        append(CobolTypeConverter.toCobolNumeric(accountInfo.currentBalance, 11, 2))
        append(CobolTypeConverter.toCobolNumeric(accountInfo.availableCredit, 11, 2))
        append(CobolTypeConverter.toCobolNumeric(stats.totalPurchases, 13, 2))
        append(CobolTypeConverter.toCobolNumeric(stats.totalPayments, 13, 2))
        append(stats.transactionCount.toString().padStart(7, '0'))
        append(if (flags.isVip) "Y" else "N")
        append(if (flags.isPaperless) "Y" else "N")
        append(if (flags.marketingOptIn) "Y" else "N")
        append(" ".repeat(20))  // FILLER
    }
}

// Nested data classes for groups
data class NameInfo(
    val lastName: String,
    val firstName: String,
    val middleInitial: Char?
) {
    val fullName: String
        get() = buildString {
            append(firstName)
            middleInitial?.let { append(" $it.") }
            append(" $lastName")
        }
}

data class AddressInfo(
    val line1: String,
    val line2: String?,
    val city: String,
    val state: String,
    val zipCode: String,
    val countryCode: String
) {
    val formattedAddress: String
        get() = buildString {
            appendLine(line1)
            line2?.let { appendLine(it) }
            append("$city, $state $zipCode")
            if (countryCode != "USA") append(" $countryCode")
        }
}

data class ContactInfo(
    val phonePrimary: String?,
    val phoneSecondary: String?,
    val email: String?
)

data class AccountInfo(
    val accountNumber: Long,
    val status: AccountStatus,
    val openDate: LocalDate?,
    val lastActivity: LocalDate?,
    val creditLimit: BigDecimal,
    val currentBalance: BigDecimal,
    val availableCredit: BigDecimal
)

data class CustomerStats(
    val totalPurchases: BigDecimal,
    val totalPayments: BigDecimal,
    val transactionCount: Int
)

data class CustomerFlags(
    val isVip: Boolean,
    val isPaperless: Boolean,
    val marketingOptIn: Boolean
)

// 88-level equivalents as enums
enum class CustomerType(val code: String) {
    INDIVIDUAL("I"),
    CORPORATE("C"),
    GOVERNMENT("G");

    companion object {
        fun fromCode(code: String): CustomerType =
            values().find { it.code == code }
                ?: throw IllegalArgumentException("Unknown customer type: $code")
    }
}

enum class AccountStatus(val code: String) {
    ACTIVE("A"),
    SUSPENDED("S"),
    CLOSED("C");

    companion object {
        fun fromCode(code: String): AccountStatus =
            values().find { it.code == code }
                ?: throw IllegalArgumentException("Unknown account status: $code")
    }
}
```

---

## Procesamiento de Archivos {#file-processing}

### COBOL File Processing Pattern

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTPROC.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO CUSTFILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT REPORT-FILE ASSIGN TO CUSTRPT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-FILE
           RECORD CONTAINS 350 CHARACTERS.
       01 CUSTOMER-RECORD     PIC X(350).

       FD REPORT-FILE
           RECORD CONTAINS 132 CHARACTERS.
       01 REPORT-RECORD       PIC X(132).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS      PIC XX.
       01 WS-RPT-STATUS       PIC XX.
       01 WS-EOF-FLAG         PIC X VALUE 'N'.
          88 END-OF-FILE      VALUE 'Y'.
       01 WS-RECORD-COUNT     PIC 9(9) VALUE 0.
       01 WS-ERROR-COUNT      PIC 9(9) VALUE 0.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-FILE UNTIL END-OF-FILE
           PERFORM 3000-FINALIZE
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT CUSTOMER-FILE
           OPEN OUTPUT REPORT-FILE
           READ CUSTOMER-FILE
               AT END SET END-OF-FILE TO TRUE
           END-READ.

       2000-PROCESS-FILE.
           ADD 1 TO WS-RECORD-COUNT
           PERFORM 2100-VALIDATE-RECORD
           PERFORM 2200-WRITE-REPORT
           READ CUSTOMER-FILE
               AT END SET END-OF-FILE TO TRUE
           END-READ.

       3000-FINALIZE.
           CLOSE CUSTOMER-FILE
           CLOSE REPORT-FILE.
```

### Kotlin Equivalente con Secuencias

```kotlin
import java.io.File
import java.io.BufferedReader
import java.io.BufferedWriter
import java.nio.file.Path
import java.nio.file.Files

/**
 * Kotlin equivalent of COBOL file processing
 * Uses sequences for memory-efficient processing of large files
 */
class CustomerFileProcessor(
    private val inputPath: Path,
    private val outputPath: Path
) {
    private var recordCount = 0L
    private var errorCount = 0L

    data class ProcessingResult(
        val recordsProcessed: Long,
        val errorCount: Long,
        val outputPath: Path
    )

    fun process(): ProcessingResult {
        // Initialize counters (equivalent to 1000-INITIALIZE)
        recordCount = 0
        errorCount = 0

        // Process file using sequences (memory efficient for large files)
        Files.newBufferedReader(inputPath).use { reader ->
            Files.newBufferedWriter(outputPath).use { writer ->
                reader.lineSequence()
                    .map { line -> processRecord(line) }  // 2000-PROCESS-FILE
                    .forEach { result ->
                        when (result) {
                            is RecordResult.Success -> {
                                writeReport(writer, result.customer)
                            }
                            is RecordResult.Error -> {
                                errorCount++
                                writeErrorRecord(writer, result)
                            }
                        }
                        recordCount++
                    }
            }
        }

        return ProcessingResult(recordCount, errorCount, outputPath)
    }

    // Equivalent to 2100-VALIDATE-RECORD
    private fun processRecord(line: String): RecordResult {
        return try {
            val customer = CustomerMasterRecord.fromCobolRecord(line)
            validateCustomer(customer)
        } catch (e: Exception) {
            RecordResult.Error(line, e.message ?: "Unknown error")
        }
    }

    private fun validateCustomer(customer: CustomerMasterRecord): RecordResult {
        val errors = mutableListOf<String>()

        // Business validations
        if (customer.nameInfo.lastName.isBlank()) {
            errors.add("Last name is required")
        }

        if (customer.accountInfo.creditLimit < BigDecimal.ZERO) {
            errors.add("Credit limit cannot be negative")
        }

        if (customer.accountInfo.currentBalance > customer.accountInfo.creditLimit) {
            errors.add("Balance exceeds credit limit")
        }

        return if (errors.isEmpty()) {
            RecordResult.Success(customer)
        } else {
            RecordResult.Error(customer.toCobolRecord(), errors.joinToString("; "))
        }
    }

    // Equivalent to 2200-WRITE-REPORT
    private fun writeReport(writer: BufferedWriter, customer: CustomerMasterRecord) {
        val reportLine = formatReportLine(customer)
        writer.write(reportLine)
        writer.newLine()
    }

    private fun formatReportLine(customer: CustomerMasterRecord): String {
        return String.format(
            "%-10d %-30s %-20s %12.2f %12.2f %s",
            customer.customerId,
            customer.nameInfo.lastName,
            customer.nameInfo.firstName,
            customer.accountInfo.creditLimit,
            customer.accountInfo.currentBalance,
            customer.accountInfo.status.code
        )
    }

    private fun writeErrorRecord(writer: BufferedWriter, error: RecordResult.Error) {
        writer.write("ERROR: ${error.message}")
        writer.newLine()
        writer.write("  Record: ${error.originalRecord.take(80)}...")
        writer.newLine()
    }
}

sealed class RecordResult {
    data class Success(val customer: CustomerMasterRecord) : RecordResult()
    data class Error(val originalRecord: String, val message: String) : RecordResult()
}
```

---

## Batch Processing con Coroutines {#batch-coroutines}

### Procesamiento Paralelo de Múltiples Archivos

```kotlin
import kotlinx.coroutines.*
import kotlinx.coroutines.channels.*
import kotlinx.coroutines.flow.*
import java.nio.file.Path
import java.time.Duration
import java.time.Instant

/**
 * COBOL batch job equivalent using Kotlin Coroutines
 * Processes multiple files in parallel with controlled concurrency
 */
class BatchProcessor(
    private val config: BatchConfig
) {
    data class BatchConfig(
        val parallelism: Int = 4,
        val batchSize: Int = 1000,
        val timeout: Duration = Duration.ofHours(2),
        val checkpointInterval: Int = 10000
    )

    data class BatchResult(
        val jobId: String,
        val filesProcessed: Int,
        val totalRecords: Long,
        val successCount: Long,
        val errorCount: Long,
        val duration: Duration,
        val status: JobStatus
    )

    enum class JobStatus { COMPLETED, FAILED, TIMED_OUT, CANCELLED }

    private val logger = LoggerFactory.getLogger(BatchProcessor::class.java)

    /**
     * Main batch entry point - equivalent to JCL job step
     */
    suspend fun runBatch(
        inputFiles: List<Path>,
        outputDirectory: Path
    ): BatchResult = coroutineScope {
        val jobId = generateJobId()
        val startTime = Instant.now()

        logger.info("Starting batch job $jobId with ${inputFiles.size} files")

        try {
            withTimeout(config.timeout.toMillis()) {
                val results = processFilesInParallel(inputFiles, outputDirectory)

                val totalRecords = results.sumOf { it.recordsProcessed }
                val totalErrors = results.sumOf { it.errorCount }

                BatchResult(
                    jobId = jobId,
                    filesProcessed = results.size,
                    totalRecords = totalRecords,
                    successCount = totalRecords - totalErrors,
                    errorCount = totalErrors,
                    duration = Duration.between(startTime, Instant.now()),
                    status = if (totalErrors == 0L) JobStatus.COMPLETED else JobStatus.COMPLETED
                )
            }
        } catch (e: TimeoutCancellationException) {
            logger.error("Batch job $jobId timed out")
            BatchResult(
                jobId = jobId,
                filesProcessed = 0,
                totalRecords = 0,
                successCount = 0,
                errorCount = 0,
                duration = Duration.between(startTime, Instant.now()),
                status = JobStatus.TIMED_OUT
            )
        } catch (e: Exception) {
            logger.error("Batch job $jobId failed", e)
            BatchResult(
                jobId = jobId,
                filesProcessed = 0,
                totalRecords = 0,
                successCount = 0,
                errorCount = 0,
                duration = Duration.between(startTime, Instant.now()),
                status = JobStatus.FAILED
            )
        }
    }

    private suspend fun processFilesInParallel(
        inputFiles: List<Path>,
        outputDirectory: Path
    ): List<CustomerFileProcessor.ProcessingResult> = coroutineScope {
        // Use semaphore to limit parallelism
        val semaphore = Semaphore(config.parallelism)

        inputFiles.map { inputFile ->
            async(Dispatchers.IO) {
                semaphore.withPermit {
                    val outputFile = outputDirectory.resolve(
                        "${inputFile.fileName}.processed"
                    )
                    val processor = CustomerFileProcessor(inputFile, outputFile)
                    processor.process()
                }
            }
        }.awaitAll()
    }

    private fun generateJobId(): String =
        "BATCH-${System.currentTimeMillis()}"
}

/**
 * Flow-based streaming processor for very large files
 * Processes records as a stream without loading entire file in memory
 */
class StreamingBatchProcessor(
    private val batchSize: Int = 1000
) {
    /**
     * Process records in batches using Flow
     * Equivalent to COBOL PERFORM with VARYING for batch processing
     */
    fun processInBatches(inputPath: Path): Flow<BatchUpdate> = flow {
        var batchNumber = 0
        var processedInBatch = 0
        var totalProcessed = 0L
        var batchErrors = 0

        Files.newBufferedReader(inputPath).useLines { lines ->
            lines.chunked(batchSize).forEach { batch ->
                batchNumber++
                processedInBatch = 0
                batchErrors = 0

                batch.forEach { line ->
                    try {
                        val customer = CustomerMasterRecord.fromCobolRecord(line)
                        processCustomer(customer)
                        processedInBatch++
                    } catch (e: Exception) {
                        batchErrors++
                    }
                }

                totalProcessed += processedInBatch

                emit(BatchUpdate(
                    batchNumber = batchNumber,
                    recordsInBatch = processedInBatch,
                    errorsInBatch = batchErrors,
                    totalProcessed = totalProcessed
                ))
            }
        }
    }

    private suspend fun processCustomer(customer: CustomerMasterRecord) {
        // Simulate async processing
        delay(1)  // Would be actual business logic
    }

    data class BatchUpdate(
        val batchNumber: Int,
        val recordsInBatch: Int,
        val errorsInBatch: Int,
        val totalProcessed: Long
    )
}

// Usage example
suspend fun main() {
    val processor = BatchProcessor(
        BatchConfig(parallelism = 8, batchSize = 5000)
    )

    val inputFiles = listOf(
        Path.of("/data/input/customers_001.dat"),
        Path.of("/data/input/customers_002.dat"),
        Path.of("/data/input/customers_003.dat")
    )

    val result = processor.runBatch(
        inputFiles = inputFiles,
        outputDirectory = Path.of("/data/output")
    )

    println("""
        Batch Job: ${result.jobId}
        Status: ${result.status}
        Files Processed: ${result.filesProcessed}
        Total Records: ${result.totalRecords}
        Success: ${result.successCount}
        Errors: ${result.errorCount}
        Duration: ${result.duration.toMinutes()} minutes
    """.trimIndent())
}
```

---

## Preservación de Lógica de Negocio {#business-logic}

### COBOL Business Logic

```cobol
      *---------------------------------------------------------
      * CALCULATE-INTEREST - Calculate monthly interest
      *---------------------------------------------------------
       5000-CALCULATE-INTEREST.
           IF CM-ACCOUNT-STATUS = 'A'
               IF CM-CURRENT-BALANCE > 0
                   COMPUTE WS-INTEREST =
                       CM-CURRENT-BALANCE * WS-INTEREST-RATE / 12
                   COMPUTE WS-INTEREST ROUNDED =
                       WS-INTEREST
                   ADD WS-INTEREST TO CM-CURRENT-BALANCE
                   SUBTRACT WS-INTEREST FROM CM-AVAILABLE-CREDIT
               END-IF
           END-IF.

      *---------------------------------------------------------
      * APPLY-LATE-FEE - Apply late fee if payment overdue
      *---------------------------------------------------------
       5100-APPLY-LATE-FEE.
           IF CM-ACCOUNT-STATUS = 'A'
               IF CM-CURRENT-BALANCE > 0
                   IF WS-DAYS-OVERDUE > 30
                       COMPUTE WS-LATE-FEE =
                           CM-CURRENT-BALANCE * WS-LATE-FEE-RATE
                       IF WS-LATE-FEE < WS-MIN-LATE-FEE
                           MOVE WS-MIN-LATE-FEE TO WS-LATE-FEE
                       END-IF
                       IF WS-LATE-FEE > WS-MAX-LATE-FEE
                           MOVE WS-MAX-LATE-FEE TO WS-LATE-FEE
                       END-IF
                       ADD WS-LATE-FEE TO CM-CURRENT-BALANCE
                       SUBTRACT WS-LATE-FEE FROM CM-AVAILABLE-CREDIT
                   END-IF
               END-IF
           END-IF.

      *---------------------------------------------------------
      * DETERMINE-CREDIT-TIER - Set credit tier based on score
      *---------------------------------------------------------
       5200-DETERMINE-CREDIT-TIER.
           EVALUATE TRUE
               WHEN WS-CREDIT-SCORE >= 750
                   MOVE 'PLATINUM' TO WS-CREDIT-TIER
                   MOVE 0.0299 TO WS-INTEREST-RATE
               WHEN WS-CREDIT-SCORE >= 700
                   MOVE 'GOLD' TO WS-CREDIT-TIER
                   MOVE 0.0599 TO WS-INTEREST-RATE
               WHEN WS-CREDIT-SCORE >= 650
                   MOVE 'SILVER' TO WS-CREDIT-TIER
                   MOVE 0.0999 TO WS-INTEREST-RATE
               WHEN WS-CREDIT-SCORE >= 600
                   MOVE 'BRONZE' TO WS-CREDIT-TIER
                   MOVE 0.1499 TO WS-INTEREST-RATE
               WHEN OTHER
                   MOVE 'STANDARD' TO WS-CREDIT-TIER
                   MOVE 0.1999 TO WS-INTEREST-RATE
           END-EVALUATE.
```

### Kotlin Business Logic (Preservada)

```kotlin
import java.math.BigDecimal
import java.math.RoundingMode

/**
 * Business logic preserved from COBOL with exact same calculations
 * Comments reference original COBOL paragraph numbers
 */
class AccountBusinessRules(
    private val config: BusinessRulesConfig
) {
    data class BusinessRulesConfig(
        val minLateFee: BigDecimal = BigDecimal("25.00"),
        val maxLateFee: BigDecimal = BigDecimal("100.00"),
        val lateFeeRate: BigDecimal = BigDecimal("0.05"),
        val lateFeeThresholdDays: Int = 30
    )

    data class InterestResult(
        val interestAmount: BigDecimal,
        val newBalance: BigDecimal,
        val newAvailableCredit: BigDecimal
    )

    data class LateFeeResult(
        val feeAmount: BigDecimal,
        val newBalance: BigDecimal,
        val newAvailableCredit: BigDecimal,
        val feeApplied: Boolean
    )

    /**
     * Equivalent to 5000-CALCULATE-INTEREST
     * Calculate monthly interest on active accounts with positive balance
     */
    fun calculateInterest(
        account: AccountInfo,
        annualInterestRate: BigDecimal
    ): InterestResult? {
        // IF CM-ACCOUNT-STATUS = 'A'
        if (account.status != AccountStatus.ACTIVE) {
            return null
        }

        // IF CM-CURRENT-BALANCE > 0
        if (account.currentBalance <= BigDecimal.ZERO) {
            return null
        }

        // COMPUTE WS-INTEREST = CM-CURRENT-BALANCE * WS-INTEREST-RATE / 12
        val monthlyRate = annualInterestRate.divide(
            BigDecimal("12"),
            10,  // Scale for intermediate calculation
            RoundingMode.HALF_UP
        )

        val interest = account.currentBalance
            .multiply(monthlyRate)
            .setScale(2, RoundingMode.HALF_UP)  // ROUNDED

        // ADD WS-INTEREST TO CM-CURRENT-BALANCE
        val newBalance = account.currentBalance.add(interest)

        // SUBTRACT WS-INTEREST FROM CM-AVAILABLE-CREDIT
        val newAvailableCredit = account.availableCredit.subtract(interest)

        return InterestResult(
            interestAmount = interest,
            newBalance = newBalance,
            newAvailableCredit = newAvailableCredit
        )
    }

    /**
     * Equivalent to 5100-APPLY-LATE-FEE
     * Apply late fee if payment is overdue by more than threshold days
     */
    fun applyLateFee(
        account: AccountInfo,
        daysOverdue: Int
    ): LateFeeResult {
        // IF CM-ACCOUNT-STATUS = 'A'
        if (account.status != AccountStatus.ACTIVE) {
            return LateFeeResult(
                feeAmount = BigDecimal.ZERO,
                newBalance = account.currentBalance,
                newAvailableCredit = account.availableCredit,
                feeApplied = false
            )
        }

        // IF CM-CURRENT-BALANCE > 0
        if (account.currentBalance <= BigDecimal.ZERO) {
            return LateFeeResult(
                feeAmount = BigDecimal.ZERO,
                newBalance = account.currentBalance,
                newAvailableCredit = account.availableCredit,
                feeApplied = false
            )
        }

        // IF WS-DAYS-OVERDUE > 30
        if (daysOverdue <= config.lateFeeThresholdDays) {
            return LateFeeResult(
                feeAmount = BigDecimal.ZERO,
                newBalance = account.currentBalance,
                newAvailableCredit = account.availableCredit,
                feeApplied = false
            )
        }

        // COMPUTE WS-LATE-FEE = CM-CURRENT-BALANCE * WS-LATE-FEE-RATE
        var lateFee = account.currentBalance
            .multiply(config.lateFeeRate)
            .setScale(2, RoundingMode.HALF_UP)

        // IF WS-LATE-FEE < WS-MIN-LATE-FEE
        if (lateFee < config.minLateFee) {
            lateFee = config.minLateFee
        }

        // IF WS-LATE-FEE > WS-MAX-LATE-FEE
        if (lateFee > config.maxLateFee) {
            lateFee = config.maxLateFee
        }

        // ADD WS-LATE-FEE TO CM-CURRENT-BALANCE
        val newBalance = account.currentBalance.add(lateFee)

        // SUBTRACT WS-LATE-FEE FROM CM-AVAILABLE-CREDIT
        val newAvailableCredit = account.availableCredit.subtract(lateFee)

        return LateFeeResult(
            feeAmount = lateFee,
            newBalance = newBalance,
            newAvailableCredit = newAvailableCredit,
            feeApplied = true
        )
    }

    /**
     * Equivalent to 5200-DETERMINE-CREDIT-TIER
     * Determine credit tier and interest rate based on credit score
     */
    fun determineCreditTier(creditScore: Int): CreditTierResult {
        // EVALUATE TRUE
        return when {
            creditScore >= 750 -> CreditTierResult(
                tier = CreditTier.PLATINUM,
                interestRate = BigDecimal("0.0299")
            )
            creditScore >= 700 -> CreditTierResult(
                tier = CreditTier.GOLD,
                interestRate = BigDecimal("0.0599")
            )
            creditScore >= 650 -> CreditTierResult(
                tier = CreditTier.SILVER,
                interestRate = BigDecimal("0.0999")
            )
            creditScore >= 600 -> CreditTierResult(
                tier = CreditTier.BRONZE,
                interestRate = BigDecimal("0.1499")
            )
            else -> CreditTierResult(
                tier = CreditTier.STANDARD,
                interestRate = BigDecimal("0.1999")
            )
        }
    }

    data class CreditTierResult(
        val tier: CreditTier,
        val interestRate: BigDecimal
    )

    enum class CreditTier {
        PLATINUM, GOLD, SILVER, BRONZE, STANDARD
    }
}
```

---

## Validaciones y Reglas de Negocio {#validaciones}

```kotlin
/**
 * Validation framework for migrated COBOL business rules
 */
sealed class ValidationResult {
    object Valid : ValidationResult()
    data class Invalid(val errors: List<ValidationError>) : ValidationResult()

    fun isValid(): Boolean = this is Valid

    operator fun plus(other: ValidationResult): ValidationResult = when {
        this is Valid && other is Valid -> Valid
        this is Valid -> other
        other is Valid -> this
        else -> Invalid(
            (this as Invalid).errors + (other as Invalid).errors
        )
    }
}

data class ValidationError(
    val field: String,
    val code: String,
    val message: String
)

/**
 * Customer validation rules migrated from COBOL
 */
class CustomerValidator {

    fun validate(customer: CustomerMasterRecord): ValidationResult {
        return validateNameInfo(customer.nameInfo) +
               validateAddressInfo(customer.addressInfo) +
               validateAccountInfo(customer.accountInfo) +
               validateBusinessRules(customer)
    }

    private fun validateNameInfo(nameInfo: NameInfo): ValidationResult {
        val errors = mutableListOf<ValidationError>()

        if (nameInfo.lastName.isBlank()) {
            errors.add(ValidationError(
                field = "lastName",
                code = "NAME001",
                message = "Last name is required"
            ))
        }

        if (nameInfo.lastName.length > 30) {
            errors.add(ValidationError(
                field = "lastName",
                code = "NAME002",
                message = "Last name exceeds maximum length of 30"
            ))
        }

        if (nameInfo.firstName.isBlank()) {
            errors.add(ValidationError(
                field = "firstName",
                code = "NAME003",
                message = "First name is required"
            ))
        }

        return if (errors.isEmpty()) ValidationResult.Valid
               else ValidationResult.Invalid(errors)
    }

    private fun validateAddressInfo(addressInfo: AddressInfo): ValidationResult {
        val errors = mutableListOf<ValidationError>()

        if (addressInfo.line1.isBlank()) {
            errors.add(ValidationError(
                field = "addressLine1",
                code = "ADDR001",
                message = "Address line 1 is required"
            ))
        }

        if (addressInfo.state.length != 2) {
            errors.add(ValidationError(
                field = "state",
                code = "ADDR002",
                message = "State must be 2 characters"
            ))
        }

        if (addressInfo.countryCode == "USA" &&
            !addressInfo.zipCode.matches(Regex("\\d{5}(-\\d{4})?"))) {
            errors.add(ValidationError(
                field = "zipCode",
                code = "ADDR003",
                message = "Invalid US ZIP code format"
            ))
        }

        return if (errors.isEmpty()) ValidationResult.Valid
               else ValidationResult.Invalid(errors)
    }

    private fun validateAccountInfo(accountInfo: AccountInfo): ValidationResult {
        val errors = mutableListOf<ValidationError>()

        if (accountInfo.creditLimit < BigDecimal.ZERO) {
            errors.add(ValidationError(
                field = "creditLimit",
                code = "ACCT001",
                message = "Credit limit cannot be negative"
            ))
        }

        if (accountInfo.currentBalance > accountInfo.creditLimit) {
            errors.add(ValidationError(
                field = "currentBalance",
                code = "ACCT002",
                message = "Current balance exceeds credit limit"
            ))
        }

        return if (errors.isEmpty()) ValidationResult.Valid
               else ValidationResult.Invalid(errors)
    }

    private fun validateBusinessRules(customer: CustomerMasterRecord): ValidationResult {
        val errors = mutableListOf<ValidationError>()

        // VIP customers must have email for exclusive offers
        if (customer.flags.isVip && customer.contactInfo.email.isNullOrBlank()) {
            errors.add(ValidationError(
                field = "email",
                code = "BUS001",
                message = "VIP customers must have email on file"
            ))
        }

        // Corporate accounts require higher credit limit
        if (customer.customerType == CustomerType.CORPORATE &&
            customer.accountInfo.creditLimit < BigDecimal("10000")) {
            errors.add(ValidationError(
                field = "creditLimit",
                code = "BUS002",
                message = "Corporate accounts require minimum credit limit of 10000"
            ))
        }

        return if (errors.isEmpty()) ValidationResult.Valid
               else ValidationResult.Invalid(errors)
    }
}
```

---

## Casos de Estudio {#casos-estudio}

### Caso 1: Sistema de Facturación Mensual

```
┌────────────────────────────────────────────────────────────────┐
│              MIGRACIÓN: BILLING BATCH SYSTEM                  │
├────────────────────────────────────────────────────────────────┤
│ ANTES (COBOL)                   │ DESPUÉS (Kotlin)             │
│ ──────────────                  │ ──────────────               │
│ 15 programas COBOL              │ 3 microservicios Kotlin      │
│ 45 JCL jobs                     │ 1 orquestador Spring Batch   │
│ 8 copybooks                     │ 8 data classes               │
│ 2500 líneas por programa        │ ~400 líneas por servicio     │
│                                 │                              │
│ Tiempo ejecución: 6 horas       │ Tiempo ejecución: 45 min     │
│ Costo mensual: $15,000          │ Costo mensual: $2,000        │
├────────────────────────────────────────────────────────────────┤
│ BENEFICIOS                                                     │
│ • Reducción de 92% en tiempo de procesamiento                  │
│ • Reducción de 87% en costos operativos                        │
│ • Mantenibilidad mejorada (menos código, más legible)          │
│ • Testing automatizado (coverage > 85%)                        │
└────────────────────────────────────────────────────────────────┘
```

### Caso 2: Validación de Transacciones

```kotlin
/**
 * Migrated from COBOL TXNVAL01 program
 * Original: 3,200 lines COBOL
 * Migrated: 450 lines Kotlin
 */
class TransactionValidationService(
    private val accountRepository: AccountRepository,
    private val limitsService: LimitsService,
    private val fraudDetection: FraudDetectionService
) {
    suspend fun validateTransaction(txn: Transaction): TransactionValidationResult {
        // Parallel validation (was sequential in COBOL)
        return coroutineScope {
            val accountCheck = async { validateAccount(txn) }
            val limitsCheck = async { validateLimits(txn) }
            val fraudCheck = async { checkFraud(txn) }

            val results = awaitAll(accountCheck, limitsCheck, fraudCheck)

            results.fold(TransactionValidationResult.Approved as TransactionValidationResult) { acc, result ->
                when {
                    acc is TransactionValidationResult.Declined -> acc
                    result is TransactionValidationResult.Declined -> result
                    else -> acc
                }
            }
        }
    }

    private suspend fun validateAccount(txn: Transaction): TransactionValidationResult {
        val account = accountRepository.findByNumber(txn.accountNumber)
            ?: return TransactionValidationResult.Declined("ACCT_NOT_FOUND", "Account not found")

        if (account.status != AccountStatus.ACTIVE) {
            return TransactionValidationResult.Declined("ACCT_INACTIVE", "Account is not active")
        }

        if (txn.amount > account.availableCredit) {
            return TransactionValidationResult.Declined("INSUFFICIENT_CREDIT", "Insufficient credit")
        }

        return TransactionValidationResult.Approved
    }

    private suspend fun validateLimits(txn: Transaction): TransactionValidationResult {
        val limits = limitsService.getLimits(txn.accountNumber)

        if (txn.amount > limits.singleTransactionLimit) {
            return TransactionValidationResult.Declined(
                "EXCEEDS_TXN_LIMIT",
                "Transaction exceeds single transaction limit"
            )
        }

        val dailyTotal = limitsService.getDailyTotal(txn.accountNumber)
        if (dailyTotal + txn.amount > limits.dailyLimit) {
            return TransactionValidationResult.Declined(
                "EXCEEDS_DAILY_LIMIT",
                "Transaction would exceed daily limit"
            )
        }

        return TransactionValidationResult.Approved
    }

    private suspend fun checkFraud(txn: Transaction): TransactionValidationResult {
        val fraudScore = fraudDetection.analyze(txn)

        return if (fraudScore > 0.8) {
            TransactionValidationResult.Declined(
                "FRAUD_SUSPECTED",
                "Transaction flagged for review"
            )
        } else {
            TransactionValidationResult.Approved
        }
    }
}

sealed class TransactionValidationResult {
    object Approved : TransactionValidationResult()
    data class Declined(val code: String, val reason: String) : TransactionValidationResult()
}
```

---

## Estrategias de Migración {#estrategias}

### Enfoque Incremental (Strangler Fig Pattern)

```
Fase 1: Análisis          Fase 2: Coexistencia      Fase 3: Migración
─────────────────         ────────────────────      ─────────────────

┌─────────────┐           ┌─────────────┐           ┌─────────────┐
│   COBOL     │           │   COBOL     │           │   Kotlin    │
│   System    │           │   (Legacy)  │           │   System    │
│             │           │      │      │           │             │
│  [Program1] │           │  [Program1] │           │  [Service1] │
│  [Program2] │           │      │      │           │  [Service2] │
│  [Program3] │           │      ▼      │           │  [Service3] │
│  [Program4] │           │  [Gateway]──┼──────►    │  [Service4] │
│             │           │      ▲      │  Kotlin   │             │
│             │           │      │      │  Services │             │
│             │           │  [Program4] │           │             │
└─────────────┘           └─────────────┘           └─────────────┘

  • Inventory              • API Gateway             • Full Kotlin
  • Dependencies           • Gradual migration       • COBOL retired
  • Risk assessment        • Feature parity         • Cloud-native
```

### Tabla de Decisión de Migración

| Criterio | Mantener COBOL | Migrar a Kotlin | Reemplazar (SaaS) |
|----------|----------------|-----------------|-------------------|
| Complejidad alta | X | | |
| Sin cambios frecuentes | X | | |
| Requiere modernización | | X | |
| Nuevos canales digitales | | X | |
| Funcionalidad commodity | | | X |
| Regulación específica | X | | |
| Escasez de talento COBOL | | X | X |

---

## Testing de Migración {#testing-migration}

```kotlin
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.*
import java.math.BigDecimal

/**
 * Parallel testing: Same inputs, same outputs
 * Compare COBOL outputs with Kotlin outputs
 */
class MigrationComparisonTest {

    private val cobolOutputPath = Path.of("test-data/cobol-output")
    private val kotlinProcessor = CustomerFileProcessor(
        inputPath = Path.of("test-data/input/customers.dat"),
        outputPath = Path.of("test-data/kotlin-output/customers.out")
    )

    @Test
    fun `Kotlin output matches COBOL output for customer processing`() {
        // Process with Kotlin
        kotlinProcessor.process()

        // Compare outputs line by line
        val cobolLines = Files.readAllLines(cobolOutputPath.resolve("customers.out"))
        val kotlinLines = Files.readAllLines(Path.of("test-data/kotlin-output/customers.out"))

        assertEquals(cobolLines.size, kotlinLines.size,
            "Record counts should match")

        cobolLines.zip(kotlinLines).forEachIndexed { index, (cobol, kotlin) ->
            assertEquals(cobol, kotlin,
                "Mismatch at line $index")
        }
    }

    @Test
    fun `Interest calculation matches COBOL precision`() {
        val rules = AccountBusinessRules(AccountBusinessRules.BusinessRulesConfig())

        // Test case from COBOL test deck
        val account = AccountInfo(
            accountNumber = 123456789012,
            status = AccountStatus.ACTIVE,
            openDate = LocalDate.of(2020, 1, 15),
            lastActivity = LocalDate.of(2024, 1, 10),
            creditLimit = BigDecimal("10000.00"),
            currentBalance = BigDecimal("5432.10"),
            availableCredit = BigDecimal("4567.90")
        )

        val result = rules.calculateInterest(
            account = account,
            annualInterestRate = BigDecimal("0.1299")
        )

        // Expected from COBOL test output
        val expectedInterest = BigDecimal("58.80")  // Exact COBOL calculation
        val expectedBalance = BigDecimal("5490.90")

        assertNotNull(result)
        assertEquals(expectedInterest, result!!.interestAmount,
            "Interest amount should match COBOL calculation")
        assertEquals(expectedBalance, result.newBalance,
            "New balance should match COBOL calculation")
    }

    @Test
    fun `Credit tier determination matches COBOL logic`() {
        val rules = AccountBusinessRules(AccountBusinessRules.BusinessRulesConfig())

        // Test all tier boundaries
        val testCases = mapOf(
            750 to CreditTier.PLATINUM,
            749 to CreditTier.GOLD,
            700 to CreditTier.GOLD,
            699 to CreditTier.SILVER,
            650 to CreditTier.SILVER,
            649 to CreditTier.BRONZE,
            600 to CreditTier.BRONZE,
            599 to CreditTier.STANDARD,
            300 to CreditTier.STANDARD
        )

        testCases.forEach { (score, expectedTier) ->
            val result = rules.determineCreditTier(score)
            assertEquals(expectedTier, result.tier,
                "Score $score should map to $expectedTier")
        }
    }

    @Test
    fun `COPYBOOK parsing produces correct data class`() {
        // Raw COBOL record from production sample
        val cobolRecord = "0000000001I" +
            "DOE                           " +  // Last name (30)
            "JOHN                " +            // First name (20)
            "Q" +                                // Middle initial (1)
            "123 MAIN STREET                     " +  // Address line 1 (40)
            "APT 4B                              " +  // Address line 2 (40)
            "SPRINGFIELD              " +       // City (25)
            "IL" +                               // State (2)
            "62701     " +                       // ZIP (10)
            "USA" +                              // Country (3)
            "555-123-4567   " +                  // Phone 1 (15)
            "555-987-6543   " +                  // Phone 2 (15)
            "john.doe@example.com                                  " +  // Email (50)
            "000123456789" +                     // Account number (12)
            "A" +                                // Status (1)
            "20200115" +                         // Open date (8)
            "20240110" +                         // Last activity (8)
            "00000100000000" +                   // Credit limit (14 packed -> display)
            "00000054321000" +                   // Current balance (14)
            "00000045678900" +                   // Available credit (14)
            "000000000123456700" +               // Total purchases (18)
            "000000000098765400" +               // Total payments (18)
            "0001234" +                          // Transaction count (7)
            "YYN" +                              // Flags (3)
            "                    "               // Filler (20)

        val customer = CustomerMasterRecord.fromCobolRecord(cobolRecord)

        assertEquals(1L, customer.customerId)
        assertEquals("DOE", customer.nameInfo.lastName)
        assertEquals("JOHN", customer.nameInfo.firstName)
        assertEquals('Q', customer.nameInfo.middleInitial)
        assertEquals(CustomerType.INDIVIDUAL, customer.customerType)
        assertEquals(AccountStatus.ACTIVE, customer.accountInfo.status)
        assertTrue(customer.flags.isVip)
        assertTrue(customer.flags.isPaperless)
        assertFalse(customer.flags.marketingOptIn)
    }
}
```

---

## Referencias

| Recurso | Descripción |
|---------|-------------|
| COBOL Language Reference | IBM COBOL for z/OS Language Reference |
| Kotlin for Enterprise | kotlinlang.org/docs/server-overview.html |
| Spring Batch | docs.spring.io/spring-batch/docs/current/reference/html |
| Legacy Modernization Patterns | martinfowler.com/bliki/StranglerFigApplication.html |

---

## Conclusión

La migración de COBOL a Kotlin representa una oportunidad para:

1. **Preservar** décadas de lógica de negocio probada
2. **Modernizar** la infraestructura hacia cloud-native
3. **Mejorar** la mantenibilidad y velocidad de desarrollo
4. **Reducir** costos operativos y de talento

El enfoque clave es la **equivalencia funcional verificable**: mismo input, mismo output,
validado automáticamente en cada paso de la migración.

---

*ARCHAEON_CORE - Legacy to Modern Migration*
*Preservando el pasado, construyendo el futuro*
