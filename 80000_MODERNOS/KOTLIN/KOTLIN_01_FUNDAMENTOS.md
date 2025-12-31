---
título: "Kotlin Fundamentos para Desarrolladores Java"
código: KOTLIN_01_FUNDAMENTOS
versión: 1.0.0
fecha_creación: 2025-12-31
última_actualización: 2025-12-31
autor: ARCHAEON_CORE
categoría: Lenguajes Modernos JVM
nivel: Intermedio
prerequisitos:
  - Java SE 8+
  - Conceptos OOP
  - Familiaridad con JVM
tags:
  - kotlin
  - jvm
  - enterprise
  - null-safety
  - coroutines
estado: activo
soul_core: ARCHAEON
---

# KOTLIN_01_FUNDAMENTOS

## Índice de Contenidos

1. [Introducción a Kotlin](#introducción-a-kotlin)
2. [Sintaxis Básica y Diferencias con Java](#sintaxis-básica)
3. [Sistema de Tipos y Null Safety](#null-safety)
4. [Smart Casts y Type Checks](#smart-casts)
5. [Data Classes](#data-classes)
6. [Sealed Classes](#sealed-classes)
7. [Extension Functions](#extension-functions)
8. [Coroutines Básico](#coroutines)
9. [Interoperabilidad con Java](#interoperabilidad)
10. [Mejores Prácticas](#mejores-prácticas)

---

## Introducción a Kotlin

Kotlin es un lenguaje de programación moderno, conciso y seguro desarrollado por JetBrains.
Diseñado para interoperar completamente con Java, representa una evolución natural para
sistemas enterprise que buscan modernización sin abandonar la inversión en JVM.

### ¿Por Qué Kotlin para Enterprise?

| Característica | Java | Kotlin | Beneficio Enterprise |
|----------------|------|--------|---------------------|
| Null Safety | Anotaciones opcionales | Integrado en el tipo | Reducción de NPEs |
| Boilerplate | Alto | Mínimo | Mayor productividad |
| Coroutines | Project Loom (reciente) | Nativo desde 1.1 | Async simplificado |
| DSL Support | Limitado | Excelente | Configuración declarativa |
| Java Interop | N/A | 100% | Migración gradual |

### Comparación Histórica: COBOL a Kotlin

```
COBOL (1959)          Java (1995)           Kotlin (2011)
     |                     |                      |
  Mainframe    -->    Enterprise    -->    Modern Enterprise
  Batch Processing     OOP + JVM            Concise + Safe
  PICTURE clauses      Type System          Null Safety
  PERFORM              Loops/Streams        Coroutines
```

---

## Sintaxis Básica y Diferencias con Java

### Declaración de Variables

```kotlin
// Kotlin uses type inference - compiler determines types automatically

// Immutable variable (recommended for enterprise code)
val companyName: String = "ACME Corporation"
val employeeCount = 5000  // Type inferred as Int

// Mutable variable (use sparingly)
var currentBalance: Double = 1500000.00
var transactionCount = 0

// Constants (compile-time)
const val MAX_TRANSACTIONS = 10000
const val API_VERSION = "v2.1.0"
```

**Equivalente Java:**
```java
// Java requires explicit types or var (Java 10+)
final String companyName = "ACME Corporation";
final int employeeCount = 5000;

double currentBalance = 1500000.00;
int transactionCount = 0;

static final int MAX_TRANSACTIONS = 10000;
static final String API_VERSION = "v2.1.0";
```

### Funciones

```kotlin
// Basic function with explicit return type
fun calculateInterest(principal: Double, rate: Double, years: Int): Double {
    return principal * rate * years / 100
}

// Single-expression function (concise syntax)
fun calculateInterest(principal: Double, rate: Double, years: Int): Double =
    principal * rate * years / 100

// Default parameters (eliminates method overloading)
fun processTransaction(
    amount: Double,
    currency: String = "USD",
    validateLimit: Boolean = true
): TransactionResult {
    // Process transaction with sensible defaults
    return TransactionResult.SUCCESS
}

// Named arguments for clarity
val result = processTransaction(
    amount = 5000.00,
    currency = "EUR",
    validateLimit = false
)

// Extension of existing class without inheritance
fun String.toAccountNumber(): String {
    return this.padStart(12, '0')
}

val accountNum = "12345".toAccountNumber()  // "000000012345"
```

### Control de Flujo

```kotlin
// When expression (enhanced switch)
fun getAccountType(code: Char): String = when (code) {
    'C' -> "Checking Account"
    'S' -> "Savings Account"
    'M' -> "Money Market"
    'I' -> "Investment Account"
    else -> "Unknown Account Type"
}

// When with ranges and conditions
fun determineCustomerTier(balance: Double): String = when {
    balance >= 1_000_000 -> "Platinum"
    balance >= 500_000 -> "Gold"
    balance >= 100_000 -> "Silver"
    balance >= 10_000 -> "Bronze"
    else -> "Standard"
}

// For loops with ranges
for (i in 1..10) {
    println("Transaction $i processed")
}

// Iterate with index
val accounts = listOf("ACC001", "ACC002", "ACC003")
for ((index, account) in accounts.withIndex()) {
    println("Processing account $index: $account")
}

// Functional approach (preferred in Kotlin)
accounts.forEachIndexed { index, account ->
    println("Processing account $index: $account")
}
```

---

## Sistema de Tipos y Null Safety {#null-safety}

El sistema de null safety de Kotlin es una de sus características más importantes
para desarrollo enterprise, eliminando la clase más común de errores en producción.

### Tipos Nullable vs Non-Nullable

```kotlin
// Non-nullable type - CANNOT be null
val customerName: String = "John Doe"
// customerName = null  // COMPILATION ERROR!

// Nullable type - CAN be null (note the ?)
val middleName: String? = null  // This is allowed

// Kotlin forces you to handle null explicitly
fun processCustomer(name: String?, id: String) {
    // Safe call operator (?.)
    val length = name?.length  // Returns null if name is null

    // Elvis operator (?:) for default values
    val displayName = name ?: "Unknown Customer"

    // Safe call chain
    val upperName = name?.trim()?.uppercase()

    // Not-null assertion (!!) - USE WITH CAUTION
    // Throws NullPointerException if null
    val forcedLength = name!!.length
}
```

### Comparación con COBOL PICTURE

```cobol
* COBOL handling of empty/null values
01 CUSTOMER-RECORD.
   05 CUSTOMER-NAME     PIC X(30).
   05 CUSTOMER-MIDDLE   PIC X(20).
   05 CUSTOMER-BALANCE  PIC 9(10)V99.

* Checking for spaces (COBOL's "null")
IF CUSTOMER-MIDDLE = SPACES
   MOVE "N/A" TO DISPLAY-MIDDLE
END-IF
```

```kotlin
// Kotlin equivalent with proper null handling
data class CustomerRecord(
    val customerName: String,           // Required, never null
    val customerMiddle: String?,        // Optional, can be null
    val customerBalance: BigDecimal     // Required, never null
)

fun displayMiddleName(customer: CustomerRecord): String {
    return customer.customerMiddle ?: "N/A"
}
```

### Tabla de Operadores Null Safety

| Operador | Nombre | Uso | Ejemplo |
|----------|--------|-----|---------|
| `?.` | Safe Call | Acceso seguro a propiedades | `name?.length` |
| `?:` | Elvis | Valor por defecto si null | `name ?: "Default"` |
| `!!` | Not-null Assertion | Forzar no-null (peligroso) | `name!!.length` |
| `?.let` | Safe Let | Ejecutar bloque si no-null | `name?.let { process(it) }` |
| `as?` | Safe Cast | Cast que retorna null si falla | `obj as? String` |

### Let, Also, Apply, Run, With

```kotlin
// let - Transform and return result
val processedName = customerName?.let { name ->
    name.trim().uppercase()
}

// also - Side effects, returns original
val customer = Customer("John").also { c ->
    logger.info("Created customer: ${c.name}")
}

// apply - Configure object, returns object
val transaction = Transaction().apply {
    amount = 1000.00
    currency = "USD"
    timestamp = Instant.now()
}

// run - Execute block and return result
val result = customer.run {
    validateAccount()
    processPayment(amount)
    generateReceipt()
}

// with - Same as run but object as parameter
val summary = with(account) {
    "Account: $number, Balance: $balance, Status: $status"
}
```

---

## Smart Casts y Type Checks {#smart-casts}

Kotlin elimina la necesidad de casts explícitos después de verificar tipos.

```kotlin
// Smart cast after type check
fun processPayment(payment: Any): String {
    return when (payment) {
        is CreditCardPayment -> {
            // payment is automatically cast to CreditCardPayment
            "Processing card ending in ${payment.lastFourDigits}"
        }
        is BankTransfer -> {
            // payment is automatically cast to BankTransfer
            "Processing transfer from ${payment.sourceBank}"
        }
        is CashPayment -> {
            "Processing cash payment of ${payment.amount}"
        }
        else -> "Unknown payment type"
    }
}

// Smart cast in conditions
fun getAccountDetails(account: Any?) {
    if (account is BusinessAccount && account.isActive) {
        // account is smart cast to BusinessAccount
        println("Business: ${account.companyName}")
    }

    // Smart cast after null check
    if (account != null) {
        // account is now non-nullable in this scope
        println(account.toString())
    }
}

// Type check without smart cast
fun checkType(obj: Any): Boolean {
    return obj is String  // Returns true/false without casting
}
```

---

## Data Classes {#data-classes}

Data classes eliminan el boilerplate de POJOs/DTOs, generando automáticamente
equals(), hashCode(), toString(), copy() y componentN().

```kotlin
// Complete data class in one line
data class Customer(
    val id: Long,
    val name: String,
    val email: String,
    val accountType: AccountType = AccountType.STANDARD,
    val balance: BigDecimal = BigDecimal.ZERO,
    val createdAt: Instant = Instant.now()
)

// Equivalent Java would require ~80 lines!

// Using data class
val customer = Customer(
    id = 1001,
    name = "ACME Corporation",
    email = "contact@acme.com"
)

// Automatic copy with modifications
val premiumCustomer = customer.copy(
    accountType = AccountType.PREMIUM,
    balance = BigDecimal("100000.00")
)

// Destructuring declarations
val (id, name, email) = customer
println("Customer $id: $name ($email)")

// In collections
val customers = listOf(customer, premiumCustomer)
customers.forEach { (id, name, _) ->
    println("Processing customer $id: $name")
}
```

### Mapeo COBOL COPYBOOK a Data Class

```cobol
* COBOL COPYBOOK
01 TRANSACTION-RECORD.
   05 TXN-ID           PIC 9(12).
   05 TXN-DATE         PIC 9(8).
   05 TXN-AMOUNT       PIC S9(13)V99 COMP-3.
   05 TXN-TYPE         PIC X(2).
      88 TXN-CREDIT    VALUE 'CR'.
      88 TXN-DEBIT     VALUE 'DB'.
   05 TXN-STATUS       PIC X(1).
   05 ACCOUNT-INFO.
      10 ACCT-NUMBER   PIC 9(10).
      10 ACCT-TYPE     PIC X(1).
```

```kotlin
// Kotlin data class equivalent
data class TransactionRecord(
    val txnId: Long,                    // PIC 9(12)
    val txnDate: LocalDate,             // PIC 9(8) -> proper date type
    val txnAmount: BigDecimal,          // PIC S9(13)V99 COMP-3
    val txnType: TransactionType,       // PIC X(2) -> enum
    val txnStatus: Char,                // PIC X(1)
    val accountInfo: AccountInfo        // Nested group
)

data class AccountInfo(
    val acctNumber: Long,               // PIC 9(10)
    val acctType: Char                  // PIC X(1)
)

enum class TransactionType(val code: String) {
    CREDIT("CR"),
    DEBIT("DB");

    companion object {
        fun fromCode(code: String): TransactionType =
            values().find { it.code == code }
                ?: throw IllegalArgumentException("Unknown type: $code")
    }
}
```

---

## Sealed Classes {#sealed-classes}

Sealed classes representan jerarquías restringidas de tipos, perfectas para
modelar estados de negocio y resultados de operaciones.

```kotlin
// Sealed class for transaction results
sealed class TransactionResult {
    data class Success(
        val transactionId: String,
        val timestamp: Instant,
        val confirmationNumber: String
    ) : TransactionResult()

    data class Failure(
        val errorCode: String,
        val message: String,
        val retryable: Boolean = false
    ) : TransactionResult()

    data class Pending(
        val transactionId: String,
        val estimatedCompletion: Instant
    ) : TransactionResult()

    object Cancelled : TransactionResult()
}

// Exhaustive when (compiler ensures all cases are handled)
fun handleTransactionResult(result: TransactionResult): String = when (result) {
    is TransactionResult.Success ->
        "Transaction ${result.transactionId} completed: ${result.confirmationNumber}"
    is TransactionResult.Failure ->
        "Error ${result.errorCode}: ${result.message}"
    is TransactionResult.Pending ->
        "Transaction pending, ETA: ${result.estimatedCompletion}"
    TransactionResult.Cancelled ->
        "Transaction was cancelled"
    // No 'else' needed - all cases covered!
}

// Sealed interface (Kotlin 1.5+)
sealed interface PaymentMethod {
    data class CreditCard(
        val number: String,
        val expiry: YearMonth,
        val cvv: String
    ) : PaymentMethod

    data class BankAccount(
        val routingNumber: String,
        val accountNumber: String
    ) : PaymentMethod

    data class DigitalWallet(
        val provider: String,
        val token: String
    ) : PaymentMethod
}
```

---

## Extension Functions {#extension-functions}

Extension functions permiten añadir funcionalidad a clases existentes sin herencia.

```kotlin
// Extensions for String (common in enterprise)
fun String.toAccountNumber(): String = this.padStart(12, '0')

fun String.maskAccountNumber(): String =
    if (this.length >= 4) "****${this.takeLast(4)}" else "****"

fun String.isValidRoutingNumber(): Boolean =
    this.length == 9 && this.all { it.isDigit() }

// Extensions for BigDecimal (financial calculations)
fun BigDecimal.formatCurrency(currency: String = "USD"): String =
    "${currency} ${this.setScale(2, RoundingMode.HALF_UP)}"

fun BigDecimal.isPositive(): Boolean = this > BigDecimal.ZERO

fun BigDecimal.percentOf(percentage: BigDecimal): BigDecimal =
    this.multiply(percentage).divide(BigDecimal(100), 2, RoundingMode.HALF_UP)

// Extension for collections
fun List<Transaction>.totalAmount(): BigDecimal =
    this.map { it.amount }.fold(BigDecimal.ZERO, BigDecimal::add)

fun List<Transaction>.byStatus(status: TransactionStatus): List<Transaction> =
    this.filter { it.status == status }

// Usage examples
val accountNum = "12345".toAccountNumber()        // "000000012345"
val masked = "1234567890".maskAccountNumber()     // "****7890"
val formatted = BigDecimal("1500.5").formatCurrency()  // "USD 1500.50"

val transactions = listOf(/* ... */)
val pendingTotal = transactions
    .byStatus(TransactionStatus.PENDING)
    .totalAmount()
```

### Extension Properties

```kotlin
// Extension properties
val String.isValidEmail: Boolean
    get() = this.matches(Regex("^[A-Za-z0-9+_.-]+@(.+)$"))

val BigDecimal.isNegative: Boolean
    get() = this < BigDecimal.ZERO

val List<Transaction>.pendingCount: Int
    get() = this.count { it.status == TransactionStatus.PENDING }

// Usage
val email = "user@example.com"
if (email.isValidEmail) {
    // Process valid email
}
```

---

## Coroutines Básico {#coroutines}

Coroutines proporcionan concurrencia ligera para operaciones asíncronas.

```kotlin
import kotlinx.coroutines.*

// Basic coroutine structure
fun main() = runBlocking {
    // Launch a coroutine (fire and forget)
    launch {
        delay(1000)
        println("Background task completed")
    }

    // Async with result
    val deferred = async {
        fetchAccountBalance("ACC001")
    }
    val balance = deferred.await()

    println("Main function continues")
}

// Suspending function (can be paused and resumed)
suspend fun fetchAccountBalance(accountId: String): BigDecimal {
    // Simulates network call
    delay(500)
    return BigDecimal("10000.00")
}

// Parallel processing
suspend fun processMultipleAccounts(accountIds: List<String>): List<AccountSummary> {
    return coroutineScope {
        accountIds.map { id ->
            async {
                // Each account processed in parallel
                fetchAndProcessAccount(id)
            }
        }.awaitAll()
    }
}

// Structured concurrency with timeout
suspend fun fetchWithTimeout(accountId: String): AccountData? {
    return withTimeoutOrNull(5000) {  // 5 second timeout
        fetchAccountData(accountId)
    }
}

// Exception handling in coroutines
suspend fun safeProcessTransaction(txn: Transaction): TransactionResult {
    return try {
        withContext(Dispatchers.IO) {
            processTransaction(txn)
        }
    } catch (e: Exception) {
        TransactionResult.Failure(
            errorCode = "PROC_ERROR",
            message = e.message ?: "Unknown error",
            retryable = true
        )
    }
}
```

### Dispatchers

```kotlin
// Different dispatchers for different workloads
suspend fun processWorkloads() {
    // CPU-intensive work
    withContext(Dispatchers.Default) {
        calculateRiskScore(portfolio)
    }

    // IO-bound work (network, database, files)
    withContext(Dispatchers.IO) {
        saveToDatabase(transaction)
        sendNotification(customer)
    }

    // Main thread (UI in Android, not common in backend)
    withContext(Dispatchers.Main) {
        updateUI(result)
    }
}

// Flow for reactive streams
fun transactionFlow(accountId: String): Flow<Transaction> = flow {
    while (true) {
        val transactions = fetchNewTransactions(accountId)
        transactions.forEach { emit(it) }
        delay(1000)  // Poll every second
    }
}

// Collecting flow
suspend fun monitorTransactions() {
    transactionFlow("ACC001")
        .filter { it.amount > BigDecimal("1000") }
        .map { HighValueTransaction(it) }
        .collect { transaction ->
            alertComplianceTeam(transaction)
        }
}
```

---

## Interoperabilidad con Java {#interoperabilidad}

Kotlin tiene interoperabilidad 100% con Java, permitiendo migración gradual.

### Llamar Java desde Kotlin

```kotlin
// Using Java classes directly
import java.util.ArrayList
import java.time.LocalDateTime
import java.math.BigDecimal

val javaList = ArrayList<String>()
javaList.add("Item 1")

val now = LocalDateTime.now()
val amount = BigDecimal("1000.00")

// Java static methods
val formattedDate = String.format("%tF", now)

// Working with Java libraries
import org.apache.commons.lang3.StringUtils

val cleaned = StringUtils.trimToEmpty(input)
```

### Anotaciones para Java Interop

```kotlin
// @JvmStatic for companion object methods
class AccountService {
    companion object {
        @JvmStatic
        fun getInstance(): AccountService = AccountService()

        @JvmField
        val DEFAULT_CURRENCY = "USD"
    }
}

// @JvmOverloads for default parameters
class TransactionProcessor {
    @JvmOverloads
    fun process(
        amount: BigDecimal,
        currency: String = "USD",
        validate: Boolean = true
    ): Result {
        // Generates multiple Java overloads automatically
        return Result.success()
    }
}

// @Throws for checked exceptions
class FileProcessor {
    @Throws(IOException::class)
    fun readTransactionFile(path: String): List<Transaction> {
        // Java code calling this will see checked exception
        return Files.readAllLines(Path.of(path))
            .map { parseTransaction(it) }
    }
}

// @JvmName for property accessors
class Account {
    @get:JvmName("getAccountBalance")
    @set:JvmName("setAccountBalance")
    var balance: BigDecimal = BigDecimal.ZERO
}
```

### Nullability en Interop

```kotlin
// Platform types from Java (unknown nullability)
// Use careful null handling when calling Java

// Java method: String getCustomerName(String id)
// In Kotlin, return type is String! (platform type)

fun processJavaResult() {
    val javaService = JavaCustomerService()

    // Option 1: Treat as nullable (safe)
    val name: String? = javaService.getCustomerName("123")
    println(name?.uppercase() ?: "Unknown")

    // Option 2: Treat as non-null (risky if Java returns null)
    val name2: String = javaService.getCustomerName("123")  // May throw NPE!
}

// Best practice: Wrap Java calls
fun getCustomerNameSafe(id: String): String? {
    return try {
        javaService.getCustomerName(id)
    } catch (e: Exception) {
        null
    }
}
```

---

## Mejores Prácticas {#mejores-prácticas}

### Estilo de Código

```kotlin
// 1. Prefer val over var
val immutableList = listOf(1, 2, 3)  // Good
var mutableVar = 0  // Only when necessary

// 2. Use data classes for DTOs
data class CustomerDto(
    val id: Long,
    val name: String,
    val email: String
)

// 3. Use sealed classes for state
sealed class LoadingState<out T> {
    object Loading : LoadingState<Nothing>()
    data class Success<T>(val data: T) : LoadingState<T>()
    data class Error(val exception: Throwable) : LoadingState<Nothing>()
}

// 4. Extension functions for domain logic
fun Customer.isEligibleForPromotion(): Boolean =
    this.accountAge > 365 && this.balance > BigDecimal("5000")

// 5. Use scope functions appropriately
val customer = customerRepository.findById(id)?.let { entity ->
    CustomerDto(
        id = entity.id,
        name = entity.name,
        email = entity.email
    )
}
```

### Patrones Enterprise

```kotlin
// Repository pattern
interface CustomerRepository {
    suspend fun findById(id: Long): Customer?
    suspend fun save(customer: Customer): Customer
    suspend fun findByEmail(email: String): Customer?
}

// Service layer
class CustomerService(
    private val customerRepository: CustomerRepository,
    private val emailService: EmailService
) {
    suspend fun registerCustomer(request: RegisterRequest): RegisterResult {
        // Validate
        if (!request.email.isValidEmail) {
            return RegisterResult.InvalidEmail
        }

        // Check duplicate
        customerRepository.findByEmail(request.email)?.let {
            return RegisterResult.EmailAlreadyExists
        }

        // Create and save
        val customer = Customer(
            name = request.name,
            email = request.email
        )
        val saved = customerRepository.save(customer)

        // Send welcome email
        emailService.sendWelcome(saved)

        return RegisterResult.Success(saved)
    }
}

// Result type for operations
sealed class RegisterResult {
    data class Success(val customer: Customer) : RegisterResult()
    object InvalidEmail : RegisterResult()
    object EmailAlreadyExists : RegisterResult()
}
```

---

## Referencias y Recursos

| Recurso | URL | Descripción |
|---------|-----|-------------|
| Kotlin Docs | kotlinlang.org | Documentación oficial |
| Kotlin Koans | play.kotlinlang.org | Tutorial interactivo |
| Coroutines Guide | kotlinlang.org/docs/coroutines-guide.html | Guía completa |
| Kotlin Style Guide | kotlinlang.org/docs/coding-conventions.html | Convenciones |

---

## Siguiente: KOTLIN_02_ENTERPRISE

El siguiente documento cubre patrones enterprise avanzados:
- Spring Boot con Kotlin
- Exposed ORM
- Testing con MockK
- Gradle Kotlin DSL

---

*ARCHAEON_CORE - Evolución de Lenguajes Legacy a Moderno*
*Kotlin: El futuro de la JVM Enterprise*
