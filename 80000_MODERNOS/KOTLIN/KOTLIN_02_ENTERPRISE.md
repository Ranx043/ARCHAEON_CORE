---
título: "Kotlin Enterprise - Frameworks y Patrones"
código: KOTLIN_02_ENTERPRISE
versión: 1.0.0
fecha_creación: 2025-12-31
última_actualización: 2025-12-31
autor: ARCHAEON_CORE
categoría: Lenguajes Modernos JVM
nivel: Avanzado
prerequisitos:
  - KOTLIN_01_FUNDAMENTOS
  - Spring Framework básico
  - SQL y bases de datos
  - Testing conceptos
tags:
  - kotlin
  - spring-boot
  - ktor
  - exposed
  - mockk
  - gradle-kotlin-dsl
  - enterprise
estado: activo
soul_core: ARCHAEON
---

# KOTLIN_02_ENTERPRISE

## Índice de Contenidos

1. [Spring Boot con Kotlin](#spring-boot-kotlin)
2. [Exposed - Kotlin SQL DSL](#exposed-orm)
3. [Ktor para Web Services](#ktor)
4. [Testing con MockK](#mockk-testing)
5. [Gradle Kotlin DSL](#gradle-kotlin-dsl)
6. [Patrones Enterprise](#patrones-enterprise)
7. [Configuración y Perfiles](#configuracion)
8. [Métricas y Observabilidad](#metricas)

---

## Spring Boot con Kotlin {#spring-boot-kotlin}

Spring Boot tiene soporte oficial de primera clase para Kotlin, aprovechando
sus características para código más limpio y seguro.

### Configuración del Proyecto

```kotlin
// build.gradle.kts
plugins {
    id("org.springframework.boot") version "3.2.0"
    id("io.spring.dependency-management") version "1.1.4"
    kotlin("jvm") version "1.9.21"
    kotlin("plugin.spring") version "1.9.21"
    kotlin("plugin.jpa") version "1.9.21"
}

dependencies {
    implementation("org.springframework.boot:spring-boot-starter-web")
    implementation("org.springframework.boot:spring-boot-starter-data-jpa")
    implementation("org.springframework.boot:spring-boot-starter-validation")
    implementation("com.fasterxml.jackson.module:jackson-module-kotlin")
    implementation("org.jetbrains.kotlin:kotlin-reflect")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-reactor")

    runtimeOnly("org.postgresql:postgresql")

    testImplementation("org.springframework.boot:spring-boot-starter-test")
    testImplementation("io.mockk:mockk:1.13.8")
}

kotlin {
    compilerOptions {
        freeCompilerArgs.addAll("-Xjsr305=strict")
    }
}
```

### Entidades JPA

```kotlin
import jakarta.persistence.*
import java.math.BigDecimal
import java.time.Instant

@Entity
@Table(name = "customers")
data class Customer(
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    val id: Long = 0,

    @Column(nullable = false, length = 100)
    val name: String,

    @Column(nullable = false, unique = true)
    val email: String,

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    val accountType: AccountType = AccountType.STANDARD,

    @Column(precision = 15, scale = 2)
    val balance: BigDecimal = BigDecimal.ZERO,

    @Column(nullable = false, updatable = false)
    val createdAt: Instant = Instant.now(),

    @Column(nullable = false)
    var updatedAt: Instant = Instant.now(),

    @OneToMany(mappedBy = "customer", cascade = [CascadeType.ALL])
    val accounts: MutableList<Account> = mutableListOf()
)

@Entity
@Table(name = "accounts")
data class Account(
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    val id: Long = 0,

    @Column(nullable = false, unique = true, length = 20)
    val accountNumber: String,

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "customer_id", nullable = false)
    val customer: Customer,

    @Enumerated(EnumType.STRING)
    val status: AccountStatus = AccountStatus.ACTIVE,

    @Column(precision = 15, scale = 2)
    var balance: BigDecimal = BigDecimal.ZERO
)

enum class AccountType { STANDARD, PREMIUM, ENTERPRISE }
enum class AccountStatus { ACTIVE, SUSPENDED, CLOSED }
```

### Repositorios

```kotlin
import org.springframework.data.jpa.repository.JpaRepository
import org.springframework.data.jpa.repository.Query
import org.springframework.stereotype.Repository

@Repository
interface CustomerRepository : JpaRepository<Customer, Long> {
    fun findByEmail(email: String): Customer?

    fun findByAccountType(type: AccountType): List<Customer>

    @Query("""
        SELECT c FROM Customer c
        WHERE c.balance >= :minBalance
        AND c.accountType = :accountType
        ORDER BY c.balance DESC
    """)
    fun findHighValueCustomers(
        minBalance: BigDecimal,
        accountType: AccountType
    ): List<Customer>

    // Kotlin extension for null-safe operations
    fun existsByEmail(email: String): Boolean
}

@Repository
interface AccountRepository : JpaRepository<Account, Long> {
    fun findByAccountNumber(accountNumber: String): Account?

    fun findByCustomerId(customerId: Long): List<Account>

    @Query("SELECT SUM(a.balance) FROM Account a WHERE a.customer.id = :customerId")
    fun getTotalBalance(customerId: Long): BigDecimal?
}
```

### Servicios con Kotlin

```kotlin
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional
import java.math.BigDecimal

@Service
class CustomerService(
    private val customerRepository: CustomerRepository,
    private val accountRepository: AccountRepository,
    private val notificationService: NotificationService
) {
    fun findById(id: Long): Customer? = customerRepository.findById(id).orElse(null)

    fun findByEmail(email: String): Customer? = customerRepository.findByEmail(email)

    @Transactional
    fun createCustomer(request: CreateCustomerRequest): CustomerResult {
        // Validate email uniqueness
        customerRepository.findByEmail(request.email)?.let {
            return CustomerResult.EmailAlreadyExists(request.email)
        }

        // Create customer
        val customer = Customer(
            name = request.name,
            email = request.email,
            accountType = request.accountType ?: AccountType.STANDARD
        )

        val saved = customerRepository.save(customer)

        // Create default account
        val account = Account(
            accountNumber = generateAccountNumber(),
            customer = saved
        )
        accountRepository.save(account)

        // Send notification
        notificationService.sendWelcomeEmail(saved)

        return CustomerResult.Success(saved)
    }

    @Transactional
    fun updateBalance(customerId: Long, amount: BigDecimal): BalanceResult {
        val customer = customerRepository.findById(customerId).orElse(null)
            ?: return BalanceResult.CustomerNotFound

        val newBalance = customer.balance.add(amount)
        if (newBalance < BigDecimal.ZERO) {
            return BalanceResult.InsufficientFunds(customer.balance)
        }

        val updated = customer.copy(
            balance = newBalance,
            updatedAt = Instant.now()
        )
        customerRepository.save(updated)

        return BalanceResult.Success(newBalance)
    }

    private fun generateAccountNumber(): String =
        "ACC${System.currentTimeMillis()}"
}

// Sealed class for results
sealed class CustomerResult {
    data class Success(val customer: Customer) : CustomerResult()
    data class EmailAlreadyExists(val email: String) : CustomerResult()
    data class ValidationError(val errors: List<String>) : CustomerResult()
}

sealed class BalanceResult {
    data class Success(val newBalance: BigDecimal) : BalanceResult()
    data class InsufficientFunds(val currentBalance: BigDecimal) : BalanceResult()
    object CustomerNotFound : BalanceResult()
}
```

### Controladores REST

```kotlin
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*
import jakarta.validation.Valid

@RestController
@RequestMapping("/api/v1/customers")
class CustomerController(
    private val customerService: CustomerService
) {
    @GetMapping("/{id}")
    fun getCustomer(@PathVariable id: Long): ResponseEntity<CustomerDto> {
        return customerService.findById(id)?.let {
            ResponseEntity.ok(it.toDto())
        } ?: ResponseEntity.notFound().build()
    }

    @PostMapping
    fun createCustomer(
        @Valid @RequestBody request: CreateCustomerRequest
    ): ResponseEntity<Any> {
        return when (val result = customerService.createCustomer(request)) {
            is CustomerResult.Success ->
                ResponseEntity.status(HttpStatus.CREATED).body(result.customer.toDto())
            is CustomerResult.EmailAlreadyExists ->
                ResponseEntity.status(HttpStatus.CONFLICT)
                    .body(ErrorResponse("Email already registered: ${result.email}"))
            is CustomerResult.ValidationError ->
                ResponseEntity.badRequest()
                    .body(ErrorResponse(result.errors.joinToString(", ")))
        }
    }

    @PatchMapping("/{id}/balance")
    fun updateBalance(
        @PathVariable id: Long,
        @RequestBody request: UpdateBalanceRequest
    ): ResponseEntity<Any> {
        return when (val result = customerService.updateBalance(id, request.amount)) {
            is BalanceResult.Success ->
                ResponseEntity.ok(BalanceResponse(result.newBalance))
            is BalanceResult.InsufficientFunds ->
                ResponseEntity.badRequest()
                    .body(ErrorResponse("Insufficient funds. Current: ${result.currentBalance}"))
            BalanceResult.CustomerNotFound ->
                ResponseEntity.notFound().build()
        }
    }
}

// DTOs
data class CreateCustomerRequest(
    @field:NotBlank val name: String,
    @field:Email val email: String,
    val accountType: AccountType? = null
)

data class CustomerDto(
    val id: Long,
    val name: String,
    val email: String,
    val accountType: AccountType,
    val balance: BigDecimal
)

data class UpdateBalanceRequest(val amount: BigDecimal)
data class BalanceResponse(val balance: BigDecimal)
data class ErrorResponse(val message: String)

// Extension function for mapping
fun Customer.toDto() = CustomerDto(
    id = this.id,
    name = this.name,
    email = this.email,
    accountType = this.accountType,
    balance = this.balance
)
```

---

## Exposed - Kotlin SQL DSL {#exposed-orm}

Exposed es el ORM oficial de JetBrains para Kotlin, con DSL type-safe para SQL.

### Configuración

```kotlin
// build.gradle.kts
dependencies {
    implementation("org.jetbrains.exposed:exposed-core:0.45.0")
    implementation("org.jetbrains.exposed:exposed-dao:0.45.0")
    implementation("org.jetbrains.exposed:exposed-jdbc:0.45.0")
    implementation("org.jetbrains.exposed:exposed-java-time:0.45.0")
    implementation("com.zaxxer:HikariCP:5.1.0")
    runtimeOnly("org.postgresql:postgresql:42.7.1")
}
```

### Definición de Tablas (DSL Style)

```kotlin
import org.jetbrains.exposed.sql.*
import org.jetbrains.exposed.sql.javatime.timestamp
import java.math.BigDecimal

// Table definitions using DSL
object Customers : Table("customers") {
    val id = long("id").autoIncrement()
    val name = varchar("name", 100)
    val email = varchar("email", 255).uniqueIndex()
    val accountType = enumerationByName<AccountType>("account_type", 20)
    val balance = decimal("balance", 15, 2).default(BigDecimal.ZERO)
    val createdAt = timestamp("created_at").defaultExpression(CurrentTimestamp())
    val updatedAt = timestamp("updated_at").defaultExpression(CurrentTimestamp())

    override val primaryKey = PrimaryKey(id)
}

object Accounts : Table("accounts") {
    val id = long("id").autoIncrement()
    val accountNumber = varchar("account_number", 20).uniqueIndex()
    val customerId = long("customer_id").references(Customers.id)
    val status = enumerationByName<AccountStatus>("status", 20)
    val balance = decimal("balance", 15, 2).default(BigDecimal.ZERO)

    override val primaryKey = PrimaryKey(id)
}

object Transactions : Table("transactions") {
    val id = long("id").autoIncrement()
    val accountId = long("account_id").references(Accounts.id)
    val amount = decimal("amount", 15, 2)
    val type = enumerationByName<TransactionType>("type", 10)
    val description = varchar("description", 500).nullable()
    val timestamp = timestamp("timestamp").defaultExpression(CurrentTimestamp())
    val referenceNumber = varchar("reference_number", 50).uniqueIndex()

    override val primaryKey = PrimaryKey(id)

    // Composite index for common queries
    init {
        index(false, accountId, timestamp)
    }
}

enum class TransactionType { CREDIT, DEBIT, TRANSFER }
```

### Operaciones CRUD con DSL

```kotlin
import org.jetbrains.exposed.sql.*
import org.jetbrains.exposed.sql.transactions.transaction
import java.time.Instant

class CustomerExposedRepository {

    fun createCustomer(name: String, email: String, type: AccountType): Long {
        return transaction {
            Customers.insertAndGetId {
                it[Customers.name] = name
                it[Customers.email] = email
                it[accountType] = type
            }.value
        }
    }

    fun findById(id: Long): CustomerData? {
        return transaction {
            Customers.select { Customers.id eq id }
                .map { it.toCustomerData() }
                .singleOrNull()
        }
    }

    fun findByEmail(email: String): CustomerData? {
        return transaction {
            Customers.select { Customers.email eq email }
                .map { it.toCustomerData() }
                .singleOrNull()
        }
    }

    fun findHighValueCustomers(minBalance: BigDecimal): List<CustomerData> {
        return transaction {
            Customers
                .select { Customers.balance greaterEq minBalance }
                .orderBy(Customers.balance, SortOrder.DESC)
                .map { it.toCustomerData() }
        }
    }

    fun updateBalance(id: Long, newBalance: BigDecimal): Boolean {
        return transaction {
            Customers.update({ Customers.id eq id }) {
                it[balance] = newBalance
                it[updatedAt] = Instant.now()
            } > 0
        }
    }

    fun deleteCustomer(id: Long): Boolean {
        return transaction {
            Customers.deleteWhere { Customers.id eq id } > 0
        }
    }

    private fun ResultRow.toCustomerData() = CustomerData(
        id = this[Customers.id],
        name = this[Customers.name],
        email = this[Customers.email],
        accountType = this[Customers.accountType],
        balance = this[Customers.balance],
        createdAt = this[Customers.createdAt]
    )
}

data class CustomerData(
    val id: Long,
    val name: String,
    val email: String,
    val accountType: AccountType,
    val balance: BigDecimal,
    val createdAt: Instant
)
```

### Consultas Complejas con Joins

```kotlin
class TransactionQueryService {

    // Join query with aggregation
    fun getCustomerTransactionSummary(customerId: Long): TransactionSummary? {
        return transaction {
            (Customers innerJoin Accounts innerJoin Transactions)
                .slice(
                    Customers.name,
                    Transactions.amount.sum(),
                    Transactions.id.count()
                )
                .select { Customers.id eq customerId }
                .groupBy(Customers.id, Customers.name)
                .map { row ->
                    TransactionSummary(
                        customerName = row[Customers.name],
                        totalAmount = row[Transactions.amount.sum()] ?: BigDecimal.ZERO,
                        transactionCount = row[Transactions.id.count()]
                    )
                }
                .singleOrNull()
        }
    }

    // Subquery example
    fun findCustomersWithRecentTransactions(days: Int): List<CustomerData> {
        val cutoffDate = Instant.now().minus(days.toLong(), ChronoUnit.DAYS)

        return transaction {
            val customersWithTransactions = Accounts
                .innerJoin(Transactions)
                .slice(Accounts.customerId)
                .select { Transactions.timestamp greater cutoffDate }
                .withDistinct()

            Customers
                .select { Customers.id inSubQuery customersWithTransactions }
                .map { it.toCustomerData() }
        }
    }

    // Batch insert for performance
    fun insertTransactions(transactions: List<TransactionInput>): Int {
        return transaction {
            Transactions.batchInsert(transactions) { txn ->
                this[Transactions.accountId] = txn.accountId
                this[Transactions.amount] = txn.amount
                this[Transactions.type] = txn.type
                this[Transactions.description] = txn.description
                this[Transactions.referenceNumber] = txn.referenceNumber
            }.size
        }
    }
}

data class TransactionSummary(
    val customerName: String,
    val totalAmount: BigDecimal,
    val transactionCount: Long
)

data class TransactionInput(
    val accountId: Long,
    val amount: BigDecimal,
    val type: TransactionType,
    val description: String?,
    val referenceNumber: String
)
```

---

## Ktor para Web Services {#ktor}

Ktor es el framework web de JetBrains, diseñado para Kotlin con coroutines nativos.

### Configuración Básica

```kotlin
// build.gradle.kts
plugins {
    kotlin("jvm") version "1.9.21"
    id("io.ktor.plugin") version "2.3.7"
    kotlin("plugin.serialization") version "1.9.21"
}

dependencies {
    implementation("io.ktor:ktor-server-core")
    implementation("io.ktor:ktor-server-netty")
    implementation("io.ktor:ktor-server-content-negotiation")
    implementation("io.ktor:ktor-serialization-kotlinx-json")
    implementation("io.ktor:ktor-server-status-pages")
    implementation("io.ktor:ktor-server-auth")
    implementation("io.ktor:ktor-server-auth-jwt")
    implementation("io.ktor:ktor-server-cors")
    implementation("io.ktor:ktor-server-call-logging")

    implementation("ch.qos.logback:logback-classic:1.4.11")

    testImplementation("io.ktor:ktor-server-tests")
    testImplementation("io.ktor:ktor-client-content-negotiation")
}

ktor {
    fatJar {
        archiveFileName.set("enterprise-api.jar")
    }
}
```

### Aplicación Ktor

```kotlin
import io.ktor.server.application.*
import io.ktor.server.engine.*
import io.ktor.server.netty.*
import io.ktor.server.routing.*
import io.ktor.server.plugins.contentnegotiation.*
import io.ktor.serialization.kotlinx.json.*
import kotlinx.serialization.json.Json

fun main() {
    embeddedServer(Netty, port = 8080, module = Application::module).start(wait = true)
}

fun Application.module() {
    // Install plugins
    configureSerialization()
    configureRouting()
    configureStatusPages()
    configureAuthentication()
    configureLogging()
}

fun Application.configureSerialization() {
    install(ContentNegotiation) {
        json(Json {
            prettyPrint = true
            isLenient = true
            ignoreUnknownKeys = true
        })
    }
}

fun Application.configureRouting() {
    val customerService = CustomerService()
    val accountService = AccountService()

    routing {
        route("/api/v1") {
            customerRoutes(customerService)
            accountRoutes(accountService)
            healthRoutes()
        }
    }
}
```

### Rutas y Handlers

```kotlin
import io.ktor.http.*
import io.ktor.server.application.*
import io.ktor.server.request.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import kotlinx.serialization.Serializable

fun Route.customerRoutes(customerService: CustomerService) {
    route("/customers") {
        // GET /api/v1/customers
        get {
            val customers = customerService.findAll()
            call.respond(customers.map { it.toResponse() })
        }

        // GET /api/v1/customers/{id}
        get("/{id}") {
            val id = call.parameters["id"]?.toLongOrNull()
                ?: return@get call.respond(HttpStatusCode.BadRequest, ErrorResponse("Invalid ID"))

            customerService.findById(id)?.let { customer ->
                call.respond(customer.toResponse())
            } ?: call.respond(HttpStatusCode.NotFound, ErrorResponse("Customer not found"))
        }

        // POST /api/v1/customers
        post {
            val request = call.receive<CreateCustomerRequest>()

            when (val result = customerService.create(request)) {
                is Result.Success -> call.respond(HttpStatusCode.Created, result.data.toResponse())
                is Result.Error -> call.respond(HttpStatusCode.BadRequest, ErrorResponse(result.message))
            }
        }

        // PUT /api/v1/customers/{id}
        put("/{id}") {
            val id = call.parameters["id"]?.toLongOrNull()
                ?: return@put call.respond(HttpStatusCode.BadRequest, ErrorResponse("Invalid ID"))

            val request = call.receive<UpdateCustomerRequest>()

            when (val result = customerService.update(id, request)) {
                is Result.Success -> call.respond(result.data.toResponse())
                is Result.Error -> call.respond(HttpStatusCode.BadRequest, ErrorResponse(result.message))
            }
        }

        // DELETE /api/v1/customers/{id}
        delete("/{id}") {
            val id = call.parameters["id"]?.toLongOrNull()
                ?: return@delete call.respond(HttpStatusCode.BadRequest, ErrorResponse("Invalid ID"))

            if (customerService.delete(id)) {
                call.respond(HttpStatusCode.NoContent)
            } else {
                call.respond(HttpStatusCode.NotFound, ErrorResponse("Customer not found"))
            }
        }
    }
}

// DTOs with kotlinx.serialization
@Serializable
data class CreateCustomerRequest(
    val name: String,
    val email: String,
    val accountType: String = "STANDARD"
)

@Serializable
data class UpdateCustomerRequest(
    val name: String? = null,
    val email: String? = null,
    val accountType: String? = null
)

@Serializable
data class CustomerResponse(
    val id: Long,
    val name: String,
    val email: String,
    val accountType: String,
    val balance: String
)

@Serializable
data class ErrorResponse(val message: String)

sealed class Result<out T> {
    data class Success<T>(val data: T) : Result<T>()
    data class Error(val message: String) : Result<Nothing>()
}
```

---

## Testing con MockK {#mockk-testing}

MockK es la librería de mocking nativa para Kotlin, con soporte para coroutines.

### Configuración

```kotlin
// build.gradle.kts
dependencies {
    testImplementation("io.mockk:mockk:1.13.8")
    testImplementation("org.junit.jupiter:junit-jupiter:5.10.1")
    testImplementation("org.assertj:assertj-core:3.24.2")
    testImplementation("org.jetbrains.kotlinx:kotlinx-coroutines-test:1.7.3")
}
```

### Tests Unitarios con MockK

```kotlin
import io.mockk.*
import io.mockk.impl.annotations.MockK
import io.mockk.impl.annotations.InjectMockKs
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import java.math.BigDecimal
import kotlin.test.assertEquals
import kotlin.test.assertIs

@ExtendWith(MockKExtension::class)
class CustomerServiceTest {

    @MockK
    private lateinit var customerRepository: CustomerRepository

    @MockK
    private lateinit var accountRepository: AccountRepository

    @MockK
    private lateinit var notificationService: NotificationService

    @InjectMockKs
    private lateinit var customerService: CustomerService

    @BeforeEach
    fun setup() {
        MockKAnnotations.init(this)
    }

    @Test
    fun `createCustomer should succeed when email is unique`() {
        // Given
        val request = CreateCustomerRequest(
            name = "John Doe",
            email = "john@example.com",
            accountType = AccountType.STANDARD
        )

        val savedCustomer = Customer(
            id = 1L,
            name = request.name,
            email = request.email
        )

        every { customerRepository.findByEmail(request.email) } returns null
        every { customerRepository.save(any()) } returns savedCustomer
        every { accountRepository.save(any()) } returns mockk()
        every { notificationService.sendWelcomeEmail(any()) } just Runs

        // When
        val result = customerService.createCustomer(request)

        // Then
        assertIs<CustomerResult.Success>(result)
        assertEquals(savedCustomer.id, result.customer.id)

        verify(exactly = 1) { customerRepository.findByEmail(request.email) }
        verify(exactly = 1) { customerRepository.save(any()) }
        verify(exactly = 1) { notificationService.sendWelcomeEmail(savedCustomer) }
    }

    @Test
    fun `createCustomer should fail when email already exists`() {
        // Given
        val request = CreateCustomerRequest(
            name = "John Doe",
            email = "existing@example.com"
        )

        every { customerRepository.findByEmail(request.email) } returns mockk()

        // When
        val result = customerService.createCustomer(request)

        // Then
        assertIs<CustomerResult.EmailAlreadyExists>(result)
        assertEquals(request.email, result.email)

        verify(exactly = 1) { customerRepository.findByEmail(request.email) }
        verify(exactly = 0) { customerRepository.save(any()) }
    }

    @Test
    fun `updateBalance should return InsufficientFunds when balance would be negative`() {
        // Given
        val customerId = 1L
        val currentBalance = BigDecimal("100.00")
        val amount = BigDecimal("-150.00")

        val customer = Customer(
            id = customerId,
            name = "Test",
            email = "test@test.com",
            balance = currentBalance
        )

        every { customerRepository.findById(customerId) } returns java.util.Optional.of(customer)

        // When
        val result = customerService.updateBalance(customerId, amount)

        // Then
        assertIs<BalanceResult.InsufficientFunds>(result)
        assertEquals(currentBalance, result.currentBalance)
    }
}
```

### Testing Coroutines

```kotlin
import kotlinx.coroutines.test.runTest
import io.mockk.coEvery
import io.mockk.coVerify

class AsyncServiceTest {

    @MockK
    private lateinit var externalApiClient: ExternalApiClient

    private lateinit var asyncService: AsyncService

    @Test
    fun `fetchData should return transformed data`() = runTest {
        // Given
        val rawData = RawData("test", 100)
        coEvery { externalApiClient.fetchData(any()) } returns rawData

        // When
        val result = asyncService.fetchAndTransform("key123")

        // Then
        assertEquals("TEST", result.name)
        assertEquals(100, result.value)

        coVerify(exactly = 1) { externalApiClient.fetchData("key123") }
    }

    @Test
    fun `fetchData should handle timeout`() = runTest {
        // Given
        coEvery { externalApiClient.fetchData(any()) } coAnswers {
            delay(10_000)  // Simulate slow response
            throw TimeoutException("Request timed out")
        }

        // When/Then
        val result = asyncService.fetchAndTransform("key123")
        assertNull(result)
    }
}
```

### Mocking Objects y Companions

```kotlin
class UtilityClassTest {

    @Test
    fun `test static-like functions`() {
        // Mock object
        mockkObject(DateUtils)
        every { DateUtils.getCurrentDate() } returns LocalDate.of(2024, 1, 15)

        // Test
        val result = someService.processWithDate()
        assertEquals("2024-01-15", result.dateString)

        unmockkObject(DateUtils)
    }

    @Test
    fun `test companion object`() {
        mockkObject(Customer.Companion)
        every { Customer.createDefault() } returns Customer(id = 999, name = "Mock")

        val customer = Customer.createDefault()
        assertEquals(999, customer.id)

        unmockkObject(Customer.Companion)
    }
}
```

---

## Gradle Kotlin DSL {#gradle-kotlin-dsl}

Gradle Kotlin DSL proporciona type-safety y autocompletado para builds.

### Estructura Multi-Módulo

```kotlin
// settings.gradle.kts (root)
rootProject.name = "enterprise-platform"

include(
    ":common",
    ":domain",
    ":infrastructure",
    ":api",
    ":batch-processor"
)

dependencyResolutionManagement {
    versionCatalogs {
        create("libs") {
            version("kotlin", "1.9.21")
            version("spring-boot", "3.2.0")
            version("exposed", "0.45.0")
            version("ktor", "2.3.7")

            library("kotlin-stdlib", "org.jetbrains.kotlin", "kotlin-stdlib").versionRef("kotlin")
            library("kotlin-coroutines", "org.jetbrains.kotlinx", "kotlinx-coroutines-core").version("1.7.3")

            plugin("kotlin-jvm", "org.jetbrains.kotlin.jvm").versionRef("kotlin")
            plugin("kotlin-spring", "org.jetbrains.kotlin.plugin.spring").versionRef("kotlin")
        }
    }
}
```

```kotlin
// build.gradle.kts (root)
plugins {
    alias(libs.plugins.kotlin.jvm) apply false
    alias(libs.plugins.kotlin.spring) apply false
}

allprojects {
    group = "com.enterprise"
    version = "1.0.0"

    repositories {
        mavenCentral()
    }
}

subprojects {
    apply(plugin = "org.jetbrains.kotlin.jvm")

    dependencies {
        implementation(libs.kotlin.stdlib)
    }

    tasks.withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
        kotlinOptions {
            freeCompilerArgs = listOf("-Xjsr305=strict")
            jvmTarget = "17"
        }
    }

    tasks.withType<Test> {
        useJUnitPlatform()
    }
}
```

### Módulo con Dependencias

```kotlin
// api/build.gradle.kts
plugins {
    alias(libs.plugins.kotlin.spring)
    id("org.springframework.boot") version "3.2.0"
}

dependencies {
    implementation(project(":domain"))
    implementation(project(":infrastructure"))

    implementation("org.springframework.boot:spring-boot-starter-web")
    implementation("org.springframework.boot:spring-boot-starter-actuator")

    testImplementation("org.springframework.boot:spring-boot-starter-test")
    testImplementation("io.mockk:mockk:1.13.8")
}

springBoot {
    mainClass.set("com.enterprise.api.ApplicationKt")
}
```

### Custom Tasks

```kotlin
// build.gradle.kts
tasks.register<Copy>("copyConfigs") {
    from("src/main/resources/configs")
    into("$buildDir/configs")
    include("*.yaml", "*.properties")
}

tasks.register("generateReport") {
    group = "reporting"
    description = "Generates project report"

    doLast {
        val report = buildString {
            appendLine("=== Project Report ===")
            appendLine("Name: ${project.name}")
            appendLine("Version: ${project.version}")
            appendLine("Modules: ${subprojects.map { it.name }}")
        }
        file("$buildDir/report.txt").writeText(report)
    }
}

tasks.named("build") {
    dependsOn("copyConfigs")
    finalizedBy("generateReport")
}
```

---

## Patrones Enterprise {#patrones-enterprise}

### Result Pattern

```kotlin
sealed class Result<out S, out E> {
    data class Success<S>(val value: S) : Result<S, Nothing>()
    data class Failure<E>(val error: E) : Result<Nothing, E>()

    inline fun <R> map(transform: (S) -> R): Result<R, E> = when (this) {
        is Success -> Success(transform(value))
        is Failure -> this
    }

    inline fun <R> flatMap(transform: (S) -> Result<R, @UnsafeVariance E>): Result<R, E> =
        when (this) {
            is Success -> transform(value)
            is Failure -> this
        }

    inline fun onSuccess(action: (S) -> Unit): Result<S, E> {
        if (this is Success) action(value)
        return this
    }

    inline fun onFailure(action: (E) -> Unit): Result<S, E> {
        if (this is Failure) action(error)
        return this
    }

    fun getOrNull(): S? = (this as? Success)?.value

    fun getOrThrow(): S = when (this) {
        is Success -> value
        is Failure -> throw IllegalStateException("Result is Failure: $error")
    }
}

// Usage
fun processOrder(order: Order): Result<OrderConfirmation, OrderError> {
    return validateOrder(order)
        .flatMap { validateInventory(it) }
        .flatMap { calculateTotal(it) }
        .flatMap { chargePayment(it) }
        .map { createConfirmation(it) }
}
```

### Repository Pattern con Genéricos

```kotlin
interface Repository<T, ID> {
    suspend fun findById(id: ID): T?
    suspend fun findAll(): List<T>
    suspend fun save(entity: T): T
    suspend fun delete(id: ID): Boolean
    suspend fun exists(id: ID): Boolean
}

abstract class BaseRepository<T, ID> : Repository<T, ID> {
    protected abstract val tableName: String

    override suspend fun exists(id: ID): Boolean = findById(id) != null

    protected fun logOperation(operation: String, id: ID?) {
        logger.debug("$operation on $tableName with id: $id")
    }
}

class CustomerRepositoryImpl(
    private val database: Database
) : BaseRepository<Customer, Long>() {

    override val tableName = "customers"

    override suspend fun findById(id: Long): Customer? {
        logOperation("findById", id)
        return transaction(database) {
            Customers.select { Customers.id eq id }
                .map { it.toCustomer() }
                .singleOrNull()
        }
    }

    // ... other implementations
}
```

---

## Configuración y Perfiles {#configuracion}

```kotlin
// application.yaml
spring:
  application:
    name: enterprise-api
  profiles:
    active: ${SPRING_PROFILES_ACTIVE:local}

---
spring:
  config:
    activate:
      on-profile: local
  datasource:
    url: jdbc:postgresql://localhost:5432/enterprise
    username: developer
    password: dev123

---
spring:
  config:
    activate:
      on-profile: production
  datasource:
    url: jdbc:postgresql://${DB_HOST}:${DB_PORT}/${DB_NAME}
    username: ${DB_USER}
    password: ${DB_PASSWORD}
```

```kotlin
// Type-safe configuration with Kotlin
@ConfigurationProperties(prefix = "app.features")
data class FeatureFlags(
    val enableNewPaymentFlow: Boolean = false,
    val maxRetryAttempts: Int = 3,
    val batchSize: Int = 100,
    val processingTimeout: Duration = Duration.ofMinutes(5)
)

@Configuration
@EnableConfigurationProperties(FeatureFlags::class)
class AppConfiguration(
    private val featureFlags: FeatureFlags
) {
    @Bean
    fun transactionProcessor(): TransactionProcessor {
        return TransactionProcessor(
            maxRetries = featureFlags.maxRetryAttempts,
            batchSize = featureFlags.batchSize
        )
    }
}
```

---

## Métricas y Observabilidad {#metricas}

```kotlin
import io.micrometer.core.instrument.MeterRegistry
import io.micrometer.core.instrument.Timer
import org.springframework.stereotype.Component

@Component
class MetricsService(private val meterRegistry: MeterRegistry) {

    private val transactionCounter = meterRegistry.counter("transactions.total")
    private val transactionTimer = meterRegistry.timer("transactions.duration")

    fun recordTransaction(type: String) {
        meterRegistry.counter("transactions.by_type", "type", type).increment()
        transactionCounter.increment()
    }

    fun <T> measureTransaction(block: () -> T): T {
        return transactionTimer.recordCallable(block)!!
    }

    fun recordBalance(customerId: Long, balance: Double) {
        meterRegistry.gauge(
            "customer.balance",
            listOf(Tag.of("customer_id", customerId.toString())),
            balance
        )
    }
}

// Usage in service
@Service
class TransactionService(
    private val metricsService: MetricsService,
    private val transactionRepository: TransactionRepository
) {
    fun processTransaction(txn: Transaction): TransactionResult {
        return metricsService.measureTransaction {
            val result = transactionRepository.save(txn)
            metricsService.recordTransaction(txn.type.name)
            TransactionResult.Success(result)
        }
    }
}
```

---

## Referencias

| Recurso | Descripción |
|---------|-------------|
| Spring Kotlin Docs | docs.spring.io/spring-framework/reference/languages/kotlin.html |
| Exposed Wiki | github.com/JetBrains/Exposed/wiki |
| Ktor Documentation | ktor.io/docs |
| MockK Guide | mockk.io |
| Gradle Kotlin DSL | docs.gradle.org/current/userguide/kotlin_dsl.html |

---

## Siguiente: KOTLIN_03_COBOL_MIGRATION

El siguiente documento cubre migración de COBOL a Kotlin:
- Mapeo de tipos COBOL
- Procesamiento batch con coroutines
- Preservación de lógica de negocio

---

*ARCHAEON_CORE - Enterprise Kotlin Patterns*
*Modernización sin Compromiso*
