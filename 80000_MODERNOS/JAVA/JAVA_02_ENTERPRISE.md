---
título: "Java Enterprise - Spring Boot para Migración COBOL"
código: JAVA-02-ENT
versión: 1.0.0
fecha_creación: 2025-12-31
última_actualización: 2025-12-31
autor: ARCHAEON_CORE
dominio: LENGUAJES_MODERNOS
especialización: JAVA_ENTERPRISE
dependencias:
  - Spring Boot 3.x
  - Spring Data JPA
  - Hibernate 6.x
  - Maven/Gradle
tags:
  - java
  - spring-boot
  - enterprise
  - cobol-migration
  - microservices
  - jpa
nivel_complejidad: AVANZADO
estado: ACTIVO
---

# ═══════════════════════════════════════════════════════════════════════════════
# JAVA 02: ENTERPRISE - SPRING BOOT PARA MIGRACIÓN COBOL
# ARCHAEON_CORE - Sistema de Documentación de Lenguajes Legacy
# ═══════════════════════════════════════════════════════════════════════════════

## 1. INTRODUCCIÓN A SPRING BOOT ENTERPRISE

### 1.1 Spring Boot como Destino de Migración COBOL

Spring Boot representa la plataforma enterprise más adoptada para la migración
de sistemas mainframe COBOL. Sus características de inyección de dependencias,
gestión transaccional y arquitectura modular facilitan la traducción de
programas COBOL a servicios empresariales modernos.

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                ARQUITECTURA COBOL vs SPRING BOOT                            │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│   MAINFRAME COBOL                        SPRING BOOT                        │
│   ┌─────────────────────┐                ┌─────────────────────┐           │
│   │ CICS/IMS            │                │ REST Controller     │           │
│   │ Transaction Manager │    ═══▶        │ Spring MVC          │           │
│   └─────────┬───────────┘                └─────────┬───────────┘           │
│             │                                      │                        │
│   ┌─────────▼───────────┐                ┌─────────▼───────────┐           │
│   │ COBOL Programs      │                │ Service Layer       │           │
│   │ Business Logic      │    ═══▶        │ @Service beans      │           │
│   └─────────┬───────────┘                └─────────┬───────────┘           │
│             │                                      │                        │
│   ┌─────────▼───────────┐                ┌─────────▼───────────┐           │
│   │ DB2/VSAM            │                │ Spring Data JPA     │           │
│   │ Data Access         │    ═══▶        │ Repositories        │           │
│   └─────────────────────┘                └─────────────────────┘           │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### 1.2 Configuración Inicial del Proyecto

**Maven pom.xml:**

```xml
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
         https://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <parent>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-parent</artifactId>
        <version>3.2.0</version>
        <relativePath/>
    </parent>

    <groupId>com.enterprise</groupId>
    <artifactId>cobol-migration</artifactId>
    <version>1.0.0-SNAPSHOT</version>
    <name>COBOL Migration Project</name>
    <description>Enterprise COBOL to Spring Boot Migration</description>

    <properties>
        <java.version>17</java.version>
    </properties>

    <dependencies>
        <!-- Spring Boot Starters -->
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-web</artifactId>
        </dependency>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-data-jpa</artifactId>
        </dependency>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-validation</artifactId>
        </dependency>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-actuator</artifactId>
        </dependency>

        <!-- Database -->
        <dependency>
            <groupId>org.postgresql</groupId>
            <artifactId>postgresql</artifactId>
            <scope>runtime</scope>
        </dependency>
        <dependency>
            <groupId>com.h2database</groupId>
            <artifactId>h2</artifactId>
            <scope>test</scope>
        </dependency>

        <!-- Connection Pool -->
        <dependency>
            <groupId>com.zaxxer</groupId>
            <artifactId>HikariCP</artifactId>
        </dependency>

        <!-- Utilities -->
        <dependency>
            <groupId>org.mapstruct</groupId>
            <artifactId>mapstruct</artifactId>
            <version>1.5.5.Final</version>
        </dependency>

        <!-- Testing -->
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-test</artifactId>
            <scope>test</scope>
        </dependency>
    </dependencies>

    <build>
        <plugins>
            <plugin>
                <groupId>org.springframework.boot</groupId>
                <artifactId>spring-boot-maven-plugin</artifactId>
            </plugin>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-compiler-plugin</artifactId>
                <configuration>
                    <annotationProcessorPaths>
                        <path>
                            <groupId>org.mapstruct</groupId>
                            <artifactId>mapstruct-processor</artifactId>
                            <version>1.5.5.Final</version>
                        </path>
                    </annotationProcessorPaths>
                </configuration>
            </plugin>
        </plugins>
    </build>
</project>
```

**application.yml:**

```yaml
spring:
  application:
    name: cobol-migration-service

  datasource:
    url: jdbc:postgresql://localhost:5432/cobol_migration
    username: ${DB_USERNAME:cobol_user}
    password: ${DB_PASSWORD:cobol_pass}
    driver-class-name: org.postgresql.Driver

    hikari:
      maximum-pool-size: 20
      minimum-idle: 5
      idle-timeout: 300000
      connection-timeout: 20000
      max-lifetime: 1200000
      pool-name: CobolMigrationPool

  jpa:
    hibernate:
      ddl-auto: validate
    show-sql: false
    properties:
      hibernate:
        format_sql: true
        jdbc:
          batch_size: 50
        order_inserts: true
        order_updates: true

  # COBOL-like batch processing settings
  batch:
    job:
      enabled: false

server:
  port: 8080
  servlet:
    context-path: /api

# Actuator for monitoring (like COBOL job statistics)
management:
  endpoints:
    web:
      exposure:
        include: health,info,metrics,prometheus
  endpoint:
    health:
      show-details: when_authorized

# Logging (replaces COBOL DISPLAY statements)
logging:
  level:
    com.enterprise: DEBUG
    org.springframework: INFO
    org.hibernate.SQL: DEBUG
  pattern:
    console: "%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n"
```

---

## 2. INYECCIÓN DE DEPENDENCIAS

### 2.1 De COBOL CALL a Spring Dependency Injection

**COBOL Program Calls:**

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN-PROGRAMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CLIENTE-DATA.
           05  WS-CLIENTE-ID       PIC 9(8).
           05  WS-CLIENTE-NOMBRE   PIC X(50).
           05  WS-CLIENTE-SALDO    PIC S9(13)V99.
       01  WS-RETURN-CODE          PIC 9(4).

       PROCEDURE DIVISION.
           MOVE 12345678 TO WS-CLIENTE-ID

           CALL 'BUSCAR-CLIENTE' USING WS-CLIENTE-DATA
                                       WS-RETURN-CODE
           IF WS-RETURN-CODE = 0
               CALL 'VALIDAR-SALDO' USING WS-CLIENTE-DATA
                                          WS-RETURN-CODE
           END-IF

           CALL 'ACTUALIZAR-DB' USING WS-CLIENTE-DATA
                                      WS-RETURN-CODE
           STOP RUN.
```

**Spring Boot Equivalente:**

```java
package com.enterprise.migration.service;

import org.springframework.stereotype.Service;
import org.springframework.beans.factory.annotation.Autowired;
import java.math.BigDecimal;

/**
 * Main service - equivalent to MAIN-PROGRAMA
 * Uses constructor injection for dependencies (CALL statements)
 */
@Service
public class ClienteMainService {

    // Dependencies injected - replaces CALL 'program-name'
    private final ClienteBusquedaService busquedaService;
    private final ClienteValidacionService validacionService;
    private final ClienteRepositoryService repositoryService;

    // Constructor injection - preferred pattern
    @Autowired
    public ClienteMainService(
            ClienteBusquedaService busquedaService,
            ClienteValidacionService validacionService,
            ClienteRepositoryService repositoryService) {
        this.busquedaService = busquedaService;
        this.validacionService = validacionService;
        this.repositoryService = repositoryService;
    }

    /**
     * Main procedure - replaces PROCEDURE DIVISION
     *
     * @param clienteId WS-CLIENTE-ID
     * @return Processing result
     */
    public ResultadoProceso procesarCliente(long clienteId) {
        // CALL 'BUSCAR-CLIENTE' USING WS-CLIENTE-DATA WS-RETURN-CODE
        var clienteData = busquedaService.buscarCliente(clienteId);

        if (clienteData.isEmpty()) {
            return ResultadoProceso.error("CLIENTE_NO_ENCONTRADO");
        }

        var cliente = clienteData.get();

        // CALL 'VALIDAR-SALDO' USING WS-CLIENTE-DATA WS-RETURN-CODE
        var validacion = validacionService.validarSaldo(cliente);

        if (!validacion.esValido()) {
            return ResultadoProceso.error(validacion.codigoError());
        }

        // CALL 'ACTUALIZAR-DB' USING WS-CLIENTE-DATA WS-RETURN-CODE
        repositoryService.actualizar(cliente);

        return ResultadoProceso.exito(cliente);
    }
}

/**
 * BUSCAR-CLIENTE subprogram equivalent
 */
@Service
public class ClienteBusquedaService {

    private final ClienteRepository repository;

    public ClienteBusquedaService(ClienteRepository repository) {
        this.repository = repository;
    }

    public Optional<ClienteDTO> buscarCliente(long clienteId) {
        return repository.findById(clienteId)
            .map(this::toDTO);
    }

    private ClienteDTO toDTO(ClienteEntity entity) {
        return new ClienteDTO(
            entity.getId(),
            entity.getNombre(),
            entity.getSaldo()
        );
    }
}

/**
 * VALIDAR-SALDO subprogram equivalent
 */
@Service
public class ClienteValidacionService {

    private static final BigDecimal SALDO_MINIMO = new BigDecimal("-1000.00");

    public ResultadoValidacion validarSaldo(ClienteDTO cliente) {
        if (cliente.saldo().compareTo(SALDO_MINIMO) < 0) {
            return ResultadoValidacion.invalido("SALDO_INSUFICIENTE");
        }
        return ResultadoValidacion.valido();
    }
}

/**
 * ACTUALIZAR-DB subprogram equivalent
 */
@Service
public class ClienteRepositoryService {

    private final ClienteRepository repository;

    public ClienteRepositoryService(ClienteRepository repository) {
        this.repository = repository;
    }

    public void actualizar(ClienteDTO cliente) {
        repository.findById(cliente.id())
            .ifPresent(entity -> {
                entity.setNombre(cliente.nombre());
                entity.setSaldo(cliente.saldo());
                repository.save(entity);
            });
    }
}

// Supporting records and interfaces
record ClienteDTO(long id, String nombre, BigDecimal saldo) {}

record ResultadoProceso(boolean exito, String codigo, ClienteDTO cliente) {
    static ResultadoProceso exito(ClienteDTO cliente) {
        return new ResultadoProceso(true, "OK", cliente);
    }
    static ResultadoProceso error(String codigo) {
        return new ResultadoProceso(false, codigo, null);
    }
}

record ResultadoValidacion(boolean esValido, String codigoError) {
    static ResultadoValidacion valido() {
        return new ResultadoValidacion(true, null);
    }
    static ResultadoValidacion invalido(String codigo) {
        return new ResultadoValidacion(false, codigo);
    }
}
```

### 2.2 Scopes y Ciclo de Vida

```java
package com.enterprise.migration.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.web.context.annotation.RequestScope;

/**
 * Bean scope configuration
 * Maps to COBOL storage concepts
 */
@Configuration
public class BeanScopeConfig {

    /**
     * Singleton scope (default)
     * Like COBOL WORKING-STORAGE in a persistent program
     */
    @Bean
    @Scope(ConfigurableBeanFactory.SCOPE_SINGLETON)
    public ConfiguracionGlobal configuracionGlobal() {
        return new ConfiguracionGlobal();
    }

    /**
     * Prototype scope
     * Like COBOL LOCAL-STORAGE - new instance each time
     */
    @Bean
    @Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    public ContextoProcesamiento contextoProcesamiento() {
        return new ContextoProcesamiento();
    }

    /**
     * Request scope
     * Like COBOL LINKAGE SECTION - per transaction data
     */
    @Bean
    @RequestScope
    public TransaccionActual transaccionActual() {
        return new TransaccionActual();
    }
}

/**
 * Global configuration - WORKING-STORAGE equivalent
 */
public class ConfiguracionGlobal {
    private String codigoEmpresa = "001";
    private String ambiente = "PROD";

    // Getters and setters
    public String getCodigoEmpresa() { return codigoEmpresa; }
    public void setCodigoEmpresa(String codigoEmpresa) {
        this.codigoEmpresa = codigoEmpresa;
    }
    public String getAmbiente() { return ambiente; }
    public void setAmbiente(String ambiente) { this.ambiente = ambiente; }
}

/**
 * Processing context - LOCAL-STORAGE equivalent
 */
public class ContextoProcesamiento {
    private String correlationId;
    private long timestampInicio;
    private int contadorRegistros;

    public ContextoProcesamiento() {
        this.correlationId = java.util.UUID.randomUUID().toString();
        this.timestampInicio = System.currentTimeMillis();
        this.contadorRegistros = 0;
    }

    public void incrementarContador() { contadorRegistros++; }

    // Getters
    public String getCorrelationId() { return correlationId; }
    public long getTimestampInicio() { return timestampInicio; }
    public int getContadorRegistros() { return contadorRegistros; }
}

/**
 * Current transaction - LINKAGE SECTION equivalent
 */
public class TransaccionActual {
    private String transactionId;
    private String userId;
    private java.time.LocalDateTime timestamp;

    public TransaccionActual() {
        this.transactionId = java.util.UUID.randomUUID().toString();
        this.timestamp = java.time.LocalDateTime.now();
    }

    // Getters and setters
    public String getTransactionId() { return transactionId; }
    public void setTransactionId(String transactionId) {
        this.transactionId = transactionId;
    }
    public String getUserId() { return userId; }
    public void setUserId(String userId) { this.userId = userId; }
    public java.time.LocalDateTime getTimestamp() { return timestamp; }
}
```

---

## 3. JPA/HIBERNATE PARA ACCESO A DATOS

### 3.1 Mapeo de Entidades COBOL a JPA

**COBOL Copybook y DB2 Table:**

```cobol
       * CLIENTE copybook (CLIENTE.CPY)
       01  CLIENTE-REGISTRO.
           05  CLI-ID               PIC 9(8).
           05  CLI-TIPO             PIC X(2).
               88  CLI-PERSONA      VALUE 'PE'.
               88  CLI-EMPRESA      VALUE 'EM'.
           05  CLI-NOMBRE           PIC X(100).
           05  CLI-DOCUMENTO        PIC X(20).
           05  CLI-DIRECCION.
               10  CLI-CALLE        PIC X(50).
               10  CLI-CIUDAD       PIC X(30).
               10  CLI-CODIGO-POST  PIC X(10).
               10  CLI-PAIS         PIC X(3).
           05  CLI-TELEFONO         PIC X(20).
           05  CLI-EMAIL            PIC X(100).
           05  CLI-SALDO            PIC S9(13)V99 COMP-3.
           05  CLI-LIMITE-CREDITO   PIC S9(13)V99 COMP-3.
           05  CLI-FECHA-ALTA       PIC 9(8).
           05  CLI-ESTADO           PIC X(1).
               88  CLI-ACTIVO       VALUE 'A'.
               88  CLI-INACTIVO     VALUE 'I'.
               88  CLI-SUSPENDIDO   VALUE 'S'.
           05  CLI-FECHA-ULT-MOV    PIC 9(14).
```

**JPA Entity Mapping:**

```java
package com.enterprise.migration.entity;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * Cliente entity - Maps to CLIENTE-REGISTRO copybook
 * DB2 table: CLIENTE
 */
@Entity
@Table(name = "CLIENTE", schema = "COBOL_SCHEMA")
public class ClienteEntity {

    // CLI-ID PIC 9(8) - Primary key
    @Id
    @Column(name = "CLI_ID", nullable = false)
    private Long id;

    // CLI-TIPO PIC X(2)
    @Column(name = "CLI_TIPO", length = 2, nullable = false)
    @Enumerated(EnumType.STRING)
    private TipoCliente tipo;

    // CLI-NOMBRE PIC X(100)
    @Column(name = "CLI_NOMBRE", length = 100, nullable = false)
    private String nombre;

    // CLI-DOCUMENTO PIC X(20)
    @Column(name = "CLI_DOCUMENTO", length = 20)
    private String documento;

    // CLI-DIRECCION (embedded group)
    @Embedded
    private DireccionEmbeddable direccion;

    // CLI-TELEFONO PIC X(20)
    @Column(name = "CLI_TELEFONO", length = 20)
    private String telefono;

    // CLI-EMAIL PIC X(100)
    @Column(name = "CLI_EMAIL", length = 100)
    private String email;

    // CLI-SALDO PIC S9(13)V99 COMP-3
    @Column(name = "CLI_SALDO", precision = 15, scale = 2)
    private BigDecimal saldo;

    // CLI-LIMITE-CREDITO PIC S9(13)V99 COMP-3
    @Column(name = "CLI_LIMITE_CREDITO", precision = 15, scale = 2)
    private BigDecimal limiteCredito;

    // CLI-FECHA-ALTA PIC 9(8) - YYYYMMDD
    @Column(name = "CLI_FECHA_ALTA")
    private LocalDate fechaAlta;

    // CLI-ESTADO PIC X(1)
    @Column(name = "CLI_ESTADO", length = 1, nullable = false)
    @Enumerated(EnumType.STRING)
    private EstadoCliente estado;

    // CLI-FECHA-ULT-MOV PIC 9(14) - YYYYMMDDHHMMSS
    @Column(name = "CLI_FECHA_ULT_MOV")
    private LocalDateTime fechaUltimoMovimiento;

    // Version for optimistic locking (not in COBOL)
    @Version
    @Column(name = "VERSION")
    private Long version;

    // Default constructor required by JPA
    public ClienteEntity() {}

    // 88-level condition equivalents
    public boolean isPersona() {
        return TipoCliente.PE.equals(this.tipo);
    }

    public boolean isEmpresa() {
        return TipoCliente.EM.equals(this.tipo);
    }

    public boolean isActivo() {
        return EstadoCliente.A.equals(this.estado);
    }

    public boolean isInactivo() {
        return EstadoCliente.I.equals(this.estado);
    }

    public boolean isSuspendido() {
        return EstadoCliente.S.equals(this.estado);
    }

    // Business logic methods
    public BigDecimal getCreditoDisponible() {
        if (limiteCredito == null || saldo == null) {
            return BigDecimal.ZERO;
        }
        return limiteCredito.add(saldo).max(BigDecimal.ZERO);
    }

    // Getters and setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public TipoCliente getTipo() { return tipo; }
    public void setTipo(TipoCliente tipo) { this.tipo = tipo; }

    public String getNombre() { return nombre; }
    public void setNombre(String nombre) { this.nombre = nombre; }

    public String getDocumento() { return documento; }
    public void setDocumento(String documento) { this.documento = documento; }

    public DireccionEmbeddable getDireccion() { return direccion; }
    public void setDireccion(DireccionEmbeddable direccion) {
        this.direccion = direccion;
    }

    public String getTelefono() { return telefono; }
    public void setTelefono(String telefono) { this.telefono = telefono; }

    public String getEmail() { return email; }
    public void setEmail(String email) { this.email = email; }

    public BigDecimal getSaldo() { return saldo; }
    public void setSaldo(BigDecimal saldo) { this.saldo = saldo; }

    public BigDecimal getLimiteCredito() { return limiteCredito; }
    public void setLimiteCredito(BigDecimal limiteCredito) {
        this.limiteCredito = limiteCredito;
    }

    public LocalDate getFechaAlta() { return fechaAlta; }
    public void setFechaAlta(LocalDate fechaAlta) { this.fechaAlta = fechaAlta; }

    public EstadoCliente getEstado() { return estado; }
    public void setEstado(EstadoCliente estado) { this.estado = estado; }

    public LocalDateTime getFechaUltimoMovimiento() {
        return fechaUltimoMovimiento;
    }
    public void setFechaUltimoMovimiento(LocalDateTime fechaUltimoMovimiento) {
        this.fechaUltimoMovimiento = fechaUltimoMovimiento;
    }

    public Long getVersion() { return version; }
    public void setVersion(Long version) { this.version = version; }
}

/**
 * Embedded address - CLI-DIRECCION group
 */
@Embeddable
public class DireccionEmbeddable {

    // CLI-CALLE PIC X(50)
    @Column(name = "CLI_CALLE", length = 50)
    private String calle;

    // CLI-CIUDAD PIC X(30)
    @Column(name = "CLI_CIUDAD", length = 30)
    private String ciudad;

    // CLI-CODIGO-POST PIC X(10)
    @Column(name = "CLI_CODIGO_POST", length = 10)
    private String codigoPostal;

    // CLI-PAIS PIC X(3)
    @Column(name = "CLI_PAIS", length = 3)
    private String pais;

    public DireccionEmbeddable() {}

    public DireccionEmbeddable(String calle, String ciudad,
                               String codigoPostal, String pais) {
        this.calle = calle;
        this.ciudad = ciudad;
        this.codigoPostal = codigoPostal;
        this.pais = pais;
    }

    // Getters and setters
    public String getCalle() { return calle; }
    public void setCalle(String calle) { this.calle = calle; }

    public String getCiudad() { return ciudad; }
    public void setCiudad(String ciudad) { this.ciudad = ciudad; }

    public String getCodigoPostal() { return codigoPostal; }
    public void setCodigoPostal(String codigoPostal) {
        this.codigoPostal = codigoPostal;
    }

    public String getPais() { return pais; }
    public void setPais(String pais) { this.pais = pais; }
}

/**
 * CLI-TIPO 88-level values
 */
public enum TipoCliente {
    PE, // CLI-PERSONA
    EM  // CLI-EMPRESA
}

/**
 * CLI-ESTADO 88-level values
 */
public enum EstadoCliente {
    A, // CLI-ACTIVO
    I, // CLI-INACTIVO
    S  // CLI-SUSPENDIDO
}
```

### 3.2 Spring Data Repository

```java
package com.enterprise.migration.repository;

import com.enterprise.migration.entity.*;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * Cliente repository - Replaces COBOL READ/WRITE/REWRITE/DELETE
 */
@Repository
public interface ClienteRepository extends JpaRepository<ClienteEntity, Long> {

    // Equivalent to: READ CLIENTE-FILE KEY IS CLI-DOCUMENTO
    Optional<ClienteEntity> findByDocumento(String documento);

    // Equivalent to: READ CLIENTE-FILE KEY IS CLI-EMAIL
    Optional<ClienteEntity> findByEmail(String email);

    // Equivalent to: START CLIENTE-FILE KEY >= CLI-NOMBRE
    List<ClienteEntity> findByNombreStartingWithIgnoreCase(String prefijo);

    // Equivalent to: SELECT with condition on 88-level value
    List<ClienteEntity> findByEstado(EstadoCliente estado);

    // Equivalent to: SELECT with multiple conditions
    List<ClienteEntity> findByTipoAndEstado(TipoCliente tipo, EstadoCliente estado);

    // Equivalent to: SELECT WHERE CLI-SALDO > :valor
    List<ClienteEntity> findBySaldoGreaterThan(BigDecimal saldo);

    // Equivalent to: SELECT WHERE CLI-FECHA-ALTA BETWEEN :desde AND :hasta
    List<ClienteEntity> findByFechaAltaBetween(LocalDate desde, LocalDate hasta);

    /**
     * Custom JPQL query - replaces embedded SQL in COBOL
     *
     * COBOL SQL equivalent:
     * EXEC SQL
     *     SELECT CLI_ID, CLI_NOMBRE, CLI_SALDO
     *     FROM CLIENTE
     *     WHERE CLI_SALDO < 0
     *       AND CLI_ESTADO = 'A'
     *     ORDER BY CLI_SALDO
     * END-EXEC
     */
    @Query("""
        SELECT c FROM ClienteEntity c
        WHERE c.saldo < 0
          AND c.estado = :estado
        ORDER BY c.saldo ASC
        """)
    List<ClienteEntity> findClientesEnDeuda(
            @Param("estado") EstadoCliente estado);

    /**
     * Projection query - returns subset of fields
     */
    @Query("""
        SELECT new com.enterprise.migration.dto.ClienteResumenDTO(
            c.id, c.nombre, c.saldo, c.limiteCredito)
        FROM ClienteEntity c
        WHERE c.tipo = :tipo
        """)
    List<ClienteResumenDTO> findResumenByTipo(@Param("tipo") TipoCliente tipo);

    /**
     * Update query - replaces COBOL REWRITE with specific field update
     *
     * COBOL equivalent:
     * MOVE NEW-SALDO TO CLI-SALDO
     * REWRITE CLIENTE-REGISTRO
     */
    @Modifying
    @Query("""
        UPDATE ClienteEntity c
        SET c.saldo = c.saldo + :monto,
            c.fechaUltimoMovimiento = CURRENT_TIMESTAMP
        WHERE c.id = :clienteId
        """)
    int actualizarSaldo(@Param("clienteId") Long clienteId,
                       @Param("monto") BigDecimal monto);

    /**
     * Bulk update - COBOL batch processing equivalent
     */
    @Modifying
    @Query("""
        UPDATE ClienteEntity c
        SET c.estado = :nuevoEstado
        WHERE c.estado = :estadoActual
          AND c.fechaUltimoMovimiento < :fechaLimite
        """)
    int inactivarClientesSinMovimiento(
            @Param("estadoActual") EstadoCliente estadoActual,
            @Param("nuevoEstado") EstadoCliente nuevoEstado,
            @Param("fechaLimite") LocalDateTime fechaLimite);

    /**
     * Count query - COBOL accumulator equivalent
     */
    long countByEstado(EstadoCliente estado);

    /**
     * Aggregate query
     */
    @Query("""
        SELECT SUM(c.saldo) FROM ClienteEntity c
        WHERE c.estado = :estado
        """)
    BigDecimal sumSaldoByEstado(@Param("estado") EstadoCliente estado);

    /**
     * Pagination support - COBOL FETCH FIRST n ROWS equivalent
     */
    List<ClienteEntity> findTop10ByEstadoOrderBySaldoDesc(EstadoCliente estado);

    /**
     * Native query for complex DB2-specific operations
     */
    @Query(value = """
        SELECT * FROM COBOL_SCHEMA.CLIENTE c
        WHERE c.CLI_ESTADO = 'A'
          AND c.CLI_SALDO > (
              SELECT AVG(c2.CLI_SALDO)
              FROM COBOL_SCHEMA.CLIENTE c2
              WHERE c2.CLI_TIPO = c.CLI_TIPO
          )
        ORDER BY c.CLI_SALDO DESC
        FETCH FIRST 100 ROWS ONLY
        """, nativeQuery = true)
    List<ClienteEntity> findClientesSaldoSobrePromedio();
}

// DTO for projection
record ClienteResumenDTO(Long id, String nombre, BigDecimal saldo,
                        BigDecimal limiteCredito) {}
```

---

## 4. REST APIs Y MICROSERVICIOS

### 4.1 Controller Layer - Reemplazo de CICS Transactions

```java
package com.enterprise.migration.controller;

import com.enterprise.migration.dto.*;
import com.enterprise.migration.service.*;
import jakarta.validation.Valid;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.*;
import java.util.List;

/**
 * Cliente REST Controller
 * Replaces CICS transactions for client management
 *
 * CICS Transaction ID mapping:
 * - CL01: List clients    -> GET /clientes
 * - CL02: Get client      -> GET /clientes/{id}
 * - CL03: Create client   -> POST /clientes
 * - CL04: Update client   -> PUT /clientes/{id}
 * - CL05: Delete client   -> DELETE /clientes/{id}
 */
@RestController
@RequestMapping("/v1/clientes")
public class ClienteController {

    private final ClienteService clienteService;

    public ClienteController(ClienteService clienteService) {
        this.clienteService = clienteService;
    }

    /**
     * CL01 - List clients with filters
     *
     * CICS equivalent:
     * EXEC CICS LINK PROGRAM('CL01LST')
     *           COMMAREA(CLIENTE-LISTA-AREA)
     * END-EXEC
     */
    @GetMapping
    public ResponseEntity<List<ClienteDTO>> listarClientes(
            @RequestParam(required = false) String tipo,
            @RequestParam(required = false) String estado,
            @RequestParam(defaultValue = "0") int pagina,
            @RequestParam(defaultValue = "20") int tamano) {

        var filtro = new ClienteFiltroDTO(tipo, estado, pagina, tamano);
        var clientes = clienteService.listar(filtro);

        return ResponseEntity.ok(clientes);
    }

    /**
     * CL02 - Get client by ID
     *
     * CICS equivalent:
     * EXEC CICS READ
     *           FILE('CLIFILE')
     *           INTO(CLIENTE-AREA)
     *           RIDFLD(CLI-ID)
     * END-EXEC
     */
    @GetMapping("/{id}")
    public ResponseEntity<ClienteDTO> obtenerCliente(@PathVariable Long id) {
        return clienteService.obtenerPorId(id)
            .map(ResponseEntity::ok)
            .orElse(ResponseEntity.notFound().build());
    }

    /**
     * CL03 - Create new client
     *
     * CICS equivalent:
     * EXEC CICS WRITE
     *           FILE('CLIFILE')
     *           FROM(CLIENTE-AREA)
     *           RIDFLD(CLI-ID)
     * END-EXEC
     */
    @PostMapping
    public ResponseEntity<ClienteDTO> crearCliente(
            @Valid @RequestBody ClienteCreacionDTO request) {

        var clienteCreado = clienteService.crear(request);

        return ResponseEntity
            .status(HttpStatus.CREATED)
            .body(clienteCreado);
    }

    /**
     * CL04 - Update existing client
     *
     * CICS equivalent:
     * EXEC CICS READ
     *           FILE('CLIFILE')
     *           INTO(CLIENTE-AREA)
     *           RIDFLD(CLI-ID)
     *           UPDATE
     * END-EXEC
     * ...
     * EXEC CICS REWRITE
     *           FILE('CLIFILE')
     *           FROM(CLIENTE-AREA)
     * END-EXEC
     */
    @PutMapping("/{id}")
    public ResponseEntity<ClienteDTO> actualizarCliente(
            @PathVariable Long id,
            @Valid @RequestBody ClienteActualizacionDTO request) {

        return clienteService.actualizar(id, request)
            .map(ResponseEntity::ok)
            .orElse(ResponseEntity.notFound().build());
    }

    /**
     * CL05 - Delete client (logical delete)
     *
     * CICS equivalent:
     * EXEC CICS DELETE
     *           FILE('CLIFILE')
     *           RIDFLD(CLI-ID)
     * END-EXEC
     */
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> eliminarCliente(@PathVariable Long id) {
        boolean eliminado = clienteService.eliminar(id);

        return eliminado
            ? ResponseEntity.noContent().build()
            : ResponseEntity.notFound().build();
    }

    /**
     * Custom operation - Update balance
     * Maps to specific COBOL paragraph
     */
    @PostMapping("/{id}/movimientos")
    public ResponseEntity<MovimientoResultadoDTO> registrarMovimiento(
            @PathVariable Long id,
            @Valid @RequestBody MovimientoDTO movimiento) {

        var resultado = clienteService.registrarMovimiento(id, movimiento);

        return ResponseEntity.ok(resultado);
    }

    /**
     * Batch operation - replaces COBOL batch program
     */
    @PostMapping("/batch/inactivar")
    public ResponseEntity<BatchResultadoDTO> inactivarSinMovimiento(
            @RequestParam int diasSinMovimiento) {

        var resultado = clienteService.inactivarClientesSinMovimiento(
            diasSinMovimiento);

        return ResponseEntity.ok(resultado);
    }
}

// DTOs
record ClienteDTO(
    Long id,
    String tipo,
    String nombre,
    String documento,
    DireccionDTO direccion,
    String telefono,
    String email,
    java.math.BigDecimal saldo,
    java.math.BigDecimal limiteCredito,
    java.time.LocalDate fechaAlta,
    String estado
) {}

record DireccionDTO(
    String calle,
    String ciudad,
    String codigoPostal,
    String pais
) {}

record ClienteFiltroDTO(
    String tipo,
    String estado,
    int pagina,
    int tamano
) {}

record ClienteCreacionDTO(
    @jakarta.validation.constraints.NotBlank String tipo,
    @jakarta.validation.constraints.NotBlank
    @jakarta.validation.constraints.Size(max = 100) String nombre,
    @jakarta.validation.constraints.NotBlank String documento,
    DireccionDTO direccion,
    String telefono,
    @jakarta.validation.constraints.Email String email
) {}

record ClienteActualizacionDTO(
    String nombre,
    DireccionDTO direccion,
    String telefono,
    String email
) {}

record MovimientoDTO(
    @jakarta.validation.constraints.NotBlank String tipo,
    @jakarta.validation.constraints.NotNull
    @jakarta.validation.constraints.Positive java.math.BigDecimal monto,
    String concepto
) {}

record MovimientoResultadoDTO(
    String transaccionId,
    java.math.BigDecimal saldoAnterior,
    java.math.BigDecimal saldoNuevo,
    String mensaje
) {}

record BatchResultadoDTO(
    int registrosProcesados,
    int registrosActualizados,
    int errores,
    long tiempoMs
) {}
```

### 4.2 Exception Handling Global

```java
package com.enterprise.migration.controller;

import org.springframework.http.*;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.bind.MethodArgumentNotValidException;
import java.time.LocalDateTime;
import java.util.*;

/**
 * Global exception handler
 * Replaces COBOL ERROR-HANDLING paragraphs
 */
@RestControllerAdvice
public class GlobalExceptionHandler {

    /**
     * Handle validation errors - like COBOL data validation
     */
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<ErrorResponse> handleValidationErrors(
            MethodArgumentNotValidException ex) {

        List<String> errores = ex.getBindingResult()
            .getFieldErrors()
            .stream()
            .map(error -> error.getField() + ": " + error.getDefaultMessage())
            .toList();

        return ResponseEntity
            .badRequest()
            .body(new ErrorResponse(
                "VALIDACION_ERROR",
                "Error de validación de datos",
                errores,
                LocalDateTime.now()
            ));
    }

    /**
     * Handle not found - like COBOL FILE STATUS 23
     */
    @ExceptionHandler(RecursoNoEncontradoException.class)
    public ResponseEntity<ErrorResponse> handleNotFound(
            RecursoNoEncontradoException ex) {

        return ResponseEntity
            .status(HttpStatus.NOT_FOUND)
            .body(new ErrorResponse(
                "RECURSO_NO_ENCONTRADO",
                ex.getMessage(),
                List.of(),
                LocalDateTime.now()
            ));
    }

    /**
     * Handle business rule violations
     */
    @ExceptionHandler(ReglaDeNegocioException.class)
    public ResponseEntity<ErrorResponse> handleBusinessRule(
            ReglaDeNegocioException ex) {

        return ResponseEntity
            .status(HttpStatus.UNPROCESSABLE_ENTITY)
            .body(new ErrorResponse(
                ex.getCodigo(),
                ex.getMessage(),
                List.of(),
                LocalDateTime.now()
            ));
    }

    /**
     * Handle duplicate key - like COBOL FILE STATUS 22
     */
    @ExceptionHandler(org.springframework.dao.DuplicateKeyException.class)
    public ResponseEntity<ErrorResponse> handleDuplicateKey(
            org.springframework.dao.DuplicateKeyException ex) {

        return ResponseEntity
            .status(HttpStatus.CONFLICT)
            .body(new ErrorResponse(
                "CLAVE_DUPLICADA",
                "El registro ya existe",
                List.of(),
                LocalDateTime.now()
            ));
    }

    /**
     * Handle optimistic locking - concurrent update detection
     */
    @ExceptionHandler(
        org.springframework.orm.ObjectOptimisticLockingFailureException.class)
    public ResponseEntity<ErrorResponse> handleOptimisticLock(
            org.springframework.orm.ObjectOptimisticLockingFailureException ex) {

        return ResponseEntity
            .status(HttpStatus.CONFLICT)
            .body(new ErrorResponse(
                "CONFLICTO_CONCURRENCIA",
                "El registro fue modificado por otro proceso",
                List.of(),
                LocalDateTime.now()
            ));
    }

    /**
     * Catch-all handler - like COBOL WHEN OTHER
     */
    @ExceptionHandler(Exception.class)
    public ResponseEntity<ErrorResponse> handleGeneral(Exception ex) {
        // Log the full exception
        ex.printStackTrace();

        return ResponseEntity
            .status(HttpStatus.INTERNAL_SERVER_ERROR)
            .body(new ErrorResponse(
                "ERROR_INTERNO",
                "Error interno del servidor",
                List.of(),
                LocalDateTime.now()
            ));
    }
}

record ErrorResponse(
    String codigo,
    String mensaje,
    List<String> detalles,
    LocalDateTime timestamp
) {}

class RecursoNoEncontradoException extends RuntimeException {
    public RecursoNoEncontradoException(String mensaje) {
        super(mensaje);
    }
}

class ReglaDeNegocioException extends RuntimeException {
    private final String codigo;

    public ReglaDeNegocioException(String codigo, String mensaje) {
        super(mensaje);
        this.codigo = codigo;
    }

    public String getCodigo() { return codigo; }
}
```

---

## 5. GESTIÓN TRANSACCIONAL

### 5.1 Transacciones Declarativas

```java
package com.enterprise.migration.service;

import com.enterprise.migration.entity.*;
import com.enterprise.migration.repository.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * Transaction management service
 * Replaces COBOL CICS SYNCPOINT and DB2 COMMIT
 */
@Service
public class TransferenciaService {

    private final CuentaRepository cuentaRepository;
    private final MovimientoRepository movimientoRepository;
    private final AuditoriaRepository auditoriaRepository;

    public TransferenciaService(
            CuentaRepository cuentaRepository,
            MovimientoRepository movimientoRepository,
            AuditoriaRepository auditoriaRepository) {
        this.cuentaRepository = cuentaRepository;
        this.movimientoRepository = movimientoRepository;
        this.auditoriaRepository = auditoriaRepository;
    }

    /**
     * Transfer funds between accounts
     *
     * COBOL equivalent:
     * EXEC CICS SYNCPOINT
     * END-EXEC
     *
     * All operations succeed or all are rolled back
     */
    @Transactional(
        isolation = Isolation.READ_COMMITTED,
        propagation = Propagation.REQUIRED,
        rollbackFor = Exception.class,
        timeout = 30
    )
    public ResultadoTransferencia transferir(
            String cuentaOrigen,
            String cuentaDestino,
            BigDecimal monto,
            String concepto) {

        // Validate accounts exist
        CuentaEntity origen = cuentaRepository.findByNumero(cuentaOrigen)
            .orElseThrow(() -> new CuentaNoEncontradaException(cuentaOrigen));

        CuentaEntity destino = cuentaRepository.findByNumero(cuentaDestino)
            .orElseThrow(() -> new CuentaNoEncontradaException(cuentaDestino));

        // Validate sufficient funds
        if (origen.getSaldo().compareTo(monto) < 0) {
            throw new SaldoInsuficienteException(cuentaOrigen, monto);
        }

        // Perform debit - UPDATE origen
        origen.setSaldo(origen.getSaldo().subtract(monto));
        origen.setFechaUltimoMovimiento(LocalDateTime.now());
        cuentaRepository.save(origen);

        // Perform credit - UPDATE destino
        destino.setSaldo(destino.getSaldo().add(monto));
        destino.setFechaUltimoMovimiento(LocalDateTime.now());
        cuentaRepository.save(destino);

        // Record movements
        String referencia = generarReferencia();

        MovimientoEntity movDebito = new MovimientoEntity();
        movDebito.setCuenta(origen);
        movDebito.setTipo(TipoMovimiento.DEBITO);
        movDebito.setMonto(monto.negate());
        movDebito.setConcepto(concepto);
        movDebito.setReferencia(referencia);
        movimientoRepository.save(movDebito);

        MovimientoEntity movCredito = new MovimientoEntity();
        movCredito.setCuenta(destino);
        movCredito.setTipo(TipoMovimiento.CREDITO);
        movCredito.setMonto(monto);
        movCredito.setConcepto(concepto);
        movCredito.setReferencia(referencia);
        movimientoRepository.save(movCredito);

        // Audit record
        registrarAuditoria(cuentaOrigen, cuentaDestino, monto, referencia);

        return new ResultadoTransferencia(
            referencia,
            origen.getSaldo(),
            destino.getSaldo(),
            "Transferencia exitosa"
        );
    }

    /**
     * Read-only transaction for queries
     * Equivalent to DB2 cursor with FOR READ ONLY
     */
    @Transactional(readOnly = true)
    public ResumenCuenta obtenerResumen(String numeroCuenta) {
        CuentaEntity cuenta = cuentaRepository.findByNumero(numeroCuenta)
            .orElseThrow(() -> new CuentaNoEncontradaException(numeroCuenta));

        List<MovimientoEntity> ultimos = movimientoRepository
            .findTop10ByCuentaOrderByFechaDesc(cuenta);

        return new ResumenCuenta(
            cuenta.getNumero(),
            cuenta.getSaldo(),
            cuenta.getLimite(),
            ultimos.stream().map(this::toMovimientoDTO).toList()
        );
    }

    /**
     * New transaction for audit (requires new connection)
     * Similar to COBOL calling separate program with own COMMIT
     */
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void registrarAuditoria(String origen, String destino,
                                   BigDecimal monto, String referencia) {
        AuditoriaEntity auditoria = new AuditoriaEntity();
        auditoria.setAccion("TRANSFERENCIA");
        auditoria.setDetalles(String.format(
            "De: %s, A: %s, Monto: %s, Ref: %s",
            origen, destino, monto, referencia
        ));
        auditoria.setFecha(LocalDateTime.now());
        auditoriaRepository.save(auditoria);
    }

    /**
     * Nested transaction example
     * COBOL equivalent: CALL with separate SYNCPOINT
     */
    @Transactional(propagation = Propagation.NESTED)
    public void actualizarEstadisticas(String numeroCuenta) {
        // Can be rolled back independently
        cuentaRepository.actualizarEstadisticas(numeroCuenta);
    }

    private String generarReferencia() {
        return "TRF" + System.currentTimeMillis();
    }

    private MovimientoDTO toMovimientoDTO(MovimientoEntity entity) {
        return new MovimientoDTO(
            entity.getTipo().name(),
            entity.getMonto(),
            entity.getConcepto(),
            entity.getFecha()
        );
    }
}

// Supporting records
record ResultadoTransferencia(
    String referencia,
    BigDecimal saldoOrigen,
    BigDecimal saldoDestino,
    String mensaje
) {}

record ResumenCuenta(
    String numero,
    BigDecimal saldo,
    BigDecimal limite,
    List<MovimientoDTO> ultimosMovimientos
) {}

record MovimientoDTO(
    String tipo,
    BigDecimal monto,
    String concepto,
    LocalDateTime fecha
) {}

class CuentaNoEncontradaException extends RuntimeException {
    public CuentaNoEncontradaException(String numero) {
        super("Cuenta no encontrada: " + numero);
    }
}

class SaldoInsuficienteException extends RuntimeException {
    public SaldoInsuficienteException(String cuenta, BigDecimal monto) {
        super("Saldo insuficiente en cuenta " + cuenta +
              " para monto " + monto);
    }
}
```

### 5.2 Propagation y Isolation Levels

```java
package com.enterprise.migration.service;

import org.springframework.transaction.annotation.*;

/**
 * Transaction propagation and isolation reference
 * Maps to COBOL/DB2 transaction behaviors
 */
public class TransactionPatterns {

    /*
     * ═══════════════════════════════════════════════════════════════════════
     * PROPAGATION MODES - How transactions are handled
     * ═══════════════════════════════════════════════════════════════════════
     *
     * REQUIRED (default)
     * - Uses existing transaction or creates new one
     * - Like COBOL paragraph running within current SYNCPOINT
     *
     * REQUIRES_NEW
     * - Always creates new transaction
     * - Like COBOL CALL to program with own COMMIT
     * - Parent transaction is suspended
     *
     * NESTED
     * - Creates savepoint within current transaction
     * - Can rollback to savepoint without rolling back parent
     * - Like DB2 SAVEPOINT
     *
     * SUPPORTS
     * - Uses transaction if exists, runs without if not
     * - For optional transactional behavior
     *
     * NOT_SUPPORTED
     * - Suspends current transaction
     * - Runs without transaction
     * - For non-transactional operations (logging, etc.)
     *
     * MANDATORY
     * - Requires existing transaction
     * - Throws exception if no transaction
     *
     * NEVER
     * - Must not have active transaction
     * - Throws exception if transaction exists
     *
     * ═══════════════════════════════════════════════════════════════════════
     * ISOLATION LEVELS - Data visibility between transactions
     * ═══════════════════════════════════════════════════════════════════════
     *
     * READ_UNCOMMITTED
     * - Can see uncommitted changes (dirty reads)
     * - Like DB2 UR (Uncommitted Read)
     * - Highest performance, lowest consistency
     *
     * READ_COMMITTED (default for most DBs)
     * - Only sees committed data
     * - Like DB2 CS (Cursor Stability)
     * - Prevents dirty reads
     *
     * REPEATABLE_READ
     * - Repeated reads return same data
     * - Like DB2 RS (Read Stability)
     * - Prevents dirty and non-repeatable reads
     *
     * SERIALIZABLE
     * - Full isolation, as if sequential execution
     * - Like DB2 RR (Repeatable Read - DB2 terminology)
     * - Prevents all anomalies, lowest concurrency
     */

    @Transactional(
        propagation = Propagation.REQUIRED,
        isolation = Isolation.READ_COMMITTED
    )
    public void operacionNormal() {
        // Standard business operation
    }

    @Transactional(
        propagation = Propagation.REQUIRES_NEW,
        isolation = Isolation.READ_COMMITTED
    )
    public void operacionIndependiente() {
        // Independent transaction (audit, logging)
    }

    @Transactional(
        propagation = Propagation.NESTED
    )
    public void operacionAnidada() {
        // Can be rolled back independently
    }

    @Transactional(
        isolation = Isolation.SERIALIZABLE,
        timeout = 10
    )
    public void operacionCritica() {
        // Critical operation requiring full isolation
    }

    @Transactional(readOnly = true)
    public void consultaSoloLectura() {
        // Read-only operations - optimized
    }
}
```

---

## 6. MAPEO DE BATCH COBOL A SPRING SERVICES

### 6.1 Conversión de Programa Batch COBOL

**COBOL Batch Program Original:**

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCH-ACTUALIZACION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENTRADA-FILE ASSIGN TO 'ENTRADA.DAT'
               ORGANIZATION IS SEQUENTIAL.
           SELECT SALIDA-FILE ASSIGN TO 'SALIDA.DAT'
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ENTRADA-FILE.
       01  REG-ENTRADA             PIC X(200).

       FD  SALIDA-FILE.
       01  REG-SALIDA              PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-CONTADORES.
           05  WS-LEIDOS           PIC 9(8) VALUE 0.
           05  WS-PROCESADOS       PIC 9(8) VALUE 0.
           05  WS-ERRORES          PIC 9(8) VALUE 0.
       01  WS-EOF                  PIC 9 VALUE 0.
           88  FIN-ARCHIVO         VALUE 1.

       PROCEDURE DIVISION.
           PERFORM INICIO
           PERFORM PROCESO UNTIL FIN-ARCHIVO
           PERFORM FIN
           STOP RUN.

       INICIO.
           OPEN INPUT ENTRADA-FILE
           OPEN OUTPUT SALIDA-FILE
           READ ENTRADA-FILE
               AT END SET FIN-ARCHIVO TO TRUE
           END-READ.

       PROCESO.
           ADD 1 TO WS-LEIDOS
           PERFORM PROCESAR-REGISTRO
           READ ENTRADA-FILE
               AT END SET FIN-ARCHIVO TO TRUE
           END-READ.

       PROCESAR-REGISTRO.
           IF REGISTRO-VALIDO
               PERFORM ACTUALIZAR-BD
               ADD 1 TO WS-PROCESADOS
           ELSE
               ADD 1 TO WS-ERRORES
           END-IF.

       FIN.
           CLOSE ENTRADA-FILE SALIDA-FILE
           DISPLAY 'LEIDOS: ' WS-LEIDOS
           DISPLAY 'PROCESADOS: ' WS-PROCESADOS
           DISPLAY 'ERRORES: ' WS-ERRORES.
```

**Spring Service Equivalent:**

```java
package com.enterprise.migration.batch;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Batch processing service - Direct COBOL batch migration
 * Without Spring Batch framework (simple migration)
 */
@Service
public class BatchActualizacionService {

    private final RegistroProcessor registroProcessor;
    private final RegistroRepository registroRepository;

    public BatchActualizacionService(
            RegistroProcessor registroProcessor,
            RegistroRepository registroRepository) {
        this.registroProcessor = registroProcessor;
        this.registroRepository = registroRepository;
    }

    /**
     * Main batch process - replaces PROCEDURE DIVISION
     */
    public ResultadoBatch ejecutarBatch(Path archivoEntrada, Path archivoSalida) {
        // WS-CONTADORES
        AtomicLong leidos = new AtomicLong(0);
        AtomicLong procesados = new AtomicLong(0);
        AtomicLong errores = new AtomicLong(0);
        long tiempoInicio = System.currentTimeMillis();

        // PERFORM INICIO
        // OPEN INPUT ENTRADA-FILE / OPEN OUTPUT SALIDA-FILE
        try (BufferedReader reader = Files.newBufferedReader(archivoEntrada);
             BufferedWriter writer = Files.newBufferedWriter(archivoSalida)) {

            String linea;

            // PERFORM PROCESO UNTIL FIN-ARCHIVO
            // READ ENTRADA-FILE AT END SET FIN-ARCHIVO TO TRUE
            while ((linea = reader.readLine()) != null) {
                // ADD 1 TO WS-LEIDOS
                leidos.incrementAndGet();

                try {
                    // PERFORM PROCESAR-REGISTRO
                    var resultado = procesarRegistro(linea);

                    if (resultado.exitoso()) {
                        // ADD 1 TO WS-PROCESADOS
                        procesados.incrementAndGet();
                        writer.write(resultado.registroSalida());
                        writer.newLine();
                    } else {
                        // ADD 1 TO WS-ERRORES
                        errores.incrementAndGet();
                        escribirError(resultado, writer);
                    }
                } catch (Exception e) {
                    errores.incrementAndGet();
                    escribirExcepcion(linea, e, writer);
                }
            }

        } catch (IOException e) {
            throw new BatchException("Error procesando archivos", e);
        }

        // PERFORM FIN - DISPLAY statistics
        long tiempoTotal = System.currentTimeMillis() - tiempoInicio;

        return new ResultadoBatch(
            leidos.get(),
            procesados.get(),
            errores.get(),
            tiempoTotal
        );
    }

    /**
     * Process single record - PROCESAR-REGISTRO paragraph
     */
    @Transactional
    private ResultadoRegistro procesarRegistro(String linea) {
        // Parse COBOL fixed-format record
        var registro = parsearRegistro(linea);

        // IF REGISTRO-VALIDO
        if (!registroProcessor.validar(registro)) {
            return ResultadoRegistro.error(
                linea,
                "Validación fallida"
            );
        }

        // PERFORM ACTUALIZAR-BD
        var entidad = registroRepository.findById(registro.id())
            .orElse(new RegistroEntity());

        entidad.setDatos(registro.datos());
        entidad.setFechaProceso(java.time.LocalDateTime.now());
        registroRepository.save(entidad);

        return ResultadoRegistro.exito(formatearSalida(registro));
    }

    private RegistroDTO parsearRegistro(String linea) {
        // Parse fixed-position COBOL record
        String id = linea.substring(0, 8).trim();
        String tipo = linea.substring(8, 10);
        String datos = linea.substring(10, 200).trim();

        return new RegistroDTO(id, tipo, datos);
    }

    private String formatearSalida(RegistroDTO registro) {
        return String.format("%-8s%-2s%-190s",
            registro.id(),
            registro.tipo(),
            registro.datos()
        );
    }

    private void escribirError(ResultadoRegistro resultado,
                               BufferedWriter writer) throws IOException {
        writer.write("ERROR: " + resultado.mensajeError());
        writer.newLine();
    }

    private void escribirExcepcion(String linea, Exception e,
                                    BufferedWriter writer) throws IOException {
        writer.write("EXCEPCION: " + e.getMessage() + " | " + linea);
        writer.newLine();
    }
}

// Supporting classes
record RegistroDTO(String id, String tipo, String datos) {}

record ResultadoRegistro(
    boolean exitoso,
    String registroSalida,
    String mensajeError
) {
    static ResultadoRegistro exito(String salida) {
        return new ResultadoRegistro(true, salida, null);
    }
    static ResultadoRegistro error(String entrada, String mensaje) {
        return new ResultadoRegistro(false, entrada, mensaje);
    }
}

record ResultadoBatch(
    long leidos,
    long procesados,
    long errores,
    long tiempoMs
) {
    public void mostrarEstadisticas() {
        // DISPLAY equivalents
        System.out.println("LEIDOS: " + leidos);
        System.out.println("PROCESADOS: " + procesados);
        System.out.println("ERRORES: " + errores);
        System.out.println("TIEMPO (ms): " + tiempoMs);
    }
}

class BatchException extends RuntimeException {
    public BatchException(String mensaje, Throwable causa) {
        super(mensaje, causa);
    }
}
```

---

## 7. CONFIGURACIÓN Y PERFILES

### 7.1 Perfiles de Ambiente

```java
package com.enterprise.migration.config;

import org.springframework.context.annotation.*;
import org.springframework.beans.factory.annotation.Value;

/**
 * Environment-specific configuration
 * Replaces COBOL JCL parameters and SYSINs
 */
@Configuration
public class AmbienteConfig {

    @Value("${app.ambiente:DEV}")
    private String ambiente;

    @Value("${app.batch.chunk-size:100}")
    private int chunkSize;

    @Value("${app.batch.max-errores:10}")
    private int maxErrores;
}

/**
 * Development profile configuration
 */
@Configuration
@Profile("dev")
public class DevConfig {

    @Bean
    public AuditoriaService auditoriaService() {
        return new AuditoriaServiceConsola(); // Log to console
    }
}

/**
 * Production profile configuration
 */
@Configuration
@Profile("prod")
public class ProdConfig {

    @Bean
    public AuditoriaService auditoriaService(AuditoriaRepository repo) {
        return new AuditoriaServiceDB(repo); // Log to database
    }
}

/**
 * Test profile configuration
 */
@Configuration
@Profile("test")
public class TestConfig {

    @Bean
    public AuditoriaService auditoriaService() {
        return new AuditoriaServiceMock(); // No-op for tests
    }
}

// Service interfaces and implementations
interface AuditoriaService {
    void registrar(String accion, String detalles);
}

class AuditoriaServiceConsola implements AuditoriaService {
    public void registrar(String accion, String detalles) {
        System.out.println("[AUDITORIA] " + accion + ": " + detalles);
    }
}

class AuditoriaServiceDB implements AuditoriaService {
    private final AuditoriaRepository repository;

    public AuditoriaServiceDB(AuditoriaRepository repository) {
        this.repository = repository;
    }

    public void registrar(String accion, String detalles) {
        // Save to database
    }
}

class AuditoriaServiceMock implements AuditoriaService {
    public void registrar(String accion, String detalles) {
        // Do nothing in tests
    }
}
```

---

## 8. TABLA DE MAPEO: CICS/COBOL A SPRING

### 8.1 Comandos CICS a Spring

| CICS Command | Spring Boot Equivalent |
|--------------|----------------------|
| `EXEC CICS LINK PROGRAM` | `@Autowired` service injection |
| `EXEC CICS XCTL` | Controller redirect / forward |
| `EXEC CICS READ FILE` | `JpaRepository.findById()` |
| `EXEC CICS WRITE FILE` | `JpaRepository.save()` |
| `EXEC CICS REWRITE FILE` | `JpaRepository.save()` (update) |
| `EXEC CICS DELETE FILE` | `JpaRepository.delete()` |
| `EXEC CICS SYNCPOINT` | `@Transactional` commit |
| `EXEC CICS SYNCPOINT ROLLBACK` | Exception thrown in `@Transactional` |
| `EXEC CICS SEND MAP` | REST Response / View render |
| `EXEC CICS RECEIVE MAP` | `@RequestBody` / `@RequestParam` |
| `EXEC CICS ASKTIME` | `LocalDateTime.now()` |
| `EXEC CICS FORMATTIME` | `DateTimeFormatter` |
| `EXEC CICS GETMAIN` | Object creation / Bean injection |
| `EXEC CICS FREEMAIN` | Garbage collection (automatic) |
| `COMMAREA` | DTO / Request/Response objects |

### 8.2 Patrones de Diseño

| COBOL Pattern | Spring Pattern |
|---------------|----------------|
| COPYBOOK | DTO / Entity classes |
| Subprogram CALL | @Service injection |
| PERFORM paragraph | Private methods |
| WORKING-STORAGE | Service fields / @Value |
| LINKAGE SECTION | Method parameters |
| FILE SECTION | Repository interface |
| Control break | Stream groupingBy |
| Error paragraph | @ExceptionHandler |

---

## 9. REFERENCIAS Y RECURSOS

### 9.1 Documentación Oficial

- [Spring Boot Reference](https://docs.spring.io/spring-boot/docs/current/reference/html/)
- [Spring Data JPA](https://docs.spring.io/spring-data/jpa/docs/current/reference/html/)
- [Spring Transaction Management](https://docs.spring.io/spring-framework/reference/data-access/transaction.html)

### 9.2 Mejores Prácticas

1. **Diseño de Capas**: Controller -> Service -> Repository
2. **Transacciones**: En capa de servicio, no en controladores
3. **DTOs**: Separar entidades JPA de objetos de API
4. **Validación**: Usar Bean Validation en DTOs
5. **Excepciones**: Global handler con códigos de error consistentes

---

```
═══════════════════════════════════════════════════════════════════════════════
                    FIN DEL DOCUMENTO JAVA_02_ENTERPRISE
                         ARCHAEON_CORE - v1.0.0
═══════════════════════════════════════════════════════════════════════════════
```
