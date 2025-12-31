---
título: "Java Database Legacy - DB2 y VSAM a Bases Modernas"
código: JAVA-04-DB
versión: 1.0.0
fecha_creación: 2025-12-31
última_actualización: 2025-12-31
autor: ARCHAEON_CORE
dominio: LENGUAJES_MODERNOS
especialización: DATABASE_MIGRATION
dependencias:
  - JDK 17+
  - JDBC
  - HikariCP
  - Spring Data JPA
tags:
  - java
  - jdbc
  - db2
  - vsam
  - database
  - migration
nivel_complejidad: AVANZADO
estado: ACTIVO
---

# ═══════════════════════════════════════════════════════════════════════════════
# JAVA 04: DATABASE LEGACY - DB2 Y VSAM A BASES MODERNAS
# ARCHAEON_CORE - Sistema de Documentación de Lenguajes Legacy
# ═══════════════════════════════════════════════════════════════════════════════

## 1. INTRODUCCIÓN A LA MIGRACIÓN DE DATOS

### 1.1 Panorama de Migración de Bases de Datos

La migración de sistemas de datos mainframe (DB2, VSAM, IMS) a bases de datos
modernas requiere comprensión profunda de ambos ecosistemas y estrategias
cuidadosas de transformación.

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                 ARQUITECTURA DE DATOS: MAINFRAME vs MODERNO                 │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│   MAINFRAME                                  MODERNO                        │
│   ┌─────────────────┐                       ┌─────────────────┐            │
│   │ DB2 z/OS        │ ═══════════════════▶  │ PostgreSQL      │            │
│   │ - SQL tables    │                       │ Oracle          │            │
│   │ - Tablespaces   │                       │ SQL Server      │            │
│   │ - Bufferpools   │                       │ MySQL           │            │
│   └─────────────────┘                       └─────────────────┘            │
│                                                                             │
│   ┌─────────────────┐                       ┌─────────────────┐            │
│   │ VSAM            │ ═══════════════════▶  │ RDBMS Tables    │            │
│   │ - KSDS (indexed)│                       │ Key-Value Store │            │
│   │ - ESDS (seq)    │                       │ Document DB     │            │
│   │ - RRDS (direct) │                       │                 │            │
│   └─────────────────┘                       └─────────────────┘            │
│                                                                             │
│   ┌─────────────────┐                       ┌─────────────────┐            │
│   │ IMS DB          │ ═══════════════════▶  │ Hierarchical    │            │
│   │ - Hierarchical  │                       │ Document DB     │            │
│   │ - Segments      │                       │ Graph DB        │            │
│   └─────────────────┘                       └─────────────────┘            │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### 1.2 Comparación de Características

| Característica | DB2 z/OS | PostgreSQL | Oracle | MySQL |
|----------------|----------|------------|--------|-------|
| SQL Compliant | Yes | Yes | Yes | Yes |
| Stored Procs | Yes | Yes | Yes | Yes |
| Partitioning | Yes | Yes | Yes | Yes |
| Clustering | Yes | Yes (RAC) | Yes | Yes |
| Lock Level | Row/Page | Row | Row | Row |
| Max Row Size | 32KB | 1.6GB | 8KB-32KB | 65KB |
| EBCDIC Support | Native | Convert | Convert | Convert |
| Package/Plan | Yes | N/A | N/A | N/A |

---

## 2. JDBC FUNDAMENTOS

### 2.1 Configuración de Conexión

```java
package com.enterprise.migration.database;

import java.sql.*;
import java.util.Properties;

/**
 * JDBC connection fundamentals for database migration
 * Replaces COBOL embedded SQL CONNECT statement
 */
public class JdbcFundamentals {

    /**
     * COBOL equivalent:
     * EXEC SQL
     *     CONNECT TO database_name
     *     USER :username USING :password
     * END-EXEC
     */
    public Connection getConnection() throws SQLException {
        String url = "jdbc:postgresql://localhost:5432/cobol_migration";
        String user = "cobol_user";
        String password = "secure_password";

        // Properties for additional configuration
        Properties props = new Properties();
        props.setProperty("user", user);
        props.setProperty("password", password);
        props.setProperty("ssl", "true");
        props.setProperty("sslfactory",
            "org.postgresql.ssl.NonValidatingFactory");

        // Application name for DB monitoring
        props.setProperty("ApplicationName", "COBOL_Migration_App");

        return DriverManager.getConnection(url, props);
    }

    /**
     * Connection with DB2-specific driver
     * For connecting to existing DB2 z/OS
     */
    public Connection getDB2Connection() throws SQLException {
        // DB2 Type 4 driver (pure Java)
        String url = "jdbc:db2://mainframe.example.com:50000/PRODDB";

        Properties props = new Properties();
        props.setProperty("user", "DB2USER");
        props.setProperty("password", "DB2PASS");

        // DB2-specific properties
        props.setProperty("currentSchema", "COBOL_SCHEMA");
        props.setProperty("progressiveStreaming", "2");
        props.setProperty("fullyMaterializeLobData", "false");

        return DriverManager.getConnection(url, props);
    }

    /**
     * Batch mode connection for ETL operations
     */
    public Connection getBatchConnection() throws SQLException {
        String url = "jdbc:postgresql://localhost:5432/cobol_migration" +
            "?reWriteBatchedInserts=true" +  // PostgreSQL batch optimization
            "&prepareThreshold=0";           // Disable server-side prepare

        return DriverManager.getConnection(url, "user", "password");
    }
}
```

### 2.2 Statement Types

```java
package com.enterprise.migration.database;

import java.math.BigDecimal;
import java.sql.*;
import java.time.LocalDate;
import java.util.*;

/**
 * JDBC Statement types and usage patterns
 * Direct mapping from COBOL embedded SQL
 */
public class JdbcStatementPatterns {

    private final Connection connection;

    public JdbcStatementPatterns(Connection connection) {
        this.connection = connection;
    }

    /**
     * Simple Statement - for static SQL
     * Use only when no parameters needed
     *
     * COBOL equivalent (static SQL):
     * EXEC SQL
     *     SELECT COUNT(*) INTO :WS-COUNT
     *     FROM CLIENTE
     *     WHERE ESTADO = 'A'
     * END-EXEC
     */
    public long countActiveClients() throws SQLException {
        String sql = "SELECT COUNT(*) FROM cliente WHERE estado = 'A'";

        try (Statement stmt = connection.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {

            if (rs.next()) {
                return rs.getLong(1);
            }
            return 0;
        }
    }

    /**
     * PreparedStatement - for parameterized queries
     * ALWAYS use for user input to prevent SQL injection
     *
     * COBOL equivalent (host variables):
     * EXEC SQL
     *     SELECT CLI_ID, CLI_NOMBRE, CLI_SALDO
     *     INTO :WS-ID, :WS-NOMBRE, :WS-SALDO
     *     FROM CLIENTE
     *     WHERE CLI_ID = :WS-INPUT-ID
     * END-EXEC
     */
    public Optional<Cliente> findClientById(long clientId) throws SQLException {
        String sql = """
            SELECT cli_id, cli_nombre, cli_saldo, cli_estado
            FROM cliente
            WHERE cli_id = ?
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            // Set parameter - :WS-INPUT-ID equivalent
            pstmt.setLong(1, clientId);

            try (ResultSet rs = pstmt.executeQuery()) {
                if (rs.next()) {
                    return Optional.of(mapCliente(rs));
                }
            }
        }
        return Optional.empty();
    }

    /**
     * PreparedStatement for INSERT
     *
     * COBOL equivalent:
     * EXEC SQL
     *     INSERT INTO CLIENTE
     *         (CLI_ID, CLI_NOMBRE, CLI_SALDO, CLI_ESTADO, CLI_FECHA_ALTA)
     *     VALUES
     *         (:WS-ID, :WS-NOMBRE, :WS-SALDO, :WS-ESTADO, CURRENT DATE)
     * END-EXEC
     */
    public void insertClient(Cliente cliente) throws SQLException {
        String sql = """
            INSERT INTO cliente
                (cli_id, cli_nombre, cli_saldo, cli_estado, cli_fecha_alta)
            VALUES (?, ?, ?, ?, CURRENT_DATE)
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setLong(1, cliente.id());
            pstmt.setString(2, cliente.nombre());
            pstmt.setBigDecimal(3, cliente.saldo());
            pstmt.setString(4, cliente.estado());

            int rowsAffected = pstmt.executeUpdate();
            if (rowsAffected != 1) {
                throw new SQLException("Insert failed, no rows affected");
            }
        }
    }

    /**
     * CallableStatement - for stored procedures
     *
     * COBOL equivalent:
     * EXEC SQL
     *     CALL CALCULAR_INTERESES(:WS-CUENTA,
     *                              :WS-FECHA-DESDE,
     *                              :WS-FECHA-HASTA,
     *                              :WS-INTERES-OUT)
     * END-EXEC
     */
    public BigDecimal callStoredProcedure(String cuenta,
                                          LocalDate fechaDesde,
                                          LocalDate fechaHasta)
            throws SQLException {

        String sql = "{ CALL calcular_intereses(?, ?, ?, ?) }";

        try (CallableStatement cstmt = connection.prepareCall(sql)) {
            // Input parameters
            cstmt.setString(1, cuenta);
            cstmt.setDate(2, Date.valueOf(fechaDesde));
            cstmt.setDate(3, Date.valueOf(fechaHasta));

            // Output parameter
            cstmt.registerOutParameter(4, Types.DECIMAL);

            cstmt.execute();

            return cstmt.getBigDecimal(4);
        }
    }

    /**
     * Batch execution - for bulk operations
     *
     * COBOL equivalent (cursor with multiple inserts):
     * EXEC SQL
     *     DECLARE INS-CURSOR CURSOR FOR
     *     INSERT INTO MOVIMIENTO (...)
     * END-EXEC
     */
    public int[] batchInsert(List<Movimiento> movimientos) throws SQLException {
        String sql = """
            INSERT INTO movimiento
                (mov_id, mov_cuenta, mov_tipo, mov_monto, mov_fecha)
            VALUES (?, ?, ?, ?, ?)
            """;

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            for (Movimiento mov : movimientos) {
                pstmt.setLong(1, mov.id());
                pstmt.setString(2, mov.cuenta());
                pstmt.setString(3, mov.tipo());
                pstmt.setBigDecimal(4, mov.monto());
                pstmt.setDate(5, Date.valueOf(mov.fecha()));

                pstmt.addBatch();
            }

            return pstmt.executeBatch();
        }
    }

    private Cliente mapCliente(ResultSet rs) throws SQLException {
        return new Cliente(
            rs.getLong("cli_id"),
            rs.getString("cli_nombre"),
            rs.getBigDecimal("cli_saldo"),
            rs.getString("cli_estado")
        );
    }

    record Cliente(long id, String nombre, BigDecimal saldo, String estado) {}
    record Movimiento(long id, String cuenta, String tipo,
                      BigDecimal monto, LocalDate fecha) {}
}
```

### 2.3 Cursor Patterns

```java
package com.enterprise.migration.database;

import java.sql.*;
import java.util.*;
import java.util.function.Consumer;

/**
 * JDBC cursor patterns - mapping from COBOL DECLARE CURSOR
 */
public class JdbcCursorPatterns {

    private final Connection connection;

    public JdbcCursorPatterns(Connection connection) {
        this.connection = connection;
    }

    /**
     * Forward-only cursor - equivalent to COBOL default cursor
     *
     * COBOL:
     * EXEC SQL
     *     DECLARE CLI-CURSOR CURSOR FOR
     *     SELECT CLI_ID, CLI_NOMBRE, CLI_SALDO
     *     FROM CLIENTE
     *     WHERE CLI_ESTADO = 'A'
     *     ORDER BY CLI_NOMBRE
     * END-EXEC
     *
     * EXEC SQL OPEN CLI-CURSOR END-EXEC
     *
     * PERFORM UNTIL SQLCODE = 100
     *     EXEC SQL
     *         FETCH CLI-CURSOR INTO :WS-ID, :WS-NOMBRE, :WS-SALDO
     *     END-EXEC
     *     IF SQLCODE = 0
     *         PERFORM PROCESS-CLIENT
     *     END-IF
     * END-PERFORM
     *
     * EXEC SQL CLOSE CLI-CURSOR END-EXEC
     */
    public void processActiveClients(Consumer<ClienteRecord> processor)
            throws SQLException {

        String sql = """
            SELECT cli_id, cli_nombre, cli_saldo
            FROM cliente
            WHERE cli_estado = 'A'
            ORDER BY cli_nombre
            """;

        // DECLARE CURSOR (implicit)
        try (PreparedStatement pstmt = connection.prepareStatement(sql,
                ResultSet.TYPE_FORWARD_ONLY,
                ResultSet.CONCUR_READ_ONLY)) {

            // Set fetch size for large result sets
            pstmt.setFetchSize(1000);

            // OPEN CLI-CURSOR (implicit with executeQuery)
            try (ResultSet rs = pstmt.executeQuery()) {

                // PERFORM UNTIL SQLCODE = 100 (end of cursor)
                while (rs.next()) {
                    // FETCH CLI-CURSOR INTO host variables
                    ClienteRecord cliente = new ClienteRecord(
                        rs.getLong("cli_id"),
                        rs.getString("cli_nombre"),
                        rs.getBigDecimal("cli_saldo")
                    );

                    // PERFORM PROCESS-CLIENT
                    processor.accept(cliente);
                }
            }
            // CLOSE CLI-CURSOR (implicit with try-with-resources)
        }
    }

    /**
     * Scrollable cursor - for random access
     *
     * COBOL equivalent:
     * EXEC SQL
     *     DECLARE SCROLL-CURSOR SCROLL CURSOR FOR
     *     SELECT * FROM CLIENTE
     * END-EXEC
     */
    public List<ClienteRecord> processWithScrollableCursor() throws SQLException {
        String sql = "SELECT cli_id, cli_nombre, cli_saldo FROM cliente";

        List<ClienteRecord> results = new ArrayList<>();

        try (PreparedStatement pstmt = connection.prepareStatement(sql,
                ResultSet.TYPE_SCROLL_INSENSITIVE,
                ResultSet.CONCUR_READ_ONLY)) {

            try (ResultSet rs = pstmt.executeQuery()) {
                // Move to last row to get count
                if (rs.last()) {
                    int totalRows = rs.getRow();
                    System.out.println("Total rows: " + totalRows);
                }

                // Move back to first
                rs.beforeFirst();

                // Process normally
                while (rs.next()) {
                    results.add(mapCliente(rs));
                }

                // Can also move backwards
                // rs.previous();

                // Or jump to specific row
                // rs.absolute(5);

                // Or move relative
                // rs.relative(-2);
            }
        }

        return results;
    }

    /**
     * Updatable cursor - for positioned updates
     *
     * COBOL:
     * EXEC SQL
     *     DECLARE UPD-CURSOR CURSOR FOR
     *     SELECT CLI_ID, CLI_SALDO FROM CLIENTE
     *     WHERE CLI_ESTADO = 'A'
     *     FOR UPDATE OF CLI_SALDO
     * END-EXEC
     *
     * ... FETCH ...
     *
     * EXEC SQL
     *     UPDATE CLIENTE SET CLI_SALDO = :NEW-SALDO
     *     WHERE CURRENT OF UPD-CURSOR
     * END-EXEC
     */
    public int updateWithCursor(java.math.BigDecimal incremento)
            throws SQLException {

        String sql = """
            SELECT cli_id, cli_saldo FROM cliente
            WHERE cli_estado = 'A'
            FOR UPDATE
            """;

        int updated = 0;

        try (PreparedStatement pstmt = connection.prepareStatement(sql,
                ResultSet.TYPE_FORWARD_ONLY,
                ResultSet.CONCUR_UPDATABLE)) {

            try (ResultSet rs = pstmt.executeQuery()) {
                while (rs.next()) {
                    // Get current value
                    java.math.BigDecimal saldoActual =
                        rs.getBigDecimal("cli_saldo");

                    // Calculate new value
                    java.math.BigDecimal nuevoSaldo =
                        saldoActual.add(incremento);

                    // UPDATE WHERE CURRENT OF cursor
                    rs.updateBigDecimal("cli_saldo", nuevoSaldo);
                    rs.updateRow();

                    updated++;
                }
            }
        }

        return updated;
    }

    /**
     * Paginated cursor - for large datasets
     * Replaces COBOL FETCH FIRST n ROWS ONLY
     */
    public List<ClienteRecord> getPaginatedResults(int offset, int limit)
            throws SQLException {

        String sql = """
            SELECT cli_id, cli_nombre, cli_saldo
            FROM cliente
            WHERE cli_estado = 'A'
            ORDER BY cli_nombre
            OFFSET ? ROWS FETCH NEXT ? ROWS ONLY
            """;

        List<ClienteRecord> results = new ArrayList<>();

        try (PreparedStatement pstmt = connection.prepareStatement(sql)) {
            pstmt.setInt(1, offset);
            pstmt.setInt(2, limit);

            try (ResultSet rs = pstmt.executeQuery()) {
                while (rs.next()) {
                    results.add(mapCliente(rs));
                }
            }
        }

        return results;
    }

    private ClienteRecord mapCliente(ResultSet rs) throws SQLException {
        return new ClienteRecord(
            rs.getLong("cli_id"),
            rs.getString("cli_nombre"),
            rs.getBigDecimal("cli_saldo")
        );
    }

    record ClienteRecord(long id, String nombre, java.math.BigDecimal saldo) {}
}
```

---

## 3. CONNECTION POOLING CON HIKARICP

### 3.1 Configuración HikariCP

```java
package com.enterprise.migration.database;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;

/**
 * HikariCP connection pool configuration
 * Essential for high-performance database access
 */
public class HikariConnectionPool {

    private static HikariDataSource dataSource;

    /**
     * Initialize connection pool
     * Call once at application startup
     */
    public static void initialize() {
        HikariConfig config = new HikariConfig();

        // ═══════════════════════════════════════════════════════════════════
        // Basic connection settings
        // ═══════════════════════════════════════════════════════════════════
        config.setJdbcUrl("jdbc:postgresql://localhost:5432/cobol_migration");
        config.setUsername("cobol_user");
        config.setPassword("secure_password");
        config.setDriverClassName("org.postgresql.Driver");

        // ═══════════════════════════════════════════════════════════════════
        // Pool sizing - critical for batch processing
        // ═══════════════════════════════════════════════════════════════════

        // Maximum pool size formula:
        // connections = ((core_count * 2) + effective_spindle_count)
        // For SSD: effective_spindle_count = 1
        // Example: 8 cores with SSD = (8 * 2) + 1 = 17 connections
        config.setMaximumPoolSize(20);

        // Minimum idle connections (for quick response)
        config.setMinimumIdle(5);

        // ═══════════════════════════════════════════════════════════════════
        // Timeout settings
        // ═══════════════════════════════════════════════════════════════════

        // Max time to wait for connection from pool (ms)
        config.setConnectionTimeout(30000); // 30 seconds

        // Max time connection can be idle before being removed
        config.setIdleTimeout(600000); // 10 minutes

        // Max lifetime of connection in pool
        config.setMaxLifetime(1800000); // 30 minutes

        // How often to validate idle connections
        config.setKeepaliveTime(60000); // 1 minute

        // ═══════════════════════════════════════════════════════════════════
        // Performance settings
        // ═══════════════════════════════════════════════════════════════════

        // Prepared statement cache per connection
        config.addDataSourceProperty("cachePrepStmts", "true");
        config.addDataSourceProperty("prepStmtCacheSize", "250");
        config.addDataSourceProperty("prepStmtCacheSqlLimit", "2048");

        // PostgreSQL specific optimizations
        config.addDataSourceProperty("useServerPrepStmts", "true");
        config.addDataSourceProperty("reWriteBatchedInserts", "true");

        // ═══════════════════════════════════════════════════════════════════
        // Monitoring and debugging
        // ═══════════════════════════════════════════════════════════════════

        // Pool name for monitoring
        config.setPoolName("CobolMigrationPool");

        // Enable JMX for monitoring
        config.setRegisterMbeans(true);

        // Leak detection threshold (ms)
        config.setLeakDetectionThreshold(60000); // 1 minute

        // Initialize data source
        dataSource = new HikariDataSource(config);
    }

    /**
     * Get connection from pool
     * Equivalent to COBOL CONNECT but much faster (pooled)
     */
    public static Connection getConnection() throws SQLException {
        return dataSource.getConnection();
    }

    /**
     * Get DataSource for Spring integration
     */
    public static DataSource getDataSource() {
        return dataSource;
    }

    /**
     * Shutdown pool
     * Call at application shutdown
     */
    public static void shutdown() {
        if (dataSource != null && !dataSource.isClosed()) {
            dataSource.close();
        }
    }

    /**
     * Get pool statistics
     */
    public static PoolStats getPoolStats() {
        return new PoolStats(
            dataSource.getHikariPoolMXBean().getActiveConnections(),
            dataSource.getHikariPoolMXBean().getIdleConnections(),
            dataSource.getHikariPoolMXBean().getTotalConnections(),
            dataSource.getHikariPoolMXBean().getThreadsAwaitingConnection()
        );
    }

    public record PoolStats(
        int active,
        int idle,
        int total,
        int waiting
    ) {}
}

/**
 * Spring Boot configuration alternative
 */
/*
@Configuration
public class DataSourceConfig {

    @Bean
    @ConfigurationProperties("spring.datasource.hikari")
    public HikariConfig hikariConfig() {
        return new HikariConfig();
    }

    @Bean
    public DataSource dataSource(HikariConfig config) {
        return new HikariDataSource(config);
    }
}

# application.yml
spring:
  datasource:
    hikari:
      jdbc-url: jdbc:postgresql://localhost:5432/cobol_migration
      username: cobol_user
      password: secure_password
      maximum-pool-size: 20
      minimum-idle: 5
      connection-timeout: 30000
      idle-timeout: 600000
      max-lifetime: 1800000
      pool-name: CobolMigrationPool
*/
```

---

## 4. TRANSACTION PATTERNS

### 4.1 Manual Transaction Management

```java
package com.enterprise.migration.database;

import java.math.BigDecimal;
import java.sql.*;

/**
 * Transaction patterns - COBOL COMMIT/ROLLBACK equivalent
 */
public class TransactionPatterns {

    /**
     * Basic transaction with commit/rollback
     *
     * COBOL equivalent:
     * EXEC SQL
     *     UPDATE CUENTA SET SALDO = SALDO - :MONTO
     *     WHERE CUENTA_ID = :CUENTA_ORIGEN
     * END-EXEC
     *
     * IF SQLCODE = 0
     *     EXEC SQL
     *         UPDATE CUENTA SET SALDO = SALDO + :MONTO
     *         WHERE CUENTA_ID = :CUENTA_DESTINO
     *     END-EXEC
     *     IF SQLCODE = 0
     *         EXEC SQL COMMIT END-EXEC
     *     ELSE
     *         EXEC SQL ROLLBACK END-EXEC
     *     END-IF
     * ELSE
     *     EXEC SQL ROLLBACK END-EXEC
     * END-IF
     */
    public TransferResult transferFunds(Connection conn,
                                        String cuentaOrigen,
                                        String cuentaDestino,
                                        BigDecimal monto) throws SQLException {
        // Disable auto-commit for transaction control
        conn.setAutoCommit(false);

        String sqlDebito = """
            UPDATE cuenta SET saldo = saldo - ?
            WHERE cuenta_id = ? AND saldo >= ?
            """;

        String sqlCredito = """
            UPDATE cuenta SET saldo = saldo + ?
            WHERE cuenta_id = ?
            """;

        try {
            // Debit origin account
            try (PreparedStatement pstmt = conn.prepareStatement(sqlDebito)) {
                pstmt.setBigDecimal(1, monto);
                pstmt.setString(2, cuentaOrigen);
                pstmt.setBigDecimal(3, monto); // Ensure sufficient balance

                int rows = pstmt.executeUpdate();
                if (rows != 1) {
                    conn.rollback();
                    return new TransferResult(false, "SALDO_INSUFICIENTE");
                }
            }

            // Credit destination account
            try (PreparedStatement pstmt = conn.prepareStatement(sqlCredito)) {
                pstmt.setBigDecimal(1, monto);
                pstmt.setString(2, cuentaDestino);

                int rows = pstmt.executeUpdate();
                if (rows != 1) {
                    conn.rollback();
                    return new TransferResult(false, "CUENTA_DESTINO_NO_EXISTE");
                }
            }

            // EXEC SQL COMMIT END-EXEC
            conn.commit();
            return new TransferResult(true, "OK");

        } catch (SQLException e) {
            // EXEC SQL ROLLBACK END-EXEC
            conn.rollback();
            throw e;
        } finally {
            // Restore auto-commit
            conn.setAutoCommit(true);
        }
    }

    /**
     * Transaction with savepoints
     *
     * COBOL DB2 equivalent:
     * EXEC SQL SAVEPOINT SP1 ON ROLLBACK RETAIN CURSORS END-EXEC
     * ... operations ...
     * EXEC SQL ROLLBACK TO SAVEPOINT SP1 END-EXEC
     */
    public void transactionWithSavepoints(Connection conn) throws SQLException {
        conn.setAutoCommit(false);
        Savepoint savepoint1 = null;
        Savepoint savepoint2 = null;

        try {
            // First batch of operations
            executeOperation1(conn);

            // Create savepoint after critical operation
            savepoint1 = conn.setSavepoint("AFTER_OP1");

            try {
                // Second batch (optional operation)
                executeOperation2(conn);

                // Another savepoint
                savepoint2 = conn.setSavepoint("AFTER_OP2");

            } catch (SQLException e) {
                // Rollback only operation 2
                conn.rollback(savepoint1);
                // Continue with operation 1 committed
            }

            try {
                // Third batch
                executeOperation3(conn);
            } catch (SQLException e) {
                // Rollback to savepoint2 if exists, else savepoint1
                if (savepoint2 != null) {
                    conn.rollback(savepoint2);
                } else {
                    conn.rollback(savepoint1);
                }
            }

            conn.commit();

        } catch (SQLException e) {
            conn.rollback();
            throw e;
        } finally {
            conn.setAutoCommit(true);
        }
    }

    /**
     * Isolation level control
     *
     * DB2 equivalent:
     * EXEC SQL SET CURRENT ISOLATION = RR END-EXEC (Repeatable Read)
     * EXEC SQL SET CURRENT ISOLATION = CS END-EXEC (Cursor Stability)
     * EXEC SQL SET CURRENT ISOLATION = UR END-EXEC (Uncommitted Read)
     */
    public void setIsolationLevel(Connection conn, IsolationLevel level)
            throws SQLException {
        switch (level) {
            case READ_UNCOMMITTED:
                // DB2 UR - Uncommitted Read
                conn.setTransactionIsolation(
                    Connection.TRANSACTION_READ_UNCOMMITTED);
                break;
            case READ_COMMITTED:
                // DB2 CS - Cursor Stability (default)
                conn.setTransactionIsolation(
                    Connection.TRANSACTION_READ_COMMITTED);
                break;
            case REPEATABLE_READ:
                // DB2 RS - Read Stability
                conn.setTransactionIsolation(
                    Connection.TRANSACTION_REPEATABLE_READ);
                break;
            case SERIALIZABLE:
                // DB2 RR - Repeatable Read
                conn.setTransactionIsolation(
                    Connection.TRANSACTION_SERIALIZABLE);
                break;
        }
    }

    enum IsolationLevel {
        READ_UNCOMMITTED,
        READ_COMMITTED,
        REPEATABLE_READ,
        SERIALIZABLE
    }

    record TransferResult(boolean success, String code) {}

    private void executeOperation1(Connection conn) throws SQLException {}
    private void executeOperation2(Connection conn) throws SQLException {}
    private void executeOperation3(Connection conn) throws SQLException {}
}
```

---

## 5. MIGRACIÓN DE SQL EMBEBIDO A JPA

### 5.1 Transformación de Queries

```java
package com.enterprise.migration.database.jpa;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * Embedded SQL to JPA migration patterns
 */
@Entity
@Table(name = "cliente", schema = "cobol_schema")
public class ClienteEntity {

    @Id
    @Column(name = "cli_id")
    private Long id;

    @Column(name = "cli_nombre", length = 100)
    private String nombre;

    @Column(name = "cli_saldo", precision = 15, scale = 2)
    private BigDecimal saldo;

    @Column(name = "cli_estado", length = 1)
    private String estado;

    @Column(name = "cli_fecha_alta")
    private LocalDate fechaAlta;

    // Relationships
    @OneToMany(mappedBy = "cliente", fetch = FetchType.LAZY)
    private List<MovimientoEntity> movimientos;

    // Getters and setters...
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    public String getNombre() { return nombre; }
    public void setNombre(String nombre) { this.nombre = nombre; }
    public BigDecimal getSaldo() { return saldo; }
    public void setSaldo(BigDecimal saldo) { this.saldo = saldo; }
    public String getEstado() { return estado; }
    public void setEstado(String estado) { this.estado = estado; }
    public LocalDate getFechaAlta() { return fechaAlta; }
    public void setFechaAlta(LocalDate fechaAlta) { this.fechaAlta = fechaAlta; }
    public List<MovimientoEntity> getMovimientos() { return movimientos; }
}

@Entity
@Table(name = "movimiento")
class MovimientoEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "cli_id")
    private ClienteEntity cliente;

    @Column(name = "mov_tipo")
    private String tipo;

    @Column(name = "mov_monto")
    private BigDecimal monto;

    @Column(name = "mov_fecha")
    private LocalDate fecha;

    // Getters and setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    public ClienteEntity getCliente() { return cliente; }
    public void setCliente(ClienteEntity cliente) { this.cliente = cliente; }
}

/**
 * Repository with embedded SQL to JPQL mappings
 */
interface ClienteRepository extends
        org.springframework.data.jpa.repository.JpaRepository<ClienteEntity, Long> {

    /**
     * COBOL embedded SQL:
     * EXEC SQL
     *     SELECT CLI_ID, CLI_NOMBRE, CLI_SALDO
     *     FROM CLIENTE
     *     WHERE CLI_ESTADO = :WS-ESTADO
     *     ORDER BY CLI_NOMBRE
     * END-EXEC
     */
    List<ClienteEntity> findByEstadoOrderByNombre(String estado);

    /**
     * COBOL embedded SQL with BETWEEN:
     * EXEC SQL
     *     SELECT * FROM CLIENTE
     *     WHERE CLI_FECHA_ALTA BETWEEN :FECHA-DESDE AND :FECHA-HASTA
     * END-EXEC
     */
    List<ClienteEntity> findByFechaAltaBetween(LocalDate desde, LocalDate hasta);

    /**
     * COBOL embedded SQL with aggregate:
     * EXEC SQL
     *     SELECT SUM(CLI_SALDO) INTO :WS-TOTAL
     *     FROM CLIENTE
     *     WHERE CLI_ESTADO = 'A'
     * END-EXEC
     */
    @org.springframework.data.jpa.repository.Query(
        "SELECT SUM(c.saldo) FROM ClienteEntity c WHERE c.estado = 'A'")
    BigDecimal sumSaldoActivos();

    /**
     * COBOL embedded SQL with JOIN:
     * EXEC SQL
     *     SELECT C.CLI_ID, C.CLI_NOMBRE, SUM(M.MOV_MONTO)
     *     FROM CLIENTE C
     *     JOIN MOVIMIENTO M ON C.CLI_ID = M.CLI_ID
     *     WHERE M.MOV_FECHA >= :FECHA
     *     GROUP BY C.CLI_ID, C.CLI_NOMBRE
     * END-EXEC
     */
    @org.springframework.data.jpa.repository.Query("""
        SELECT new com.enterprise.migration.dto.ClienteMovimientosDTO(
            c.id, c.nombre, SUM(m.monto))
        FROM ClienteEntity c
        JOIN c.movimientos m
        WHERE m.fecha >= :fecha
        GROUP BY c.id, c.nombre
        """)
    List<ClienteMovimientosDTO> findClientesConMovimientos(
        @org.springframework.data.repository.query.Param("fecha")
        LocalDate fecha);

    /**
     * COBOL UPDATE with conditions:
     * EXEC SQL
     *     UPDATE CLIENTE
     *     SET CLI_ESTADO = 'I'
     *     WHERE CLI_ESTADO = 'A'
     *       AND CLI_FECHA_ALTA < :FECHA-LIMITE
     * END-EXEC
     */
    @org.springframework.data.jpa.repository.Modifying
    @org.springframework.data.jpa.repository.Query("""
        UPDATE ClienteEntity c
        SET c.estado = 'I'
        WHERE c.estado = 'A'
          AND c.fechaAlta < :fechaLimite
        """)
    int inactivarClientesAntiguos(
        @org.springframework.data.repository.query.Param("fechaLimite")
        LocalDate fechaLimite);
}

record ClienteMovimientosDTO(Long id, String nombre, BigDecimal totalMovimientos) {}
```

---

## 6. VSAM A PATRONES RELACIONALES

### 6.1 KSDS a Tabla con Clave Primaria

```java
package com.enterprise.migration.vsam;

import jakarta.persistence.*;
import java.math.BigDecimal;

/**
 * VSAM KSDS to relational table mapping
 *
 * COBOL VSAM definition:
 * //KSDSFILE DD DSN=PROD.CLIENTE.KSDS,DISP=SHR
 *
 * DEFINE CLUSTER (NAME(PROD.CLIENTE.KSDS) -
 *        INDEXED -
 *        KEYS(8 0) -
 *        RECORDSIZE(200 200) -
 *        SHAREOPTIONS(2 3))
 */
@Entity
@Table(name = "cliente",
       indexes = {
           @Index(name = "idx_cliente_nombre", columnList = "cli_nombre"),
           @Index(name = "idx_cliente_estado", columnList = "cli_estado")
       })
public class VsamKsdsToRelational {

    // Primary key - VSAM KEYS(8 0) - 8 bytes starting at position 0
    @Id
    @Column(name = "cli_id", length = 8)
    private String clienteId;

    // Alternate key - VSAM AIX
    @Column(name = "cli_documento", length = 20, unique = true)
    private String documento;

    @Column(name = "cli_nombre", length = 50)
    private String nombre;

    @Column(name = "cli_saldo", precision = 13, scale = 2)
    private BigDecimal saldo;

    @Column(name = "cli_estado", length = 1)
    private String estado;

    // Constructors
    public VsamKsdsToRelational() {}

    public VsamKsdsToRelational(String clienteId) {
        this.clienteId = clienteId;
    }

    // Getters and setters
    public String getClienteId() { return clienteId; }
    public void setClienteId(String clienteId) { this.clienteId = clienteId; }
    public String getDocumento() { return documento; }
    public void setDocumento(String documento) { this.documento = documento; }
    public String getNombre() { return nombre; }
    public void setNombre(String nombre) { this.nombre = nombre; }
    public BigDecimal getSaldo() { return saldo; }
    public void setSaldo(BigDecimal saldo) { this.saldo = saldo; }
    public String getEstado() { return estado; }
    public void setEstado(String estado) { this.estado = estado; }
}

/**
 * Repository implementing VSAM-like operations
 */
interface VsamKsdsRepository extends
        org.springframework.data.jpa.repository.JpaRepository<
            VsamKsdsToRelational, String> {

    /**
     * VSAM READ with primary key
     * READ KSDS-FILE INTO CLIENTE-RECORD KEY IS CLIENTE-ID
     */
    // Inherited from JpaRepository: findById(String id)

    /**
     * VSAM READ with alternate key
     * READ KSDS-FILE INTO CLIENTE-RECORD KEY IS CLI-DOCUMENTO
     */
    java.util.Optional<VsamKsdsToRelational> findByDocumento(String documento);

    /**
     * VSAM START (position for browse)
     * START KSDS-FILE KEY >= CLIENTE-ID
     */
    java.util.List<VsamKsdsToRelational>
        findByClienteIdGreaterThanEqualOrderByClienteId(String clienteId);

    /**
     * VSAM READ NEXT (sequential after START)
     * READ KSDS-FILE NEXT INTO CLIENTE-RECORD
     */
    // Use Stream or Pageable for sequential access
    java.util.stream.Stream<VsamKsdsToRelational>
        streamByClienteIdGreaterThanEqual(String clienteId);
}
```

### 6.2 ESDS a Tabla Secuencial

```java
package com.enterprise.migration.vsam;

import jakarta.persistence.*;
import java.time.LocalDateTime;

/**
 * VSAM ESDS (Entry-Sequenced Data Set) to relational table
 * ESDS has no key - records accessed by RBA (Relative Byte Address)
 *
 * Migration: Use auto-increment ID as sequence number
 */
@Entity
@Table(name = "auditoria_log")
public class VsamEsdsToRelational {

    // Auto-increment replaces RBA positioning
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "log_id")
    private Long id;

    @Column(name = "log_timestamp")
    private LocalDateTime timestamp;

    @Column(name = "log_tipo", length = 10)
    private String tipo;

    @Column(name = "log_usuario", length = 8)
    private String usuario;

    @Column(name = "log_mensaje", length = 200)
    private String mensaje;

    // Pre-persist hook to set timestamp
    @PrePersist
    protected void onCreate() {
        timestamp = LocalDateTime.now();
    }

    // Constructors
    public VsamEsdsToRelational() {}

    public VsamEsdsToRelational(String tipo, String usuario, String mensaje) {
        this.tipo = tipo;
        this.usuario = usuario;
        this.mensaje = mensaje;
    }

    // Getters and setters
    public Long getId() { return id; }
    public LocalDateTime getTimestamp() { return timestamp; }
    public String getTipo() { return tipo; }
    public void setTipo(String tipo) { this.tipo = tipo; }
    public String getUsuario() { return usuario; }
    public void setUsuario(String usuario) { this.usuario = usuario; }
    public String getMensaje() { return mensaje; }
    public void setMensaje(String mensaje) { this.mensaje = mensaje; }
}

/**
 * Repository for ESDS-like operations
 */
interface VsamEsdsRepository extends
        org.springframework.data.jpa.repository.JpaRepository<
            VsamEsdsToRelational, Long> {

    /**
     * VSAM WRITE (always appends)
     * WRITE ESDS-FILE FROM LOG-RECORD
     */
    // Inherited: save()

    /**
     * VSAM READ (sequential from beginning or position)
     * READ ESDS-FILE INTO LOG-RECORD
     */
    java.util.List<VsamEsdsToRelational> findAllByOrderByIdAsc();

    /**
     * Browse from specific position
     */
    java.util.List<VsamEsdsToRelational>
        findByIdGreaterThanOrderByIdAsc(Long afterId);

    /**
     * Browse by date range
     */
    java.util.List<VsamEsdsToRelational>
        findByTimestampBetweenOrderByIdAsc(
            LocalDateTime desde, LocalDateTime hasta);
}
```

---

## 7. TABLA DE MAPEO: SQL DB2 A SQL ESTÁNDAR

### 7.1 Funciones y Sintaxis

| DB2 z/OS | PostgreSQL | Oracle | MySQL |
|----------|------------|--------|-------|
| `CURRENT DATE` | `CURRENT_DATE` | `SYSDATE` | `CURDATE()` |
| `CURRENT TIME` | `CURRENT_TIME` | `SYSTIMESTAMP` | `CURTIME()` |
| `CURRENT TIMESTAMP` | `CURRENT_TIMESTAMP` | `SYSTIMESTAMP` | `NOW()` |
| `DATE(expr)` | `CAST(expr AS DATE)` | `TO_DATE()` | `DATE()` |
| `CHAR(expr)` | `CAST(expr AS VARCHAR)` | `TO_CHAR()` | `CAST()` |
| `INTEGER(expr)` | `CAST(expr AS INTEGER)` | `TO_NUMBER()` | `CAST()` |
| `DECIMAL(expr,p,s)` | `CAST(expr AS DECIMAL(p,s))` | `TO_NUMBER()` | `CAST()` |
| `SUBSTR(s,p,l)` | `SUBSTRING(s,p,l)` | `SUBSTR()` | `SUBSTRING()` |
| `LENGTH(s)` | `LENGTH(s)` | `LENGTH()` | `LENGTH()` |
| `LTRIM/RTRIM` | `LTRIM/RTRIM` | `LTRIM/RTRIM` | `LTRIM/RTRIM` |
| `COALESCE` | `COALESCE` | `NVL` | `COALESCE` |
| `NULLIF` | `NULLIF` | `NULLIF` | `NULLIF` |
| `FETCH FIRST n ROWS` | `LIMIT n` | `FETCH FIRST n ROWS` | `LIMIT n` |
| `OFFSET n ROWS` | `OFFSET n` | `OFFSET n ROWS` | `OFFSET n` |
| `WITH ... AS` | `WITH ... AS` | `WITH ... AS` | `WITH ... AS` |
| `ROW_NUMBER()` | `ROW_NUMBER()` | `ROW_NUMBER()` | `ROW_NUMBER()` |

### 7.2 Tipos de Datos

| DB2 z/OS | PostgreSQL | Oracle | MySQL |
|----------|------------|--------|-------|
| `SMALLINT` | `SMALLINT` | `NUMBER(5)` | `SMALLINT` |
| `INTEGER` | `INTEGER` | `NUMBER(10)` | `INT` |
| `BIGINT` | `BIGINT` | `NUMBER(19)` | `BIGINT` |
| `DECIMAL(p,s)` | `DECIMAL(p,s)` | `NUMBER(p,s)` | `DECIMAL(p,s)` |
| `REAL` | `REAL` | `BINARY_FLOAT` | `FLOAT` |
| `DOUBLE` | `DOUBLE PRECISION` | `BINARY_DOUBLE` | `DOUBLE` |
| `CHAR(n)` | `CHAR(n)` | `CHAR(n)` | `CHAR(n)` |
| `VARCHAR(n)` | `VARCHAR(n)` | `VARCHAR2(n)` | `VARCHAR(n)` |
| `DATE` | `DATE` | `DATE` | `DATE` |
| `TIME` | `TIME` | `TIMESTAMP` | `TIME` |
| `TIMESTAMP` | `TIMESTAMP` | `TIMESTAMP` | `DATETIME` |
| `CLOB` | `TEXT` | `CLOB` | `LONGTEXT` |
| `BLOB` | `BYTEA` | `BLOB` | `LONGBLOB` |

---

## 8. REFERENCIAS Y RECURSOS

### 8.1 Documentación de Bases de Datos

- [PostgreSQL Documentation](https://www.postgresql.org/docs/)
- [HikariCP GitHub](https://github.com/brettwooldridge/HikariCP)
- [Spring Data JPA Reference](https://docs.spring.io/spring-data/jpa/reference/html/)
- [IBM DB2 Documentation](https://www.ibm.com/docs/en/db2)

### 8.2 Mejores Prácticas de Migración

1. **Mantener compatibilidad de tipos**: Usar BigDecimal para decimales
2. **Connection pooling**: Siempre usar pools en producción
3. **Prepared statements**: Prevenir SQL injection
4. **Transacciones explícitas**: Control claro de commit/rollback
5. **Índices apropiados**: Migrar índices VSAM a índices SQL

---

```
═══════════════════════════════════════════════════════════════════════════════
                    FIN DEL DOCUMENTO JAVA_04_DB_LEGACY
                         ARCHAEON_CORE - v1.0.0
═══════════════════════════════════════════════════════════════════════════════
```
