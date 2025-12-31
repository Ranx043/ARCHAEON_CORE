---
título: "Java Batch Processing - Spring Batch para Migración COBOL"
código: JAVA-05-BATCH
versión: 1.0.0
fecha_creación: 2025-12-31
última_actualización: 2025-12-31
autor: ARCHAEON_CORE
dominio: LENGUAJES_MODERNOS
especialización: BATCH_PROCESSING
dependencias:
  - Spring Batch 5.x
  - Spring Boot 3.x
  - JDK 17+
tags:
  - java
  - spring-batch
  - cobol-migration
  - batch
  - etl
  - job-scheduling
nivel_complejidad: AVANZADO
estado: ACTIVO
---

# ═══════════════════════════════════════════════════════════════════════════════
# JAVA 05: BATCH PROCESSING - SPRING BATCH PARA MIGRACIÓN COBOL
# ARCHAEON_CORE - Sistema de Documentación de Lenguajes Legacy
# ═══════════════════════════════════════════════════════════════════════════════

## 1. INTRODUCCIÓN A SPRING BATCH

### 1.1 Spring Batch como Reemplazo de COBOL Batch

Spring Batch es el framework de procesamiento batch más robusto del ecosistema
Java, diseñado para manejar volúmenes de datos empresariales equivalentes a
los programas batch COBOL del mainframe.

```
┌─────────────────────────────────────────────────────────────────────────────┐
│              ARQUITECTURA BATCH: COBOL vs SPRING BATCH                      │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│   COBOL BATCH (JCL)                      SPRING BATCH                       │
│   ┌─────────────────┐                    ┌─────────────────┐               │
│   │ JCL JOB         │  ══════════════▶   │ Job             │               │
│   │ //JOBNAME JOB   │                    │ @Bean Job       │               │
│   └────────┬────────┘                    └────────┬────────┘               │
│            │                                      │                         │
│   ┌────────▼────────┐                    ┌────────▼────────┐               │
│   │ EXEC PGM=prog   │  ══════════════▶   │ Step            │               │
│   │ (Job Step)      │                    │ @Bean Step      │               │
│   └────────┬────────┘                    └────────┬────────┘               │
│            │                                      │                         │
│   ┌────────▼────────┐                    ┌────────▼────────┐               │
│   │ COBOL PROGRAM   │                    │ Tasklet or      │               │
│   │ READ/PROCESS/   │  ══════════════▶   │ Chunk:          │               │
│   │ WRITE           │                    │ Reader/Processor│               │
│   │                 │                    │ /Writer         │               │
│   └─────────────────┘                    └─────────────────┘               │
│                                                                             │
│   Key Equivalences:                                                         │
│   • JCL JOB CARD    → Job Configuration                                     │
│   • JCL STEP        → Step Definition                                       │
│   • DD statements   → ItemReader/ItemWriter                                 │
│   • PARM=           → JobParameters                                         │
│   • COND/IF         → Flow/Decision                                         │
│   • RESTART         → JobRepository (automatic)                             │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### 1.2 Dependencias Maven

```xml
<dependencies>
    <!-- Spring Batch -->
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-batch</artifactId>
    </dependency>

    <!-- Database for JobRepository -->
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-data-jpa</artifactId>
    </dependency>

    <dependency>
        <groupId>org.postgresql</groupId>
        <artifactId>postgresql</artifactId>
        <scope>runtime</scope>
    </dependency>

    <!-- Testing -->
    <dependency>
        <groupId>org.springframework.batch</groupId>
        <artifactId>spring-batch-test</artifactId>
        <scope>test</scope>
    </dependency>
</dependencies>
```

---

## 2. CONFIGURACIÓN BÁSICA DE SPRING BATCH

### 2.1 Configuración de Aplicación

```java
package com.enterprise.batch.config;

import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableScheduling;

/**
 * Main application class for Spring Batch
 * Equivalent to JCL job setup
 */
@SpringBootApplication
@EnableBatchProcessing
@EnableScheduling
public class BatchApplication {

    public static void main(String[] args) {
        // Run batch job and exit
        System.exit(SpringApplication.exit(
            SpringApplication.run(BatchApplication.class, args)
        ));
    }
}
```

```yaml
# application.yml
spring:
  batch:
    # Job repository database
    jdbc:
      initialize-schema: always  # Create batch tables

    job:
      enabled: false  # Don't auto-run jobs on startup

  datasource:
    url: jdbc:postgresql://localhost:5432/batch_db
    username: batch_user
    password: batch_pass

# Batch-specific settings
batch:
  input-path: /data/input
  output-path: /data/output
  chunk-size: 1000
  max-errors: 100
```

### 2.2 Job y Step Configuration

```java
package com.enterprise.batch.config;

import com.enterprise.batch.model.*;
import com.enterprise.batch.processor.*;
import com.enterprise.batch.reader.*;
import com.enterprise.batch.writer.*;
import org.springframework.batch.core.*;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.*;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * Job configuration
 *
 * COBOL JCL equivalent:
 * //PROCCLNT JOB (ACCT),'PROCESO CLIENTES',CLASS=A,MSGCLASS=X
 * //STEP010  EXEC PGM=PROCCLNT
 * //ENTRADA  DD DSN=PROD.CLIENTES.ENTRADA,DISP=SHR
 * //SALIDA   DD DSN=PROD.CLIENTES.SALIDA,DISP=(NEW,CATLG)
 * //ERRORES  DD DSN=PROD.CLIENTES.ERRORES,DISP=(NEW,CATLG)
 */
@Configuration
public class ClienteJobConfig {

    private final JobRepository jobRepository;
    private final PlatformTransactionManager transactionManager;

    public ClienteJobConfig(JobRepository jobRepository,
                           PlatformTransactionManager transactionManager) {
        this.jobRepository = jobRepository;
        this.transactionManager = transactionManager;
    }

    /**
     * Job definition - equivalent to JCL JOB card
     *
     * //PROCCLNT JOB (ACCT),'PROCESO CLIENTES',CLASS=A
     */
    @Bean
    public Job procesarClientesJob(Step validarArchivoStep,
                                   Step procesarClientesStep,
                                   Step generarReporteStep,
                                   JobCompletionListener listener) {
        return new JobBuilder("procesarClientesJob", jobRepository)
            .incrementer(new RunIdIncrementer())
            .listener(listener)
            // Steps ejecutados secuencialmente como JCL steps
            .start(validarArchivoStep)
            .next(procesarClientesStep)
            .next(generarReporteStep)
            .build();
    }

    /**
     * Validation step - Tasklet pattern
     *
     * //STEP010  EXEC PGM=VALIDA
     */
    @Bean
    public Step validarArchivoStep(ValidarArchivoTasklet tasklet) {
        return new StepBuilder("validarArchivoStep", jobRepository)
            .tasklet(tasklet, transactionManager)
            .build();
    }

    /**
     * Processing step - Chunk pattern
     *
     * //STEP020  EXEC PGM=PROCESA
     * //ENTRADA  DD DSN=PROD.CLIENTES.ENTRADA,DISP=SHR
     * //SALIDA   DD DSN=PROD.CLIENTES.SALIDA,DISP=(NEW,CATLG)
     */
    @Bean
    public Step procesarClientesStep(ItemReader<ClienteInput> reader,
                                     ItemProcessor<ClienteInput, ClienteOutput> processor,
                                     ItemWriter<ClienteOutput> writer,
                                     StepExecutionListener stepListener) {
        return new StepBuilder("procesarClientesStep", jobRepository)
            .<ClienteInput, ClienteOutput>chunk(1000, transactionManager)
            .reader(reader)
            .processor(processor)
            .writer(writer)
            .faultTolerant()
            .skipLimit(100)                    // Max errors before abort
            .skip(DataValidationException.class)
            .retryLimit(3)
            .retry(TransientDataAccessException.class)
            .listener(stepListener)
            .build();
    }

    /**
     * Report step - Tasklet pattern
     *
     * //STEP030  EXEC PGM=REPORTE
     */
    @Bean
    public Step generarReporteStep(GenerarReporteTasklet tasklet) {
        return new StepBuilder("generarReporteStep", jobRepository)
            .tasklet(tasklet, transactionManager)
            .build();
    }
}

// Exception classes
class DataValidationException extends RuntimeException {
    public DataValidationException(String message) {
        super(message);
    }
}

class TransientDataAccessException extends RuntimeException {
    public TransientDataAccessException(String message) {
        super(message);
    }
}
```

---

## 3. CHUNK PROCESSING PATTERN

### 3.1 ItemReader - Lectura de Datos

```java
package com.enterprise.batch.reader;

import com.enterprise.batch.model.ClienteInput;
import org.springframework.batch.item.*;
import org.springframework.batch.item.file.*;
import org.springframework.batch.item.file.mapping.*;
import org.springframework.batch.item.file.transform.*;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.FileSystemResource;

/**
 * ItemReader configurations for different file types
 */
@Configuration
public class ReaderConfig {

    @Value("${batch.input-path}")
    private String inputPath;

    /**
     * Fixed-length file reader - COBOL copybook format
     *
     * COBOL FD:
     * FD  ENTRADA-FILE.
     * 01  CLIENTE-REG.
     *     05  CLI-ID          PIC 9(8).
     *     05  CLI-NOMBRE      PIC X(50).
     *     05  CLI-SALDO       PIC S9(11)V99.
     *     05  CLI-ESTADO      PIC X(1).
     */
    @Bean
    public FlatFileItemReader<ClienteInput> fixedLengthReader() {
        return new FlatFileItemReaderBuilder<ClienteInput>()
            .name("clienteFixedReader")
            .resource(new FileSystemResource(inputPath + "/clientes.dat"))
            .lineMapper(fixedLengthLineMapper())
            .build();
    }

    private LineMapper<ClienteInput> fixedLengthLineMapper() {
        DefaultLineMapper<ClienteInput> mapper = new DefaultLineMapper<>();

        // Fixed-length tokenizer matching COBOL copybook
        FixedLengthTokenizer tokenizer = new FixedLengthTokenizer();
        tokenizer.setNames("id", "nombre", "saldo", "estado");
        tokenizer.setColumns(
            new Range(1, 8),    // CLI-ID PIC 9(8)
            new Range(9, 58),   // CLI-NOMBRE PIC X(50)
            new Range(59, 72),  // CLI-SALDO PIC S9(11)V99
            new Range(73, 73)   // CLI-ESTADO PIC X(1)
        );

        mapper.setLineTokenizer(tokenizer);
        mapper.setFieldSetMapper(fieldSet -> {
            ClienteInput cliente = new ClienteInput();
            cliente.setId(Long.parseLong(fieldSet.readString("id").trim()));
            cliente.setNombre(fieldSet.readString("nombre").trim());

            // Parse COBOL decimal S9(11)V99
            String saldoStr = fieldSet.readString("saldo").trim();
            cliente.setSaldo(parseCobolDecimal(saldoStr, 2));

            cliente.setEstado(fieldSet.readString("estado"));
            return cliente;
        });

        return mapper;
    }

    private java.math.BigDecimal parseCobolDecimal(String value, int scale) {
        if (value == null || value.isBlank()) {
            return java.math.BigDecimal.ZERO;
        }

        // Handle trailing sign
        boolean negative = value.endsWith("-");
        value = value.replace("-", "").replace("+", "");

        java.math.BigDecimal result = new java.math.BigDecimal(value);
        result = result.movePointLeft(scale);

        return negative ? result.negate() : result;
    }

    /**
     * CSV/Delimited file reader
     *
     * For modern data interchange formats
     */
    @Bean
    public FlatFileItemReader<ClienteInput> delimitedReader() {
        return new FlatFileItemReaderBuilder<ClienteInput>()
            .name("clienteDelimitedReader")
            .resource(new FileSystemResource(inputPath + "/clientes.csv"))
            .linesToSkip(1)  // Skip header
            .delimited()
            .delimiter(",")
            .names("id", "nombre", "saldo", "estado")
            .fieldSetMapper(new BeanWrapperFieldSetMapper<>() {{
                setTargetType(ClienteInput.class);
            }})
            .build();
    }

    /**
     * Database reader with paging - Cursor equivalent
     *
     * COBOL equivalent:
     * EXEC SQL DECLARE CLI-CURSOR CURSOR FOR
     *     SELECT * FROM CLIENTE WHERE ESTADO = 'A'
     * END-EXEC
     */
    @Bean
    public org.springframework.batch.item.database.JdbcPagingItemReader<ClienteInput>
            databaseReader(javax.sql.DataSource dataSource) {

        var reader = new org.springframework.batch.item.database
            .builder.JdbcPagingItemReaderBuilder<ClienteInput>()
            .name("clienteDbReader")
            .dataSource(dataSource)
            .selectClause("SELECT cli_id, cli_nombre, cli_saldo, cli_estado")
            .fromClause("FROM cliente")
            .whereClause("WHERE cli_estado = 'A'")
            .sortKeys(java.util.Map.of("cli_id",
                org.springframework.batch.item.database.Order.ASCENDING))
            .rowMapper((rs, rowNum) -> {
                ClienteInput c = new ClienteInput();
                c.setId(rs.getLong("cli_id"));
                c.setNombre(rs.getString("cli_nombre"));
                c.setSaldo(rs.getBigDecimal("cli_saldo"));
                c.setEstado(rs.getString("cli_estado"));
                return c;
            })
            .pageSize(1000)
            .build();

        return reader;
    }

    /**
     * Multi-file reader - Process multiple input files
     *
     * JCL equivalent:
     * //ENTRADA  DD DSN=PROD.FILE1,DISP=SHR
     * //         DD DSN=PROD.FILE2,DISP=SHR
     */
    @Bean
    public org.springframework.batch.item.file.MultiResourceItemReader<ClienteInput>
            multiFileReader() {

        var reader = new org.springframework.batch.item.file
            .MultiResourceItemReader<ClienteInput>();

        // Find all matching files
        org.springframework.core.io.Resource[] resources =
            new org.springframework.core.io.support.PathMatchingResourcePatternResolver()
                .getResources("file:" + inputPath + "/clientes_*.dat")
                .clone();

        reader.setResources(resources);
        reader.setDelegate(fixedLengthReader());

        return reader;
    }
}

/**
 * Input model class
 */
class ClienteInput {
    private Long id;
    private String nombre;
    private java.math.BigDecimal saldo;
    private String estado;

    // Getters and setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    public String getNombre() { return nombre; }
    public void setNombre(String nombre) { this.nombre = nombre; }
    public java.math.BigDecimal getSaldo() { return saldo; }
    public void setSaldo(java.math.BigDecimal saldo) { this.saldo = saldo; }
    public String getEstado() { return estado; }
    public void setEstado(String estado) { this.estado = estado; }
}
```

### 3.2 ItemProcessor - Transformación de Datos

```java
package com.enterprise.batch.processor;

import com.enterprise.batch.model.*;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.stereotype.Component;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;

/**
 * ItemProcessor - Business logic transformation
 *
 * COBOL equivalent:
 * PROCESO-CLIENTE.
 *     IF CLI-ESTADO = 'A'
 *         PERFORM CALCULAR-CATEGORIA
 *         PERFORM CALCULAR-INTERES
 *         MOVE 'P' TO CLI-ESTADO-SALIDA
 *     ELSE
 *         SET REGISTRO-RECHAZADO TO TRUE
 *     END-IF.
 */
@Component
public class ClienteProcessor
        implements ItemProcessor<ClienteInput, ClienteOutput> {

    private static final BigDecimal TASA_INTERES = new BigDecimal("0.05");

    /**
     * Process single item
     * Return null to skip/filter the record
     */
    @Override
    public ClienteOutput process(ClienteInput input) throws Exception {
        // IF CLI-ESTADO = 'A'
        if (!"A".equals(input.getEstado())) {
            // SET REGISTRO-RECHAZADO TO TRUE
            // Return null to skip this record
            return null;
        }

        // Validate data
        validarDatos(input);

        // Create output record
        ClienteOutput output = new ClienteOutput();

        // MOVE corresponding fields
        output.setId(input.getId());
        output.setNombre(input.getNombre().toUpperCase());
        output.setSaldoOriginal(input.getSaldo());

        // PERFORM CALCULAR-CATEGORIA
        output.setCategoria(calcularCategoria(input.getSaldo()));

        // PERFORM CALCULAR-INTERES
        BigDecimal interes = calcularInteres(input.getSaldo());
        output.setInteres(interes);
        output.setSaldoConInteres(input.getSaldo().add(interes));

        // Set processing metadata
        output.setFechaProceso(LocalDate.now());
        output.setEstadoProceso("P");

        return output;
    }

    private void validarDatos(ClienteInput input) {
        if (input.getId() == null || input.getId() <= 0) {
            throw new DataValidationException("ID inválido: " + input.getId());
        }

        if (input.getNombre() == null || input.getNombre().isBlank()) {
            throw new DataValidationException(
                "Nombre vacío para cliente: " + input.getId());
        }

        if (input.getSaldo() == null) {
            throw new DataValidationException(
                "Saldo nulo para cliente: " + input.getId());
        }
    }

    /**
     * CALCULAR-CATEGORIA paragraph
     *
     * EVALUATE TRUE
     *     WHEN CLI-SALDO >= 100000 MOVE 'VIP' TO CLI-CATEGORIA
     *     WHEN CLI-SALDO >= 50000  MOVE 'GOLD' TO CLI-CATEGORIA
     *     WHEN CLI-SALDO >= 10000  MOVE 'SILVER' TO CLI-CATEGORIA
     *     WHEN OTHER               MOVE 'BASIC' TO CLI-CATEGORIA
     * END-EVALUATE
     */
    private String calcularCategoria(BigDecimal saldo) {
        if (saldo.compareTo(new BigDecimal("100000")) >= 0) {
            return "VIP";
        } else if (saldo.compareTo(new BigDecimal("50000")) >= 0) {
            return "GOLD";
        } else if (saldo.compareTo(new BigDecimal("10000")) >= 0) {
            return "SILVER";
        } else {
            return "BASIC";
        }
    }

    /**
     * CALCULAR-INTERES paragraph
     *
     * COMPUTE CLI-INTERES ROUNDED = CLI-SALDO * WS-TASA-INTERES
     */
    private BigDecimal calcularInteres(BigDecimal saldo) {
        return saldo.multiply(TASA_INTERES)
            .setScale(2, RoundingMode.HALF_UP);
    }
}

/**
 * Output model class
 */
class ClienteOutput {
    private Long id;
    private String nombre;
    private BigDecimal saldoOriginal;
    private BigDecimal interes;
    private BigDecimal saldoConInteres;
    private String categoria;
    private LocalDate fechaProceso;
    private String estadoProceso;

    // Getters and setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    public String getNombre() { return nombre; }
    public void setNombre(String nombre) { this.nombre = nombre; }
    public BigDecimal getSaldoOriginal() { return saldoOriginal; }
    public void setSaldoOriginal(BigDecimal saldoOriginal) {
        this.saldoOriginal = saldoOriginal;
    }
    public BigDecimal getInteres() { return interes; }
    public void setInteres(BigDecimal interes) { this.interes = interes; }
    public BigDecimal getSaldoConInteres() { return saldoConInteres; }
    public void setSaldoConInteres(BigDecimal saldoConInteres) {
        this.saldoConInteres = saldoConInteres;
    }
    public String getCategoria() { return categoria; }
    public void setCategoria(String categoria) { this.categoria = categoria; }
    public LocalDate getFechaProceso() { return fechaProceso; }
    public void setFechaProceso(LocalDate fechaProceso) {
        this.fechaProceso = fechaProceso;
    }
    public String getEstadoProceso() { return estadoProceso; }
    public void setEstadoProceso(String estadoProceso) {
        this.estadoProceso = estadoProceso;
    }
}

/**
 * Composite processor - Chain multiple processors
 *
 * COBOL equivalent:
 * PERFORM VALIDAR-DATOS
 * PERFORM TRANSFORMAR-DATOS
 * PERFORM ENRIQUECER-DATOS
 */
@Component
class CompositeClienteProcessor
        implements ItemProcessor<ClienteInput, ClienteOutput> {

    private final ValidacionProcessor validacionProcessor;
    private final TransformacionProcessor transformacionProcessor;
    private final EnriquecimientoProcessor enriquecimientoProcessor;

    public CompositeClienteProcessor(
            ValidacionProcessor validacionProcessor,
            TransformacionProcessor transformacionProcessor,
            EnriquecimientoProcessor enriquecimientoProcessor) {
        this.validacionProcessor = validacionProcessor;
        this.transformacionProcessor = transformacionProcessor;
        this.enriquecimientoProcessor = enriquecimientoProcessor;
    }

    @Override
    public ClienteOutput process(ClienteInput input) throws Exception {
        // Chain processors
        ClienteInput validated = validacionProcessor.process(input);
        if (validated == null) return null;

        ClienteOutput transformed = transformacionProcessor.process(validated);
        if (transformed == null) return null;

        return enriquecimientoProcessor.process(transformed);
    }
}

// Individual processors (simplified)
@Component
class ValidacionProcessor implements ItemProcessor<ClienteInput, ClienteInput> {
    @Override
    public ClienteInput process(ClienteInput item) { return item; }
}

@Component
class TransformacionProcessor
        implements ItemProcessor<ClienteInput, ClienteOutput> {
    @Override
    public ClienteOutput process(ClienteInput item) {
        return new ClienteOutput();
    }
}

@Component
class EnriquecimientoProcessor
        implements ItemProcessor<ClienteOutput, ClienteOutput> {
    @Override
    public ClienteOutput process(ClienteOutput item) { return item; }
}
```

### 3.3 ItemWriter - Escritura de Resultados

```java
package com.enterprise.batch.writer;

import com.enterprise.batch.model.ClienteOutput;
import org.springframework.batch.item.*;
import org.springframework.batch.item.database.*;
import org.springframework.batch.item.file.*;
import org.springframework.batch.item.file.transform.*;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.FileSystemResource;
import javax.sql.DataSource;
import java.math.BigDecimal;

/**
 * ItemWriter configurations
 */
@Configuration
public class WriterConfig {

    @Value("${batch.output-path}")
    private String outputPath;

    /**
     * Fixed-length file writer - COBOL format output
     *
     * COBOL FD:
     * FD  SALIDA-FILE.
     * 01  CLIENTE-SALIDA.
     *     05  CLI-ID          PIC 9(8).
     *     05  CLI-NOMBRE      PIC X(50).
     *     05  CLI-SALDO-ORIG  PIC S9(11)V99.
     *     05  CLI-INTERES     PIC S9(9)V99.
     *     05  CLI-SALDO-FINAL PIC S9(11)V99.
     *     05  CLI-CATEGORIA   PIC X(6).
     *     05  CLI-FECHA       PIC 9(8).
     *     05  CLI-ESTADO      PIC X(1).
     */
    @Bean
    public FlatFileItemWriter<ClienteOutput> fixedLengthWriter() {
        return new FlatFileItemWriterBuilder<ClienteOutput>()
            .name("clienteFixedWriter")
            .resource(new FileSystemResource(outputPath + "/clientes_proc.dat"))
            .lineAggregator(item -> {
                StringBuilder sb = new StringBuilder();

                // CLI-ID PIC 9(8)
                sb.append(String.format("%08d", item.getId()));

                // CLI-NOMBRE PIC X(50)
                sb.append(String.format("%-50s", item.getNombre()));

                // CLI-SALDO-ORIG PIC S9(11)V99
                sb.append(formatCobolDecimal(item.getSaldoOriginal(), 11, 2));

                // CLI-INTERES PIC S9(9)V99
                sb.append(formatCobolDecimal(item.getInteres(), 9, 2));

                // CLI-SALDO-FINAL PIC S9(11)V99
                sb.append(formatCobolDecimal(item.getSaldoConInteres(), 11, 2));

                // CLI-CATEGORIA PIC X(6)
                sb.append(String.format("%-6s", item.getCategoria()));

                // CLI-FECHA PIC 9(8) YYYYMMDD
                sb.append(item.getFechaProceso().format(
                    java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd")));

                // CLI-ESTADO PIC X(1)
                sb.append(item.getEstadoProceso());

                return sb.toString();
            })
            .build();
    }

    private String formatCobolDecimal(BigDecimal value, int intDigits,
                                      int decDigits) {
        if (value == null) {
            value = BigDecimal.ZERO;
        }

        boolean negative = value.compareTo(BigDecimal.ZERO) < 0;
        value = value.abs();

        // Move decimal point right and format as integer
        long unscaled = value.movePointRight(decDigits).longValue();
        String formatted = String.format("%0" + (intDigits + decDigits) + "d",
            unscaled);

        // Add trailing sign
        return formatted + (negative ? "-" : "+");
    }

    /**
     * CSV file writer - Modern format
     */
    @Bean
    public FlatFileItemWriter<ClienteOutput> csvWriter() {
        return new FlatFileItemWriterBuilder<ClienteOutput>()
            .name("clienteCsvWriter")
            .resource(new FileSystemResource(outputPath + "/clientes_proc.csv"))
            .headerCallback(writer ->
                writer.write("ID,NOMBRE,SALDO_ORIG,INTERES,SALDO_FINAL," +
                            "CATEGORIA,FECHA,ESTADO"))
            .delimited()
            .delimiter(",")
            .names("id", "nombre", "saldoOriginal", "interes",
                   "saldoConInteres", "categoria", "fechaProceso",
                   "estadoProceso")
            .build();
    }

    /**
     * Database writer
     *
     * COBOL equivalent:
     * EXEC SQL
     *     INSERT INTO CLIENTE_PROCESADO
     *         (CLI_ID, CLI_NOMBRE, CLI_SALDO, ...)
     *     VALUES (:CLI-ID, :CLI-NOMBRE, :CLI-SALDO, ...)
     * END-EXEC
     */
    @Bean
    public JdbcBatchItemWriter<ClienteOutput> databaseWriter(
            DataSource dataSource) {

        return new JdbcBatchItemWriterBuilder<ClienteOutput>()
            .dataSource(dataSource)
            .sql("""
                INSERT INTO cliente_procesado
                    (cli_id, cli_nombre, cli_saldo_orig, cli_interes,
                     cli_saldo_final, cli_categoria, cli_fecha_proceso,
                     cli_estado)
                VALUES
                    (:id, :nombre, :saldoOriginal, :interes,
                     :saldoConInteres, :categoria, :fechaProceso,
                     :estadoProceso)
                """)
            .beanMapped()
            .build();
    }

    /**
     * Composite writer - Write to multiple destinations
     *
     * JCL equivalent:
     * //SALIDA1  DD DSN=PROD.ARCHIVO1,...
     * //SALIDA2  DD DSN=PROD.ARCHIVO2,...
     */
    @Bean
    public org.springframework.batch.item.support.CompositeItemWriter<ClienteOutput>
            compositeWriter(FlatFileItemWriter<ClienteOutput> fileWriter,
                           JdbcBatchItemWriter<ClienteOutput> dbWriter) {

        var writer = new org.springframework.batch.item.support
            .CompositeItemWriter<ClienteOutput>();

        writer.setDelegates(java.util.List.of(fileWriter, dbWriter));
        return writer;
    }

    /**
     * Classified writer - Route to different writers based on data
     *
     * COBOL equivalent:
     * EVALUATE CLI-CATEGORIA
     *     WHEN 'VIP'   WRITE VIP-FILE
     *     WHEN 'GOLD'  WRITE GOLD-FILE
     *     WHEN OTHER   WRITE STANDARD-FILE
     * END-EVALUATE
     */
    @Bean
    public org.springframework.batch.item.support.ClassifierCompositeItemWriter<ClienteOutput>
            classifiedWriter() {

        var writer = new org.springframework.batch.item.support
            .ClassifierCompositeItemWriter<ClienteOutput>();

        writer.setClassifier(item -> {
            return switch (item.getCategoria()) {
                case "VIP" -> vipWriter();
                case "GOLD" -> goldWriter();
                default -> standardWriter();
            };
        });

        return writer;
    }

    private ItemWriter<ClienteOutput> vipWriter() {
        return new FlatFileItemWriterBuilder<ClienteOutput>()
            .name("vipWriter")
            .resource(new FileSystemResource(outputPath + "/vip_clientes.dat"))
            .delimited()
            .names("id", "nombre", "saldoConInteres")
            .build();
    }

    private ItemWriter<ClienteOutput> goldWriter() {
        return new FlatFileItemWriterBuilder<ClienteOutput>()
            .name("goldWriter")
            .resource(new FileSystemResource(outputPath + "/gold_clientes.dat"))
            .delimited()
            .names("id", "nombre", "saldoConInteres")
            .build();
    }

    private ItemWriter<ClienteOutput> standardWriter() {
        return new FlatFileItemWriterBuilder<ClienteOutput>()
            .name("standardWriter")
            .resource(new FileSystemResource(outputPath + "/standard_clientes.dat"))
            .delimited()
            .names("id", "nombre", "saldoConInteres")
            .build();
    }
}
```

---

## 4. ERROR HANDLING Y RESTART

### 4.1 Skip y Retry Policies

```java
package com.enterprise.batch.config;

import org.springframework.batch.core.*;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.retry.backoff.*;
import org.springframework.retry.policy.SimpleRetryPolicy;

/**
 * Error handling configuration
 *
 * COBOL equivalent:
 * IF SQLCODE NOT = 0
 *     ADD 1 TO WS-ERROR-COUNT
 *     IF WS-ERROR-COUNT > WS-MAX-ERRORS
 *         PERFORM ABORT-JOB
 *     ELSE
 *         PERFORM LOG-ERROR
 *         CONTINUE
 *     END-IF
 * END-IF
 */
@Configuration
public class ErrorHandlingConfig {

    @Bean
    public Step faultTolerantStep(
            org.springframework.batch.core.repository.JobRepository jobRepository,
            org.springframework.transaction.PlatformTransactionManager txManager,
            org.springframework.batch.item.ItemReader<ClienteInput> reader,
            ItemProcessor<ClienteInput, ClienteOutput> processor,
            org.springframework.batch.item.ItemWriter<ClienteOutput> writer,
            SkipListener<ClienteInput, ClienteOutput> skipListener) {

        return new StepBuilder("faultTolerantStep", jobRepository)
            .<ClienteInput, ClienteOutput>chunk(1000, txManager)
            .reader(reader)
            .processor(processor)
            .writer(writer)

            // Enable fault tolerance
            .faultTolerant()

            // Skip policy - continue processing despite errors
            .skipLimit(100)  // Max records to skip
            .skip(DataValidationException.class)
            .skip(java.lang.NumberFormatException.class)

            // No skip for critical errors
            .noSkip(java.lang.OutOfMemoryError.class)

            // Retry policy - retry transient failures
            .retryLimit(3)
            .retry(java.net.SocketTimeoutException.class)
            .retry(org.springframework.dao.TransientDataAccessException.class)

            // Backoff policy for retries
            .backOffPolicy(exponentialBackoff())

            // Listeners for monitoring
            .listener(skipListener)

            .build();
    }

    private BackOffPolicy exponentialBackoff() {
        ExponentialBackOffPolicy policy = new ExponentialBackOffPolicy();
        policy.setInitialInterval(1000);  // 1 second
        policy.setMultiplier(2.0);        // Double each retry
        policy.setMaxInterval(30000);     // Max 30 seconds
        return policy;
    }
}

/**
 * Skip listener for logging and reporting skipped records
 */
@org.springframework.stereotype.Component
class BatchSkipListener implements SkipListener<ClienteInput, ClienteOutput> {

    private final java.util.concurrent.atomic.AtomicInteger skipCount =
        new java.util.concurrent.atomic.AtomicInteger(0);

    @Override
    public void onSkipInRead(Throwable t) {
        // Error reading record
        logError("READ", null, t);
    }

    @Override
    public void onSkipInProcess(ClienteInput item, Throwable t) {
        // Error processing record
        logError("PROCESS", item, t);
    }

    @Override
    public void onSkipInWrite(ClienteOutput item, Throwable t) {
        // Error writing record
        logError("WRITE", item, t);
    }

    private void logError(String phase, Object item, Throwable t) {
        int count = skipCount.incrementAndGet();
        System.err.printf(
            "SKIP #%d [%s]: %s - %s%n",
            count, phase,
            item != null ? item.toString() : "N/A",
            t.getMessage()
        );

        // Could also write to error file or database
    }

    public int getSkipCount() {
        return skipCount.get();
    }
}
```

### 4.2 Restart y Checkpointing

```java
package com.enterprise.batch.restart;

import org.springframework.batch.core.*;
import org.springframework.batch.core.launch.*;
import org.springframework.batch.core.repository.*;
import org.springframework.stereotype.Service;
import java.util.*;

/**
 * Job restart service
 *
 * COBOL equivalent:
 * //STEP010  EXEC PGM=PROGRAMA,RESTART=STEP020
 *
 * Spring Batch automatically tracks:
 * - Last successful commit point
 * - Job parameters
 * - Execution context
 */
@Service
public class JobRestartService {

    private final JobLauncher jobLauncher;
    private final JobRepository jobRepository;
    private final Job job;

    public JobRestartService(JobLauncher jobLauncher,
                            JobRepository jobRepository,
                            Job job) {
        this.jobLauncher = jobLauncher;
        this.jobRepository = jobRepository;
        this.job = job;
    }

    /**
     * Start new job execution
     */
    public JobExecution startJob(Map<String, String> parameters)
            throws Exception {

        JobParametersBuilder builder = new JobParametersBuilder();
        parameters.forEach(builder::addString);

        // Add unique run ID for re-execution
        builder.addLong("run.id", System.currentTimeMillis());

        return jobLauncher.run(job, builder.toJobParameters());
    }

    /**
     * Restart failed job from last checkpoint
     *
     * Spring Batch automatically resumes from:
     * - Last committed chunk in chunk-oriented step
     * - Last savepoint in tasklet step
     */
    public JobExecution restartJob(long executionId) throws Exception {
        JobExecution lastExecution = jobRepository.getLastJobExecution(
            job.getName(),
            new JobParametersBuilder()
                .addLong("execution.id", executionId)
                .toJobParameters()
        );

        if (lastExecution == null) {
            throw new JobExecutionException(
                "No execution found with ID: " + executionId);
        }

        if (!lastExecution.getStatus().equals(BatchStatus.FAILED)) {
            throw new JobExecutionException(
                "Can only restart FAILED executions");
        }

        // Restart with same parameters
        return jobLauncher.run(job, lastExecution.getJobParameters());
    }

    /**
     * Get execution status
     */
    public JobExecutionStatus getStatus(long executionId) {
        JobExecution execution = jobRepository.getJobExecution(executionId);

        if (execution == null) {
            return new JobExecutionStatus("UNKNOWN", 0, 0, 0);
        }

        long readCount = 0;
        long writeCount = 0;
        long skipCount = 0;

        for (StepExecution step : execution.getStepExecutions()) {
            readCount += step.getReadCount();
            writeCount += step.getWriteCount();
            skipCount += step.getSkipCount();
        }

        return new JobExecutionStatus(
            execution.getStatus().name(),
            readCount,
            writeCount,
            skipCount
        );
    }

    public record JobExecutionStatus(
        String status,
        long readCount,
        long writeCount,
        long skipCount
    ) {}
}
```

---

## 5. JOB SCHEDULING Y ORQUESTACIÓN

### 5.1 Flujos Condicionales

```java
package com.enterprise.batch.flow;

import org.springframework.batch.core.*;
import org.springframework.batch.core.job.builder.*;
import org.springframework.batch.core.job.flow.*;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Conditional job flows
 *
 * JCL equivalent:
 * //STEP010  EXEC PGM=VALIDA
 * //STEP020  EXEC PGM=PROCESA,COND=(0,NE,STEP010)
 * //STEP030  EXEC PGM=REPORTE,COND=(0,NE,STEP020)
 * //STEPERR  EXEC PGM=ERROR,COND=(0,EQ,STEP020)
 */
@Configuration
public class ConditionalFlowConfig {

    @Bean
    public Job conditionalJob(JobRepository jobRepository,
                             Step validarStep,
                             Step procesarStep,
                             Step reporteStep,
                             Step errorStep) {

        return new JobBuilder("conditionalJob", jobRepository)
            // Start with validation
            .start(validarStep)

            // If validation succeeds, process
            .on("COMPLETED").to(procesarStep)

            // If validation fails, go to error handling
            .from(validarStep)
            .on("FAILED").to(errorStep)

            // After processing, generate report
            .from(procesarStep)
            .on("COMPLETED").to(reporteStep)

            // If processing fails, go to error handling
            .from(procesarStep)
            .on("FAILED").to(errorStep)

            // End states
            .from(reporteStep)
            .on("*").end()

            .from(errorStep)
            .on("*").fail()

            .build()
            .build();
    }

    /**
     * Custom exit status for complex conditions
     *
     * JCL equivalent:
     * //STEP010  EXEC PGM=EVALUA
     * //         IF (STEP010.RC = 4) THEN
     * //STEP020    EXEC PGM=OPCION_A
     * //         ELSE IF (STEP010.RC = 8) THEN
     * //STEP030    EXEC PGM=OPCION_B
     * //         ENDIF
     */
    @Bean
    public Job multiPathJob(JobRepository jobRepository,
                           Step evaluarStep,
                           Step opcionAStep,
                           Step opcionBStep,
                           Step opcionDefaultStep) {

        return new JobBuilder("multiPathJob", jobRepository)
            .start(evaluarStep)
            .on("OPCION_A").to(opcionAStep)
            .from(evaluarStep)
            .on("OPCION_B").to(opcionBStep)
            .from(evaluarStep)
            .on("*").to(opcionDefaultStep)  // Default path
            .end()
            .build();
    }

    /**
     * Step that determines next path
     */
    @Bean
    public Step evaluarStep(JobRepository jobRepository,
                           org.springframework.transaction.PlatformTransactionManager txManager,
                           EvaluarDecider decider) {

        return new StepBuilder("evaluarStep", jobRepository)
            .tasklet((contribution, chunkContext) -> {
                // Evaluate condition and set exit status
                String resultado = evaluarCondicion();

                if ("A".equals(resultado)) {
                    contribution.setExitStatus(new ExitStatus("OPCION_A"));
                } else if ("B".equals(resultado)) {
                    contribution.setExitStatus(new ExitStatus("OPCION_B"));
                }

                return org.springframework.batch.repeat.RepeatStatus.FINISHED;
            }, txManager)
            .build();
    }

    private String evaluarCondicion() {
        // Business logic to determine path
        return "A";
    }
}

/**
 * Job Execution Decider for complex flow control
 */
@org.springframework.stereotype.Component
class EvaluarDecider implements JobExecutionDecider {

    @Override
    public FlowExecutionStatus decide(JobExecution jobExecution,
                                       StepExecution stepExecution) {
        // Access step context for decision
        String resultado = stepExecution.getExecutionContext()
            .getString("resultado", "DEFAULT");

        return new FlowExecutionStatus(resultado);
    }
}
```

### 5.2 Parallel Processing

```java
package com.enterprise.batch.parallel;

import org.springframework.batch.core.*;
import org.springframework.batch.core.job.builder.FlowBuilder;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.job.flow.Flow;
import org.springframework.batch.core.job.flow.support.SimpleFlow;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.SimpleAsyncTaskExecutor;
import org.springframework.core.task.TaskExecutor;

/**
 * Parallel step execution
 *
 * JCL equivalent with parallel regions:
 * Multiple steps running concurrently
 */
@Configuration
public class ParallelProcessingConfig {

    /**
     * Split flow - Run multiple flows in parallel
     */
    @Bean
    public Job parallelJob(JobRepository jobRepository,
                          Flow clientesFlow,
                          Flow productosFlow,
                          Flow movimientosFlow,
                          Step consolidarStep) {

        // Parallel flows
        Flow parallelFlow = new FlowBuilder<SimpleFlow>("parallelFlow")
            .split(taskExecutor())
            .add(clientesFlow, productosFlow, movimientosFlow)
            .build();

        return new JobBuilder("parallelJob", jobRepository)
            .start(parallelFlow)
            .next(consolidarStep)  // After all parallel flows complete
            .end()
            .build();
    }

    @Bean
    public Flow clientesFlow(Step procesarClientesStep) {
        return new FlowBuilder<SimpleFlow>("clientesFlow")
            .start(procesarClientesStep)
            .build();
    }

    @Bean
    public Flow productosFlow(Step procesarProductosStep) {
        return new FlowBuilder<SimpleFlow>("productosFlow")
            .start(procesarProductosStep)
            .build();
    }

    @Bean
    public Flow movimientosFlow(Step procesarMovimientosStep) {
        return new FlowBuilder<SimpleFlow>("movimientosFlow")
            .start(procesarMovimientosStep)
            .build();
    }

    /**
     * Task executor for parallel execution
     */
    @Bean
    public TaskExecutor taskExecutor() {
        SimpleAsyncTaskExecutor executor = new SimpleAsyncTaskExecutor();
        executor.setConcurrencyLimit(4);  // Max parallel threads
        return executor;
    }

    /**
     * Partitioned step - Process data in parallel partitions
     *
     * Like processing multiple input files simultaneously
     */
    @Bean
    public Step partitionedStep(JobRepository jobRepository,
                               org.springframework.batch.core.partition.support.Partitioner partitioner,
                               Step workerStep) {

        return new StepBuilder("partitionedStep", jobRepository)
            .partitioner("workerStep", partitioner)
            .step(workerStep)
            .gridSize(4)  // Number of partitions
            .taskExecutor(taskExecutor())
            .build();
    }
}

/**
 * Range partitioner - Split data by ID ranges
 */
@org.springframework.stereotype.Component
class RangePartitioner implements
        org.springframework.batch.core.partition.support.Partitioner {

    private final javax.sql.DataSource dataSource;

    public RangePartitioner(javax.sql.DataSource dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public java.util.Map<String, org.springframework.batch.item.ExecutionContext>
            partition(int gridSize) {

        java.util.Map<String, org.springframework.batch.item.ExecutionContext>
            partitions = new java.util.HashMap<>();

        // Get min/max IDs
        long minId = getMinId();
        long maxId = getMaxId();
        long range = (maxId - minId) / gridSize + 1;

        for (int i = 0; i < gridSize; i++) {
            org.springframework.batch.item.ExecutionContext context =
                new org.springframework.batch.item.ExecutionContext();

            long start = minId + (i * range);
            long end = Math.min(start + range - 1, maxId);

            context.putLong("minId", start);
            context.putLong("maxId", end);

            partitions.put("partition" + i, context);
        }

        return partitions;
    }

    private long getMinId() {
        // Query database for min ID
        return 1L;
    }

    private long getMaxId() {
        // Query database for max ID
        return 1000000L;
    }
}
```

---

## 6. PERFORMANCE OPTIMIZATION

### 6.1 Chunk Size Tuning

```java
package com.enterprise.batch.performance;

/**
 * Performance tuning guidelines
 *
 * Chunk size considerations:
 * - Too small: Excessive commit overhead
 * - Too large: Memory issues, long recovery time
 *
 * General guidelines:
 * - Start with 100-1000 items
 * - Increase if commits are slow
 * - Decrease if memory is constrained
 */
public class PerformanceTuning {

    /*
     * ═══════════════════════════════════════════════════════════════════════
     * CHUNK SIZE RECOMMENDATIONS
     * ═══════════════════════════════════════════════════════════════════════
     *
     * Data Type          Recommended Chunk Size
     * ──────────────────────────────────────────
     * Small records      1000 - 5000
     * Medium records     500 - 1000
     * Large records      100 - 500
     * Complex processing 50 - 200
     *
     * Database writes    500 - 2000 (batch insert)
     * File writes        1000 - 5000
     * API calls          10 - 100
     *
     * ═══════════════════════════════════════════════════════════════════════
     * MEMORY CALCULATION
     * ═══════════════════════════════════════════════════════════════════════
     *
     * Memory per chunk = chunk_size * (input_size + output_size + overhead)
     *
     * Example:
     * - Input record: 500 bytes
     * - Output record: 800 bytes
     * - Overhead: 200 bytes
     * - Chunk size: 1000
     *
     * Memory = 1000 * (500 + 800 + 200) = 1.5 MB per chunk
     *
     * ═══════════════════════════════════════════════════════════════════════
     * COMMIT INTERVAL TUNING
     * ═══════════════════════════════════════════════════════════════════════
     *
     * Target: 1-5 seconds per commit
     *
     * If commits are too fast (< 1s):
     * - Increase chunk size
     * - Reduce logging
     *
     * If commits are too slow (> 5s):
     * - Decrease chunk size
     * - Optimize database
     * - Add indexes
     */
}
```

### 6.2 Database Optimization

```java
package com.enterprise.batch.performance;

import org.springframework.batch.item.database.builder.*;
import javax.sql.DataSource;

/**
 * Database performance optimizations
 */
public class DatabaseOptimizations {

    /**
     * Optimized database reader
     */
    public org.springframework.batch.item.database.JdbcCursorItemReader<Object>
            optimizedReader(DataSource dataSource) {

        return new JdbcCursorItemReaderBuilder<Object>()
            .name("optimizedReader")
            .dataSource(dataSource)
            .sql("SELECT * FROM large_table WHERE status = 'P'")
            .rowMapper((rs, rowNum) -> new Object())

            // Fetch size optimization
            .fetchSize(1000)  // Match chunk size

            // Statement timeout
            .queryTimeout(300)  // 5 minutes

            // Use server-side cursor (PostgreSQL)
            .useSharedExtendedConnection(true)

            .build();
    }

    /**
     * Optimized database writer
     */
    public org.springframework.batch.item.database.JdbcBatchItemWriter<Object>
            optimizedWriter(DataSource dataSource) {

        return new JdbcBatchItemWriterBuilder<Object>()
            .dataSource(dataSource)
            .sql("""
                INSERT INTO target_table (col1, col2, col3)
                VALUES (:col1, :col2, :col3)
                ON CONFLICT (col1) DO UPDATE SET
                    col2 = EXCLUDED.col2,
                    col3 = EXCLUDED.col3
                """)
            .beanMapped()

            // Assert updates - fail if rows not affected
            .assertUpdates(false)  // Disable for upserts

            .build();
    }
}
```

---

## 7. TABLA DE MAPEO: JCL A SPRING BATCH

### 7.1 Conceptos Principales

| JCL/COBOL | Spring Batch | Descripción |
|-----------|--------------|-------------|
| JOB card | `@Bean Job` | Define trabajo completo |
| EXEC PGM | `Step` | Unidad de ejecución |
| DD statement | `ItemReader/Writer` | Definición de archivos |
| PARM= | `JobParameters` | Parámetros de entrada |
| COND= | `on().to()` | Flujo condicional |
| RESTART= | Automatic | Reinicio desde checkpoint |
| DISP=SHR | Shared resource | Recurso compartido |
| SORT | `Sort in reader/writer` | Ordenamiento |
| MERGE | `CompositeItemReader` | Fusión de archivos |
| IEBGENER | `FlatFileItemReader/Writer` | Copia de archivos |
| IDCAMS | Repository operations | Operaciones VSAM |

### 7.2 Patrones de Procesamiento

| COBOL Pattern | Spring Batch Pattern |
|---------------|---------------------|
| `READ UNTIL EOF` | `ItemReader.read()` returns null |
| `WRITE OUTPUT` | `ItemWriter.write(chunk)` |
| `PERFORM PROCESS` | `ItemProcessor.process()` |
| `ADD 1 TO COUNT` | `StepExecution.getReadCount()` |
| `SORT WORK-FILE` | Pre-sort or `SortItemReader` |
| `MERGE FILES` | `MultiResourceItemReader` |
| `CONTROL BREAK` | Group processing in processor |

---

## 8. REFERENCIAS Y RECURSOS

### 8.1 Documentación Oficial

- [Spring Batch Reference](https://docs.spring.io/spring-batch/reference/html/)
- [Spring Batch Architecture](https://docs.spring.io/spring-batch/docs/current/reference/html/domain.html)
- [Item Readers and Writers](https://docs.spring.io/spring-batch/docs/current/reference/html/readersAndWriters.html)

### 8.2 Mejores Prácticas

1. **Chunk Size**: Ajustar según tipo de datos y recursos
2. **Idempotencia**: Diseñar para reinicio seguro
3. **Monitoreo**: Usar Spring Batch Admin o custom listeners
4. **Testing**: Usar `@SpringBatchTest` para pruebas
5. **Logging**: Balancear detalle vs rendimiento

---

```
═══════════════════════════════════════════════════════════════════════════════
                    FIN DEL DOCUMENTO JAVA_05_BATCH
                         ARCHAEON_CORE - v1.0.0
═══════════════════════════════════════════════════════════════════════════════
```
