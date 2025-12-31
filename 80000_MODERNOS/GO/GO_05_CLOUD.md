---
title: "GO_05_CLOUD"
version: "1.0.0"
date: "2025-12-31"
author: "ARCHAEON_CORE"
domain: "SOUL_CORE/ARCHAEON/MODERNOS/GO"
type: "technical_documentation"
classification: "cloud_native"
language: "Go"
paradigm: "cloud_native_systems"
keywords:
  - cloud_native
  - docker
  - kubernetes
  - operators
  - grpc
  - protobuf
  - microservices
  - observability
  - distributed_systems
dependencies:
  - go_1.21+
  - docker
  - kubernetes
  - grpc
  - prometheus
  - opentelemetry
related_docs:
  - GO_01_FUNDAMENTOS.md
  - GO_02_CONCURRENCIA.md
  - GO_04_SISTEMAS.md
---

# GO_05_CLOUD

## Tabla de Contenidos

1. [Patrones Cloud Native](#patrones-cloud-native)
2. [Docker y Contenedores](#docker-y-contenedores)
3. [Kubernetes Operators](#kubernetes-operators)
4. [gRPC y Protocol Buffers](#grpc-y-protocol-buffers)
5. [Arquitectura de Microservicios](#arquitectura-de-microservicios)
6. [Observabilidad](#observabilidad)
7. [Mejores Prácticas Cloud Native](#mejores-prácticas-cloud-native)

---

## Patrones Cloud Native

### Los 12 Factores en Go

```go
package main

/*
THE TWELVE-FACTOR APP IN GO:

1. CODEBASE - One codebase tracked in version control
   - Use Go modules for dependency management
   - Single repository per service

2. DEPENDENCIES - Explicitly declare and isolate dependencies
   - go.mod and go.sum for dependency declaration
   - Vendor dependencies for reproducible builds

3. CONFIG - Store config in the environment
   - Use environment variables
   - Viper for configuration management

4. BACKING SERVICES - Treat backing services as attached resources
   - Database, cache, queue as URLs
   - Connection pooling and health checks

5. BUILD, RELEASE, RUN - Strictly separate stages
   - Multi-stage Docker builds
   - Immutable releases

6. PROCESSES - Execute the app as stateless processes
   - No sticky sessions
   - External state storage (Redis, PostgreSQL)

7. PORT BINDING - Export services via port binding
   - Self-contained HTTP/gRPC servers
   - No external web server required

8. CONCURRENCY - Scale out via the process model
   - Horizontal scaling with Kubernetes
   - Goroutines for internal concurrency

9. DISPOSABILITY - Maximize robustness with fast startup and graceful shutdown
   - Handle SIGTERM gracefully
   - Connection draining

10. DEV/PROD PARITY - Keep development, staging, and production similar
    - Docker Compose for local development
    - Same images across environments

11. LOGS - Treat logs as event streams
    - Write to stdout/stderr
    - Structured logging (JSON)

12. ADMIN PROCESSES - Run admin/management tasks as one-off processes
    - Database migrations as jobs
    - CLI commands for maintenance
*/

import (
    "context"
    "encoding/json"
    "fmt"
    "log/slog"
    "net/http"
    "os"
    "os/signal"
    "syscall"
    "time"
)

// Factor 3: Config from environment
type Config struct {
    Port        string
    DatabaseURL string
    RedisURL    string
    LogLevel    string
    ServiceName string
}

func LoadConfig() *Config {
    return &Config{
        Port:        getEnv("PORT", "8080"),
        DatabaseURL: getEnv("DATABASE_URL", "postgres://localhost/mydb"),
        RedisURL:    getEnv("REDIS_URL", "redis://localhost:6379"),
        LogLevel:    getEnv("LOG_LEVEL", "info"),
        ServiceName: getEnv("SERVICE_NAME", "my-service"),
    }
}

func getEnv(key, defaultVal string) string {
    if val := os.Getenv(key); val != "" {
        return val
    }
    return defaultVal
}

// Factor 11: Structured logging
func setupLogging(level string) *slog.Logger {
    var logLevel slog.Level
    switch level {
    case "debug":
        logLevel = slog.LevelDebug
    case "info":
        logLevel = slog.LevelInfo
    case "warn":
        logLevel = slog.LevelWarn
    case "error":
        logLevel = slog.LevelError
    default:
        logLevel = slog.LevelInfo
    }

    opts := &slog.HandlerOptions{Level: logLevel}
    handler := slog.NewJSONHandler(os.Stdout, opts)
    return slog.New(handler)
}

// Factor 9: Graceful shutdown
func main() {
    config := LoadConfig()
    logger := setupLogging(config.LogLevel)

    // Create server
    mux := http.NewServeMux()
    mux.HandleFunc("/health", healthHandler)
    mux.HandleFunc("/ready", readyHandler)

    server := &http.Server{
        Addr:    ":" + config.Port,
        Handler: mux,
    }

    // Start server in goroutine
    go func() {
        logger.Info("Starting server", "port", config.Port)
        if err := server.ListenAndServe(); err != http.ErrServerClosed {
            logger.Error("Server error", "error", err)
            os.Exit(1)
        }
    }()

    // Wait for interrupt signal
    quit := make(chan os.Signal, 1)
    signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
    <-quit

    logger.Info("Shutting down server...")

    // Graceful shutdown with timeout
    ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
    defer cancel()

    if err := server.Shutdown(ctx); err != nil {
        logger.Error("Shutdown error", "error", err)
    }

    logger.Info("Server stopped")
}

func healthHandler(w http.ResponseWriter, r *http.Request) {
    json.NewEncoder(w).Encode(map[string]string{"status": "healthy"})
}

func readyHandler(w http.ResponseWriter, r *http.Request) {
    // Check dependencies
    json.NewEncoder(w).Encode(map[string]string{"status": "ready"})
}
```

### Health Checks y Readiness

```go
package health

import (
    "context"
    "database/sql"
    "encoding/json"
    "net/http"
    "sync"
    "time"

    "github.com/redis/go-redis/v9"
)

// Health check status
type Status string

const (
    StatusHealthy   Status = "healthy"
    StatusUnhealthy Status = "unhealthy"
    StatusDegraded  Status = "degraded"
)

// Component health check
type ComponentHealth struct {
    Name    string        `json:"name"`
    Status  Status        `json:"status"`
    Latency time.Duration `json:"latency_ms"`
    Error   string        `json:"error,omitempty"`
}

// Overall health response
type HealthResponse struct {
    Status     Status            `json:"status"`
    Components []ComponentHealth `json:"components"`
    Version    string            `json:"version"`
    Timestamp  time.Time         `json:"timestamp"`
}

// Health checker
type Checker struct {
    db      *sql.DB
    redis   *redis.Client
    version string
    mu      sync.RWMutex
    cached  *HealthResponse
    cacheTTL time.Duration
}

func NewChecker(db *sql.DB, redis *redis.Client, version string) *Checker {
    return &Checker{
        db:       db,
        redis:    redis,
        version:  version,
        cacheTTL: 5 * time.Second,
    }
}

func (c *Checker) Check(ctx context.Context) *HealthResponse {
    c.mu.RLock()
    if c.cached != nil && time.Since(c.cached.Timestamp) < c.cacheTTL {
        c.mu.RUnlock()
        return c.cached
    }
    c.mu.RUnlock()

    // Perform checks in parallel
    var wg sync.WaitGroup
    components := make([]ComponentHealth, 0, 2)
    componentsCh := make(chan ComponentHealth, 2)

    // Database check
    wg.Add(1)
    go func() {
        defer wg.Done()
        componentsCh <- c.checkDatabase(ctx)
    }()

    // Redis check
    wg.Add(1)
    go func() {
        defer wg.Done()
        componentsCh <- c.checkRedis(ctx)
    }()

    go func() {
        wg.Wait()
        close(componentsCh)
    }()

    for comp := range componentsCh {
        components = append(components, comp)
    }

    // Determine overall status
    overallStatus := StatusHealthy
    for _, comp := range components {
        if comp.Status == StatusUnhealthy {
            overallStatus = StatusUnhealthy
            break
        }
        if comp.Status == StatusDegraded {
            overallStatus = StatusDegraded
        }
    }

    response := &HealthResponse{
        Status:     overallStatus,
        Components: components,
        Version:    c.version,
        Timestamp:  time.Now(),
    }

    c.mu.Lock()
    c.cached = response
    c.mu.Unlock()

    return response
}

func (c *Checker) checkDatabase(ctx context.Context) ComponentHealth {
    start := time.Now()
    comp := ComponentHealth{Name: "database"}

    ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
    defer cancel()

    if err := c.db.PingContext(ctx); err != nil {
        comp.Status = StatusUnhealthy
        comp.Error = err.Error()
    } else {
        comp.Status = StatusHealthy
    }

    comp.Latency = time.Since(start)
    return comp
}

func (c *Checker) checkRedis(ctx context.Context) ComponentHealth {
    start := time.Now()
    comp := ComponentHealth{Name: "redis"}

    ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
    defer cancel()

    if _, err := c.redis.Ping(ctx).Result(); err != nil {
        comp.Status = StatusUnhealthy
        comp.Error = err.Error()
    } else {
        comp.Status = StatusHealthy
    }

    comp.Latency = time.Since(start)
    return comp
}

// HTTP handlers
func (c *Checker) LivenessHandler() http.HandlerFunc {
    return func(w http.ResponseWriter, r *http.Request) {
        // Liveness: is the process alive?
        w.Header().Set("Content-Type", "application/json")
        json.NewEncoder(w).Encode(map[string]string{
            "status": "alive",
        })
    }
}

func (c *Checker) ReadinessHandler() http.HandlerFunc {
    return func(w http.ResponseWriter, r *http.Request) {
        // Readiness: can the process accept traffic?
        health := c.Check(r.Context())

        w.Header().Set("Content-Type", "application/json")

        if health.Status == StatusUnhealthy {
            w.WriteHeader(http.StatusServiceUnavailable)
        }

        json.NewEncoder(w).Encode(health)
    }
}

func (c *Checker) HealthHandler() http.HandlerFunc {
    return func(w http.ResponseWriter, r *http.Request) {
        health := c.Check(r.Context())

        w.Header().Set("Content-Type", "application/json")

        switch health.Status {
        case StatusUnhealthy:
            w.WriteHeader(http.StatusServiceUnavailable)
        case StatusDegraded:
            w.WriteHeader(http.StatusMultiStatus)
        }

        json.NewEncoder(w).Encode(health)
    }
}
```

---

## Docker y Contenedores

### Dockerfile Optimizado

```dockerfile
# syntax=docker/dockerfile:1.4

# ============================================
# Stage 1: Build dependencies
# ============================================
FROM golang:1.21-alpine AS deps

RUN apk add --no-cache git ca-certificates tzdata

WORKDIR /app

# Copy dependency files first for better caching
COPY go.mod go.sum ./
RUN --mount=type=cache,target=/go/pkg/mod \
    go mod download

# ============================================
# Stage 2: Build application
# ============================================
FROM deps AS builder

# Copy source code
COPY . .

# Build with optimizations
ARG VERSION=dev
ARG GIT_COMMIT=unknown
ARG BUILD_TIME=unknown

RUN --mount=type=cache,target=/go/pkg/mod \
    --mount=type=cache,target=/root/.cache/go-build \
    CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build \
    -ldflags="-s -w \
        -X 'main.Version=${VERSION}' \
        -X 'main.GitCommit=${GIT_COMMIT}' \
        -X 'main.BuildTime=${BUILD_TIME}'" \
    -o /app/server ./cmd/server

# ============================================
# Stage 3: Production image
# ============================================
FROM scratch AS production

# Import from builder
COPY --from=builder /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/
COPY --from=builder /usr/share/zoneinfo /usr/share/zoneinfo
COPY --from=builder /app/server /server

# Create non-root user
COPY --from=builder /etc/passwd /etc/passwd
USER nobody

# Expose port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
    CMD ["/server", "healthcheck"]

ENTRYPOINT ["/server"]
CMD ["serve"]

# ============================================
# Stage 4: Debug image (optional)
# ============================================
FROM alpine:3.19 AS debug

RUN apk add --no-cache ca-certificates tzdata curl

COPY --from=builder /app/server /server

EXPOSE 8080

ENTRYPOINT ["/server"]
CMD ["serve"]
```

### Docker Compose para Desarrollo

```yaml
# docker-compose.yml
version: '3.8'

services:
  app:
    build:
      context: .
      dockerfile: Dockerfile
      target: debug
      args:
        VERSION: dev
    ports:
      - "8080:8080"
      - "9090:9090"  # Metrics
    environment:
      - PORT=8080
      - DATABASE_URL=postgres://user:pass@postgres:5432/mydb?sslmode=disable
      - REDIS_URL=redis://redis:6379
      - LOG_LEVEL=debug
      - OTEL_EXPORTER_OTLP_ENDPOINT=http://jaeger:4317
    depends_on:
      postgres:
        condition: service_healthy
      redis:
        condition: service_healthy
    volumes:
      - ./:/app:ro
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8080/health"]
      interval: 10s
      timeout: 5s
      retries: 3

  postgres:
    image: postgres:15-alpine
    environment:
      POSTGRES_USER: user
      POSTGRES_PASSWORD: pass
      POSTGRES_DB: mydb
    ports:
      - "5432:5432"
    volumes:
      - postgres_data:/var/lib/postgresql/data
      - ./migrations:/docker-entrypoint-initdb.d:ro
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U user -d mydb"]
      interval: 5s
      timeout: 5s
      retries: 5

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"
    volumes:
      - redis_data:/data
    healthcheck:
      test: ["CMD", "redis-cli", "ping"]
      interval: 5s
      timeout: 5s
      retries: 5

  prometheus:
    image: prom/prometheus:latest
    ports:
      - "9091:9090"
    volumes:
      - ./prometheus.yml:/etc/prometheus/prometheus.yml:ro
    command:
      - '--config.file=/etc/prometheus/prometheus.yml'
      - '--storage.tsdb.path=/prometheus'

  grafana:
    image: grafana/grafana:latest
    ports:
      - "3000:3000"
    environment:
      - GF_SECURITY_ADMIN_PASSWORD=admin
    volumes:
      - grafana_data:/var/lib/grafana
      - ./grafana/dashboards:/etc/grafana/provisioning/dashboards:ro
      - ./grafana/datasources:/etc/grafana/provisioning/datasources:ro

  jaeger:
    image: jaegertracing/all-in-one:latest
    ports:
      - "16686:16686"  # UI
      - "4317:4317"    # OTLP gRPC
      - "4318:4318"    # OTLP HTTP
    environment:
      - COLLECTOR_OTLP_ENABLED=true

volumes:
  postgres_data:
  redis_data:
  grafana_data:
```

---

## Kubernetes Operators

### Custom Resource Definition

```yaml
# api/v1/database_types.yaml
apiVersion: apiextensions.k8s.io/v1
kind: CustomResourceDefinition
metadata:
  name: databases.myapp.example.com
spec:
  group: myapp.example.com
  names:
    kind: Database
    listKind: DatabaseList
    plural: databases
    singular: database
    shortNames:
      - db
  scope: Namespaced
  versions:
    - name: v1
      served: true
      storage: true
      schema:
        openAPIV3Schema:
          type: object
          properties:
            spec:
              type: object
              required:
                - engine
                - version
              properties:
                engine:
                  type: string
                  enum: [postgres, mysql, mongodb]
                version:
                  type: string
                replicas:
                  type: integer
                  minimum: 1
                  default: 1
                storage:
                  type: object
                  properties:
                    size:
                      type: string
                      default: "10Gi"
                    storageClass:
                      type: string
            status:
              type: object
              properties:
                phase:
                  type: string
                  enum: [Pending, Creating, Running, Failed]
                endpoint:
                  type: string
                replicas:
                  type: integer
                conditions:
                  type: array
                  items:
                    type: object
                    properties:
                      type:
                        type: string
                      status:
                        type: string
                      lastTransitionTime:
                        type: string
                        format: date-time
                      reason:
                        type: string
                      message:
                        type: string
      subresources:
        status: {}
      additionalPrinterColumns:
        - name: Engine
          type: string
          jsonPath: .spec.engine
        - name: Version
          type: string
          jsonPath: .spec.version
        - name: Phase
          type: string
          jsonPath: .status.phase
        - name: Age
          type: date
          jsonPath: .metadata.creationTimestamp
```

### Operator Controller

```go
// controllers/database_controller.go
package controllers

import (
    "context"
    "fmt"
    "time"

    appsv1 "k8s.io/api/apps/v1"
    corev1 "k8s.io/api/core/v1"
    "k8s.io/apimachinery/pkg/api/errors"
    metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
    "k8s.io/apimachinery/pkg/runtime"
    "k8s.io/apimachinery/pkg/types"
    ctrl "sigs.k8s.io/controller-runtime"
    "sigs.k8s.io/controller-runtime/pkg/client"
    "sigs.k8s.io/controller-runtime/pkg/log"

    myappv1 "github.com/example/operator/api/v1"
)

// DatabaseReconciler reconciles a Database object
type DatabaseReconciler struct {
    client.Client
    Scheme *runtime.Scheme
}

// +kubebuilder:rbac:groups=myapp.example.com,resources=databases,verbs=get;list;watch;create;update;patch;delete
// +kubebuilder:rbac:groups=myapp.example.com,resources=databases/status,verbs=get;update;patch
// +kubebuilder:rbac:groups=myapp.example.com,resources=databases/finalizers,verbs=update
// +kubebuilder:rbac:groups=apps,resources=statefulsets,verbs=get;list;watch;create;update;patch;delete
// +kubebuilder:rbac:groups=core,resources=services,verbs=get;list;watch;create;update;patch;delete
// +kubebuilder:rbac:groups=core,resources=secrets,verbs=get;list;watch;create;update;patch;delete

func (r *DatabaseReconciler) Reconcile(ctx context.Context, req ctrl.Request) (ctrl.Result, error) {
    logger := log.FromContext(ctx)

    // Fetch the Database instance
    database := &myappv1.Database{}
    if err := r.Get(ctx, req.NamespacedName, database); err != nil {
        if errors.IsNotFound(err) {
            logger.Info("Database resource not found, ignoring")
            return ctrl.Result{}, nil
        }
        logger.Error(err, "Failed to get Database")
        return ctrl.Result{}, err
    }

    // Check if being deleted
    if !database.ObjectMeta.DeletionTimestamp.IsZero() {
        return r.reconcileDelete(ctx, database)
    }

    // Add finalizer if not present
    if !containsString(database.ObjectMeta.Finalizers, databaseFinalizer) {
        database.ObjectMeta.Finalizers = append(database.ObjectMeta.Finalizers, databaseFinalizer)
        if err := r.Update(ctx, database); err != nil {
            return ctrl.Result{}, err
        }
    }

    // Reconcile resources
    result, err := r.reconcileResources(ctx, database)
    if err != nil {
        // Update status to Failed
        database.Status.Phase = "Failed"
        r.Status().Update(ctx, database)
        return result, err
    }

    return result, nil
}

func (r *DatabaseReconciler) reconcileResources(ctx context.Context, db *myappv1.Database) (ctrl.Result, error) {
    logger := log.FromContext(ctx)

    // Update status to Creating
    if db.Status.Phase == "" {
        db.Status.Phase = "Creating"
        if err := r.Status().Update(ctx, db); err != nil {
            return ctrl.Result{}, err
        }
    }

    // Reconcile Secret
    if err := r.reconcileSecret(ctx, db); err != nil {
        return ctrl.Result{}, err
    }

    // Reconcile StatefulSet
    if err := r.reconcileStatefulSet(ctx, db); err != nil {
        return ctrl.Result{}, err
    }

    // Reconcile Service
    if err := r.reconcileService(ctx, db); err != nil {
        return ctrl.Result{}, err
    }

    // Check StatefulSet status
    sts := &appsv1.StatefulSet{}
    if err := r.Get(ctx, types.NamespacedName{Name: db.Name, Namespace: db.Namespace}, sts); err != nil {
        return ctrl.Result{}, err
    }

    // Update status
    db.Status.Replicas = int(sts.Status.ReadyReplicas)
    db.Status.Endpoint = fmt.Sprintf("%s.%s.svc.cluster.local", db.Name, db.Namespace)

    if sts.Status.ReadyReplicas == *sts.Spec.Replicas {
        db.Status.Phase = "Running"
    } else {
        db.Status.Phase = "Creating"
    }

    if err := r.Status().Update(ctx, db); err != nil {
        return ctrl.Result{}, err
    }

    logger.Info("Reconciliation complete", "phase", db.Status.Phase)

    // Requeue if not yet running
    if db.Status.Phase != "Running" {
        return ctrl.Result{RequeueAfter: 10 * time.Second}, nil
    }

    return ctrl.Result{}, nil
}

func (r *DatabaseReconciler) reconcileStatefulSet(ctx context.Context, db *myappv1.Database) error {
    sts := &appsv1.StatefulSet{
        ObjectMeta: metav1.ObjectMeta{
            Name:      db.Name,
            Namespace: db.Namespace,
        },
    }

    _, err := ctrl.CreateOrUpdate(ctx, r.Client, sts, func() error {
        r.buildStatefulSet(db, sts)
        return ctrl.SetControllerReference(db, sts, r.Scheme)
    })

    return err
}

func (r *DatabaseReconciler) buildStatefulSet(db *myappv1.Database, sts *appsv1.StatefulSet) {
    replicas := int32(db.Spec.Replicas)
    labels := map[string]string{
        "app":      "database",
        "database": db.Name,
        "engine":   db.Spec.Engine,
    }

    sts.Spec = appsv1.StatefulSetSpec{
        Replicas:    &replicas,
        ServiceName: db.Name,
        Selector: &metav1.LabelSelector{
            MatchLabels: labels,
        },
        Template: corev1.PodTemplateSpec{
            ObjectMeta: metav1.ObjectMeta{
                Labels: labels,
            },
            Spec: corev1.PodSpec{
                Containers: []corev1.Container{
                    {
                        Name:  "database",
                        Image: r.getImage(db),
                        Ports: []corev1.ContainerPort{
                            {
                                Name:          "db",
                                ContainerPort: r.getPort(db),
                            },
                        },
                        EnvFrom: []corev1.EnvFromSource{
                            {
                                SecretRef: &corev1.SecretEnvSource{
                                    LocalObjectReference: corev1.LocalObjectReference{
                                        Name: db.Name + "-credentials",
                                    },
                                },
                            },
                        },
                        VolumeMounts: []corev1.VolumeMount{
                            {
                                Name:      "data",
                                MountPath: r.getDataPath(db),
                            },
                        },
                        ReadinessProbe: r.getReadinessProbe(db),
                        LivenessProbe:  r.getLivenessProbe(db),
                    },
                },
            },
        },
        VolumeClaimTemplates: []corev1.PersistentVolumeClaim{
            {
                ObjectMeta: metav1.ObjectMeta{
                    Name: "data",
                },
                Spec: corev1.PersistentVolumeClaimSpec{
                    AccessModes: []corev1.PersistentVolumeAccessMode{
                        corev1.ReadWriteOnce,
                    },
                    Resources: corev1.ResourceRequirements{
                        Requests: corev1.ResourceList{
                            corev1.ResourceStorage: resource.MustParse(db.Spec.Storage.Size),
                        },
                    },
                },
            },
        },
    }
}

func (r *DatabaseReconciler) getImage(db *myappv1.Database) string {
    switch db.Spec.Engine {
    case "postgres":
        return fmt.Sprintf("postgres:%s", db.Spec.Version)
    case "mysql":
        return fmt.Sprintf("mysql:%s", db.Spec.Version)
    case "mongodb":
        return fmt.Sprintf("mongo:%s", db.Spec.Version)
    default:
        return ""
    }
}

func (r *DatabaseReconciler) SetupWithManager(mgr ctrl.Manager) error {
    return ctrl.NewControllerManagedBy(mgr).
        For(&myappv1.Database{}).
        Owns(&appsv1.StatefulSet{}).
        Owns(&corev1.Service{}).
        Owns(&corev1.Secret{}).
        Complete(r)
}

const databaseFinalizer = "myapp.example.com/finalizer"

func containsString(slice []string, s string) bool {
    for _, item := range slice {
        if item == s {
            return true
        }
    }
    return false
}
```

---

## gRPC y Protocol Buffers

### Definición del Servicio

```protobuf
// api/proto/user/v1/user.proto
syntax = "proto3";

package user.v1;

option go_package = "github.com/example/api/user/v1;userv1";

import "google/protobuf/timestamp.proto";
import "google/protobuf/empty.proto";

// User represents a user in the system
message User {
    string id = 1;
    string email = 2;
    string name = 3;
    UserStatus status = 4;
    google.protobuf.Timestamp created_at = 5;
    google.protobuf.Timestamp updated_at = 6;
    map<string, string> metadata = 7;
}

enum UserStatus {
    USER_STATUS_UNSPECIFIED = 0;
    USER_STATUS_ACTIVE = 1;
    USER_STATUS_INACTIVE = 2;
    USER_STATUS_SUSPENDED = 3;
}

// CreateUserRequest creates a new user
message CreateUserRequest {
    string email = 1;
    string name = 2;
    string password = 3;
    map<string, string> metadata = 4;
}

message CreateUserResponse {
    User user = 1;
}

// GetUserRequest retrieves a user by ID
message GetUserRequest {
    string id = 1;
}

message GetUserResponse {
    User user = 1;
}

// UpdateUserRequest updates an existing user
message UpdateUserRequest {
    string id = 1;
    optional string email = 2;
    optional string name = 3;
    optional UserStatus status = 4;
    map<string, string> metadata = 5;
}

message UpdateUserResponse {
    User user = 1;
}

// DeleteUserRequest deletes a user
message DeleteUserRequest {
    string id = 1;
}

// ListUsersRequest lists users with pagination
message ListUsersRequest {
    int32 page_size = 1;
    string page_token = 2;
    string filter = 3;  // e.g., "status=ACTIVE"
    string order_by = 4; // e.g., "created_at desc"
}

message ListUsersResponse {
    repeated User users = 1;
    string next_page_token = 2;
    int32 total_count = 3;
}

// UserService manages user operations
service UserService {
    // CreateUser creates a new user
    rpc CreateUser(CreateUserRequest) returns (CreateUserResponse);

    // GetUser retrieves a user by ID
    rpc GetUser(GetUserRequest) returns (GetUserResponse);

    // UpdateUser updates an existing user
    rpc UpdateUser(UpdateUserRequest) returns (UpdateUserResponse);

    // DeleteUser deletes a user
    rpc DeleteUser(DeleteUserRequest) returns (google.protobuf.Empty);

    // ListUsers lists users with pagination
    rpc ListUsers(ListUsersRequest) returns (ListUsersResponse);

    // StreamUsers streams user updates
    rpc StreamUsers(ListUsersRequest) returns (stream User);
}
```

### Implementación del Servidor gRPC

```go
// internal/grpc/server.go
package grpc

import (
    "context"
    "fmt"
    "net"
    "time"

    "google.golang.org/grpc"
    "google.golang.org/grpc/codes"
    "google.golang.org/grpc/health"
    "google.golang.org/grpc/health/grpc_health_v1"
    "google.golang.org/grpc/keepalive"
    "google.golang.org/grpc/reflection"
    "google.golang.org/grpc/status"
    "google.golang.org/protobuf/types/known/emptypb"
    "google.golang.org/protobuf/types/known/timestamppb"

    userv1 "github.com/example/api/user/v1"
    "github.com/example/internal/service"
)

// Server wraps the gRPC server
type Server struct {
    grpcServer *grpc.Server
    listener   net.Listener
    userSvc    *service.UserService
}

// userServer implements UserServiceServer
type userServer struct {
    userv1.UnimplementedUserServiceServer
    svc *service.UserService
}

// NewServer creates a new gRPC server
func NewServer(port int, userSvc *service.UserService, opts ...grpc.ServerOption) (*Server, error) {
    listener, err := net.Listen("tcp", fmt.Sprintf(":%d", port))
    if err != nil {
        return nil, fmt.Errorf("failed to listen: %w", err)
    }

    // Default options
    defaultOpts := []grpc.ServerOption{
        grpc.KeepaliveParams(keepalive.ServerParameters{
            MaxConnectionIdle:     15 * time.Minute,
            MaxConnectionAge:      30 * time.Minute,
            MaxConnectionAgeGrace: 5 * time.Second,
            Time:                  5 * time.Minute,
            Timeout:               1 * time.Second,
        }),
        grpc.KeepaliveEnforcementPolicy(keepalive.EnforcementPolicy{
            MinTime:             5 * time.Minute,
            PermitWithoutStream: true,
        }),
    }

    allOpts := append(defaultOpts, opts...)
    grpcServer := grpc.NewServer(allOpts...)

    // Register services
    userv1.RegisterUserServiceServer(grpcServer, &userServer{svc: userSvc})

    // Register health service
    healthServer := health.NewServer()
    grpc_health_v1.RegisterHealthServer(grpcServer, healthServer)
    healthServer.SetServingStatus("user.v1.UserService", grpc_health_v1.HealthCheckResponse_SERVING)

    // Enable reflection for debugging
    reflection.Register(grpcServer)

    return &Server{
        grpcServer: grpcServer,
        listener:   listener,
        userSvc:    userSvc,
    }, nil
}

func (s *Server) Start() error {
    return s.grpcServer.Serve(s.listener)
}

func (s *Server) Stop() {
    s.grpcServer.GracefulStop()
}

// CreateUser implements UserServiceServer
func (s *userServer) CreateUser(ctx context.Context, req *userv1.CreateUserRequest) (*userv1.CreateUserResponse, error) {
    if req.Email == "" {
        return nil, status.Error(codes.InvalidArgument, "email is required")
    }
    if req.Name == "" {
        return nil, status.Error(codes.InvalidArgument, "name is required")
    }

    user, err := s.svc.CreateUser(ctx, &service.CreateUserInput{
        Email:    req.Email,
        Name:     req.Name,
        Password: req.Password,
        Metadata: req.Metadata,
    })
    if err != nil {
        return nil, s.mapError(err)
    }

    return &userv1.CreateUserResponse{
        User: s.toProtoUser(user),
    }, nil
}

func (s *userServer) GetUser(ctx context.Context, req *userv1.GetUserRequest) (*userv1.GetUserResponse, error) {
    if req.Id == "" {
        return nil, status.Error(codes.InvalidArgument, "id is required")
    }

    user, err := s.svc.GetUser(ctx, req.Id)
    if err != nil {
        return nil, s.mapError(err)
    }

    return &userv1.GetUserResponse{
        User: s.toProtoUser(user),
    }, nil
}

func (s *userServer) UpdateUser(ctx context.Context, req *userv1.UpdateUserRequest) (*userv1.UpdateUserResponse, error) {
    if req.Id == "" {
        return nil, status.Error(codes.InvalidArgument, "id is required")
    }

    input := &service.UpdateUserInput{
        ID:       req.Id,
        Metadata: req.Metadata,
    }

    if req.Email != nil {
        input.Email = *req.Email
    }
    if req.Name != nil {
        input.Name = *req.Name
    }
    if req.Status != nil {
        input.Status = s.fromProtoStatus(*req.Status)
    }

    user, err := s.svc.UpdateUser(ctx, input)
    if err != nil {
        return nil, s.mapError(err)
    }

    return &userv1.UpdateUserResponse{
        User: s.toProtoUser(user),
    }, nil
}

func (s *userServer) DeleteUser(ctx context.Context, req *userv1.DeleteUserRequest) (*emptypb.Empty, error) {
    if req.Id == "" {
        return nil, status.Error(codes.InvalidArgument, "id is required")
    }

    if err := s.svc.DeleteUser(ctx, req.Id); err != nil {
        return nil, s.mapError(err)
    }

    return &emptypb.Empty{}, nil
}

func (s *userServer) ListUsers(ctx context.Context, req *userv1.ListUsersRequest) (*userv1.ListUsersResponse, error) {
    pageSize := int(req.PageSize)
    if pageSize <= 0 {
        pageSize = 20
    }
    if pageSize > 100 {
        pageSize = 100
    }

    users, nextToken, total, err := s.svc.ListUsers(ctx, &service.ListUsersInput{
        PageSize:  pageSize,
        PageToken: req.PageToken,
        Filter:    req.Filter,
        OrderBy:   req.OrderBy,
    })
    if err != nil {
        return nil, s.mapError(err)
    }

    protoUsers := make([]*userv1.User, len(users))
    for i, u := range users {
        protoUsers[i] = s.toProtoUser(u)
    }

    return &userv1.ListUsersResponse{
        Users:         protoUsers,
        NextPageToken: nextToken,
        TotalCount:    int32(total),
    }, nil
}

func (s *userServer) StreamUsers(req *userv1.ListUsersRequest, stream userv1.UserService_StreamUsersServer) error {
    ctx := stream.Context()

    userChan, err := s.svc.StreamUsers(ctx, req.Filter)
    if err != nil {
        return s.mapError(err)
    }

    for {
        select {
        case <-ctx.Done():
            return nil
        case user, ok := <-userChan:
            if !ok {
                return nil
            }
            if err := stream.Send(s.toProtoUser(user)); err != nil {
                return err
            }
        }
    }
}

func (s *userServer) toProtoUser(u *service.User) *userv1.User {
    return &userv1.User{
        Id:        u.ID,
        Email:     u.Email,
        Name:      u.Name,
        Status:    s.toProtoStatus(u.Status),
        CreatedAt: timestamppb.New(u.CreatedAt),
        UpdatedAt: timestamppb.New(u.UpdatedAt),
        Metadata:  u.Metadata,
    }
}

func (s *userServer) toProtoStatus(status string) userv1.UserStatus {
    switch status {
    case "active":
        return userv1.UserStatus_USER_STATUS_ACTIVE
    case "inactive":
        return userv1.UserStatus_USER_STATUS_INACTIVE
    case "suspended":
        return userv1.UserStatus_USER_STATUS_SUSPENDED
    default:
        return userv1.UserStatus_USER_STATUS_UNSPECIFIED
    }
}

func (s *userServer) fromProtoStatus(status userv1.UserStatus) string {
    switch status {
    case userv1.UserStatus_USER_STATUS_ACTIVE:
        return "active"
    case userv1.UserStatus_USER_STATUS_INACTIVE:
        return "inactive"
    case userv1.UserStatus_USER_STATUS_SUSPENDED:
        return "suspended"
    default:
        return ""
    }
}

func (s *userServer) mapError(err error) error {
    switch {
    case errors.Is(err, service.ErrNotFound):
        return status.Error(codes.NotFound, err.Error())
    case errors.Is(err, service.ErrAlreadyExists):
        return status.Error(codes.AlreadyExists, err.Error())
    case errors.Is(err, service.ErrInvalidInput):
        return status.Error(codes.InvalidArgument, err.Error())
    default:
        return status.Error(codes.Internal, "internal error")
    }
}
```

### Cliente gRPC

```go
// pkg/client/user.go
package client

import (
    "context"
    "time"

    "google.golang.org/grpc"
    "google.golang.org/grpc/credentials/insecure"
    "google.golang.org/grpc/keepalive"

    userv1 "github.com/example/api/user/v1"
)

type UserClient struct {
    conn   *grpc.ClientConn
    client userv1.UserServiceClient
}

type UserClientConfig struct {
    Address     string
    Timeout     time.Duration
    Insecure    bool
}

func NewUserClient(cfg UserClientConfig) (*UserClient, error) {
    opts := []grpc.DialOption{
        grpc.WithKeepaliveParams(keepalive.ClientParameters{
            Time:                10 * time.Second,
            Timeout:             3 * time.Second,
            PermitWithoutStream: true,
        }),
    }

    if cfg.Insecure {
        opts = append(opts, grpc.WithTransportCredentials(insecure.NewCredentials()))
    }

    conn, err := grpc.Dial(cfg.Address, opts...)
    if err != nil {
        return nil, err
    }

    return &UserClient{
        conn:   conn,
        client: userv1.NewUserServiceClient(conn),
    }, nil
}

func (c *UserClient) Close() error {
    return c.conn.Close()
}

func (c *UserClient) CreateUser(ctx context.Context, email, name, password string) (*userv1.User, error) {
    resp, err := c.client.CreateUser(ctx, &userv1.CreateUserRequest{
        Email:    email,
        Name:     name,
        Password: password,
    })
    if err != nil {
        return nil, err
    }
    return resp.User, nil
}

func (c *UserClient) GetUser(ctx context.Context, id string) (*userv1.User, error) {
    resp, err := c.client.GetUser(ctx, &userv1.GetUserRequest{Id: id})
    if err != nil {
        return nil, err
    }
    return resp.User, nil
}

func (c *UserClient) ListUsers(ctx context.Context, pageSize int, pageToken string) ([]*userv1.User, string, error) {
    resp, err := c.client.ListUsers(ctx, &userv1.ListUsersRequest{
        PageSize:  int32(pageSize),
        PageToken: pageToken,
    })
    if err != nil {
        return nil, "", err
    }
    return resp.Users, resp.NextPageToken, nil
}
```

---

## Arquitectura de Microservicios

### Patrón Gateway/BFF

```go
// internal/gateway/gateway.go
package gateway

import (
    "context"
    "encoding/json"
    "net/http"
    "time"

    "github.com/gorilla/mux"
    "go.opentelemetry.io/contrib/instrumentation/net/http/otelhttp"

    userv1 "github.com/example/api/user/v1"
    "github.com/example/pkg/client"
)

type Gateway struct {
    router     *mux.Router
    userClient *client.UserClient
}

type Config struct {
    UserServiceAddr string
    Timeout         time.Duration
}

func NewGateway(cfg Config) (*Gateway, error) {
    userClient, err := client.NewUserClient(client.UserClientConfig{
        Address:  cfg.UserServiceAddr,
        Timeout:  cfg.Timeout,
        Insecure: true,
    })
    if err != nil {
        return nil, err
    }

    g := &Gateway{
        router:     mux.NewRouter(),
        userClient: userClient,
    }

    g.setupRoutes()
    return g, nil
}

func (g *Gateway) setupRoutes() {
    // API versioning
    api := g.router.PathPrefix("/api/v1").Subrouter()

    // User routes
    api.HandleFunc("/users", g.listUsers).Methods("GET")
    api.HandleFunc("/users", g.createUser).Methods("POST")
    api.HandleFunc("/users/{id}", g.getUser).Methods("GET")
    api.HandleFunc("/users/{id}", g.updateUser).Methods("PUT")
    api.HandleFunc("/users/{id}", g.deleteUser).Methods("DELETE")

    // Health routes
    g.router.HandleFunc("/health", g.healthCheck).Methods("GET")
    g.router.HandleFunc("/ready", g.readinessCheck).Methods("GET")
}

func (g *Gateway) Handler() http.Handler {
    return otelhttp.NewHandler(g.router, "gateway")
}

func (g *Gateway) listUsers(w http.ResponseWriter, r *http.Request) {
    ctx := r.Context()

    users, nextToken, err := g.userClient.ListUsers(ctx, 20, r.URL.Query().Get("page_token"))
    if err != nil {
        g.writeError(w, err)
        return
    }

    g.writeJSON(w, http.StatusOK, map[string]interface{}{
        "users":           users,
        "next_page_token": nextToken,
    })
}

func (g *Gateway) createUser(w http.ResponseWriter, r *http.Request) {
    ctx := r.Context()

    var req struct {
        Email    string `json:"email"`
        Name     string `json:"name"`
        Password string `json:"password"`
    }

    if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
        g.writeError(w, err)
        return
    }

    user, err := g.userClient.CreateUser(ctx, req.Email, req.Name, req.Password)
    if err != nil {
        g.writeError(w, err)
        return
    }

    g.writeJSON(w, http.StatusCreated, map[string]interface{}{
        "user": user,
    })
}

func (g *Gateway) getUser(w http.ResponseWriter, r *http.Request) {
    ctx := r.Context()
    vars := mux.Vars(r)

    user, err := g.userClient.GetUser(ctx, vars["id"])
    if err != nil {
        g.writeError(w, err)
        return
    }

    g.writeJSON(w, http.StatusOK, map[string]interface{}{
        "user": user,
    })
}

func (g *Gateway) writeJSON(w http.ResponseWriter, status int, data interface{}) {
    w.Header().Set("Content-Type", "application/json")
    w.WriteHeader(status)
    json.NewEncoder(w).Encode(data)
}

func (g *Gateway) writeError(w http.ResponseWriter, err error) {
    status := http.StatusInternalServerError
    message := "internal server error"

    // Map gRPC errors to HTTP
    // ... error mapping logic

    g.writeJSON(w, status, map[string]interface{}{
        "error": message,
    })
}
```

---

## Observabilidad

### Logging Estructurado

```go
// pkg/logging/logger.go
package logging

import (
    "context"
    "log/slog"
    "os"
    "runtime"
    "time"
)

type contextKey string

const (
    RequestIDKey   contextKey = "request_id"
    UserIDKey      contextKey = "user_id"
    TraceIDKey     contextKey = "trace_id"
)

type Logger struct {
    *slog.Logger
}

func NewLogger(level string, format string) *Logger {
    var logLevel slog.Level
    switch level {
    case "debug":
        logLevel = slog.LevelDebug
    case "info":
        logLevel = slog.LevelInfo
    case "warn":
        logLevel = slog.LevelWarn
    case "error":
        logLevel = slog.LevelError
    default:
        logLevel = slog.LevelInfo
    }

    opts := &slog.HandlerOptions{
        Level: logLevel,
        AddSource: logLevel == slog.LevelDebug,
    }

    var handler slog.Handler
    if format == "json" {
        handler = slog.NewJSONHandler(os.Stdout, opts)
    } else {
        handler = slog.NewTextHandler(os.Stdout, opts)
    }

    return &Logger{
        Logger: slog.New(handler),
    }
}

func (l *Logger) WithContext(ctx context.Context) *Logger {
    attrs := []slog.Attr{}

    if requestID, ok := ctx.Value(RequestIDKey).(string); ok {
        attrs = append(attrs, slog.String("request_id", requestID))
    }
    if userID, ok := ctx.Value(UserIDKey).(string); ok {
        attrs = append(attrs, slog.String("user_id", userID))
    }
    if traceID, ok := ctx.Value(TraceIDKey).(string); ok {
        attrs = append(attrs, slog.String("trace_id", traceID))
    }

    if len(attrs) == 0 {
        return l
    }

    return &Logger{
        Logger: slog.New(l.Handler().WithAttrs(attrs)),
    }
}

func (l *Logger) WithError(err error) *Logger {
    return &Logger{
        Logger: l.Logger.With("error", err.Error()),
    }
}

func (l *Logger) WithDuration(d time.Duration) *Logger {
    return &Logger{
        Logger: l.Logger.With("duration_ms", d.Milliseconds()),
    }
}
```

### Métricas con Prometheus

```go
// pkg/metrics/metrics.go
package metrics

import (
    "net/http"
    "time"

    "github.com/prometheus/client_golang/prometheus"
    "github.com/prometheus/client_golang/prometheus/promauto"
    "github.com/prometheus/client_golang/prometheus/promhttp"
)

var (
    // HTTP metrics
    httpRequestsTotal = promauto.NewCounterVec(
        prometheus.CounterOpts{
            Name: "http_requests_total",
            Help: "Total number of HTTP requests",
        },
        []string{"method", "path", "status"},
    )

    httpRequestDuration = promauto.NewHistogramVec(
        prometheus.HistogramOpts{
            Name:    "http_request_duration_seconds",
            Help:    "HTTP request latencies in seconds",
            Buckets: prometheus.DefBuckets,
        },
        []string{"method", "path"},
    )

    httpRequestsInFlight = promauto.NewGauge(
        prometheus.GaugeOpts{
            Name: "http_requests_in_flight",
            Help: "Number of HTTP requests currently being processed",
        },
    )

    // gRPC metrics
    grpcRequestsTotal = promauto.NewCounterVec(
        prometheus.CounterOpts{
            Name: "grpc_requests_total",
            Help: "Total number of gRPC requests",
        },
        []string{"method", "status"},
    )

    grpcRequestDuration = promauto.NewHistogramVec(
        prometheus.HistogramOpts{
            Name:    "grpc_request_duration_seconds",
            Help:    "gRPC request latencies in seconds",
            Buckets: prometheus.DefBuckets,
        },
        []string{"method"},
    )

    // Business metrics
    usersCreated = promauto.NewCounter(
        prometheus.CounterOpts{
            Name: "users_created_total",
            Help: "Total number of users created",
        },
    )

    activeUsers = promauto.NewGauge(
        prometheus.GaugeOpts{
            Name: "active_users",
            Help: "Number of currently active users",
        },
    )

    // Database metrics
    dbConnectionsOpen = promauto.NewGauge(
        prometheus.GaugeOpts{
            Name: "db_connections_open",
            Help: "Number of open database connections",
        },
    )

    dbQueryDuration = promauto.NewHistogramVec(
        prometheus.HistogramOpts{
            Name:    "db_query_duration_seconds",
            Help:    "Database query latencies in seconds",
            Buckets: []float64{.001, .005, .01, .025, .05, .1, .25, .5, 1},
        },
        []string{"query"},
    )
)

// HTTP Middleware
func HTTPMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        start := time.Now()
        httpRequestsInFlight.Inc()
        defer httpRequestsInFlight.Dec()

        wrapped := &responseWriter{ResponseWriter: w, statusCode: http.StatusOK}
        next.ServeHTTP(wrapped, r)

        duration := time.Since(start).Seconds()
        path := r.URL.Path

        httpRequestsTotal.WithLabelValues(r.Method, path, http.StatusText(wrapped.statusCode)).Inc()
        httpRequestDuration.WithLabelValues(r.Method, path).Observe(duration)
    })
}

type responseWriter struct {
    http.ResponseWriter
    statusCode int
}

func (rw *responseWriter) WriteHeader(code int) {
    rw.statusCode = code
    rw.ResponseWriter.WriteHeader(code)
}

// Record functions
func RecordGRPCRequest(method string, status string, duration time.Duration) {
    grpcRequestsTotal.WithLabelValues(method, status).Inc()
    grpcRequestDuration.WithLabelValues(method).Observe(duration.Seconds())
}

func RecordUserCreated() {
    usersCreated.Inc()
}

func SetActiveUsers(count float64) {
    activeUsers.Set(count)
}

func RecordDBQuery(query string, duration time.Duration) {
    dbQueryDuration.WithLabelValues(query).Observe(duration.Seconds())
}

func SetDBConnections(count float64) {
    dbConnectionsOpen.Set(count)
}

// Handler returns the Prometheus metrics handler
func Handler() http.Handler {
    return promhttp.Handler()
}
```

### Tracing con OpenTelemetry

```go
// pkg/tracing/tracing.go
package tracing

import (
    "context"
    "time"

    "go.opentelemetry.io/otel"
    "go.opentelemetry.io/otel/attribute"
    "go.opentelemetry.io/otel/exporters/otlp/otlptrace/otlptracegrpc"
    "go.opentelemetry.io/otel/propagation"
    "go.opentelemetry.io/otel/sdk/resource"
    sdktrace "go.opentelemetry.io/otel/sdk/trace"
    semconv "go.opentelemetry.io/otel/semconv/v1.17.0"
    "go.opentelemetry.io/otel/trace"
)

type Config struct {
    ServiceName    string
    ServiceVersion string
    Environment    string
    OTLPEndpoint   string
    SampleRate     float64
}

func InitTracer(ctx context.Context, cfg Config) (func(), error) {
    // Create OTLP exporter
    exporter, err := otlptracegrpc.New(ctx,
        otlptracegrpc.WithEndpoint(cfg.OTLPEndpoint),
        otlptracegrpc.WithInsecure(),
    )
    if err != nil {
        return nil, err
    }

    // Create resource
    res, err := resource.Merge(
        resource.Default(),
        resource.NewWithAttributes(
            semconv.SchemaURL,
            semconv.ServiceName(cfg.ServiceName),
            semconv.ServiceVersion(cfg.ServiceVersion),
            attribute.String("environment", cfg.Environment),
        ),
    )
    if err != nil {
        return nil, err
    }

    // Create sampler
    var sampler sdktrace.Sampler
    if cfg.SampleRate >= 1.0 {
        sampler = sdktrace.AlwaysSample()
    } else if cfg.SampleRate <= 0.0 {
        sampler = sdktrace.NeverSample()
    } else {
        sampler = sdktrace.TraceIDRatioBased(cfg.SampleRate)
    }

    // Create trace provider
    tp := sdktrace.NewTracerProvider(
        sdktrace.WithBatcher(exporter,
            sdktrace.WithBatchTimeout(5*time.Second),
        ),
        sdktrace.WithResource(res),
        sdktrace.WithSampler(sampler),
    )

    // Set global tracer provider
    otel.SetTracerProvider(tp)

    // Set global propagator
    otel.SetTextMapPropagator(propagation.NewCompositeTextMapPropagator(
        propagation.TraceContext{},
        propagation.Baggage{},
    ))

    // Return shutdown function
    return func() {
        ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
        defer cancel()
        tp.Shutdown(ctx)
    }, nil
}

// Tracer returns a named tracer
func Tracer(name string) trace.Tracer {
    return otel.Tracer(name)
}

// StartSpan starts a new span
func StartSpan(ctx context.Context, name string, opts ...trace.SpanStartOption) (context.Context, trace.Span) {
    return Tracer("app").Start(ctx, name, opts...)
}

// AddEvent adds an event to the current span
func AddEvent(ctx context.Context, name string, attrs ...attribute.KeyValue) {
    span := trace.SpanFromContext(ctx)
    span.AddEvent(name, trace.WithAttributes(attrs...))
}

// SetError records an error on the current span
func SetError(ctx context.Context, err error) {
    span := trace.SpanFromContext(ctx)
    span.RecordError(err)
}

// SetAttributes sets attributes on the current span
func SetAttributes(ctx context.Context, attrs ...attribute.KeyValue) {
    span := trace.SpanFromContext(ctx)
    span.SetAttributes(attrs...)
}

// Example usage
func ExampleUsage(ctx context.Context) error {
    ctx, span := StartSpan(ctx, "process-order")
    defer span.End()

    // Add attributes
    SetAttributes(ctx,
        attribute.String("order.id", "12345"),
        attribute.Int("order.items", 3),
    )

    // Add event
    AddEvent(ctx, "order.validated")

    // Call downstream service
    if err := callPaymentService(ctx); err != nil {
        SetError(ctx, err)
        return err
    }

    return nil
}
```

---

## Mejores Prácticas Cloud Native

### Lista de Verificación

```go
/*
CLOUD NATIVE CHECKLIST:

1. CONTAINERIZATION
   [x] Multi-stage Docker builds
   [x] Minimal base images (scratch, distroless)
   [x] Non-root user in containers
   [x] Health checks in Dockerfile
   [x] Proper signal handling (SIGTERM)

2. CONFIGURATION
   [x] Environment variables for config
   [x] ConfigMaps and Secrets in K8s
   [x] Feature flags for gradual rollouts
   [x] No hardcoded values

3. OBSERVABILITY
   [x] Structured logging (JSON)
   [x] Metrics exposed (Prometheus format)
   [x] Distributed tracing (OpenTelemetry)
   [x] Health endpoints (/health, /ready)

4. RESILIENCE
   [x] Circuit breakers
   [x] Retries with exponential backoff
   [x] Timeouts on all external calls
   [x] Graceful degradation

5. SECURITY
   [x] TLS for all communications
   [x] Authentication/Authorization
   [x] Secrets management
   [x] Container security scanning

6. SCALING
   [x] Stateless design
   [x] Horizontal Pod Autoscaler ready
   [x] Resource limits defined
   [x] PodDisruptionBudget configured

7. DEPLOYMENT
   [x] Rolling updates
   [x] Rollback capability
   [x] Blue-green or canary deployments
   [x] Infrastructure as Code

8. SERVICE MESH (if applicable)
   [x] Istio/Linkerd integration
   [x] mTLS between services
   [x] Traffic management
   [x] Observability integration
*/

// Example circuit breaker implementation
type CircuitBreaker struct {
    maxFailures  int
    resetTimeout time.Duration
    failures     int
    lastFailure  time.Time
    state        string // closed, open, half-open
    mu           sync.Mutex
}

func NewCircuitBreaker(maxFailures int, resetTimeout time.Duration) *CircuitBreaker {
    return &CircuitBreaker{
        maxFailures:  maxFailures,
        resetTimeout: resetTimeout,
        state:        "closed",
    }
}

func (cb *CircuitBreaker) Execute(fn func() error) error {
    cb.mu.Lock()

    if cb.state == "open" {
        if time.Since(cb.lastFailure) > cb.resetTimeout {
            cb.state = "half-open"
        } else {
            cb.mu.Unlock()
            return errors.New("circuit breaker is open")
        }
    }

    cb.mu.Unlock()

    err := fn()

    cb.mu.Lock()
    defer cb.mu.Unlock()

    if err != nil {
        cb.failures++
        cb.lastFailure = time.Now()

        if cb.failures >= cb.maxFailures {
            cb.state = "open"
        }
        return err
    }

    cb.failures = 0
    cb.state = "closed"
    return nil
}
```

---

## Referencias

- [Cloud Native Go](https://www.oreilly.com/library/view/cloud-native-go/9781492076322/)
- [Kubernetes Operators](https://kubernetes.io/docs/concepts/extend-kubernetes/operator/)
- [gRPC Go](https://grpc.io/docs/languages/go/)
- [OpenTelemetry Go](https://opentelemetry.io/docs/instrumentation/go/)
- [Prometheus Go Client](https://github.com/prometheus/client_golang)
- [The Twelve-Factor App](https://12factor.net/)

---

*ARCHAEON_CORE - Preservando la evolución del código*
*Go en la nube: Infraestructura moderna para sistemas distribuidos*
