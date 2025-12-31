---
title: "GO_04_SISTEMAS"
version: "1.0.0"
date: "2025-12-31"
author: "ARCHAEON_CORE"
domain: "SOUL_CORE/ARCHAEON/MODERNOS/GO"
type: "technical_documentation"
classification: "systems_programming"
language: "Go"
paradigm: "systems_programming"
legacy_bridge: "C_utilities"
keywords:
  - cli
  - cobra
  - viper
  - filesystem
  - networking
  - cross_compilation
  - static_linking
dependencies:
  - go_1.21+
  - cobra
  - viper
related_docs:
  - GO_01_FUNDAMENTOS.md
  - GO_03_C_BRIDGE.md
  - GO_05_CLOUD.md
---

# GO_04_SISTEMAS

## Tabla de Contenidos

1. [Herramientas CLI con Cobra/Viper](#herramientas-cli-con-cobraviper)
2. [Operaciones de Sistema de Archivos](#operaciones-de-sistema-de-archivos)
3. [Programación de Red](#programación-de-red)
4. [Cross-Compilation](#cross-compilation)
5. [Static Linking](#static-linking)
6. [Reemplazando Utilidades C](#reemplazando-utilidades-c)
7. [Mejores Prácticas](#mejores-prácticas)

---

## Herramientas CLI con Cobra/Viper

### Estructura Básica con Cobra

```go
// cmd/root.go
package cmd

import (
    "fmt"
    "os"

    "github.com/spf13/cobra"
    "github.com/spf13/viper"
)

var (
    cfgFile string
    verbose bool
)

// rootCmd represents the base command
var rootCmd = &cobra.Command{
    Use:   "mytool",
    Short: "A powerful CLI tool",
    Long: `mytool is a CLI application that demonstrates
how to build robust command-line tools with Go.

It supports configuration files, environment variables,
and command-line flags.`,
    PersistentPreRun: func(cmd *cobra.Command, args []string) {
        if verbose {
            fmt.Println("Verbose mode enabled")
        }
    },
}

// Execute adds all child commands to the root command
func Execute() {
    if err := rootCmd.Execute(); err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(1)
    }
}

func init() {
    cobra.OnInitialize(initConfig)

    // Persistent flags - available to this command and all subcommands
    rootCmd.PersistentFlags().StringVar(&cfgFile, "config", "",
        "config file (default is $HOME/.mytool.yaml)")
    rootCmd.PersistentFlags().BoolVarP(&verbose, "verbose", "v", false,
        "verbose output")

    // Bind flags to viper
    viper.BindPFlag("verbose", rootCmd.PersistentFlags().Lookup("verbose"))
}

func initConfig() {
    if cfgFile != "" {
        viper.SetConfigFile(cfgFile)
    } else {
        home, err := os.UserHomeDir()
        cobra.CheckErr(err)

        viper.AddConfigPath(home)
        viper.AddConfigPath(".")
        viper.SetConfigType("yaml")
        viper.SetConfigName(".mytool")
    }

    // Environment variables
    viper.SetEnvPrefix("MYTOOL")
    viper.AutomaticEnv()

    // Read config file
    if err := viper.ReadInConfig(); err == nil {
        fmt.Println("Using config file:", viper.ConfigFileUsed())
    }
}
```

### Subcomandos

```go
// cmd/server.go
package cmd

import (
    "context"
    "fmt"
    "net/http"
    "os"
    "os/signal"
    "syscall"
    "time"

    "github.com/spf13/cobra"
    "github.com/spf13/viper"
)

var (
    serverPort int
    serverHost string
)

var serverCmd = &cobra.Command{
    Use:   "server",
    Short: "Start the HTTP server",
    Long:  `Start a HTTP server that handles incoming requests.`,
    RunE:  runServer,
}

func init() {
    rootCmd.AddCommand(serverCmd)

    serverCmd.Flags().IntVarP(&serverPort, "port", "p", 8080, "port to listen on")
    serverCmd.Flags().StringVar(&serverHost, "host", "0.0.0.0", "host to bind to")

    viper.BindPFlag("server.port", serverCmd.Flags().Lookup("port"))
    viper.BindPFlag("server.host", serverCmd.Flags().Lookup("host"))
}

func runServer(cmd *cobra.Command, args []string) error {
    addr := fmt.Sprintf("%s:%d", serverHost, serverPort)

    mux := http.NewServeMux()
    mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
        fmt.Fprintf(w, "Hello from mytool server!")
    })
    mux.HandleFunc("/health", func(w http.ResponseWriter, r *http.Request) {
        w.WriteHeader(http.StatusOK)
        fmt.Fprintf(w, "OK")
    })

    server := &http.Server{
        Addr:         addr,
        Handler:      mux,
        ReadTimeout:  15 * time.Second,
        WriteTimeout: 15 * time.Second,
        IdleTimeout:  60 * time.Second,
    }

    // Graceful shutdown
    go func() {
        sigChan := make(chan os.Signal, 1)
        signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
        <-sigChan

        fmt.Println("\nShutting down server...")

        ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
        defer cancel()

        if err := server.Shutdown(ctx); err != nil {
            fmt.Printf("Server shutdown error: %v\n", err)
        }
    }()

    fmt.Printf("Server starting on %s\n", addr)
    if err := server.ListenAndServe(); err != http.ErrServerClosed {
        return err
    }

    fmt.Println("Server stopped")
    return nil
}

// cmd/version.go
package cmd

import (
    "fmt"
    "runtime"

    "github.com/spf13/cobra"
)

var (
    // Set at build time
    Version   = "dev"
    GitCommit = "unknown"
    BuildTime = "unknown"
)

var versionCmd = &cobra.Command{
    Use:   "version",
    Short: "Print version information",
    Run: func(cmd *cobra.Command, args []string) {
        fmt.Printf("mytool version %s\n", Version)
        fmt.Printf("  Git commit: %s\n", GitCommit)
        fmt.Printf("  Built:      %s\n", BuildTime)
        fmt.Printf("  Go version: %s\n", runtime.Version())
        fmt.Printf("  OS/Arch:    %s/%s\n", runtime.GOOS, runtime.GOARCH)
    },
}

func init() {
    rootCmd.AddCommand(versionCmd)
}
```

### Configuración Avanzada con Viper

```go
// config/config.go
package config

import (
    "fmt"
    "os"
    "time"

    "github.com/spf13/viper"
)

type Config struct {
    App      AppConfig      `mapstructure:"app"`
    Server   ServerConfig   `mapstructure:"server"`
    Database DatabaseConfig `mapstructure:"database"`
    Logging  LoggingConfig  `mapstructure:"logging"`
}

type AppConfig struct {
    Name        string `mapstructure:"name"`
    Environment string `mapstructure:"environment"`
    Debug       bool   `mapstructure:"debug"`
}

type ServerConfig struct {
    Host         string        `mapstructure:"host"`
    Port         int           `mapstructure:"port"`
    ReadTimeout  time.Duration `mapstructure:"read_timeout"`
    WriteTimeout time.Duration `mapstructure:"write_timeout"`
    TLS          TLSConfig     `mapstructure:"tls"`
}

type TLSConfig struct {
    Enabled  bool   `mapstructure:"enabled"`
    CertFile string `mapstructure:"cert_file"`
    KeyFile  string `mapstructure:"key_file"`
}

type DatabaseConfig struct {
    Driver          string        `mapstructure:"driver"`
    Host            string        `mapstructure:"host"`
    Port            int           `mapstructure:"port"`
    Name            string        `mapstructure:"name"`
    User            string        `mapstructure:"user"`
    Password        string        `mapstructure:"password"`
    MaxOpenConns    int           `mapstructure:"max_open_conns"`
    MaxIdleConns    int           `mapstructure:"max_idle_conns"`
    ConnMaxLifetime time.Duration `mapstructure:"conn_max_lifetime"`
}

type LoggingConfig struct {
    Level  string `mapstructure:"level"`
    Format string `mapstructure:"format"`
    Output string `mapstructure:"output"`
}

func Load() (*Config, error) {
    // Set defaults
    setDefaults()

    // Configuration search paths
    viper.SetConfigName("config")
    viper.SetConfigType("yaml")
    viper.AddConfigPath(".")
    viper.AddConfigPath("./config")
    viper.AddConfigPath("/etc/mytool")

    // Environment variables
    viper.SetEnvPrefix("MYTOOL")
    viper.AutomaticEnv()

    // Bind environment variables
    bindEnvVars()

    // Read configuration
    if err := viper.ReadInConfig(); err != nil {
        if _, ok := err.(viper.ConfigFileNotFoundError); !ok {
            return nil, fmt.Errorf("error reading config: %w", err)
        }
        // Config file not found, use defaults and env vars
    }

    var config Config
    if err := viper.Unmarshal(&config); err != nil {
        return nil, fmt.Errorf("error unmarshaling config: %w", err)
    }

    return &config, nil
}

func setDefaults() {
    // App defaults
    viper.SetDefault("app.name", "mytool")
    viper.SetDefault("app.environment", "development")
    viper.SetDefault("app.debug", false)

    // Server defaults
    viper.SetDefault("server.host", "0.0.0.0")
    viper.SetDefault("server.port", 8080)
    viper.SetDefault("server.read_timeout", "15s")
    viper.SetDefault("server.write_timeout", "15s")
    viper.SetDefault("server.tls.enabled", false)

    // Database defaults
    viper.SetDefault("database.driver", "postgres")
    viper.SetDefault("database.host", "localhost")
    viper.SetDefault("database.port", 5432)
    viper.SetDefault("database.max_open_conns", 25)
    viper.SetDefault("database.max_idle_conns", 5)
    viper.SetDefault("database.conn_max_lifetime", "5m")

    // Logging defaults
    viper.SetDefault("logging.level", "info")
    viper.SetDefault("logging.format", "json")
    viper.SetDefault("logging.output", "stdout")
}

func bindEnvVars() {
    // Database password from env
    viper.BindEnv("database.password", "MYTOOL_DB_PASSWORD")
    viper.BindEnv("database.user", "MYTOOL_DB_USER")
}

// Example config.yaml:
/*
app:
  name: mytool
  environment: production
  debug: false

server:
  host: 0.0.0.0
  port: 8080
  read_timeout: 30s
  write_timeout: 30s
  tls:
    enabled: true
    cert_file: /etc/ssl/certs/server.crt
    key_file: /etc/ssl/private/server.key

database:
  driver: postgres
  host: db.example.com
  port: 5432
  name: mydb
  max_open_conns: 50
  max_idle_conns: 10
  conn_max_lifetime: 10m

logging:
  level: info
  format: json
  output: stdout
*/
```

### Interactividad y Prompts

```go
// cmd/interactive.go
package cmd

import (
    "bufio"
    "fmt"
    "os"
    "strings"
    "syscall"

    "github.com/spf13/cobra"
    "golang.org/x/term"
)

var interactiveCmd = &cobra.Command{
    Use:   "init",
    Short: "Initialize a new project interactively",
    RunE:  runInteractive,
}

func init() {
    rootCmd.AddCommand(interactiveCmd)
}

func runInteractive(cmd *cobra.Command, args []string) error {
    reader := bufio.NewReader(os.Stdin)

    // Text input
    projectName, err := prompt(reader, "Project name", "myproject")
    if err != nil {
        return err
    }

    // Selection
    projectType, err := promptSelect(reader, "Project type", []string{"web", "cli", "library"})
    if err != nil {
        return err
    }

    // Boolean
    useDocker, err := promptBool(reader, "Use Docker?", true)
    if err != nil {
        return err
    }

    // Password (hidden input)
    if projectType == "web" {
        password, err := promptPassword("Enter secret key")
        if err != nil {
            return err
        }
        fmt.Printf("Secret key set (length: %d)\n", len(password))
    }

    // Confirmation
    if ok, _ := promptBool(reader, "Create project?", true); !ok {
        fmt.Println("Cancelled")
        return nil
    }

    fmt.Printf("\nCreating %s project '%s'...\n", projectType, projectName)
    if useDocker {
        fmt.Println("  - Docker support enabled")
    }

    return nil
}

func prompt(reader *bufio.Reader, label, defaultVal string) (string, error) {
    if defaultVal != "" {
        fmt.Printf("%s [%s]: ", label, defaultVal)
    } else {
        fmt.Printf("%s: ", label)
    }

    input, err := reader.ReadString('\n')
    if err != nil {
        return "", err
    }

    input = strings.TrimSpace(input)
    if input == "" {
        return defaultVal, nil
    }

    return input, nil
}

func promptSelect(reader *bufio.Reader, label string, options []string) (string, error) {
    fmt.Printf("%s:\n", label)
    for i, opt := range options {
        fmt.Printf("  %d) %s\n", i+1, opt)
    }
    fmt.Printf("Select [1]: ")

    input, err := reader.ReadString('\n')
    if err != nil {
        return "", err
    }

    input = strings.TrimSpace(input)
    if input == "" {
        return options[0], nil
    }

    var idx int
    fmt.Sscanf(input, "%d", &idx)

    if idx < 1 || idx > len(options) {
        return options[0], nil
    }

    return options[idx-1], nil
}

func promptBool(reader *bufio.Reader, label string, defaultVal bool) (bool, error) {
    defaultStr := "N"
    if defaultVal {
        defaultStr = "Y"
    }

    fmt.Printf("%s [%s]: ", label, defaultStr)

    input, err := reader.ReadString('\n')
    if err != nil {
        return false, err
    }

    input = strings.TrimSpace(strings.ToLower(input))

    switch input {
    case "":
        return defaultVal, nil
    case "y", "yes", "true", "1":
        return true, nil
    default:
        return false, nil
    }
}

func promptPassword(label string) (string, error) {
    fmt.Printf("%s: ", label)

    password, err := term.ReadPassword(int(syscall.Stdin))
    fmt.Println() // New line after password input

    if err != nil {
        return "", err
    }

    return string(password), nil
}
```

---

## Operaciones de Sistema de Archivos

### Operaciones Básicas

```go
package filesystem

import (
    "errors"
    "fmt"
    "io"
    "io/fs"
    "os"
    "path/filepath"
    "time"
)

// File information wrapper
type FileInfo struct {
    Path       string
    Name       string
    Size       int64
    Mode       fs.FileMode
    ModTime    time.Time
    IsDir      bool
    Extension  string
}

// Read entire file
func ReadFile(path string) ([]byte, error) {
    return os.ReadFile(path)
}

// Read file with size limit
func ReadFileLimited(path string, maxSize int64) ([]byte, error) {
    f, err := os.Open(path)
    if err != nil {
        return nil, err
    }
    defer f.Close()

    info, err := f.Stat()
    if err != nil {
        return nil, err
    }

    if info.Size() > maxSize {
        return nil, fmt.Errorf("file too large: %d > %d", info.Size(), maxSize)
    }

    return io.ReadAll(f)
}

// Write file with options
func WriteFile(path string, data []byte, perm fs.FileMode) error {
    return os.WriteFile(path, data, perm)
}

// Write file atomically (write to temp, then rename)
func WriteFileAtomic(path string, data []byte, perm fs.FileMode) error {
    dir := filepath.Dir(path)
    base := filepath.Base(path)

    // Create temp file in same directory
    temp, err := os.CreateTemp(dir, base+".tmp.*")
    if err != nil {
        return err
    }
    tempPath := temp.Name()

    // Clean up on error
    success := false
    defer func() {
        if !success {
            temp.Close()
            os.Remove(tempPath)
        }
    }()

    // Write data
    if _, err := temp.Write(data); err != nil {
        return err
    }

    // Sync to disk
    if err := temp.Sync(); err != nil {
        return err
    }

    // Close before rename
    if err := temp.Close(); err != nil {
        return err
    }

    // Set permissions
    if err := os.Chmod(tempPath, perm); err != nil {
        return err
    }

    // Atomic rename
    if err := os.Rename(tempPath, path); err != nil {
        return err
    }

    success = true
    return nil
}

// Copy file
func CopyFile(src, dst string) error {
    srcFile, err := os.Open(src)
    if err != nil {
        return err
    }
    defer srcFile.Close()

    srcInfo, err := srcFile.Stat()
    if err != nil {
        return err
    }

    dstFile, err := os.OpenFile(dst, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, srcInfo.Mode())
    if err != nil {
        return err
    }
    defer dstFile.Close()

    if _, err := io.Copy(dstFile, srcFile); err != nil {
        return err
    }

    return dstFile.Sync()
}

// Copy directory recursively
func CopyDir(src, dst string) error {
    return filepath.WalkDir(src, func(path string, d fs.DirEntry, err error) error {
        if err != nil {
            return err
        }

        relPath, err := filepath.Rel(src, path)
        if err != nil {
            return err
        }

        dstPath := filepath.Join(dst, relPath)

        if d.IsDir() {
            info, err := d.Info()
            if err != nil {
                return err
            }
            return os.MkdirAll(dstPath, info.Mode())
        }

        return CopyFile(path, dstPath)
    })
}

// Check if file exists
func FileExists(path string) bool {
    _, err := os.Stat(path)
    return err == nil
}

// Check if directory exists
func DirExists(path string) bool {
    info, err := os.Stat(path)
    return err == nil && info.IsDir()
}

// Get file info
func GetFileInfo(path string) (*FileInfo, error) {
    info, err := os.Stat(path)
    if err != nil {
        return nil, err
    }

    return &FileInfo{
        Path:      path,
        Name:      info.Name(),
        Size:      info.Size(),
        Mode:      info.Mode(),
        ModTime:   info.ModTime(),
        IsDir:     info.IsDir(),
        Extension: filepath.Ext(path),
    }, nil
}

// List files in directory
func ListFiles(dir string, recursive bool) ([]FileInfo, error) {
    var files []FileInfo

    walkFn := func(path string, d fs.DirEntry, err error) error {
        if err != nil {
            return err
        }

        // Skip root directory
        if path == dir {
            return nil
        }

        // Skip subdirectories if not recursive
        if !recursive && d.IsDir() {
            return fs.SkipDir
        }

        info, err := d.Info()
        if err != nil {
            return err
        }

        files = append(files, FileInfo{
            Path:      path,
            Name:      info.Name(),
            Size:      info.Size(),
            Mode:      info.Mode(),
            ModTime:   info.ModTime(),
            IsDir:     info.IsDir(),
            Extension: filepath.Ext(path),
        })

        return nil
    }

    if err := filepath.WalkDir(dir, walkFn); err != nil {
        return nil, err
    }

    return files, nil
}
```

### Operaciones Avanzadas

```go
package filesystem

import (
    "crypto/sha256"
    "encoding/hex"
    "io"
    "os"
    "path/filepath"
    "sort"
    "strings"
)

// Calculate file checksum
func FileChecksum(path string) (string, error) {
    f, err := os.Open(path)
    if err != nil {
        return "", err
    }
    defer f.Close()

    h := sha256.New()
    if _, err := io.Copy(h, f); err != nil {
        return "", err
    }

    return hex.EncodeToString(h.Sum(nil)), nil
}

// Find files matching pattern
func FindFiles(root string, pattern string) ([]string, error) {
    var matches []string

    err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
        if err != nil {
            return err
        }

        if d.IsDir() {
            return nil
        }

        matched, err := filepath.Match(pattern, d.Name())
        if err != nil {
            return err
        }

        if matched {
            matches = append(matches, path)
        }

        return nil
    })

    return matches, err
}

// Find files by extension
func FindByExtension(root string, ext string) ([]string, error) {
    if !strings.HasPrefix(ext, ".") {
        ext = "." + ext
    }

    var matches []string

    err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
        if err != nil {
            return err
        }

        if !d.IsDir() && strings.EqualFold(filepath.Ext(path), ext) {
            matches = append(matches, path)
        }

        return nil
    })

    return matches, err
}

// Directory size
func DirSize(path string) (int64, error) {
    var size int64

    err := filepath.WalkDir(path, func(_ string, d fs.DirEntry, err error) error {
        if err != nil {
            return err
        }

        if !d.IsDir() {
            info, err := d.Info()
            if err != nil {
                return err
            }
            size += info.Size()
        }

        return nil
    })

    return size, err
}

// Clean old files
func CleanOldFiles(dir string, maxAge time.Duration, pattern string) (int, error) {
    cutoff := time.Now().Add(-maxAge)
    count := 0

    err := filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
        if err != nil {
            return err
        }

        if d.IsDir() {
            return nil
        }

        // Check pattern
        if pattern != "" {
            matched, _ := filepath.Match(pattern, d.Name())
            if !matched {
                return nil
            }
        }

        info, err := d.Info()
        if err != nil {
            return err
        }

        if info.ModTime().Before(cutoff) {
            if err := os.Remove(path); err != nil {
                return err
            }
            count++
        }

        return nil
    })

    return count, err
}

// Watch directory for changes (simple polling)
type FileChange struct {
    Path   string
    Event  string // created, modified, deleted
    Time   time.Time
}

func WatchDir(dir string, interval time.Duration, callback func(FileChange)) (stop func()) {
    state := make(map[string]time.Time)
    done := make(chan struct{})

    // Initial scan
    filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
        if err != nil || d.IsDir() {
            return nil
        }
        info, _ := d.Info()
        state[path] = info.ModTime()
        return nil
    })

    go func() {
        ticker := time.NewTicker(interval)
        defer ticker.Stop()

        for {
            select {
            case <-done:
                return
            case <-ticker.C:
                newState := make(map[string]time.Time)

                filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
                    if err != nil || d.IsDir() {
                        return nil
                    }
                    info, _ := d.Info()
                    newState[path] = info.ModTime()

                    oldTime, exists := state[path]
                    if !exists {
                        callback(FileChange{Path: path, Event: "created", Time: time.Now()})
                    } else if !oldTime.Equal(info.ModTime()) {
                        callback(FileChange{Path: path, Event: "modified", Time: time.Now()})
                    }

                    return nil
                })

                // Check for deleted files
                for path := range state {
                    if _, exists := newState[path]; !exists {
                        callback(FileChange{Path: path, Event: "deleted", Time: time.Now()})
                    }
                }

                state = newState
            }
        }
    }()

    return func() { close(done) }
}

// Temporary directory with cleanup
type TempDir struct {
    Path string
}

func NewTempDir(prefix string) (*TempDir, error) {
    path, err := os.MkdirTemp("", prefix)
    if err != nil {
        return nil, err
    }
    return &TempDir{Path: path}, nil
}

func (t *TempDir) Cleanup() error {
    return os.RemoveAll(t.Path)
}

func (t *TempDir) File(name string) string {
    return filepath.Join(t.Path, name)
}
```

---

## Programación de Red

### Cliente y Servidor TCP

```go
package network

import (
    "bufio"
    "context"
    "encoding/json"
    "fmt"
    "io"
    "net"
    "sync"
    "time"
)

// Message protocol
type Message struct {
    Type    string          `json:"type"`
    Payload json.RawMessage `json:"payload"`
    Time    time.Time       `json:"time"`
}

// TCP Server
type TCPServer struct {
    addr     string
    listener net.Listener
    clients  map[net.Conn]struct{}
    mu       sync.RWMutex
    handler  func(*Client, Message) error
    done     chan struct{}
}

type Client struct {
    conn    net.Conn
    encoder *json.Encoder
    decoder *json.Decoder
}

func NewTCPServer(addr string, handler func(*Client, Message) error) *TCPServer {
    return &TCPServer{
        addr:    addr,
        clients: make(map[net.Conn]struct{}),
        handler: handler,
        done:    make(chan struct{}),
    }
}

func (s *TCPServer) Start() error {
    var err error
    s.listener, err = net.Listen("tcp", s.addr)
    if err != nil {
        return err
    }

    fmt.Printf("Server listening on %s\n", s.addr)

    go s.acceptLoop()
    return nil
}

func (s *TCPServer) acceptLoop() {
    for {
        conn, err := s.listener.Accept()
        if err != nil {
            select {
            case <-s.done:
                return
            default:
                fmt.Printf("Accept error: %v\n", err)
                continue
            }
        }

        s.mu.Lock()
        s.clients[conn] = struct{}{}
        s.mu.Unlock()

        go s.handleClient(conn)
    }
}

func (s *TCPServer) handleClient(conn net.Conn) {
    defer func() {
        conn.Close()
        s.mu.Lock()
        delete(s.clients, conn)
        s.mu.Unlock()
    }()

    client := &Client{
        conn:    conn,
        encoder: json.NewEncoder(conn),
        decoder: json.NewDecoder(conn),
    }

    for {
        var msg Message
        if err := client.decoder.Decode(&msg); err != nil {
            if err != io.EOF {
                fmt.Printf("Decode error: %v\n", err)
            }
            return
        }

        if err := s.handler(client, msg); err != nil {
            fmt.Printf("Handler error: %v\n", err)
        }
    }
}

func (s *TCPServer) Broadcast(msg Message) {
    s.mu.RLock()
    defer s.mu.RUnlock()

    for conn := range s.clients {
        encoder := json.NewEncoder(conn)
        encoder.Encode(msg)
    }
}

func (s *TCPServer) Stop() error {
    close(s.done)

    if s.listener != nil {
        s.listener.Close()
    }

    s.mu.Lock()
    for conn := range s.clients {
        conn.Close()
    }
    s.mu.Unlock()

    return nil
}

func (c *Client) Send(msg Message) error {
    return c.encoder.Encode(msg)
}

func (c *Client) RemoteAddr() string {
    return c.conn.RemoteAddr().String()
}

// TCP Client
type TCPClient struct {
    conn    net.Conn
    encoder *json.Encoder
    decoder *json.Decoder
}

func DialTCP(addr string, timeout time.Duration) (*TCPClient, error) {
    conn, err := net.DialTimeout("tcp", addr, timeout)
    if err != nil {
        return nil, err
    }

    return &TCPClient{
        conn:    conn,
        encoder: json.NewEncoder(conn),
        decoder: json.NewDecoder(conn),
    }, nil
}

func (c *TCPClient) Send(msg Message) error {
    msg.Time = time.Now()
    return c.encoder.Encode(msg)
}

func (c *TCPClient) Receive() (*Message, error) {
    var msg Message
    if err := c.decoder.Decode(&msg); err != nil {
        return nil, err
    }
    return &msg, nil
}

func (c *TCPClient) Close() error {
    return c.conn.Close()
}

// Example usage
func ExampleServer() {
    handler := func(client *Client, msg Message) error {
        fmt.Printf("Received from %s: %s\n", client.RemoteAddr(), msg.Type)

        // Echo back
        response := Message{
            Type:    "response",
            Payload: msg.Payload,
            Time:    time.Now(),
        }
        return client.Send(response)
    }

    server := NewTCPServer(":8000", handler)
    if err := server.Start(); err != nil {
        fmt.Printf("Server error: %v\n", err)
    }
}
```

### HTTP Cliente Avanzado

```go
package network

import (
    "bytes"
    "context"
    "encoding/json"
    "fmt"
    "io"
    "net/http"
    "net/url"
    "time"
)

// HTTP Client with retries and configuration
type HTTPClient struct {
    client     *http.Client
    baseURL    string
    headers    map[string]string
    maxRetries int
    retryDelay time.Duration
}

type HTTPClientOption func(*HTTPClient)

func WithTimeout(timeout time.Duration) HTTPClientOption {
    return func(c *HTTPClient) {
        c.client.Timeout = timeout
    }
}

func WithHeader(key, value string) HTTPClientOption {
    return func(c *HTTPClient) {
        c.headers[key] = value
    }
}

func WithRetries(maxRetries int, delay time.Duration) HTTPClientOption {
    return func(c *HTTPClient) {
        c.maxRetries = maxRetries
        c.retryDelay = delay
    }
}

func NewHTTPClient(baseURL string, opts ...HTTPClientOption) *HTTPClient {
    c := &HTTPClient{
        client: &http.Client{
            Timeout: 30 * time.Second,
            Transport: &http.Transport{
                MaxIdleConns:        100,
                MaxIdleConnsPerHost: 10,
                IdleConnTimeout:     90 * time.Second,
            },
        },
        baseURL:    baseURL,
        headers:    make(map[string]string),
        maxRetries: 3,
        retryDelay: time.Second,
    }

    for _, opt := range opts {
        opt(c)
    }

    return c
}

type Response struct {
    StatusCode int
    Headers    http.Header
    Body       []byte
}

func (c *HTTPClient) Do(ctx context.Context, method, path string, body interface{}) (*Response, error) {
    var bodyReader io.Reader
    if body != nil {
        jsonBody, err := json.Marshal(body)
        if err != nil {
            return nil, fmt.Errorf("marshal body: %w", err)
        }
        bodyReader = bytes.NewReader(jsonBody)
    }

    fullURL := c.baseURL + path

    var lastErr error
    for attempt := 0; attempt <= c.maxRetries; attempt++ {
        if attempt > 0 {
            time.Sleep(c.retryDelay * time.Duration(attempt))
        }

        req, err := http.NewRequestWithContext(ctx, method, fullURL, bodyReader)
        if err != nil {
            return nil, fmt.Errorf("create request: %w", err)
        }

        // Set headers
        req.Header.Set("Content-Type", "application/json")
        for k, v := range c.headers {
            req.Header.Set(k, v)
        }

        resp, err := c.client.Do(req)
        if err != nil {
            lastErr = err
            continue
        }

        defer resp.Body.Close()
        respBody, err := io.ReadAll(resp.Body)
        if err != nil {
            lastErr = err
            continue
        }

        // Retry on 5xx errors
        if resp.StatusCode >= 500 && attempt < c.maxRetries {
            lastErr = fmt.Errorf("server error: %d", resp.StatusCode)
            continue
        }

        return &Response{
            StatusCode: resp.StatusCode,
            Headers:    resp.Header,
            Body:       respBody,
        }, nil
    }

    return nil, fmt.Errorf("max retries exceeded: %w", lastErr)
}

func (c *HTTPClient) Get(ctx context.Context, path string) (*Response, error) {
    return c.Do(ctx, http.MethodGet, path, nil)
}

func (c *HTTPClient) Post(ctx context.Context, path string, body interface{}) (*Response, error) {
    return c.Do(ctx, http.MethodPost, path, body)
}

func (c *HTTPClient) Put(ctx context.Context, path string, body interface{}) (*Response, error) {
    return c.Do(ctx, http.MethodPut, path, body)
}

func (c *HTTPClient) Delete(ctx context.Context, path string) (*Response, error) {
    return c.Do(ctx, http.MethodDelete, path, nil)
}

// Decode response body
func (r *Response) Decode(v interface{}) error {
    return json.Unmarshal(r.Body, v)
}

// Check if response is successful
func (r *Response) IsSuccess() bool {
    return r.StatusCode >= 200 && r.StatusCode < 300
}

// Example
func ExampleHTTPClient() {
    client := NewHTTPClient("https://api.example.com",
        WithTimeout(10*time.Second),
        WithHeader("Authorization", "Bearer token"),
        WithRetries(3, time.Second),
    )

    ctx := context.Background()

    // GET request
    resp, err := client.Get(ctx, "/users/1")
    if err != nil {
        fmt.Printf("Error: %v\n", err)
        return
    }

    if resp.IsSuccess() {
        var user struct {
            ID   int    `json:"id"`
            Name string `json:"name"`
        }
        resp.Decode(&user)
        fmt.Printf("User: %+v\n", user)
    }

    // POST request
    newUser := map[string]string{"name": "John", "email": "john@example.com"}
    resp, err = client.Post(ctx, "/users", newUser)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
        return
    }
    fmt.Printf("Created: %s\n", string(resp.Body))
}
```

---

## Cross-Compilation

### Compilación para Múltiples Plataformas

```bash
#!/bin/bash
# build.sh - Cross-compilation script

APP_NAME="mytool"
VERSION="1.0.0"
BUILD_TIME=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
GIT_COMMIT=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")

LDFLAGS="-s -w \
    -X main.Version=${VERSION} \
    -X main.GitCommit=${GIT_COMMIT} \
    -X main.BuildTime=${BUILD_TIME}"

# Output directory
OUT_DIR="dist"
mkdir -p "$OUT_DIR"

# Build matrix
PLATFORMS=(
    "linux/amd64"
    "linux/arm64"
    "linux/arm"
    "darwin/amd64"
    "darwin/arm64"
    "windows/amd64"
    "windows/arm64"
    "freebsd/amd64"
)

for PLATFORM in "${PLATFORMS[@]}"; do
    OS="${PLATFORM%/*}"
    ARCH="${PLATFORM#*/}"

    OUTPUT="${OUT_DIR}/${APP_NAME}-${VERSION}-${OS}-${ARCH}"

    if [ "$OS" = "windows" ]; then
        OUTPUT="${OUTPUT}.exe"
    fi

    echo "Building for $OS/$ARCH..."

    CGO_ENABLED=0 GOOS="$OS" GOARCH="$ARCH" go build \
        -ldflags "$LDFLAGS" \
        -o "$OUTPUT" \
        .

    if [ $? -eq 0 ]; then
        echo "  -> $OUTPUT"
    else
        echo "  -> FAILED"
    fi
done

# Create checksums
cd "$OUT_DIR"
sha256sum * > checksums.sha256
echo "Checksums:"
cat checksums.sha256
```

### Makefile para Builds

```makefile
# Makefile

APP_NAME := mytool
VERSION := $(shell git describe --tags --always --dirty 2>/dev/null || echo "dev")
GIT_COMMIT := $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")
BUILD_TIME := $(shell date -u +"%Y-%m-%dT%H:%M:%SZ")

LDFLAGS := -s -w \
    -X 'main.Version=$(VERSION)' \
    -X 'main.GitCommit=$(GIT_COMMIT)' \
    -X 'main.BuildTime=$(BUILD_TIME)'

GO := go
GOFLAGS := -trimpath

# Directories
OUT_DIR := dist
BIN_DIR := bin

.PHONY: all build clean test lint release

all: build

build:
	@mkdir -p $(BIN_DIR)
	$(GO) build $(GOFLAGS) -ldflags "$(LDFLAGS)" -o $(BIN_DIR)/$(APP_NAME) .

build-linux-amd64:
	@mkdir -p $(OUT_DIR)
	CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
		$(GO) build $(GOFLAGS) -ldflags "$(LDFLAGS)" \
		-o $(OUT_DIR)/$(APP_NAME)-linux-amd64 .

build-linux-arm64:
	@mkdir -p $(OUT_DIR)
	CGO_ENABLED=0 GOOS=linux GOARCH=arm64 \
		$(GO) build $(GOFLAGS) -ldflags "$(LDFLAGS)" \
		-o $(OUT_DIR)/$(APP_NAME)-linux-arm64 .

build-darwin-amd64:
	@mkdir -p $(OUT_DIR)
	CGO_ENABLED=0 GOOS=darwin GOARCH=amd64 \
		$(GO) build $(GOFLAGS) -ldflags "$(LDFLAGS)" \
		-o $(OUT_DIR)/$(APP_NAME)-darwin-amd64 .

build-darwin-arm64:
	@mkdir -p $(OUT_DIR)
	CGO_ENABLED=0 GOOS=darwin GOARCH=arm64 \
		$(GO) build $(GOFLAGS) -ldflags "$(LDFLAGS)" \
		-o $(OUT_DIR)/$(APP_NAME)-darwin-arm64 .

build-windows-amd64:
	@mkdir -p $(OUT_DIR)
	CGO_ENABLED=0 GOOS=windows GOARCH=amd64 \
		$(GO) build $(GOFLAGS) -ldflags "$(LDFLAGS)" \
		-o $(OUT_DIR)/$(APP_NAME)-windows-amd64.exe .

release: build-linux-amd64 build-linux-arm64 build-darwin-amd64 build-darwin-arm64 build-windows-amd64
	@cd $(OUT_DIR) && sha256sum * > checksums.sha256
	@echo "Release builds in $(OUT_DIR)/"

test:
	$(GO) test -v -race -cover ./...

lint:
	golangci-lint run

clean:
	rm -rf $(BIN_DIR) $(OUT_DIR)
	$(GO) clean

install: build
	cp $(BIN_DIR)/$(APP_NAME) $(GOPATH)/bin/

# Docker build
docker-build:
	docker build -t $(APP_NAME):$(VERSION) .

docker-push:
	docker push $(APP_NAME):$(VERSION)
```

---

## Static Linking

### Compilación Estática Completa

```go
// +build linux

package main

/*
Static linking notes for Go:

1. Pure Go (no cgo):
   CGO_ENABLED=0 go build -ldflags '-s -w'

2. With cgo and static linking:
   CGO_ENABLED=1 go build -ldflags '-s -w -extldflags "-static"'

3. For musl libc (Alpine):
   CC=musl-gcc go build -ldflags '-s -w -extldflags "-static"'

4. Static linking with specific libraries:
   go build -ldflags '-s -w -extldflags "-static -lpthread -ldl"'
*/

import "fmt"

func main() {
    fmt.Println("Statically linked binary")
}
```

### Dockerfile Multi-Stage para Binarios Estáticos

```dockerfile
# Dockerfile
# Stage 1: Build
FROM golang:1.21-alpine AS builder

# Install build dependencies
RUN apk add --no-cache git ca-certificates tzdata

WORKDIR /app

# Copy go mod files
COPY go.mod go.sum ./
RUN go mod download

# Copy source code
COPY . .

# Build static binary
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build \
    -ldflags='-s -w -extldflags "-static"' \
    -o /app/mytool .

# Stage 2: Minimal runtime image
FROM scratch

# Copy CA certificates for HTTPS
COPY --from=builder /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/

# Copy timezone data
COPY --from=builder /usr/share/zoneinfo /usr/share/zoneinfo

# Copy binary
COPY --from=builder /app/mytool /mytool

# Non-root user (optional, requires creating in builder)
# USER 1000:1000

ENTRYPOINT ["/mytool"]
```

```dockerfile
# Dockerfile.distroless - Using distroless base
FROM golang:1.21 AS builder

WORKDIR /app
COPY go.mod go.sum ./
RUN go mod download
COPY . .

RUN CGO_ENABLED=0 go build -ldflags='-s -w' -o /mytool .

FROM gcr.io/distroless/static-debian12:nonroot

COPY --from=builder /mytool /mytool

USER nonroot:nonroot

ENTRYPOINT ["/mytool"]
```

---

## Reemplazando Utilidades C

### Ejemplo: Reemplazo de cat

```go
// cmd/cat/main.go
package main

import (
    "bufio"
    "flag"
    "fmt"
    "io"
    "os"
)

var (
    numberLines    = flag.Bool("n", false, "number all output lines")
    numberNonBlank = flag.Bool("b", false, "number non-blank output lines")
    showEnds       = flag.Bool("E", false, "display $ at end of each line")
    showTabs       = flag.Bool("T", false, "display TAB characters as ^I")
    squeezeBlank   = flag.Bool("s", false, "suppress repeated empty output lines")
)

func main() {
    flag.Parse()

    args := flag.Args()
    if len(args) == 0 {
        cat(os.Stdin, "stdin")
        return
    }

    for _, filename := range args {
        if filename == "-" {
            cat(os.Stdin, "stdin")
            continue
        }

        f, err := os.Open(filename)
        if err != nil {
            fmt.Fprintf(os.Stderr, "cat: %s: %v\n", filename, err)
            continue
        }

        cat(f, filename)
        f.Close()
    }
}

func cat(r io.Reader, name string) {
    scanner := bufio.NewScanner(r)
    lineNum := 0
    prevBlank := false

    for scanner.Scan() {
        line := scanner.Text()
        isBlank := len(line) == 0

        // Squeeze blank lines
        if *squeezeBlank && isBlank && prevBlank {
            continue
        }
        prevBlank = isBlank

        // Number lines
        if *numberLines || (*numberNonBlank && !isBlank) {
            lineNum++
            fmt.Printf("%6d\t", lineNum)
        }

        // Process line content
        output := line
        if *showTabs {
            output = replaceTabs(output)
        }

        fmt.Print(output)

        // Show line endings
        if *showEnds {
            fmt.Print("$")
        }
        fmt.Println()
    }

    if err := scanner.Err(); err != nil {
        fmt.Fprintf(os.Stderr, "cat: %s: %v\n", name, err)
    }
}

func replaceTabs(s string) string {
    result := make([]byte, 0, len(s))
    for i := 0; i < len(s); i++ {
        if s[i] == '\t' {
            result = append(result, '^', 'I')
        } else {
            result = append(result, s[i])
        }
    }
    return string(result)
}
```

### Ejemplo: Reemplazo de grep

```go
// cmd/grep/main.go
package main

import (
    "bufio"
    "flag"
    "fmt"
    "os"
    "regexp"
    "strings"
)

var (
    ignoreCase     = flag.Bool("i", false, "ignore case distinctions")
    invertMatch    = flag.Bool("v", false, "select non-matching lines")
    countOnly      = flag.Bool("c", false, "print only a count of matching lines")
    lineNumbers    = flag.Bool("n", false, "print line number with output lines")
    filesWithMatch = flag.Bool("l", false, "print only names of files with matches")
    quiet          = flag.Bool("q", false, "suppress all normal output")
    recursive      = flag.Bool("r", false, "read all files under each directory")
    fixedStrings   = flag.Bool("F", false, "interpret pattern as fixed string")
    afterContext   = flag.Int("A", 0, "print NUM lines after match")
    beforeContext  = flag.Int("B", 0, "print NUM lines before match")
)

type matcher interface {
    Match(line string) bool
}

type regexMatcher struct {
    re *regexp.Regexp
}

func (m *regexMatcher) Match(line string) bool {
    return m.re.MatchString(line)
}

type stringMatcher struct {
    pattern string
    ignoreCase bool
}

func (m *stringMatcher) Match(line string) bool {
    if m.ignoreCase {
        return strings.Contains(strings.ToLower(line), strings.ToLower(m.pattern))
    }
    return strings.Contains(line, m.pattern)
}

func main() {
    flag.Parse()

    args := flag.Args()
    if len(args) < 1 {
        fmt.Fprintln(os.Stderr, "Usage: grep [OPTIONS] PATTERN [FILE...]")
        os.Exit(2)
    }

    pattern := args[0]
    files := args[1:]

    // Create matcher
    var m matcher
    if *fixedStrings {
        m = &stringMatcher{pattern: pattern, ignoreCase: *ignoreCase}
    } else {
        flags := ""
        if *ignoreCase {
            flags = "(?i)"
        }
        re, err := regexp.Compile(flags + pattern)
        if err != nil {
            fmt.Fprintf(os.Stderr, "grep: invalid pattern: %v\n", err)
            os.Exit(2)
        }
        m = &regexMatcher{re: re}
    }

    // Process files or stdin
    exitCode := 1 // No matches found
    multipleFiles := len(files) > 1

    if len(files) == 0 {
        if grep(m, os.Stdin, "", false) {
            exitCode = 0
        }
    } else {
        for _, filename := range files {
            f, err := os.Open(filename)
            if err != nil {
                fmt.Fprintf(os.Stderr, "grep: %s: %v\n", filename, err)
                continue
            }

            if grep(m, f, filename, multipleFiles) {
                exitCode = 0
            }
            f.Close()
        }
    }

    os.Exit(exitCode)
}

func grep(m matcher, r *os.File, filename string, showFilename bool) bool {
    scanner := bufio.NewScanner(r)
    lineNum := 0
    matchCount := 0
    hasMatch := false

    // Context buffers
    beforeBuffer := make([]string, 0, *beforeContext)
    afterCount := 0

    for scanner.Scan() {
        lineNum++
        line := scanner.Text()
        matched := m.Match(line)

        if *invertMatch {
            matched = !matched
        }

        if matched {
            hasMatch = true
            matchCount++

            if *quiet {
                return true
            }

            if *filesWithMatch {
                fmt.Println(filename)
                return true
            }

            if !*countOnly {
                // Print before context
                for _, bline := range beforeBuffer {
                    printLine(filename, showFilename, bline, false)
                }
                beforeBuffer = beforeBuffer[:0]

                // Print matching line
                printLine(filename, showFilename, formatLine(lineNum, line), true)
                afterCount = *afterContext
            }
        } else {
            // Handle after context
            if afterCount > 0 {
                printLine(filename, showFilename, formatLine(lineNum, line), false)
                afterCount--
            }

            // Update before context buffer
            if *beforeContext > 0 {
                if len(beforeBuffer) >= *beforeContext {
                    beforeBuffer = beforeBuffer[1:]
                }
                beforeBuffer = append(beforeBuffer, formatLine(lineNum, line))
            }
        }
    }

    if *countOnly {
        if showFilename {
            fmt.Printf("%s:%d\n", filename, matchCount)
        } else {
            fmt.Println(matchCount)
        }
    }

    return hasMatch
}

func formatLine(lineNum int, line string) string {
    if *lineNumbers {
        return fmt.Sprintf("%d:%s", lineNum, line)
    }
    return line
}

func printLine(filename string, showFilename bool, line string, isMatch bool) {
    if showFilename {
        sep := ":"
        if !isMatch {
            sep = "-"
        }
        fmt.Printf("%s%s%s\n", filename, sep, line)
    } else {
        fmt.Println(line)
    }
}
```

---

## Mejores Prácticas

### Lista de Verificación para Herramientas de Sistema

```go
/*
SYSTEMS PROGRAMMING CHECKLIST:

1. ERROR HANDLING
   [x] Check all error returns
   [x] Use meaningful error messages with context
   [x] Exit with appropriate codes (0=success, 1=error, 2=usage)
   [x] Handle SIGINT/SIGTERM gracefully

2. FILE OPERATIONS
   [x] Always close files (use defer)
   [x] Check file permissions before operations
   [x] Handle large files with streaming
   [x] Use atomic writes for critical data

3. NETWORKING
   [x] Set appropriate timeouts
   [x] Handle connection errors gracefully
   [x] Support cancellation via context
   [x] Implement retries with backoff

4. CLI DESIGN
   [x] Follow Unix conventions (-h, --help, --)
   [x] Provide meaningful --version output
   [x] Support stdin/stdout for piping
   [x] Return appropriate exit codes

5. CROSS-PLATFORM
   [x] Use path/filepath for paths
   [x] Handle line endings correctly
   [x] Test on all target platforms
   [x] Avoid platform-specific code where possible

6. PERFORMANCE
   [x] Use buffered I/O
   [x] Minimize allocations in hot paths
   [x] Profile before optimizing
   [x] Consider memory-mapped files for large data

7. SECURITY
   [x] Validate all input
   [x] Use secure random for crypto
   [x] Handle sensitive data carefully
   [x] Set appropriate file permissions
*/
```

---

## Referencias

- [Cobra Documentation](https://cobra.dev/)
- [Viper Documentation](https://github.com/spf13/viper)
- [Go File System](https://pkg.go.dev/io/fs)
- [Go Network Programming](https://pkg.go.dev/net)
- [Cross-Compilation Guide](https://golang.org/doc/install/source#environment)

---

*ARCHAEON_CORE - Preservando la evolución del código*
*De utilidades C a herramientas Go: La nueva era del sistemas*
