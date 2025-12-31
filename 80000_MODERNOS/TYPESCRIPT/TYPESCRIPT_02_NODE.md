---
título: "TypeScript 02 - Node.js y Foreign Function Interface"
módulo: ARCHAEON_CORE
sección: 80000_MODERNOS/TYPESCRIPT
versión: 1.0.0
fecha_creación: 2025-12-31
autor: ARCHAEON Sistema de Documentación
descripción: >
  Integración de TypeScript con Node.js, desarrollo de APIs con Express/Fastify,
  acceso a bases de datos, y FFI para llamar bibliotecas nativas de C.
tags:
  - typescript
  - nodejs
  - express
  - fastify
  - ffi
  - native-addons
requisitos:
  - Node.js 18+
  - TypeScript 5.0+
  - npm/pnpm
  - Compilador C (para native addons)
---

# TYPESCRIPT 02 - NODE.JS Y FOREIGN FUNCTION INTERFACE

## Tabla de Contenidos

1. [Node.js con TypeScript](#nodejs-con-typescript)
2. [Express con TypeScript](#express-con-typescript)
3. [Fastify con TypeScript](#fastify-con-typescript)
4. [Acceso a Bases de Datos](#acceso-a-bases-de-datos)
5. [FFI con node-ffi-napi](#ffi-con-node-ffi-napi)
6. [N-API Native Addons](#n-api-native-addons)
7. [Patrones de Integración](#patrones-de-integración)

---

## Node.js con TypeScript

### Configuración Inicial

```bash
# Initialize project
mkdir my-node-ts && cd my-node-ts
npm init -y

# Install TypeScript and Node types
npm install typescript @types/node -D

# Install development tools
npm install tsx -D  # Fast TypeScript execution
npm install tsup -D # For building

# Initialize TypeScript config
npx tsc --init
```

### tsconfig.json para Node.js

```json
{
  "$schema": "https://json.schemastore.org/tsconfig",
  "compilerOptions": {
    "target": "ES2022",
    "module": "NodeNext",
    "moduleResolution": "NodeNext",
    "lib": ["ES2022"],
    "outDir": "./dist",
    "rootDir": "./src",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true,
    "resolveJsonModule": true,
    "declaration": true,
    "declarationMap": true,
    "sourceMap": true,
    "noUncheckedIndexedAccess": true,
    "noImplicitReturns": true,
    "types": ["node"]
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "dist"]
}
```

### package.json Scripts

```json
{
  "name": "my-node-ts",
  "version": "1.0.0",
  "type": "module",
  "scripts": {
    "dev": "tsx watch src/index.ts",
    "build": "tsup src/index.ts --format esm,cjs --dts",
    "start": "node dist/index.js",
    "typecheck": "tsc --noEmit",
    "lint": "eslint src --ext .ts"
  },
  "engines": {
    "node": ">=18.0.0"
  }
}
```

### Aplicación Básica Node.js

```typescript
// src/index.ts
import { createServer, IncomingMessage, ServerResponse } from "node:http";
import { readFile } from "node:fs/promises";
import { join } from "node:path";

interface AppConfig {
  port: number;
  host: string;
  env: "development" | "production" | "test";
}

const config: AppConfig = {
  port: parseInt(process.env.PORT ?? "3000", 10),
  host: process.env.HOST ?? "localhost",
  env: (process.env.NODE_ENV as AppConfig["env"]) ?? "development",
};

// Type-safe request handler
type RequestHandler = (
  req: IncomingMessage,
  res: ServerResponse
) => Promise<void>;

const handleRequest: RequestHandler = async (req, res) => {
  const { method, url } = req;

  console.log(`${method} ${url}`);

  if (url === "/" && method === "GET") {
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(JSON.stringify({ message: "Hello from TypeScript!" }));
    return;
  }

  if (url === "/health" && method === "GET") {
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(
      JSON.stringify({
        status: "healthy",
        timestamp: new Date().toISOString(),
        uptime: process.uptime(),
      })
    );
    return;
  }

  res.writeHead(404, { "Content-Type": "application/json" });
  res.end(JSON.stringify({ error: "Not Found" }));
};

const server = createServer((req, res) => {
  handleRequest(req, res).catch((error) => {
    console.error("Request error:", error);
    res.writeHead(500, { "Content-Type": "application/json" });
    res.end(JSON.stringify({ error: "Internal Server Error" }));
  });
});

server.listen(config.port, config.host, () => {
  console.log(`Server running at http://${config.host}:${config.port}`);
  console.log(`Environment: ${config.env}`);
});

// Graceful shutdown
process.on("SIGTERM", () => {
  console.log("SIGTERM received, shutting down gracefully");
  server.close(() => {
    console.log("Server closed");
    process.exit(0);
  });
});
```

---

## Express con TypeScript

### Configuración de Express

```bash
npm install express
npm install @types/express -D
```

### Aplicación Express Tipada

```typescript
// src/app.ts
import express, {
  Application,
  Request,
  Response,
  NextFunction,
  RequestHandler,
} from "express";

// Custom type extensions
declare global {
  namespace Express {
    interface Request {
      userId?: string;
      startTime?: number;
    }
  }
}

// DTOs and interfaces
interface CreateUserDto {
  name: string;
  email: string;
  password: string;
}

interface UserResponse {
  id: string;
  name: string;
  email: string;
  createdAt: string;
}

interface ApiResponse<T> {
  success: boolean;
  data?: T;
  error?: string;
  meta?: {
    page?: number;
    total?: number;
    timestamp: string;
  };
}

// Typed request helpers
type TypedRequest<
  TBody = unknown,
  TParams = Record<string, string>,
  TQuery = Record<string, string>
> = Request<TParams, unknown, TBody, TQuery>;

type TypedResponse<T> = Response<ApiResponse<T>>;

// Middleware types
const loggerMiddleware: RequestHandler = (req, res, next) => {
  req.startTime = Date.now();
  res.on("finish", () => {
    const duration = Date.now() - (req.startTime ?? 0);
    console.log(`${req.method} ${req.path} - ${res.statusCode} (${duration}ms)`);
  });
  next();
};

const authMiddleware: RequestHandler = (req, res, next) => {
  const token = req.headers.authorization?.replace("Bearer ", "");

  if (!token) {
    res.status(401).json({
      success: false,
      error: "Authentication required",
      meta: { timestamp: new Date().toISOString() },
    });
    return;
  }

  // Verify token (simplified)
  req.userId = "user-123"; // Would come from token verification
  next();
};

// Error handler
interface AppError extends Error {
  statusCode?: number;
  code?: string;
}

const errorHandler = (
  err: AppError,
  req: Request,
  res: Response,
  next: NextFunction
) => {
  console.error("Error:", err);

  const statusCode = err.statusCode ?? 500;
  const message = err.message || "Internal Server Error";

  res.status(statusCode).json({
    success: false,
    error: message,
    meta: { timestamp: new Date().toISOString() },
  });
};

// Controller functions
const getUsers: RequestHandler = async (req, res, next) => {
  try {
    const users: UserResponse[] = [
      { id: "1", name: "Alice", email: "alice@example.com", createdAt: new Date().toISOString() },
      { id: "2", name: "Bob", email: "bob@example.com", createdAt: new Date().toISOString() },
    ];

    res.json({
      success: true,
      data: users,
      meta: { total: users.length, timestamp: new Date().toISOString() },
    });
  } catch (error) {
    next(error);
  }
};

const createUser = async (
  req: TypedRequest<CreateUserDto>,
  res: TypedResponse<UserResponse>,
  next: NextFunction
) => {
  try {
    const { name, email, password } = req.body;

    // Validation
    if (!name || !email || !password) {
      res.status(400).json({
        success: false,
        error: "Name, email, and password are required",
        meta: { timestamp: new Date().toISOString() },
      });
      return;
    }

    const newUser: UserResponse = {
      id: crypto.randomUUID(),
      name,
      email,
      createdAt: new Date().toISOString(),
    };

    res.status(201).json({
      success: true,
      data: newUser,
      meta: { timestamp: new Date().toISOString() },
    });
  } catch (error) {
    next(error);
  }
};

// Application setup
const app: Application = express();

app.use(express.json());
app.use(loggerMiddleware);

// Routes
app.get("/users", getUsers);
app.post("/users", createUser);
app.get("/protected", authMiddleware, (req, res) => {
  res.json({
    success: true,
    data: { userId: req.userId },
    meta: { timestamp: new Date().toISOString() },
  });
});

// Error handling
app.use(errorHandler);

export { app };
```

### Router Modular

```typescript
// src/routes/users.router.ts
import { Router } from "express";
import { UserController } from "../controllers/user.controller";
import { validateBody } from "../middleware/validation";
import { createUserSchema, updateUserSchema } from "../schemas/user.schema";

const router = Router();
const controller = new UserController();

router.get("/", controller.getAll);
router.get("/:id", controller.getById);
router.post("/", validateBody(createUserSchema), controller.create);
router.put("/:id", validateBody(updateUserSchema), controller.update);
router.delete("/:id", controller.delete);

export { router as usersRouter };

// src/controllers/user.controller.ts
import { Request, Response, NextFunction } from "express";
import { UserService } from "../services/user.service";

export class UserController {
  private userService = new UserService();

  getAll = async (req: Request, res: Response, next: NextFunction) => {
    try {
      const users = await this.userService.findAll();
      res.json({ success: true, data: users });
    } catch (error) {
      next(error);
    }
  };

  getById = async (req: Request, res: Response, next: NextFunction) => {
    try {
      const user = await this.userService.findById(req.params.id);
      if (!user) {
        res.status(404).json({ success: false, error: "User not found" });
        return;
      }
      res.json({ success: true, data: user });
    } catch (error) {
      next(error);
    }
  };

  create = async (req: Request, res: Response, next: NextFunction) => {
    try {
      const user = await this.userService.create(req.body);
      res.status(201).json({ success: true, data: user });
    } catch (error) {
      next(error);
    }
  };

  update = async (req: Request, res: Response, next: NextFunction) => {
    try {
      const user = await this.userService.update(req.params.id, req.body);
      res.json({ success: true, data: user });
    } catch (error) {
      next(error);
    }
  };

  delete = async (req: Request, res: Response, next: NextFunction) => {
    try {
      await this.userService.delete(req.params.id);
      res.status(204).send();
    } catch (error) {
      next(error);
    }
  };
}
```

---

## Fastify con TypeScript

### Configuración de Fastify

```bash
npm install fastify @fastify/type-provider-typebox @sinclair/typebox
npm install @fastify/cors @fastify/helmet @fastify/swagger @fastify/swagger-ui
```

### Aplicación Fastify con TypeBox

```typescript
// src/server.ts
import Fastify, { FastifyInstance, FastifyRequest, FastifyReply } from "fastify";
import { TypeBoxTypeProvider } from "@fastify/type-provider-typebox";
import { Type, Static } from "@sinclair/typebox";
import cors from "@fastify/cors";
import helmet from "@fastify/helmet";
import swagger from "@fastify/swagger";
import swaggerUi from "@fastify/swagger-ui";

// Schema definitions with TypeBox
const UserSchema = Type.Object({
  id: Type.String({ format: "uuid" }),
  name: Type.String({ minLength: 1, maxLength: 100 }),
  email: Type.String({ format: "email" }),
  createdAt: Type.String({ format: "date-time" }),
});

const CreateUserSchema = Type.Object({
  name: Type.String({ minLength: 1, maxLength: 100 }),
  email: Type.String({ format: "email" }),
  password: Type.String({ minLength: 8 }),
});

const UpdateUserSchema = Type.Partial(
  Type.Object({
    name: Type.String({ minLength: 1, maxLength: 100 }),
    email: Type.String({ format: "email" }),
  })
);

const UserParamsSchema = Type.Object({
  id: Type.String({ format: "uuid" }),
});

const PaginationQuerySchema = Type.Object({
  page: Type.Optional(Type.Integer({ minimum: 1, default: 1 })),
  limit: Type.Optional(Type.Integer({ minimum: 1, maximum: 100, default: 20 })),
});

const ApiResponseSchema = <T extends Static<typeof Type.Any>>(dataSchema: T) =>
  Type.Object({
    success: Type.Boolean(),
    data: Type.Optional(dataSchema),
    error: Type.Optional(Type.String()),
    meta: Type.Optional(
      Type.Object({
        page: Type.Optional(Type.Integer()),
        total: Type.Optional(Type.Integer()),
        timestamp: Type.String(),
      })
    ),
  });

// Type inference from schemas
type User = Static<typeof UserSchema>;
type CreateUserDto = Static<typeof CreateUserSchema>;
type UpdateUserDto = Static<typeof UpdateUserSchema>;
type UserParams = Static<typeof UserParamsSchema>;
type PaginationQuery = Static<typeof PaginationQuerySchema>;

// Build server
async function buildServer(): Promise<FastifyInstance> {
  const server = Fastify({
    logger: {
      level: "info",
      transport: {
        target: "pino-pretty",
        options: { colorize: true },
      },
    },
  }).withTypeProvider<TypeBoxTypeProvider>();

  // Register plugins
  await server.register(cors, {
    origin: true,
    credentials: true,
  });

  await server.register(helmet, {
    contentSecurityPolicy: false,
  });

  await server.register(swagger, {
    openapi: {
      info: {
        title: "TypeScript API",
        description: "API documentation",
        version: "1.0.0",
      },
      servers: [{ url: "http://localhost:3000" }],
    },
  });

  await server.register(swaggerUi, {
    routePrefix: "/docs",
  });

  // In-memory store (replace with database)
  const users: User[] = [];

  // Routes with full type safety
  server.get(
    "/users",
    {
      schema: {
        querystring: PaginationQuerySchema,
        response: {
          200: ApiResponseSchema(Type.Array(UserSchema)),
        },
      },
    },
    async (request, reply) => {
      const { page = 1, limit = 20 } = request.query;
      const start = (page - 1) * limit;
      const paginatedUsers = users.slice(start, start + limit);

      return {
        success: true,
        data: paginatedUsers,
        meta: {
          page,
          total: users.length,
          timestamp: new Date().toISOString(),
        },
      };
    }
  );

  server.get(
    "/users/:id",
    {
      schema: {
        params: UserParamsSchema,
        response: {
          200: ApiResponseSchema(UserSchema),
          404: ApiResponseSchema(Type.Null()),
        },
      },
    },
    async (request, reply) => {
      const user = users.find((u) => u.id === request.params.id);

      if (!user) {
        reply.code(404);
        return {
          success: false,
          error: "User not found",
          meta: { timestamp: new Date().toISOString() },
        };
      }

      return {
        success: true,
        data: user,
        meta: { timestamp: new Date().toISOString() },
      };
    }
  );

  server.post(
    "/users",
    {
      schema: {
        body: CreateUserSchema,
        response: {
          201: ApiResponseSchema(UserSchema),
        },
      },
    },
    async (request, reply) => {
      const { name, email } = request.body;

      const newUser: User = {
        id: crypto.randomUUID(),
        name,
        email,
        createdAt: new Date().toISOString(),
      };

      users.push(newUser);

      reply.code(201);
      return {
        success: true,
        data: newUser,
        meta: { timestamp: new Date().toISOString() },
      };
    }
  );

  server.put(
    "/users/:id",
    {
      schema: {
        params: UserParamsSchema,
        body: UpdateUserSchema,
        response: {
          200: ApiResponseSchema(UserSchema),
          404: ApiResponseSchema(Type.Null()),
        },
      },
    },
    async (request, reply) => {
      const index = users.findIndex((u) => u.id === request.params.id);

      if (index === -1) {
        reply.code(404);
        return {
          success: false,
          error: "User not found",
          meta: { timestamp: new Date().toISOString() },
        };
      }

      users[index] = { ...users[index], ...request.body };

      return {
        success: true,
        data: users[index],
        meta: { timestamp: new Date().toISOString() },
      };
    }
  );

  server.delete(
    "/users/:id",
    {
      schema: {
        params: UserParamsSchema,
        response: {
          204: Type.Null(),
          404: ApiResponseSchema(Type.Null()),
        },
      },
    },
    async (request, reply) => {
      const index = users.findIndex((u) => u.id === request.params.id);

      if (index === -1) {
        reply.code(404);
        return {
          success: false,
          error: "User not found",
          meta: { timestamp: new Date().toISOString() },
        };
      }

      users.splice(index, 1);
      reply.code(204);
    }
  );

  return server;
}

// Start server
const start = async () => {
  const server = await buildServer();

  try {
    await server.listen({ port: 3000, host: "0.0.0.0" });
  } catch (err) {
    server.log.error(err);
    process.exit(1);
  }
};

start();
```

---

## Acceso a Bases de Datos

### Prisma con TypeScript

```bash
npm install prisma @prisma/client
npx prisma init
```

```prisma
// prisma/schema.prisma
generator client {
  provider = "prisma-client-js"
}

datasource db {
  provider = "postgresql"
  url      = env("DATABASE_URL")
}

model User {
  id        String   @id @default(uuid())
  email     String   @unique
  name      String?
  password  String
  posts     Post[]
  createdAt DateTime @default(now())
  updatedAt DateTime @updatedAt
}

model Post {
  id        String   @id @default(uuid())
  title     String
  content   String?
  published Boolean  @default(false)
  author    User     @relation(fields: [authorId], references: [id])
  authorId  String
  createdAt DateTime @default(now())
  updatedAt DateTime @updatedAt
}
```

```typescript
// src/database/prisma.ts
import { PrismaClient } from "@prisma/client";

declare global {
  var prisma: PrismaClient | undefined;
}

export const prisma =
  global.prisma ||
  new PrismaClient({
    log: ["query", "info", "warn", "error"],
  });

if (process.env.NODE_ENV !== "production") {
  global.prisma = prisma;
}

// src/repositories/user.repository.ts
import { prisma } from "../database/prisma";
import { Prisma, User } from "@prisma/client";

export class UserRepository {
  async findAll(params?: {
    skip?: number;
    take?: number;
    where?: Prisma.UserWhereInput;
    orderBy?: Prisma.UserOrderByWithRelationInput;
  }): Promise<User[]> {
    return prisma.user.findMany(params);
  }

  async findById(id: string): Promise<User | null> {
    return prisma.user.findUnique({
      where: { id },
      include: { posts: true },
    });
  }

  async findByEmail(email: string): Promise<User | null> {
    return prisma.user.findUnique({ where: { email } });
  }

  async create(data: Prisma.UserCreateInput): Promise<User> {
    return prisma.user.create({ data });
  }

  async update(id: string, data: Prisma.UserUpdateInput): Promise<User> {
    return prisma.user.update({
      where: { id },
      data,
    });
  }

  async delete(id: string): Promise<User> {
    return prisma.user.delete({ where: { id } });
  }

  async count(where?: Prisma.UserWhereInput): Promise<number> {
    return prisma.user.count({ where });
  }
}
```

### Drizzle ORM

```typescript
// src/database/drizzle.ts
import { drizzle } from "drizzle-orm/node-postgres";
import { pgTable, uuid, varchar, timestamp, boolean, text } from "drizzle-orm/pg-core";
import { eq, and, or, desc, asc } from "drizzle-orm";
import { Pool } from "pg";

// Schema definition
export const users = pgTable("users", {
  id: uuid("id").primaryKey().defaultRandom(),
  email: varchar("email", { length: 255 }).notNull().unique(),
  name: varchar("name", { length: 100 }),
  password: varchar("password", { length: 255 }).notNull(),
  createdAt: timestamp("created_at").defaultNow().notNull(),
  updatedAt: timestamp("updated_at").defaultNow().notNull(),
});

export const posts = pgTable("posts", {
  id: uuid("id").primaryKey().defaultRandom(),
  title: varchar("title", { length: 255 }).notNull(),
  content: text("content"),
  published: boolean("published").default(false).notNull(),
  authorId: uuid("author_id").notNull().references(() => users.id),
  createdAt: timestamp("created_at").defaultNow().notNull(),
  updatedAt: timestamp("updated_at").defaultNow().notNull(),
});

// Type inference
export type User = typeof users.$inferSelect;
export type NewUser = typeof users.$inferInsert;
export type Post = typeof posts.$inferSelect;
export type NewPost = typeof posts.$inferInsert;

// Database connection
const pool = new Pool({
  connectionString: process.env.DATABASE_URL,
});

export const db = drizzle(pool);

// Repository pattern with Drizzle
export class DrizzleUserRepository {
  async findAll(): Promise<User[]> {
    return db.select().from(users).orderBy(desc(users.createdAt));
  }

  async findById(id: string): Promise<User | undefined> {
    const result = await db.select().from(users).where(eq(users.id, id)).limit(1);
    return result[0];
  }

  async create(data: NewUser): Promise<User> {
    const result = await db.insert(users).values(data).returning();
    return result[0];
  }

  async update(id: string, data: Partial<NewUser>): Promise<User> {
    const result = await db
      .update(users)
      .set({ ...data, updatedAt: new Date() })
      .where(eq(users.id, id))
      .returning();
    return result[0];
  }

  async delete(id: string): Promise<void> {
    await db.delete(users).where(eq(users.id, id));
  }
}
```

---

## FFI con node-ffi-napi

### Llamando Bibliotecas C desde Node.js

```bash
npm install ffi-napi ref-napi ref-struct-napi ref-array-napi
npm install @types/ffi-napi @types/ref-napi -D
```

```typescript
// src/ffi/native-lib.ts
import ffi from "ffi-napi";
import ref from "ref-napi";
import StructType from "ref-struct-napi";
import ArrayType from "ref-array-napi";

// Define C types
const int = ref.types.int;
const double = ref.types.double;
const CString = ref.types.CString;
const voidPtr = ref.refType(ref.types.void);

// Define a C struct
const Point = StructType({
  x: double,
  y: double,
});

const PointPtr = ref.refType(Point);

const Rectangle = StructType({
  topLeft: Point,
  bottomRight: Point,
});

// Define array types
const IntArray = ArrayType(int);
const DoubleArray = ArrayType(double);

// Define the library interface
interface MathLib {
  add: (a: number, b: number) => number;
  multiply: (a: number, b: number) => number;
  calculate_distance: (p1: typeof Point, p2: typeof Point) => number;
  sort_array: (arr: typeof IntArray, length: number) => void;
  process_string: (input: string) => string;
}

// Load the native library
const libPath =
  process.platform === "win32"
    ? "./libs/mathlib.dll"
    : process.platform === "darwin"
    ? "./libs/libmath.dylib"
    : "./libs/libmath.so";

const mathLib = ffi.Library(libPath, {
  add: [int, [int, int]],
  multiply: [int, [int, int]],
  calculate_distance: [double, [PointPtr, PointPtr]],
  sort_array: ["void", [ref.refType(int), int]],
  process_string: [CString, [CString]],
}) as unknown as MathLib;

// TypeScript wrapper for type safety
export class NativeMath {
  static add(a: number, b: number): number {
    return mathLib.add(a, b);
  }

  static multiply(a: number, b: number): number {
    return mathLib.multiply(a, b);
  }

  static calculateDistance(
    p1: { x: number; y: number },
    p2: { x: number; y: number }
  ): number {
    const point1 = new Point({ x: p1.x, y: p1.y });
    const point2 = new Point({ x: p2.x, y: p2.y });
    return mathLib.calculate_distance(point1.ref(), point2.ref());
  }

  static sortArray(arr: number[]): number[] {
    const nativeArray = new IntArray(arr);
    mathLib.sort_array(nativeArray.buffer, arr.length);
    return Array.from(nativeArray);
  }

  static processString(input: string): string {
    return mathLib.process_string(input);
  }
}

// Example C library source (mathlib.c)
/*
#include <math.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    double x;
    double y;
} Point;

int add(int a, int b) {
    return a + b;
}

int multiply(int a, int b) {
    return a * b;
}

double calculate_distance(Point* p1, Point* p2) {
    double dx = p2->x - p1->x;
    double dy = p2->y - p1->y;
    return sqrt(dx*dx + dy*dy);
}

void sort_array(int* arr, int length) {
    for (int i = 0; i < length - 1; i++) {
        for (int j = 0; j < length - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                int temp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
            }
        }
    }
}

char* process_string(const char* input) {
    size_t len = strlen(input);
    char* result = malloc(len + 1);
    for (size_t i = 0; i < len; i++) {
        result[i] = toupper(input[i]);
    }
    result[len] = '\0';
    return result;
}
*/

// Usage example
async function demonstrateFFI() {
  console.log("Addition:", NativeMath.add(5, 3));
  console.log("Multiplication:", NativeMath.multiply(4, 7));

  const distance = NativeMath.calculateDistance(
    { x: 0, y: 0 },
    { x: 3, y: 4 }
  );
  console.log("Distance:", distance); // Should be 5

  const sorted = NativeMath.sortArray([5, 2, 8, 1, 9, 3]);
  console.log("Sorted array:", sorted);

  const processed = NativeMath.processString("hello world");
  console.log("Processed string:", processed);
}
```

### Manejo de Callbacks con FFI

```typescript
// src/ffi/callbacks.ts
import ffi from "ffi-napi";
import ref from "ref-napi";

// Define callback type
const CallbackType = ffi.Function("void", ["int", "int"]);

interface AsyncLib {
  register_callback: (callback: Buffer) => void;
  trigger_callback: (a: number, b: number) => void;
  async_operation: (callback: Buffer, data: number) => void;
}

const asyncLib = ffi.Library("./libs/asynclib", {
  register_callback: ["void", [CallbackType]],
  trigger_callback: ["void", ["int", "int"]],
  async_operation: ["void", [ffi.Function("void", ["int"]), "int"]],
}) as unknown as AsyncLib;

// Create TypeScript callback wrapper
export class NativeAsync {
  private static callbacks: Map<string, ffi.Callback> = new Map();

  static registerCallback(
    name: string,
    handler: (a: number, b: number) => void
  ): void {
    // Create native callback
    const callback = ffi.Callback("void", ["int", "int"], handler);

    // Store reference to prevent garbage collection
    this.callbacks.set(name, callback);

    // Register with native library
    asyncLib.register_callback(callback);
  }

  static triggerCallback(a: number, b: number): void {
    asyncLib.trigger_callback(a, b);
  }

  static asyncOperation(data: number): Promise<number> {
    return new Promise((resolve) => {
      const callback = ffi.Callback("void", ["int"], (result: number) => {
        resolve(result);
      });

      // Keep reference during async operation
      process.nextTick(() => {});

      asyncLib.async_operation(callback, data);
    });
  }
}
```

---

## N-API Native Addons

### Estructura del Proyecto

```
my-native-addon/
├── binding.gyp
├── package.json
├── src/
│   ├── addon.cc
│   └── math_operations.cc
├── lib/
│   └── index.ts
└── tsconfig.json
```

### binding.gyp

```python
{
  "targets": [
    {
      "target_name": "native_addon",
      "sources": [
        "src/addon.cc",
        "src/math_operations.cc"
      ],
      "include_dirs": [
        "<!@(node -p \"require('node-addon-api').include\")"
      ],
      "defines": ["NAPI_DISABLE_CPP_EXCEPTIONS"],
      "cflags!": ["-fno-exceptions"],
      "cflags_cc!": ["-fno-exceptions"],
      "xcode_settings": {
        "GCC_ENABLE_CPP_EXCEPTIONS": "YES",
        "CLANG_CXX_LIBRARY": "libc++",
        "MACOSX_DEPLOYMENT_TARGET": "10.7"
      },
      "msvs_settings": {
        "VCCLCompilerTool": {
          "ExceptionHandling": 1
        }
      }
    }
  ]
}
```

### Código C++ del Addon

```cpp
// src/addon.cc
#include <napi.h>
#include "math_operations.h"

// Simple synchronous function
Napi::Number Add(const Napi::CallbackInfo& info) {
    Napi::Env env = info.Env();

    if (info.Length() < 2) {
        Napi::TypeError::New(env, "Expected 2 arguments")
            .ThrowAsJavaScriptException();
        return Napi::Number::New(env, 0);
    }

    if (!info[0].IsNumber() || !info[1].IsNumber()) {
        Napi::TypeError::New(env, "Arguments must be numbers")
            .ThrowAsJavaScriptException();
        return Napi::Number::New(env, 0);
    }

    double a = info[0].As<Napi::Number>().DoubleValue();
    double b = info[1].As<Napi::Number>().DoubleValue();

    return Napi::Number::New(env, a + b);
}

// Async worker for heavy computation
class ComputeAsyncWorker : public Napi::AsyncWorker {
public:
    ComputeAsyncWorker(Napi::Function& callback, int value)
        : Napi::AsyncWorker(callback), value_(value), result_(0) {}

    void Execute() override {
        // Heavy computation in background thread
        result_ = fibonacci(value_);
    }

    void OnOK() override {
        Napi::HandleScope scope(Env());
        Callback().Call({Env().Null(), Napi::Number::New(Env(), result_)});
    }

private:
    int value_;
    long long result_;

    long long fibonacci(int n) {
        if (n <= 1) return n;
        long long a = 0, b = 1;
        for (int i = 2; i <= n; i++) {
            long long c = a + b;
            a = b;
            b = c;
        }
        return b;
    }
};

Napi::Value ComputeAsync(const Napi::CallbackInfo& info) {
    Napi::Env env = info.Env();

    int value = info[0].As<Napi::Number>().Int32Value();
    Napi::Function callback = info[1].As<Napi::Function>();

    ComputeAsyncWorker* worker = new ComputeAsyncWorker(callback, value);
    worker->Queue();

    return env.Undefined();
}

// Promise-based async
Napi::Value ComputePromise(const Napi::CallbackInfo& info) {
    Napi::Env env = info.Env();

    int value = info[0].As<Napi::Number>().Int32Value();

    auto deferred = Napi::Promise::Deferred::New(env);

    // Create async worker with promise
    class PromiseWorker : public Napi::AsyncWorker {
    public:
        PromiseWorker(Napi::Env& env, Napi::Promise::Deferred deferred, int value)
            : Napi::AsyncWorker(env), deferred_(deferred), value_(value) {}

        void Execute() override {
            result_ = compute(value_);
        }

        void OnOK() override {
            deferred_.Resolve(Napi::Number::New(Env(), result_));
        }

        void OnError(const Napi::Error& error) override {
            deferred_.Reject(error.Value());
        }

    private:
        Napi::Promise::Deferred deferred_;
        int value_;
        double result_;

        double compute(int n) {
            double sum = 0;
            for (int i = 0; i < n * 1000000; i++) {
                sum += i * 0.001;
            }
            return sum;
        }
    };

    PromiseWorker* worker = new PromiseWorker(env, deferred, value);
    worker->Queue();

    return deferred.Promise();
}

// Object wrapping
class MathClass : public Napi::ObjectWrap<MathClass> {
public:
    static Napi::Object Init(Napi::Env env, Napi::Object exports) {
        Napi::Function func = DefineClass(env, "MathClass", {
            InstanceMethod("add", &MathClass::Add),
            InstanceMethod("multiply", &MathClass::Multiply),
            InstanceAccessor("value", &MathClass::GetValue, &MathClass::SetValue),
        });

        exports.Set("MathClass", func);
        return exports;
    }

    MathClass(const Napi::CallbackInfo& info)
        : Napi::ObjectWrap<MathClass>(info), value_(0) {
        if (info.Length() > 0 && info[0].IsNumber()) {
            value_ = info[0].As<Napi::Number>().DoubleValue();
        }
    }

private:
    double value_;

    Napi::Value Add(const Napi::CallbackInfo& info) {
        double arg = info[0].As<Napi::Number>().DoubleValue();
        return Napi::Number::New(info.Env(), value_ + arg);
    }

    Napi::Value Multiply(const Napi::CallbackInfo& info) {
        double arg = info[0].As<Napi::Number>().DoubleValue();
        return Napi::Number::New(info.Env(), value_ * arg);
    }

    Napi::Value GetValue(const Napi::CallbackInfo& info) {
        return Napi::Number::New(info.Env(), value_);
    }

    void SetValue(const Napi::CallbackInfo& info, const Napi::Value& value) {
        value_ = value.As<Napi::Number>().DoubleValue();
    }
};

// Module initialization
Napi::Object Init(Napi::Env env, Napi::Object exports) {
    exports.Set("add", Napi::Function::New(env, Add));
    exports.Set("computeAsync", Napi::Function::New(env, ComputeAsync));
    exports.Set("computePromise", Napi::Function::New(env, ComputePromise));
    MathClass::Init(env, exports);
    return exports;
}

NODE_API_MODULE(native_addon, Init)
```

### TypeScript Bindings

```typescript
// lib/index.ts
import path from "node:path";
import { createRequire } from "node:module";

const require = createRequire(import.meta.url);

interface NativeAddon {
  add(a: number, b: number): number;
  computeAsync(value: number, callback: (err: Error | null, result: number) => void): void;
  computePromise(value: number): Promise<number>;
  MathClass: new (initialValue?: number) => MathClassInstance;
}

interface MathClassInstance {
  add(value: number): number;
  multiply(value: number): number;
  value: number;
}

// Load native addon
const addonPath = path.join(__dirname, "../build/Release/native_addon.node");
const addon: NativeAddon = require(addonPath);

// Export with type safety
export function add(a: number, b: number): number {
  return addon.add(a, b);
}

export function computeAsync(value: number): Promise<number> {
  return new Promise((resolve, reject) => {
    addon.computeAsync(value, (err, result) => {
      if (err) reject(err);
      else resolve(result);
    });
  });
}

export function computePromise(value: number): Promise<number> {
  return addon.computePromise(value);
}

export class MathHelper {
  private instance: MathClassInstance;

  constructor(initialValue: number = 0) {
    this.instance = new addon.MathClass(initialValue);
  }

  add(value: number): number {
    return this.instance.add(value);
  }

  multiply(value: number): number {
    return this.instance.multiply(value);
  }

  get value(): number {
    return this.instance.value;
  }

  set value(v: number) {
    this.instance.value = v;
  }
}

// Usage example
async function main() {
  // Synchronous function
  console.log("add(5, 3):", add(5, 3));

  // Async callback style
  const asyncResult = await computeAsync(40);
  console.log("computeAsync(40):", asyncResult);

  // Promise style
  const promiseResult = await computePromise(10);
  console.log("computePromise(10):", promiseResult);

  // Class wrapper
  const math = new MathHelper(100);
  console.log("math.add(50):", math.add(50));
  console.log("math.multiply(2):", math.multiply(2));

  math.value = 200;
  console.log("math.value:", math.value);
}

main().catch(console.error);
```

---

## Patrones de Integración

### Tabla Comparativa: FFI vs N-API

| Característica     | node-ffi-napi          | N-API Native Addons  |
| ------------------ | ---------------------- | -------------------- |
| Rendimiento        | Medio                  | Alto                 |
| Facilidad de uso   | Alta                   | Media                |
| Compilación        | No requiere            | Requiere C++ toolchain |
| Mantenimiento      | Bajo                   | Medio                |
| Portabilidad       | Buena                  | Excelente            |
| Acceso a Node APIs | Limitado               | Completo             |
| Async/Workers      | Manual                 | Integrado            |
| Tipo de uso        | Bibliotecas existentes | Código optimizado    |

### Cuándo Usar Cada Enfoque

```typescript
// Use FFI when:
// - Integrating existing C libraries
// - Quick prototyping
// - No C++ knowledge required

// Use N-API when:
// - Performance is critical
// - Need full Node.js API access
// - Building reusable native modules
// - Complex async operations

// Example decision function
type IntegrationMethod = "ffi" | "napi" | "wasm" | "pure-js";

interface IntegrationRequirements {
  performanceCritical: boolean;
  existingCLibrary: boolean;
  asyncOperations: boolean;
  crossPlatform: boolean;
  buildToolchainAvailable: boolean;
}

function chooseIntegrationMethod(reqs: IntegrationRequirements): IntegrationMethod {
  if (reqs.existingCLibrary && !reqs.performanceCritical) {
    return "ffi";
  }

  if (reqs.performanceCritical && reqs.buildToolchainAvailable) {
    return "napi";
  }

  if (reqs.crossPlatform && !reqs.buildToolchainAvailable) {
    return "wasm";
  }

  return "pure-js";
}
```

---

## Referencias y Recursos

### Documentación Oficial

- [Node.js Documentation](https://nodejs.org/docs/)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/)
- [N-API Documentation](https://nodejs.org/api/n-api.html)
- [node-addon-api](https://github.com/nodejs/node-addon-api)

### Herramientas de Desarrollo

| Herramienta       | Propósito                          |
| ----------------- | ---------------------------------- |
| `node-gyp`        | Compilación de native addons       |
| `prebuild`        | Pre-built binaries distribution    |
| `node-pre-gyp`    | Binary deployment                  |
| `ffi-napi`        | Foreign Function Interface         |
| `ref-napi`        | C type references                  |

---

## Notas de ARCHAEON

> Este documento cubre la integración de TypeScript con código nativo,
> representando el puente entre JavaScript moderno y bibliotecas legacy
> escritas en C/C++. El FFI y N-API permiten reutilizar código existente
> mientras se mantiene la seguridad de tipos de TypeScript.

**Próximo documento:** [TYPESCRIPT_03_WASM.md](./TYPESCRIPT_03_WASM.md) - WebAssembly y TypeScript
