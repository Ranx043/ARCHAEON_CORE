---
título: "TypeScript 04 - Patrones Full Stack y Arquitectura"
módulo: ARCHAEON_CORE
sección: 80000_MODERNOS/TYPESCRIPT
versión: 1.0.0
fecha_creación: 2025-12-31
autor: ARCHAEON Sistema de Documentación
descripción: >
  Patrones arquitectónicos full stack con TypeScript, diseño de APIs,
  validación de datos, manejo de errores, testing y deployment.
tags:
  - typescript
  - fullstack
  - api-design
  - validation
  - testing
  - deployment
requisitos:
  - Node.js 18+
  - TypeScript 5.0+
  - Framework web (Express/Fastify/Hono)
  - Testing framework (Vitest/Jest)
---

# TYPESCRIPT 04 - PATRONES FULL STACK Y ARQUITECTURA

## Tabla de Contenidos

1. [Arquitectura Full Stack](#arquitectura-full-stack)
2. [Diseño de APIs](#diseño-de-apis)
3. [Validación de Datos](#validación-de-datos)
4. [Manejo de Errores](#manejo-de-errores)
5. [Estrategias de Testing](#estrategias-de-testing)
6. [Patrones de Deployment](#patrones-de-deployment)
7. [Monorepos y Compartición de Código](#monorepos-y-compartición-de-código)

---

## Arquitectura Full Stack

### Estructura de Proyecto Recomendada

```
my-fullstack-app/
├── packages/
│   ├── shared/              # Shared types and utilities
│   │   ├── src/
│   │   │   ├── types/       # Shared type definitions
│   │   │   ├── schemas/     # Validation schemas
│   │   │   ├── utils/       # Shared utilities
│   │   │   └── index.ts
│   │   ├── package.json
│   │   └── tsconfig.json
│   │
│   ├── api/                 # Backend API
│   │   ├── src/
│   │   │   ├── controllers/
│   │   │   ├── services/
│   │   │   ├── repositories/
│   │   │   ├── middleware/
│   │   │   ├── routes/
│   │   │   └── index.ts
│   │   ├── package.json
│   │   └── tsconfig.json
│   │
│   └── web/                 # Frontend application
│       ├── src/
│       │   ├── components/
│       │   ├── pages/
│       │   ├── hooks/
│       │   ├── services/
│       │   └── App.tsx
│       ├── package.json
│       └── tsconfig.json
│
├── package.json             # Root package.json
├── pnpm-workspace.yaml      # Workspace configuration
└── tsconfig.base.json       # Base TypeScript config
```

### Tipos Compartidos

```typescript
// packages/shared/src/types/api.ts
export interface ApiResponse<T> {
  success: boolean;
  data?: T;
  error?: ApiError;
  meta?: ResponseMeta;
}

export interface ApiError {
  code: string;
  message: string;
  details?: Record<string, unknown>;
  stack?: string;
}

export interface ResponseMeta {
  timestamp: string;
  requestId: string;
  pagination?: PaginationMeta;
}

export interface PaginationMeta {
  page: number;
  pageSize: number;
  totalItems: number;
  totalPages: number;
  hasNextPage: boolean;
  hasPreviousPage: boolean;
}

// packages/shared/src/types/entities.ts
export interface User {
  id: string;
  email: string;
  name: string;
  role: UserRole;
  createdAt: Date;
  updatedAt: Date;
}

export type UserRole = "admin" | "user" | "guest";

export interface CreateUserDto {
  email: string;
  name: string;
  password: string;
  role?: UserRole;
}

export interface UpdateUserDto {
  email?: string;
  name?: string;
  role?: UserRole;
}

export interface LoginDto {
  email: string;
  password: string;
}

export interface AuthResponse {
  user: Omit<User, "createdAt" | "updatedAt">;
  accessToken: string;
  refreshToken: string;
  expiresIn: number;
}

// packages/shared/src/types/product.ts
export interface Product {
  id: string;
  name: string;
  description: string;
  price: number;
  category: string;
  stock: number;
  images: string[];
  createdAt: Date;
  updatedAt: Date;
}

export interface CreateProductDto {
  name: string;
  description: string;
  price: number;
  category: string;
  stock: number;
  images?: string[];
}

export interface ProductFilters {
  category?: string;
  minPrice?: number;
  maxPrice?: number;
  inStock?: boolean;
  search?: string;
}

// packages/shared/src/types/order.ts
export interface Order {
  id: string;
  userId: string;
  items: OrderItem[];
  status: OrderStatus;
  total: number;
  shippingAddress: Address;
  createdAt: Date;
  updatedAt: Date;
}

export interface OrderItem {
  productId: string;
  productName: string;
  quantity: number;
  unitPrice: number;
  total: number;
}

export type OrderStatus =
  | "pending"
  | "confirmed"
  | "processing"
  | "shipped"
  | "delivered"
  | "cancelled";

export interface Address {
  street: string;
  city: string;
  state: string;
  postalCode: string;
  country: string;
}
```

### API Client Tipado

```typescript
// packages/shared/src/api-client.ts
import type {
  ApiResponse,
  User,
  CreateUserDto,
  UpdateUserDto,
  Product,
  ProductFilters,
  Order,
  AuthResponse,
  LoginDto,
  PaginationMeta,
} from "./types";

type HttpMethod = "GET" | "POST" | "PUT" | "PATCH" | "DELETE";

interface RequestOptions {
  headers?: Record<string, string>;
  signal?: AbortSignal;
}

interface PaginatedResponse<T> extends ApiResponse<T[]> {
  meta?: { pagination: PaginationMeta };
}

export class ApiClient {
  private baseUrl: string;
  private accessToken: string | null = null;

  constructor(baseUrl: string) {
    this.baseUrl = baseUrl;
  }

  setAccessToken(token: string | null): void {
    this.accessToken = token;
  }

  private async request<T>(
    method: HttpMethod,
    path: string,
    body?: unknown,
    options: RequestOptions = {}
  ): Promise<ApiResponse<T>> {
    const headers: Record<string, string> = {
      "Content-Type": "application/json",
      ...options.headers,
    };

    if (this.accessToken) {
      headers["Authorization"] = `Bearer ${this.accessToken}`;
    }

    const response = await fetch(`${this.baseUrl}${path}`, {
      method,
      headers,
      body: body ? JSON.stringify(body) : undefined,
      signal: options.signal,
    });

    const data: ApiResponse<T> = await response.json();

    if (!response.ok) {
      throw new ApiClientError(
        data.error?.message ?? "Request failed",
        data.error?.code ?? "UNKNOWN_ERROR",
        response.status
      );
    }

    return data;
  }

  // Auth endpoints
  async login(credentials: LoginDto): Promise<AuthResponse> {
    const response = await this.request<AuthResponse>(
      "POST",
      "/auth/login",
      credentials
    );
    if (response.data) {
      this.accessToken = response.data.accessToken;
    }
    return response.data!;
  }

  async logout(): Promise<void> {
    await this.request("POST", "/auth/logout");
    this.accessToken = null;
  }

  async refreshToken(refreshToken: string): Promise<AuthResponse> {
    const response = await this.request<AuthResponse>(
      "POST",
      "/auth/refresh",
      { refreshToken }
    );
    if (response.data) {
      this.accessToken = response.data.accessToken;
    }
    return response.data!;
  }

  // User endpoints
  async getUsers(page = 1, pageSize = 20): Promise<PaginatedResponse<User>> {
    return this.request<User[]>(
      "GET",
      `/users?page=${page}&pageSize=${pageSize}`
    );
  }

  async getUser(id: string): Promise<User> {
    const response = await this.request<User>("GET", `/users/${id}`);
    return response.data!;
  }

  async createUser(data: CreateUserDto): Promise<User> {
    const response = await this.request<User>("POST", "/users", data);
    return response.data!;
  }

  async updateUser(id: string, data: UpdateUserDto): Promise<User> {
    const response = await this.request<User>("PATCH", `/users/${id}`, data);
    return response.data!;
  }

  async deleteUser(id: string): Promise<void> {
    await this.request("DELETE", `/users/${id}`);
  }

  // Product endpoints
  async getProducts(
    filters?: ProductFilters,
    page = 1,
    pageSize = 20
  ): Promise<PaginatedResponse<Product>> {
    const params = new URLSearchParams({
      page: String(page),
      pageSize: String(pageSize),
    });

    if (filters) {
      Object.entries(filters).forEach(([key, value]) => {
        if (value !== undefined) {
          params.set(key, String(value));
        }
      });
    }

    return this.request<Product[]>("GET", `/products?${params}`);
  }

  async getProduct(id: string): Promise<Product> {
    const response = await this.request<Product>("GET", `/products/${id}`);
    return response.data!;
  }

  // Order endpoints
  async getOrders(page = 1, pageSize = 20): Promise<PaginatedResponse<Order>> {
    return this.request<Order[]>(
      "GET",
      `/orders?page=${page}&pageSize=${pageSize}`
    );
  }

  async getOrder(id: string): Promise<Order> {
    const response = await this.request<Order>("GET", `/orders/${id}`);
    return response.data!;
  }

  async createOrder(items: { productId: string; quantity: number }[]): Promise<Order> {
    const response = await this.request<Order>("POST", "/orders", { items });
    return response.data!;
  }
}

export class ApiClientError extends Error {
  constructor(
    message: string,
    public readonly code: string,
    public readonly statusCode: number
  ) {
    super(message);
    this.name = "ApiClientError";
  }
}
```

---

## Diseño de APIs

### RESTful API Patterns

```typescript
// packages/api/src/routes/users.ts
import { Router } from "express";
import { z } from "zod";
import { validateBody, validateParams, validateQuery } from "../middleware/validation";
import { UserController } from "../controllers/user.controller";
import { authMiddleware, requireRole } from "../middleware/auth";

const router = Router();
const controller = new UserController();

// Schema definitions
const CreateUserSchema = z.object({
  email: z.string().email(),
  name: z.string().min(2).max(100),
  password: z.string().min(8).max(72),
  role: z.enum(["admin", "user", "guest"]).optional().default("user"),
});

const UpdateUserSchema = z.object({
  email: z.string().email().optional(),
  name: z.string().min(2).max(100).optional(),
  role: z.enum(["admin", "user", "guest"]).optional(),
});

const UserIdSchema = z.object({
  id: z.string().uuid(),
});

const UserQuerySchema = z.object({
  page: z.coerce.number().int().positive().optional().default(1),
  pageSize: z.coerce.number().int().min(1).max(100).optional().default(20),
  search: z.string().optional(),
  role: z.enum(["admin", "user", "guest"]).optional(),
  sortBy: z.enum(["name", "email", "createdAt"]).optional().default("createdAt"),
  sortOrder: z.enum(["asc", "desc"]).optional().default("desc"),
});

// Routes
router.get(
  "/",
  authMiddleware,
  validateQuery(UserQuerySchema),
  controller.getAll
);

router.get(
  "/:id",
  authMiddleware,
  validateParams(UserIdSchema),
  controller.getById
);

router.post(
  "/",
  authMiddleware,
  requireRole(["admin"]),
  validateBody(CreateUserSchema),
  controller.create
);

router.patch(
  "/:id",
  authMiddleware,
  validateParams(UserIdSchema),
  validateBody(UpdateUserSchema),
  controller.update
);

router.delete(
  "/:id",
  authMiddleware,
  requireRole(["admin"]),
  validateParams(UserIdSchema),
  controller.delete
);

export { router as usersRouter };
```

### Controller Pattern

```typescript
// packages/api/src/controllers/user.controller.ts
import { Request, Response, NextFunction } from "express";
import { UserService } from "../services/user.service";
import { ApiResponse, User, CreateUserDto, UpdateUserDto } from "@myapp/shared";

export class UserController {
  private userService = new UserService();

  getAll = async (req: Request, res: Response, next: NextFunction) => {
    try {
      const { page, pageSize, search, role, sortBy, sortOrder } = req.query as {
        page: number;
        pageSize: number;
        search?: string;
        role?: string;
        sortBy: string;
        sortOrder: "asc" | "desc";
      };

      const result = await this.userService.findAll({
        page,
        pageSize,
        search,
        role,
        sortBy,
        sortOrder,
      });

      const response: ApiResponse<User[]> = {
        success: true,
        data: result.users,
        meta: {
          timestamp: new Date().toISOString(),
          requestId: req.id,
          pagination: {
            page,
            pageSize,
            totalItems: result.total,
            totalPages: Math.ceil(result.total / pageSize),
            hasNextPage: page * pageSize < result.total,
            hasPreviousPage: page > 1,
          },
        },
      };

      res.json(response);
    } catch (error) {
      next(error);
    }
  };

  getById = async (req: Request, res: Response, next: NextFunction) => {
    try {
      const { id } = req.params;
      const user = await this.userService.findById(id);

      if (!user) {
        res.status(404).json({
          success: false,
          error: {
            code: "USER_NOT_FOUND",
            message: `User with id ${id} not found`,
          },
          meta: {
            timestamp: new Date().toISOString(),
            requestId: req.id,
          },
        });
        return;
      }

      const response: ApiResponse<User> = {
        success: true,
        data: user,
        meta: {
          timestamp: new Date().toISOString(),
          requestId: req.id,
        },
      };

      res.json(response);
    } catch (error) {
      next(error);
    }
  };

  create = async (req: Request, res: Response, next: NextFunction) => {
    try {
      const data: CreateUserDto = req.body;
      const user = await this.userService.create(data);

      const response: ApiResponse<User> = {
        success: true,
        data: user,
        meta: {
          timestamp: new Date().toISOString(),
          requestId: req.id,
        },
      };

      res.status(201).json(response);
    } catch (error) {
      next(error);
    }
  };

  update = async (req: Request, res: Response, next: NextFunction) => {
    try {
      const { id } = req.params;
      const data: UpdateUserDto = req.body;
      const user = await this.userService.update(id, data);

      const response: ApiResponse<User> = {
        success: true,
        data: user,
        meta: {
          timestamp: new Date().toISOString(),
          requestId: req.id,
        },
      };

      res.json(response);
    } catch (error) {
      next(error);
    }
  };

  delete = async (req: Request, res: Response, next: NextFunction) => {
    try {
      const { id } = req.params;
      await this.userService.delete(id);

      res.status(204).send();
    } catch (error) {
      next(error);
    }
  };
}
```

### Service Pattern

```typescript
// packages/api/src/services/user.service.ts
import { CreateUserDto, UpdateUserDto, User } from "@myapp/shared";
import { UserRepository } from "../repositories/user.repository";
import { PasswordService } from "./password.service";
import { AppError } from "../utils/errors";

interface FindAllOptions {
  page: number;
  pageSize: number;
  search?: string;
  role?: string;
  sortBy: string;
  sortOrder: "asc" | "desc";
}

interface FindAllResult {
  users: User[];
  total: number;
}

export class UserService {
  private userRepository = new UserRepository();
  private passwordService = new PasswordService();

  async findAll(options: FindAllOptions): Promise<FindAllResult> {
    const { page, pageSize, search, role, sortBy, sortOrder } = options;
    const skip = (page - 1) * pageSize;

    const [users, total] = await Promise.all([
      this.userRepository.findMany({
        skip,
        take: pageSize,
        where: {
          ...(search && {
            OR: [
              { name: { contains: search, mode: "insensitive" } },
              { email: { contains: search, mode: "insensitive" } },
            ],
          }),
          ...(role && { role }),
        },
        orderBy: { [sortBy]: sortOrder },
      }),
      this.userRepository.count({
        where: {
          ...(search && {
            OR: [
              { name: { contains: search, mode: "insensitive" } },
              { email: { contains: search, mode: "insensitive" } },
            ],
          }),
          ...(role && { role }),
        },
      }),
    ]);

    return { users, total };
  }

  async findById(id: string): Promise<User | null> {
    return this.userRepository.findById(id);
  }

  async findByEmail(email: string): Promise<User | null> {
    return this.userRepository.findByEmail(email);
  }

  async create(data: CreateUserDto): Promise<User> {
    // Check if email already exists
    const existing = await this.userRepository.findByEmail(data.email);
    if (existing) {
      throw new AppError("EMAIL_EXISTS", "Email already in use", 409);
    }

    // Hash password
    const hashedPassword = await this.passwordService.hash(data.password);

    // Create user
    return this.userRepository.create({
      ...data,
      password: hashedPassword,
    });
  }

  async update(id: string, data: UpdateUserDto): Promise<User> {
    const user = await this.userRepository.findById(id);
    if (!user) {
      throw new AppError("USER_NOT_FOUND", `User with id ${id} not found`, 404);
    }

    // Check email uniqueness if email is being updated
    if (data.email && data.email !== user.email) {
      const existing = await this.userRepository.findByEmail(data.email);
      if (existing) {
        throw new AppError("EMAIL_EXISTS", "Email already in use", 409);
      }
    }

    return this.userRepository.update(id, data);
  }

  async delete(id: string): Promise<void> {
    const user = await this.userRepository.findById(id);
    if (!user) {
      throw new AppError("USER_NOT_FOUND", `User with id ${id} not found`, 404);
    }

    await this.userRepository.delete(id);
  }

  async verifyPassword(email: string, password: string): Promise<User | null> {
    const user = await this.userRepository.findByEmailWithPassword(email);
    if (!user) {
      return null;
    }

    const isValid = await this.passwordService.verify(password, user.password);
    if (!isValid) {
      return null;
    }

    // Return user without password
    const { password: _, ...userWithoutPassword } = user;
    return userWithoutPassword as User;
  }
}
```

---

## Validación de Datos

### Zod Schemas

```typescript
// packages/shared/src/schemas/user.schema.ts
import { z } from "zod";

// Base schemas
export const emailSchema = z.string().email("Invalid email address").toLowerCase();

export const passwordSchema = z
  .string()
  .min(8, "Password must be at least 8 characters")
  .max(72, "Password must not exceed 72 characters")
  .regex(/[a-z]/, "Password must contain at least one lowercase letter")
  .regex(/[A-Z]/, "Password must contain at least one uppercase letter")
  .regex(/[0-9]/, "Password must contain at least one number")
  .regex(/[^a-zA-Z0-9]/, "Password must contain at least one special character");

export const nameSchema = z
  .string()
  .min(2, "Name must be at least 2 characters")
  .max(100, "Name must not exceed 100 characters")
  .regex(/^[a-zA-Z\s'-]+$/, "Name can only contain letters, spaces, hyphens and apostrophes");

export const userRoleSchema = z.enum(["admin", "user", "guest"]);

// Create user schema
export const createUserSchema = z.object({
  email: emailSchema,
  name: nameSchema,
  password: passwordSchema,
  role: userRoleSchema.optional().default("user"),
});

export type CreateUserInput = z.infer<typeof createUserSchema>;

// Update user schema
export const updateUserSchema = z.object({
  email: emailSchema.optional(),
  name: nameSchema.optional(),
  role: userRoleSchema.optional(),
});

export type UpdateUserInput = z.infer<typeof updateUserSchema>;

// Login schema
export const loginSchema = z.object({
  email: emailSchema,
  password: z.string().min(1, "Password is required"),
});

export type LoginInput = z.infer<typeof loginSchema>;

// Pagination schema
export const paginationSchema = z.object({
  page: z.coerce.number().int().positive().optional().default(1),
  pageSize: z.coerce.number().int().min(1).max(100).optional().default(20),
});

// User filters schema
export const userFiltersSchema = paginationSchema.extend({
  search: z.string().optional(),
  role: userRoleSchema.optional(),
  sortBy: z.enum(["name", "email", "createdAt"]).optional().default("createdAt"),
  sortOrder: z.enum(["asc", "desc"]).optional().default("desc"),
});

export type UserFilters = z.infer<typeof userFiltersSchema>;
```

### io-ts Alternative

```typescript
// packages/shared/src/schemas/io-ts/user.schema.ts
import * as t from "io-ts";
import { either } from "fp-ts/Either";
import { pipe } from "fp-ts/function";

// Custom codecs
const EmailCodec = new t.Type<string, string, unknown>(
  "Email",
  (u): u is string => typeof u === "string",
  (u, c) =>
    pipe(
      t.string.validate(u, c),
      either.chain((s) =>
        /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(s)
          ? t.success(s.toLowerCase())
          : t.failure(u, c, "Invalid email format")
      )
    ),
  (a) => a
);

const NonEmptyString = t.brand(
  t.string,
  (s): s is t.Branded<string, { readonly NonEmpty: unique symbol }> => s.length > 0,
  "NonEmpty"
);

// User role codec
const UserRole = t.union([t.literal("admin"), t.literal("user"), t.literal("guest")]);

// User codec
const User = t.type({
  id: t.string,
  email: EmailCodec,
  name: t.string,
  role: UserRole,
  createdAt: t.string,
  updatedAt: t.string,
});

const CreateUser = t.type({
  email: EmailCodec,
  name: NonEmptyString,
  password: NonEmptyString,
  role: t.union([UserRole, t.undefined]),
});

const UpdateUser = t.partial({
  email: EmailCodec,
  name: NonEmptyString,
  role: UserRole,
});

// Type inference
type UserType = t.TypeOf<typeof User>;
type CreateUserType = t.TypeOf<typeof CreateUser>;
type UpdateUserType = t.TypeOf<typeof UpdateUser>;

// Validation function
function validate<T>(codec: t.Type<T>, data: unknown): T {
  const result = codec.decode(data);
  if (result._tag === "Left") {
    const errors = result.left.map((e) => ({
      path: e.context.map((c) => c.key).join("."),
      message: e.message ?? "Invalid value",
    }));
    throw new ValidationError(errors);
  }
  return result.right;
}

class ValidationError extends Error {
  constructor(public readonly errors: { path: string; message: string }[]) {
    super("Validation failed");
    this.name = "ValidationError";
  }
}

export { User, CreateUser, UpdateUser, validate, ValidationError };
export type { UserType, CreateUserType, UpdateUserType };
```

### Validation Middleware

```typescript
// packages/api/src/middleware/validation.ts
import { Request, Response, NextFunction, RequestHandler } from "express";
import { z, ZodSchema } from "zod";
import { ApiResponse } from "@myapp/shared";

interface ValidationErrorDetail {
  field: string;
  message: string;
  code: string;
}

export function validateBody<T extends ZodSchema>(
  schema: T
): RequestHandler {
  return async (req: Request, res: Response, next: NextFunction) => {
    try {
      req.body = schema.parse(req.body);
      next();
    } catch (error) {
      if (error instanceof z.ZodError) {
        const details: ValidationErrorDetail[] = error.errors.map((e) => ({
          field: e.path.join("."),
          message: e.message,
          code: e.code,
        }));

        const response: ApiResponse<never> = {
          success: false,
          error: {
            code: "VALIDATION_ERROR",
            message: "Request body validation failed",
            details: { errors: details },
          },
          meta: {
            timestamp: new Date().toISOString(),
            requestId: req.id,
          },
        };

        res.status(400).json(response);
        return;
      }
      next(error);
    }
  };
}

export function validateQuery<T extends ZodSchema>(
  schema: T
): RequestHandler {
  return async (req: Request, res: Response, next: NextFunction) => {
    try {
      req.query = schema.parse(req.query);
      next();
    } catch (error) {
      if (error instanceof z.ZodError) {
        const details: ValidationErrorDetail[] = error.errors.map((e) => ({
          field: e.path.join("."),
          message: e.message,
          code: e.code,
        }));

        const response: ApiResponse<never> = {
          success: false,
          error: {
            code: "VALIDATION_ERROR",
            message: "Query parameter validation failed",
            details: { errors: details },
          },
          meta: {
            timestamp: new Date().toISOString(),
            requestId: req.id,
          },
        };

        res.status(400).json(response);
        return;
      }
      next(error);
    }
  };
}

export function validateParams<T extends ZodSchema>(
  schema: T
): RequestHandler {
  return async (req: Request, res: Response, next: NextFunction) => {
    try {
      req.params = schema.parse(req.params);
      next();
    } catch (error) {
      if (error instanceof z.ZodError) {
        const details: ValidationErrorDetail[] = error.errors.map((e) => ({
          field: e.path.join("."),
          message: e.message,
          code: e.code,
        }));

        const response: ApiResponse<never> = {
          success: false,
          error: {
            code: "VALIDATION_ERROR",
            message: "Path parameter validation failed",
            details: { errors: details },
          },
          meta: {
            timestamp: new Date().toISOString(),
            requestId: req.id,
          },
        };

        res.status(400).json(response);
        return;
      }
      next(error);
    }
  };
}
```

---

## Manejo de Errores

### Error Classes

```typescript
// packages/api/src/utils/errors.ts
export class AppError extends Error {
  constructor(
    public readonly code: string,
    message: string,
    public readonly statusCode: number = 500,
    public readonly details?: Record<string, unknown>
  ) {
    super(message);
    this.name = "AppError";
    Error.captureStackTrace(this, this.constructor);
  }

  toJSON() {
    return {
      code: this.code,
      message: this.message,
      ...(this.details && { details: this.details }),
    };
  }
}

export class NotFoundError extends AppError {
  constructor(resource: string, id?: string) {
    super(
      "NOT_FOUND",
      id ? `${resource} with id ${id} not found` : `${resource} not found`,
      404
    );
    this.name = "NotFoundError";
  }
}

export class ValidationError extends AppError {
  constructor(errors: Array<{ field: string; message: string }>) {
    super("VALIDATION_ERROR", "Validation failed", 400, { errors });
    this.name = "ValidationError";
  }
}

export class ConflictError extends AppError {
  constructor(message: string) {
    super("CONFLICT", message, 409);
    this.name = "ConflictError";
  }
}

export class UnauthorizedError extends AppError {
  constructor(message: string = "Authentication required") {
    super("UNAUTHORIZED", message, 401);
    this.name = "UnauthorizedError";
  }
}

export class ForbiddenError extends AppError {
  constructor(message: string = "Access denied") {
    super("FORBIDDEN", message, 403);
    this.name = "ForbiddenError";
  }
}

export class RateLimitError extends AppError {
  constructor(retryAfter: number) {
    super("RATE_LIMIT_EXCEEDED", "Too many requests", 429, { retryAfter });
    this.name = "RateLimitError";
  }
}

export class ServiceUnavailableError extends AppError {
  constructor(service: string) {
    super(
      "SERVICE_UNAVAILABLE",
      `${service} is currently unavailable`,
      503
    );
    this.name = "ServiceUnavailableError";
  }
}
```

### Error Handling Middleware

```typescript
// packages/api/src/middleware/error-handler.ts
import { Request, Response, NextFunction, ErrorRequestHandler } from "express";
import { AppError } from "../utils/errors";
import { ApiResponse } from "@myapp/shared";
import { logger } from "../utils/logger";

export const errorHandler: ErrorRequestHandler = (
  err: Error,
  req: Request,
  res: Response,
  next: NextFunction
) => {
  // Log the error
  logger.error({
    message: err.message,
    stack: err.stack,
    requestId: req.id,
    path: req.path,
    method: req.method,
  });

  // Handle known errors
  if (err instanceof AppError) {
    const response: ApiResponse<never> = {
      success: false,
      error: {
        code: err.code,
        message: err.message,
        details: err.details,
        ...(process.env.NODE_ENV !== "production" && { stack: err.stack }),
      },
      meta: {
        timestamp: new Date().toISOString(),
        requestId: req.id,
      },
    };

    res.status(err.statusCode).json(response);
    return;
  }

  // Handle unknown errors
  const response: ApiResponse<never> = {
    success: false,
    error: {
      code: "INTERNAL_SERVER_ERROR",
      message:
        process.env.NODE_ENV === "production"
          ? "An unexpected error occurred"
          : err.message,
      ...(process.env.NODE_ENV !== "production" && { stack: err.stack }),
    },
    meta: {
      timestamp: new Date().toISOString(),
      requestId: req.id,
    },
  };

  res.status(500).json(response);
};

// Async error wrapper
export function asyncHandler<T>(
  fn: (req: Request, res: Response, next: NextFunction) => Promise<T>
): (req: Request, res: Response, next: NextFunction) => void {
  return (req, res, next) => {
    Promise.resolve(fn(req, res, next)).catch(next);
  };
}

// Not found handler
export const notFoundHandler = (req: Request, res: Response) => {
  const response: ApiResponse<never> = {
    success: false,
    error: {
      code: "NOT_FOUND",
      message: `Route ${req.method} ${req.path} not found`,
    },
    meta: {
      timestamp: new Date().toISOString(),
      requestId: req.id,
    },
  };

  res.status(404).json(response);
};
```

### Result Pattern

```typescript
// packages/shared/src/utils/result.ts
export type Result<T, E = Error> =
  | { success: true; data: T }
  | { success: false; error: E };

export function ok<T>(data: T): Result<T, never> {
  return { success: true, data };
}

export function err<E>(error: E): Result<never, E> {
  return { success: false, error };
}

export function isOk<T, E>(result: Result<T, E>): result is { success: true; data: T } {
  return result.success;
}

export function isErr<T, E>(result: Result<T, E>): result is { success: false; error: E } {
  return !result.success;
}

export function map<T, U, E>(
  result: Result<T, E>,
  fn: (value: T) => U
): Result<U, E> {
  if (isOk(result)) {
    return ok(fn(result.data));
  }
  return result;
}

export function mapErr<T, E, F>(
  result: Result<T, E>,
  fn: (error: E) => F
): Result<T, F> {
  if (isErr(result)) {
    return err(fn(result.error));
  }
  return result;
}

export function flatMap<T, U, E>(
  result: Result<T, E>,
  fn: (value: T) => Result<U, E>
): Result<U, E> {
  if (isOk(result)) {
    return fn(result.data);
  }
  return result;
}

export function unwrap<T, E>(result: Result<T, E>): T {
  if (isOk(result)) {
    return result.data;
  }
  throw result.error;
}

export function unwrapOr<T, E>(result: Result<T, E>, defaultValue: T): T {
  if (isOk(result)) {
    return result.data;
  }
  return defaultValue;
}

// Usage in service
class SafeUserService {
  async findById(id: string): Promise<Result<User, AppError>> {
    try {
      const user = await this.repository.findById(id);
      if (!user) {
        return err(new NotFoundError("User", id));
      }
      return ok(user);
    } catch (error) {
      return err(new AppError("DATABASE_ERROR", "Failed to fetch user", 500));
    }
  }

  async create(data: CreateUserDto): Promise<Result<User, AppError>> {
    try {
      const existing = await this.repository.findByEmail(data.email);
      if (existing) {
        return err(new ConflictError("Email already in use"));
      }

      const user = await this.repository.create(data);
      return ok(user);
    } catch (error) {
      return err(new AppError("DATABASE_ERROR", "Failed to create user", 500));
    }
  }
}
```

---

## Estrategias de Testing

### Unit Testing

```typescript
// packages/api/src/services/__tests__/user.service.test.ts
import { describe, it, expect, beforeEach, vi } from "vitest";
import { UserService } from "../user.service";
import { UserRepository } from "../../repositories/user.repository";
import { PasswordService } from "../password.service";
import { AppError } from "../../utils/errors";

// Mock dependencies
vi.mock("../../repositories/user.repository");
vi.mock("../password.service");

describe("UserService", () => {
  let userService: UserService;
  let mockUserRepository: vi.Mocked<UserRepository>;
  let mockPasswordService: vi.Mocked<PasswordService>;

  const mockUser = {
    id: "user-123",
    email: "test@example.com",
    name: "Test User",
    role: "user" as const,
    createdAt: new Date(),
    updatedAt: new Date(),
  };

  beforeEach(() => {
    mockUserRepository = new UserRepository() as vi.Mocked<UserRepository>;
    mockPasswordService = new PasswordService() as vi.Mocked<PasswordService>;
    userService = new UserService();

    // Reset mocks
    vi.clearAllMocks();
  });

  describe("findById", () => {
    it("should return user when found", async () => {
      mockUserRepository.findById.mockResolvedValue(mockUser);

      const result = await userService.findById("user-123");

      expect(result).toEqual(mockUser);
      expect(mockUserRepository.findById).toHaveBeenCalledWith("user-123");
    });

    it("should return null when user not found", async () => {
      mockUserRepository.findById.mockResolvedValue(null);

      const result = await userService.findById("nonexistent");

      expect(result).toBeNull();
    });
  });

  describe("create", () => {
    const createDto = {
      email: "new@example.com",
      name: "New User",
      password: "SecurePass123!",
    };

    it("should create user successfully", async () => {
      mockUserRepository.findByEmail.mockResolvedValue(null);
      mockPasswordService.hash.mockResolvedValue("hashed_password");
      mockUserRepository.create.mockResolvedValue({
        ...mockUser,
        email: createDto.email,
        name: createDto.name,
      });

      const result = await userService.create(createDto);

      expect(result.email).toBe(createDto.email);
      expect(mockPasswordService.hash).toHaveBeenCalledWith(createDto.password);
    });

    it("should throw error if email exists", async () => {
      mockUserRepository.findByEmail.mockResolvedValue(mockUser);

      await expect(userService.create(createDto)).rejects.toThrow(AppError);
      await expect(userService.create(createDto)).rejects.toMatchObject({
        code: "EMAIL_EXISTS",
        statusCode: 409,
      });
    });
  });

  describe("update", () => {
    it("should update user successfully", async () => {
      mockUserRepository.findById.mockResolvedValue(mockUser);
      mockUserRepository.update.mockResolvedValue({
        ...mockUser,
        name: "Updated Name",
      });

      const result = await userService.update("user-123", { name: "Updated Name" });

      expect(result.name).toBe("Updated Name");
    });

    it("should throw NotFoundError if user does not exist", async () => {
      mockUserRepository.findById.mockResolvedValue(null);

      await expect(
        userService.update("nonexistent", { name: "New Name" })
      ).rejects.toMatchObject({
        code: "USER_NOT_FOUND",
        statusCode: 404,
      });
    });
  });
});
```

### Integration Testing

```typescript
// packages/api/src/__tests__/users.integration.test.ts
import { describe, it, expect, beforeAll, afterAll, beforeEach } from "vitest";
import request from "supertest";
import { app } from "../app";
import { prisma } from "../database/prisma";
import { createTestUser, generateAuthToken } from "./helpers";

describe("Users API Integration", () => {
  let authToken: string;
  let adminToken: string;

  beforeAll(async () => {
    // Setup test database
    await prisma.$connect();
  });

  afterAll(async () => {
    await prisma.$disconnect();
  });

  beforeEach(async () => {
    // Clean database
    await prisma.user.deleteMany();

    // Create test users
    const user = await createTestUser({ role: "user" });
    const admin = await createTestUser({ role: "admin" });

    authToken = generateAuthToken(user);
    adminToken = generateAuthToken(admin);
  });

  describe("GET /api/users", () => {
    it("should return paginated users", async () => {
      // Create additional users
      await createTestUser({ email: "user1@test.com" });
      await createTestUser({ email: "user2@test.com" });

      const response = await request(app)
        .get("/api/users")
        .set("Authorization", `Bearer ${authToken}`)
        .query({ page: 1, pageSize: 10 });

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(response.body.data).toHaveLength(4); // 2 base + 2 additional
      expect(response.body.meta.pagination).toBeDefined();
    });

    it("should filter by role", async () => {
      const response = await request(app)
        .get("/api/users")
        .set("Authorization", `Bearer ${authToken}`)
        .query({ role: "admin" });

      expect(response.status).toBe(200);
      expect(response.body.data).toHaveLength(1);
      expect(response.body.data[0].role).toBe("admin");
    });

    it("should require authentication", async () => {
      const response = await request(app).get("/api/users");

      expect(response.status).toBe(401);
      expect(response.body.error.code).toBe("UNAUTHORIZED");
    });
  });

  describe("POST /api/users", () => {
    it("should create user as admin", async () => {
      const newUser = {
        email: "newuser@test.com",
        name: "New User",
        password: "SecurePass123!",
      };

      const response = await request(app)
        .post("/api/users")
        .set("Authorization", `Bearer ${adminToken}`)
        .send(newUser);

      expect(response.status).toBe(201);
      expect(response.body.data.email).toBe(newUser.email);
    });

    it("should reject non-admin users", async () => {
      const response = await request(app)
        .post("/api/users")
        .set("Authorization", `Bearer ${authToken}`)
        .send({
          email: "new@test.com",
          name: "New",
          password: "Pass123!",
        });

      expect(response.status).toBe(403);
    });

    it("should validate input", async () => {
      const response = await request(app)
        .post("/api/users")
        .set("Authorization", `Bearer ${adminToken}`)
        .send({
          email: "invalid-email",
          name: "",
          password: "weak",
        });

      expect(response.status).toBe(400);
      expect(response.body.error.code).toBe("VALIDATION_ERROR");
    });
  });
});
```

### E2E Testing

```typescript
// packages/api/src/__tests__/e2e/auth-flow.e2e.test.ts
import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { chromium, Browser, Page } from "playwright";

describe("Authentication Flow E2E", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    browser = await chromium.launch();
    page = await browser.newPage();
  });

  afterAll(async () => {
    await browser.close();
  });

  it("should complete full auth flow", async () => {
    // Visit login page
    await page.goto("http://localhost:3000/login");

    // Fill login form
    await page.fill('input[name="email"]', "test@example.com");
    await page.fill('input[name="password"]', "TestPass123!");

    // Submit form
    await page.click('button[type="submit"]');

    // Wait for redirect
    await page.waitForURL("**/dashboard");

    // Verify logged in
    const userName = await page.textContent("[data-testid='user-name']");
    expect(userName).toBe("Test User");

    // Logout
    await page.click("[data-testid='logout-button']");
    await page.waitForURL("**/login");
  });
});
```

---

## Patrones de Deployment

### Docker Configuration

```dockerfile
# Dockerfile
FROM node:20-alpine AS base
RUN npm install -g pnpm

# Dependencies stage
FROM base AS deps
WORKDIR /app
COPY pnpm-lock.yaml package.json pnpm-workspace.yaml ./
COPY packages/shared/package.json ./packages/shared/
COPY packages/api/package.json ./packages/api/
RUN pnpm install --frozen-lockfile

# Build stage
FROM base AS builder
WORKDIR /app
COPY --from=deps /app/node_modules ./node_modules
COPY --from=deps /app/packages/shared/node_modules ./packages/shared/node_modules
COPY --from=deps /app/packages/api/node_modules ./packages/api/node_modules
COPY . .
RUN pnpm run build

# Production stage
FROM node:20-alpine AS runner
WORKDIR /app

ENV NODE_ENV=production

RUN addgroup --system --gid 1001 nodejs
RUN adduser --system --uid 1001 api

COPY --from=builder /app/packages/api/dist ./dist
COPY --from=builder /app/packages/api/package.json ./
COPY --from=builder /app/node_modules ./node_modules

USER api

EXPOSE 3000

CMD ["node", "dist/index.js"]
```

### GitHub Actions CI/CD

```yaml
# .github/workflows/ci.yml
name: CI/CD Pipeline

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

env:
  REGISTRY: ghcr.io
  IMAGE_NAME: ${{ github.repository }}

jobs:
  lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: pnpm/action-setup@v2
        with:
          version: 8
      - uses: actions/setup-node@v4
        with:
          node-version: 20
          cache: "pnpm"
      - run: pnpm install
      - run: pnpm lint

  typecheck:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: pnpm/action-setup@v2
        with:
          version: 8
      - uses: actions/setup-node@v4
        with:
          node-version: 20
          cache: "pnpm"
      - run: pnpm install
      - run: pnpm typecheck

  test:
    runs-on: ubuntu-latest
    services:
      postgres:
        image: postgres:15
        env:
          POSTGRES_USER: test
          POSTGRES_PASSWORD: test
          POSTGRES_DB: test
        ports:
          - 5432:5432
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5

    steps:
      - uses: actions/checkout@v4
      - uses: pnpm/action-setup@v2
        with:
          version: 8
      - uses: actions/setup-node@v4
        with:
          node-version: 20
          cache: "pnpm"
      - run: pnpm install
      - run: pnpm test
        env:
          DATABASE_URL: postgresql://test:test@localhost:5432/test

  build:
    needs: [lint, typecheck, test]
    runs-on: ubuntu-latest
    if: github.event_name == 'push' && github.ref == 'refs/heads/main'

    steps:
      - uses: actions/checkout@v4

      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v3

      - name: Login to Container Registry
        uses: docker/login-action@v3
        with:
          registry: ${{ env.REGISTRY }}
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}

      - name: Build and push
        uses: docker/build-push-action@v5
        with:
          context: .
          push: true
          tags: ${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}:${{ github.sha }}
          cache-from: type=gha
          cache-to: type=gha,mode=max

  deploy:
    needs: build
    runs-on: ubuntu-latest
    if: github.event_name == 'push' && github.ref == 'refs/heads/main'

    steps:
      - name: Deploy to Production
        run: |
          # Add deployment commands here
          echo "Deploying ${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}:${{ github.sha }}"
```

### Environment Configuration

```typescript
// packages/api/src/config/env.ts
import { z } from "zod";

const envSchema = z.object({
  // Server
  NODE_ENV: z.enum(["development", "production", "test"]).default("development"),
  PORT: z.coerce.number().default(3000),
  HOST: z.string().default("0.0.0.0"),

  // Database
  DATABASE_URL: z.string().url(),

  // Auth
  JWT_SECRET: z.string().min(32),
  JWT_EXPIRES_IN: z.string().default("1d"),
  REFRESH_TOKEN_EXPIRES_IN: z.string().default("7d"),

  // External services
  REDIS_URL: z.string().url().optional(),
  SMTP_HOST: z.string().optional(),
  SMTP_PORT: z.coerce.number().optional(),
  SMTP_USER: z.string().optional(),
  SMTP_PASS: z.string().optional(),

  // Feature flags
  ENABLE_SWAGGER: z.coerce.boolean().default(true),
  ENABLE_RATE_LIMITING: z.coerce.boolean().default(true),
  LOG_LEVEL: z.enum(["debug", "info", "warn", "error"]).default("info"),
});

export type Env = z.infer<typeof envSchema>;

function loadEnv(): Env {
  const result = envSchema.safeParse(process.env);

  if (!result.success) {
    console.error("Invalid environment configuration:");
    console.error(result.error.format());
    process.exit(1);
  }

  return result.data;
}

export const env = loadEnv();
```

---

## Monorepos y Compartición de Código

### pnpm Workspace

```yaml
# pnpm-workspace.yaml
packages:
  - "packages/*"
  - "apps/*"
```

### Base tsconfig

```json
// tsconfig.base.json
{
  "$schema": "https://json.schemastore.org/tsconfig",
  "compilerOptions": {
    "target": "ES2022",
    "lib": ["ES2022"],
    "module": "NodeNext",
    "moduleResolution": "NodeNext",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true,
    "declaration": true,
    "declarationMap": true,
    "sourceMap": true,
    "noUncheckedIndexedAccess": true,
    "noImplicitReturns": true,
    "resolveJsonModule": true,
    "isolatedModules": true
  }
}
```

### Package Configuration

```json
// packages/shared/package.json
{
  "name": "@myapp/shared",
  "version": "1.0.0",
  "type": "module",
  "main": "./dist/index.js",
  "types": "./dist/index.d.ts",
  "exports": {
    ".": {
      "types": "./dist/index.d.ts",
      "import": "./dist/index.js"
    },
    "./types": {
      "types": "./dist/types/index.d.ts",
      "import": "./dist/types/index.js"
    },
    "./schemas": {
      "types": "./dist/schemas/index.d.ts",
      "import": "./dist/schemas/index.js"
    }
  },
  "scripts": {
    "build": "tsup src/index.ts --format esm --dts",
    "dev": "tsup src/index.ts --format esm --dts --watch",
    "typecheck": "tsc --noEmit"
  },
  "dependencies": {
    "zod": "^3.22.0"
  },
  "devDependencies": {
    "tsup": "^8.0.0",
    "typescript": "^5.3.0"
  }
}
```

---

## Referencias y Recursos

### Documentación Oficial

- [TypeScript Handbook](https://www.typescriptlang.org/docs/)
- [Zod Documentation](https://zod.dev/)
- [Vitest Documentation](https://vitest.dev/)
- [pnpm Workspaces](https://pnpm.io/workspaces)

### Herramientas Recomendadas

| Herramienta  | Propósito                    |
| ------------ | ---------------------------- |
| `zod`        | Schema validation            |
| `vitest`     | Unit/integration testing     |
| `playwright` | E2E testing                  |
| `tsup`       | Library bundling             |
| `turborepo`  | Monorepo build system        |
| `pnpm`       | Fast package manager         |
| `docker`     | Containerization             |
| `gh actions` | CI/CD                        |

---

## Notas de ARCHAEON

> Este documento completa la serie de TypeScript en ARCHAEON, cubriendo
> los patrones arquitectónicos modernos para desarrollo full stack.
> La combinación de TypeScript con herramientas modernas de validación,
> testing y deployment proporciona una base sólida para aplicaciones
> empresariales mantenibles y escalables.

**Serie completa:**
1. [TYPESCRIPT_01_FUNDAMENTOS.md](./TYPESCRIPT_01_FUNDAMENTOS.md)
2. [TYPESCRIPT_02_NODE.md](./TYPESCRIPT_02_NODE.md)
3. [TYPESCRIPT_03_WASM.md](./TYPESCRIPT_03_WASM.md)
4. TYPESCRIPT_04_FULLSTACK.md (actual)
