---
título: "TypeScript 01 - Fundamentos del Sistema de Tipos"
módulo: ARCHAEON_CORE
sección: 80000_MODERNOS/TYPESCRIPT
versión: 1.0.0
fecha_creación: 2025-12-31
autor: ARCHAEON Sistema de Documentación
descripción: >
  Fundamentos completos del sistema de tipos de TypeScript, incluyendo
  interfaces, tipos genéricos, guardas de tipo y configuración avanzada.
tags:
  - typescript
  - types
  - generics
  - interfaces
  - type-guards
requisitos:
  - JavaScript ES6+
  - Node.js 18+
  - TypeScript 5.0+
---

# TYPESCRIPT 01 - FUNDAMENTOS DEL SISTEMA DE TIPOS

## Tabla de Contenidos

1. [Introducción al Sistema de Tipos](#introducción-al-sistema-de-tipos)
2. [Tipos Primitivos y Literales](#tipos-primitivos-y-literales)
3. [Interfaces vs Types](#interfaces-vs-types)
4. [Genéricos y Tipos Utilitarios](#genéricos-y-tipos-utilitarios)
5. [Guardas de Tipo y Narrowing](#guardas-de-tipo-y-narrowing)
6. [Modo Estricto y Mejores Prácticas](#modo-estricto-y-mejores-prácticas)
7. [Configuración de tsconfig.json](#configuración-de-tsconfigjson)
8. [Patrones Avanzados](#patrones-avanzados)

---

## Introducción al Sistema de Tipos

TypeScript extiende JavaScript con un sistema de tipos estático que permite
detectar errores en tiempo de compilación, mejorando la mantenibilidad y
documentación del código.

### Filosofía del Sistema de Tipos

```typescript
// TypeScript uses structural typing (duck typing)
// If it looks like a duck and quacks like a duck, it's a duck

interface Duck {
  quack(): void;
  swim(): void;
}

interface Bird {
  quack(): void;
  swim(): void;
  fly(): void;
}

// Bird is assignable to Duck because it has all Duck's properties
function makeDuckQuack(duck: Duck): void {
  duck.quack();
}

const bird: Bird = {
  quack: () => console.log("Quack!"),
  swim: () => console.log("Swimming..."),
  fly: () => console.log("Flying..."),
};

makeDuckQuack(bird); // Valid: Bird structurally matches Duck
```

### Inferencia de Tipos

```typescript
// TypeScript infers types when possible
let message = "Hello"; // Inferred as string
let count = 42; // Inferred as number
let isActive = true; // Inferred as boolean

// Array inference
const numbers = [1, 2, 3]; // Inferred as number[]
const mixed = [1, "two", true]; // Inferred as (number | string | boolean)[]

// Object inference
const user = {
  name: "Alice",
  age: 30,
  email: "alice@example.com",
};
// Inferred as { name: string; age: number; email: string }

// Function return type inference
function add(a: number, b: number) {
  return a + b; // Return type inferred as number
}

// Contextual typing
const names = ["Alice", "Bob", "Charlie"];
names.forEach((name) => {
  // 'name' is inferred as string from array context
  console.log(name.toUpperCase());
});
```

---

## Tipos Primitivos y Literales

### Tipos Primitivos Básicos

```typescript
// Primitive types
let str: string = "Hello, TypeScript";
let num: number = 42;
let bigInt: bigint = 9007199254740991n;
let bool: boolean = true;
let sym: symbol = Symbol("unique");
let undef: undefined = undefined;
let nul: null = null;

// Special types
let any_val: any = "anything"; // Escape hatch - avoid when possible
let unknown_val: unknown = "safer any"; // Type-safe alternative to any
let never_val: never; // For functions that never return
let void_val: void = undefined; // For functions with no return value
```

### Tipos Literales

```typescript
// Literal types constrain values to specific literals
type Direction = "north" | "south" | "east" | "west";
type HttpStatus = 200 | 201 | 400 | 404 | 500;
type Toggle = true | false;

// Literal inference with const
let mutableDirection = "north"; // Type: string
const immutableDirection = "north"; // Type: "north"

// const assertion for literal types
const config = {
  endpoint: "https://api.example.com",
  timeout: 5000,
} as const;
// Type: { readonly endpoint: "https://api.example.com"; readonly timeout: 5000 }

// Template literal types
type EventName = `on${Capitalize<string>}`;
type CssValue = `${number}px` | `${number}em` | `${number}rem`;

const margin: CssValue = "16px"; // Valid
const padding: CssValue = "2em"; // Valid
```

### Arrays y Tuplas

```typescript
// Array types
const numbers: number[] = [1, 2, 3, 4, 5];
const strings: Array<string> = ["a", "b", "c"];

// Readonly arrays
const readonlyNumbers: readonly number[] = [1, 2, 3];
const readonlyStrings: ReadonlyArray<string> = ["a", "b", "c"];

// Tuple types - fixed length arrays with specific types
type Point2D = [number, number];
type Point3D = [number, number, number];
type NamedPoint = [name: string, x: number, y: number];

const point: Point2D = [10, 20];
const origin: Point3D = [0, 0, 0];
const landmark: NamedPoint = ["Home", 40.7128, -74.006];

// Optional tuple elements
type OptionalCoord = [number, number, number?];
const coord2d: OptionalCoord = [10, 20];
const coord3d: OptionalCoord = [10, 20, 30];

// Rest elements in tuples
type StringNumberPair = [string, ...number[]];
const data: StringNumberPair = ["values", 1, 2, 3, 4, 5];

// Readonly tuples
type ReadonlyPoint = readonly [number, number];
const immutablePoint: ReadonlyPoint = [10, 20];
// immutablePoint[0] = 5; // Error: Cannot assign to readonly
```

---

## Interfaces vs Types

### Interfaces

```typescript
// Basic interface definition
interface User {
  id: number;
  name: string;
  email: string;
  createdAt: Date;
}

// Optional and readonly properties
interface Profile {
  readonly id: string;
  username: string;
  bio?: string; // Optional
  avatar?: string;
}

// Method signatures
interface Calculator {
  add(a: number, b: number): number;
  subtract(a: number, b: number): number;
  multiply: (a: number, b: number) => number;
}

// Index signatures
interface StringMap {
  [key: string]: string;
}

interface NumberDictionary {
  [index: number]: string;
  length: number; // OK: length is a number
  // name: string; // Error: string not assignable to number index
}

// Interface extension (inheritance)
interface Animal {
  name: string;
  age: number;
}

interface Dog extends Animal {
  breed: string;
  bark(): void;
}

// Multiple inheritance
interface Flyable {
  fly(): void;
  altitude: number;
}

interface Swimmable {
  swim(): void;
  depth: number;
}

interface Duck extends Animal, Flyable, Swimmable {
  quack(): void;
}

// Declaration merging (interfaces only)
interface Config {
  apiUrl: string;
}

interface Config {
  timeout: number;
}

// Config now has both apiUrl and timeout
const config: Config = {
  apiUrl: "https://api.example.com",
  timeout: 5000,
};
```

### Type Aliases

```typescript
// Basic type alias
type ID = string | number;
type Timestamp = number;

// Object type alias
type Point = {
  x: number;
  y: number;
};

// Union types (types only, not interfaces)
type Status = "pending" | "approved" | "rejected";
type Result<T> = { success: true; data: T } | { success: false; error: string };

// Intersection types
type Named = { name: string };
type Aged = { age: number };
type Person = Named & Aged; // { name: string; age: number }

// Mapped types (types only)
type Readonly<T> = {
  readonly [P in keyof T]: T[P];
};

type Partial<T> = {
  [P in keyof T]?: T[P];
};

// Conditional types (types only)
type NonNullable<T> = T extends null | undefined ? never : T;
type ExtractString<T> = T extends string ? T : never;

// Template literal types
type HttpMethod = "GET" | "POST" | "PUT" | "DELETE";
type Endpoint = `/${string}`;
type ApiRoute = `${HttpMethod} ${Endpoint}`;

const route: ApiRoute = "GET /users"; // Valid
```

### Cuándo Usar Cada Uno

| Característica              | Interface | Type |
| --------------------------- | --------- | ---- |
| Extensión/Herencia          | ✅        | ✅   |
| Declaración fusionada       | ✅        | ❌   |
| Union types                 | ❌        | ✅   |
| Intersection types          | ✅        | ✅   |
| Tipos primitivos            | ❌        | ✅   |
| Tuplas                      | ❌        | ✅   |
| Mapped types                | ❌        | ✅   |
| Conditional types           | ❌        | ✅   |
| Implementar en clases       | ✅        | ✅   |
| Mejor error messages        | ✅        | ❌   |
| Rendimiento compilación     | ✅        | ❌   |

```typescript
// Recommendation: Use interfaces for object shapes, types for everything else

// Prefer interface for objects
interface UserEntity {
  id: string;
  name: string;
  email: string;
}

// Prefer type for unions, intersections, and complex types
type RequestResult<T> =
  | { status: "success"; data: T }
  | { status: "error"; message: string }
  | { status: "loading" };

// Prefer type for function signatures
type EventHandler<T> = (event: T) => void;
type AsyncOperation<T, R> = (input: T) => Promise<R>;
```

---

## Genéricos y Tipos Utilitarios

### Fundamentos de Genéricos

```typescript
// Basic generic function
function identity<T>(value: T): T {
  return value;
}

const strResult = identity<string>("hello"); // Explicit type
const numResult = identity(42); // Inferred as number

// Generic interface
interface Container<T> {
  value: T;
  getValue(): T;
  setValue(value: T): void;
}

// Generic class
class Box<T> {
  private contents: T;

  constructor(value: T) {
    this.contents = value;
  }

  get(): T {
    return this.contents;
  }

  set(value: T): void {
    this.contents = value;
  }
}

const stringBox = new Box<string>("hello");
const numberBox = new Box(42); // Inferred as Box<number>

// Multiple type parameters
function pair<T, U>(first: T, second: U): [T, U] {
  return [first, second];
}

const p = pair("hello", 42); // [string, number]

// Generic constraints
interface Lengthwise {
  length: number;
}

function logLength<T extends Lengthwise>(value: T): number {
  console.log(value.length);
  return value.length;
}

logLength("hello"); // 5 - strings have length
logLength([1, 2, 3]); // 3 - arrays have length
// logLength(42); // Error: number doesn't have length

// keyof constraint
function getProperty<T, K extends keyof T>(obj: T, key: K): T[K] {
  return obj[key];
}

const user = { name: "Alice", age: 30 };
const name = getProperty(user, "name"); // string
const age = getProperty(user, "age"); // number
// getProperty(user, "invalid"); // Error
```

### Tipos Utilitarios Incorporados

```typescript
interface User {
  id: string;
  name: string;
  email: string;
  password: string;
  createdAt: Date;
  updatedAt: Date;
}

// Partial<T> - Makes all properties optional
type PartialUser = Partial<User>;
const update: PartialUser = { name: "New Name" };

// Required<T> - Makes all properties required
interface Config {
  host?: string;
  port?: number;
}
type RequiredConfig = Required<Config>;

// Readonly<T> - Makes all properties readonly
type ReadonlyUser = Readonly<User>;
const immutableUser: ReadonlyUser = {
  id: "1",
  name: "Alice",
  email: "alice@example.com",
  password: "secret",
  createdAt: new Date(),
  updatedAt: new Date(),
};
// immutableUser.name = "Bob"; // Error

// Pick<T, K> - Select specific properties
type UserCredentials = Pick<User, "email" | "password">;
// { email: string; password: string }

// Omit<T, K> - Remove specific properties
type PublicUser = Omit<User, "password">;
// { id: string; name: string; email: string; createdAt: Date; updatedAt: Date }

// Record<K, V> - Create object type with key type K and value type V
type UserRoles = Record<string, string[]>;
const roles: UserRoles = {
  admin: ["read", "write", "delete"],
  user: ["read"],
};

// Exclude<T, U> - Remove types from union
type Status = "pending" | "approved" | "rejected" | "cancelled";
type ActiveStatus = Exclude<Status, "cancelled">; // "pending" | "approved" | "rejected"

// Extract<T, U> - Keep only types that match
type StringOrNumber = string | number | boolean;
type JustStrings = Extract<StringOrNumber, string>; // string

// NonNullable<T> - Remove null and undefined
type MaybeString = string | null | undefined;
type DefiniteString = NonNullable<MaybeString>; // string

// ReturnType<T> - Get function return type
function fetchUser() {
  return { id: "1", name: "Alice" };
}
type FetchUserResult = ReturnType<typeof fetchUser>;
// { id: string; name: string }

// Parameters<T> - Get function parameter types as tuple
function createUser(name: string, age: number, email: string) {
  return { name, age, email };
}
type CreateUserParams = Parameters<typeof createUser>;
// [string, number, string]

// Awaited<T> - Unwrap Promise type
type PromiseString = Promise<string>;
type ResolvedString = Awaited<PromiseString>; // string
```

### Tipos Utilitarios Personalizados

```typescript
// DeepPartial - Recursive partial
type DeepPartial<T> = {
  [P in keyof T]?: T[P] extends object ? DeepPartial<T[P]> : T[P];
};

interface NestedConfig {
  server: {
    host: string;
    port: number;
    ssl: {
      enabled: boolean;
      cert: string;
    };
  };
}

const partialConfig: DeepPartial<NestedConfig> = {
  server: {
    ssl: {
      enabled: true,
    },
  },
};

// DeepReadonly
type DeepReadonly<T> = {
  readonly [P in keyof T]: T[P] extends object ? DeepReadonly<T[P]> : T[P];
};

// Mutable - Remove readonly
type Mutable<T> = {
  -readonly [P in keyof T]: T[P];
};

// RequiredKeys - Get required keys
type RequiredKeys<T> = {
  [K in keyof T]-?: {} extends Pick<T, K> ? never : K;
}[keyof T];

// OptionalKeys - Get optional keys
type OptionalKeys<T> = {
  [K in keyof T]-?: {} extends Pick<T, K> ? K : never;
}[keyof T];

// PickByType - Pick properties by type
type PickByType<T, U> = {
  [P in keyof T as T[P] extends U ? P : never]: T[P];
};

interface Mixed {
  name: string;
  age: number;
  active: boolean;
  email: string;
}

type StringProps = PickByType<Mixed, string>; // { name: string; email: string }

// OmitByType - Omit properties by type
type OmitByType<T, U> = {
  [P in keyof T as T[P] extends U ? never : P]: T[P];
};

type NonStringProps = OmitByType<Mixed, string>; // { age: number; active: boolean }
```

---

## Guardas de Tipo y Narrowing

### Narrowing Básico

```typescript
// typeof narrowing
function printValue(value: string | number) {
  if (typeof value === "string") {
    // TypeScript knows value is string here
    console.log(value.toUpperCase());
  } else {
    // TypeScript knows value is number here
    console.log(value.toFixed(2));
  }
}

// Truthiness narrowing
function printLength(value: string | null | undefined) {
  if (value) {
    // value is string (not null or undefined)
    console.log(value.length);
  }
}

// Equality narrowing
function compare(a: string | number, b: string | boolean) {
  if (a === b) {
    // Both must be string (only common type)
    console.log(a.toUpperCase(), b.toUpperCase());
  }
}

// in operator narrowing
interface Fish {
  swim(): void;
}

interface Bird {
  fly(): void;
}

function move(animal: Fish | Bird) {
  if ("swim" in animal) {
    animal.swim();
  } else {
    animal.fly();
  }
}

// instanceof narrowing
function formatDate(value: Date | string) {
  if (value instanceof Date) {
    return value.toISOString();
  } else {
    return new Date(value).toISOString();
  }
}
```

### Guardas de Tipo Personalizadas

```typescript
// Type predicate (is)
interface Cat {
  meow(): void;
  purr(): void;
}

interface Dog {
  bark(): void;
  wagTail(): void;
}

type Pet = Cat | Dog;

// Custom type guard function
function isCat(pet: Pet): pet is Cat {
  return "meow" in pet;
}

function isDog(pet: Pet): pet is Dog {
  return "bark" in pet;
}

function handlePet(pet: Pet) {
  if (isCat(pet)) {
    pet.meow(); // TypeScript knows it's a Cat
    pet.purr();
  } else {
    pet.bark(); // TypeScript knows it's a Dog
    pet.wagTail();
  }
}

// Assertion function (asserts)
function assertIsString(value: unknown): asserts value is string {
  if (typeof value !== "string") {
    throw new Error(`Expected string, got ${typeof value}`);
  }
}

function processInput(input: unknown) {
  assertIsString(input);
  // TypeScript knows input is string after assertion
  console.log(input.toUpperCase());
}

// Combined type guards
interface ApiResponse<T> {
  success: boolean;
  data?: T;
  error?: string;
}

function isSuccessResponse<T>(
  response: ApiResponse<T>
): response is ApiResponse<T> & { success: true; data: T } {
  return response.success === true && response.data !== undefined;
}

function handleResponse<T>(response: ApiResponse<T>) {
  if (isSuccessResponse(response)) {
    // response.data is guaranteed to exist and be T
    console.log(response.data);
  } else {
    console.error(response.error);
  }
}
```

### Discriminated Unions

```typescript
// Discriminated union pattern
interface LoadingState {
  status: "loading";
}

interface SuccessState<T> {
  status: "success";
  data: T;
}

interface ErrorState {
  status: "error";
  error: string;
}

type RequestState<T> = LoadingState | SuccessState<T> | ErrorState;

function renderState<T>(state: RequestState<T>) {
  switch (state.status) {
    case "loading":
      return "Loading...";
    case "success":
      return `Data: ${JSON.stringify(state.data)}`;
    case "error":
      return `Error: ${state.error}`;
    default:
      // Exhaustiveness check
      const _exhaustive: never = state;
      return _exhaustive;
  }
}

// Complex discriminated union
type Shape =
  | { kind: "circle"; radius: number }
  | { kind: "rectangle"; width: number; height: number }
  | { kind: "triangle"; base: number; height: number };

function calculateArea(shape: Shape): number {
  switch (shape.kind) {
    case "circle":
      return Math.PI * shape.radius ** 2;
    case "rectangle":
      return shape.width * shape.height;
    case "triangle":
      return (shape.base * shape.height) / 2;
  }
}

// Nested discriminated unions
type Event =
  | {
      type: "user";
      action:
        | { kind: "login"; userId: string }
        | { kind: "logout"; userId: string }
        | { kind: "signup"; email: string };
    }
  | {
      type: "system";
      action:
        | { kind: "startup" }
        | { kind: "shutdown" }
        | { kind: "error"; message: string };
    };

function handleEvent(event: Event) {
  if (event.type === "user") {
    switch (event.action.kind) {
      case "login":
        console.log(`User ${event.action.userId} logged in`);
        break;
      case "logout":
        console.log(`User ${event.action.userId} logged out`);
        break;
      case "signup":
        console.log(`New user signed up: ${event.action.email}`);
        break;
    }
  } else {
    switch (event.action.kind) {
      case "startup":
        console.log("System starting up");
        break;
      case "shutdown":
        console.log("System shutting down");
        break;
      case "error":
        console.error(`System error: ${event.action.message}`);
        break;
    }
  }
}
```

---

## Modo Estricto y Mejores Prácticas

### Opciones de Modo Estricto

```typescript
// tsconfig.json strict mode enables:
// - strictNullChecks: null and undefined are distinct types
// - strictFunctionTypes: stricter function type checking
// - strictBindCallApply: stricter bind, call, apply checking
// - strictPropertyInitialization: class properties must be initialized
// - noImplicitAny: error on implicit any
// - noImplicitThis: error on implicit this
// - useUnknownInCatchVariables: catch variables are unknown
// - alwaysStrict: emit "use strict" in output

// strictNullChecks example
function getLength(str: string | null): number {
  // return str.length; // Error: str might be null
  return str?.length ?? 0; // Safe with nullish coalescing
}

// strictPropertyInitialization example
class User {
  name: string; // Error without initialization
  email!: string; // Definite assignment assertion (use carefully)

  constructor(name: string) {
    this.name = name;
    // email will be set later via setter
  }
}

// noImplicitAny example
function processData(data: unknown) {
  // data.foo; // Error: unknown doesn't have foo
  if (typeof data === "object" && data !== null && "foo" in data) {
    console.log((data as { foo: unknown }).foo);
  }
}
```

### Mejores Prácticas de Tipado

```typescript
// 1. Prefer unknown over any
function parseJSON(json: string): unknown {
  return JSON.parse(json);
}

// 2. Use const assertions for literals
const config = {
  api: "https://api.example.com",
  version: 1,
} as const;

// 3. Avoid enums, prefer union types
// Bad
enum StatusEnum {
  Pending = "PENDING",
  Active = "ACTIVE",
  Closed = "CLOSED",
}

// Good
type Status = "PENDING" | "ACTIVE" | "CLOSED";

// 4. Use branded types for primitive wrappers
type UserId = string & { readonly __brand: unique symbol };
type OrderId = string & { readonly __brand: unique symbol };

function createUserId(id: string): UserId {
  return id as UserId;
}

function getUser(id: UserId) {
  /* ... */
}
// getUser("raw-string"); // Error
// getUser(orderId); // Error - OrderId !== UserId

// 5. Prefer interfaces for public APIs
export interface UserService {
  getUser(id: string): Promise<User>;
  createUser(data: CreateUserDto): Promise<User>;
  updateUser(id: string, data: UpdateUserDto): Promise<User>;
  deleteUser(id: string): Promise<void>;
}

// 6. Use readonly for immutable data
interface ImmutableConfig {
  readonly host: string;
  readonly port: number;
  readonly options: Readonly<{
    timeout: number;
    retries: number;
  }>;
}

// 7. Document complex types with JSDoc
/**
 * Represents the result of an async operation
 * @template T - The type of data on success
 * @template E - The type of error on failure
 */
type Result<T, E = Error> =
  | { success: true; data: T }
  | { success: false; error: E };
```

---

## Configuración de tsconfig.json

### Configuración Recomendada Base

```json
{
  "$schema": "https://json.schemastore.org/tsconfig",
  "compilerOptions": {
    // Type Checking
    "strict": true,
    "noUncheckedIndexedAccess": true,
    "noImplicitReturns": true,
    "noFallthroughCasesInSwitch": true,
    "noUnusedLocals": true,
    "noUnusedParameters": true,
    "exactOptionalPropertyTypes": true,

    // Modules
    "module": "NodeNext",
    "moduleResolution": "NodeNext",
    "resolveJsonModule": true,
    "esModuleInterop": true,
    "isolatedModules": true,

    // Emit
    "target": "ES2022",
    "lib": ["ES2022"],
    "outDir": "./dist",
    "declaration": true,
    "declarationMap": true,
    "sourceMap": true,

    // Paths
    "baseUrl": ".",
    "paths": {
      "@/*": ["src/*"],
      "@/utils/*": ["src/utils/*"],
      "@/types/*": ["src/types/*"]
    },

    // Other
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "dist", "**/*.test.ts"]
}
```

### Configuración para Diferentes Entornos

```json
// tsconfig.node.json - For Node.js backend
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": {
    "module": "NodeNext",
    "moduleResolution": "NodeNext",
    "target": "ES2022",
    "lib": ["ES2022"],
    "types": ["node"]
  }
}
```

```json
// tsconfig.browser.json - For browser/frontend
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": {
    "module": "ESNext",
    "moduleResolution": "Bundler",
    "target": "ES2020",
    "lib": ["ES2020", "DOM", "DOM.Iterable"],
    "jsx": "react-jsx"
  }
}
```

```json
// tsconfig.lib.json - For library development
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": {
    "declaration": true,
    "declarationDir": "./types",
    "emitDeclarationOnly": false,
    "composite": true
  }
}
```

### Tabla de Opciones Importantes

| Opción                           | Descripción                            | Valor Recomendado |
| -------------------------------- | -------------------------------------- | ----------------- |
| `strict`                         | Habilita todas las opciones estrictas  | `true`            |
| `noUncheckedIndexedAccess`       | Index accesses return `T \| undefined` | `true`            |
| `noImplicitReturns`              | Error si no todos los paths retornan   | `true`            |
| `exactOptionalPropertyTypes`     | Diferencia `undefined` de opcional     | `true`            |
| `isolatedModules`                | Asegura compatibilidad con bundlers    | `true`            |
| `skipLibCheck`                   | Omite verificación de .d.ts            | `true`            |
| `forceConsistentCasingInFileNames` | Previene errores de case en imports  | `true`            |

---

## Patrones Avanzados

### Template Literal Types

```typescript
// Route type safety
type HttpMethod = "GET" | "POST" | "PUT" | "DELETE" | "PATCH";
type Route = `/${string}`;

type ApiEndpoint = `${HttpMethod} ${Route}`;

function registerRoute(endpoint: ApiEndpoint, handler: () => void) {
  // ...
}

registerRoute("GET /users", () => {}); // Valid
registerRoute("POST /users/:id", () => {}); // Valid
// registerRoute("INVALID /users", () => {}); // Error

// CSS-in-JS type safety
type CSSUnit = "px" | "em" | "rem" | "%" | "vh" | "vw";
type CSSValue = `${number}${CSSUnit}`;

interface Spacing {
  margin: CSSValue;
  padding: CSSValue;
}

const spacing: Spacing = {
  margin: "16px",
  padding: "1.5rem",
};
```

### Tipos Condicionales Avanzados

```typescript
// Infer keyword
type GetArrayElement<T> = T extends (infer E)[] ? E : never;
type Numbers = GetArrayElement<number[]>; // number

type GetPromiseValue<T> = T extends Promise<infer V> ? V : never;
type Value = GetPromiseValue<Promise<string>>; // string

type GetFunctionArgs<T> = T extends (...args: infer A) => any ? A : never;
type Args = GetFunctionArgs<(a: string, b: number) => void>; // [string, number]

// Distributive conditional types
type ToArray<T> = T extends any ? T[] : never;
type StrOrNumArray = ToArray<string | number>; // string[] | number[]

// Non-distributive version
type ToArrayNonDist<T> = [T] extends [any] ? T[] : never;
type MixedArray = ToArrayNonDist<string | number>; // (string | number)[]

// Recursive conditional types
type Flatten<T> = T extends Array<infer U> ? Flatten<U> : T;
type Nested = number[][][];
type Flat = Flatten<Nested>; // number
```

### Variadic Tuple Types

```typescript
// Spread in tuple types
type Concat<T extends unknown[], U extends unknown[]> = [...T, ...U];
type Combined = Concat<[1, 2], [3, 4]>; // [1, 2, 3, 4]

// Typed function composition
type Last<T extends unknown[]> = T extends [...infer _, infer L] ? L : never;
type First<T extends unknown[]> = T extends [infer F, ...infer _] ? F : never;

type Pop<T extends unknown[]> = T extends [...infer R, infer _] ? R : never;
type Shift<T extends unknown[]> = T extends [infer _, ...infer R] ? R : never;

type Push<T extends unknown[], V> = [...T, V];
type Unshift<T extends unknown[], V> = [V, ...T];

// Example usage
type Original = [1, 2, 3, 4, 5];
type WithoutLast = Pop<Original>; // [1, 2, 3, 4]
type WithoutFirst = Shift<Original>; // [2, 3, 4, 5]
type WithExtra = Push<Original, 6>; // [1, 2, 3, 4, 5, 6]
```

---

## Referencias y Recursos

### Documentación Oficial

- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/)
- [TypeScript Deep Dive](https://basarat.gitbook.io/typescript/)
- [Type Challenges](https://github.com/type-challenges/type-challenges)

### Herramientas Recomendadas

| Herramienta | Propósito                    |
| ----------- | ---------------------------- |
| `ts-node`   | Ejecución directa de TS      |
| `tsx`       | Alternativa rápida a ts-node |
| `tsc`       | Compilador oficial           |
| `tsup`      | Bundler para bibliotecas     |
| `vitest`    | Testing con soporte nativo   |

---

## Notas de ARCHAEON

> Este documento forma parte del módulo ARCHAEON especializado en la evolución
> de lenguajes legacy hacia tecnologías modernas. TypeScript representa el
> puente entre JavaScript dinámico y sistemas de tipos estáticos robustos.

**Próximo documento:** [TYPESCRIPT_02_NODE.md](./TYPESCRIPT_02_NODE.md) - Integración con Node.js y FFI
