# PROTO_01: ANÁLISIS DE CÓDIGO LEGACY

> *"Antes de traducir, primero debemos comprender. El código ancestral guarda secretos que solo la paciencia y el análisis metódico pueden revelar."*

## Índice
1. [Principios del Análisis](#1-principios-del-análisis)
2. [Análisis Léxico](#2-análisis-léxico)
3. [Análisis Sintáctico](#3-análisis-sintáctico)
4. [Análisis Semántico](#4-análisis-semántico)
5. [Análisis de Flujo de Datos](#5-análisis-de-flujo-de-datos)
6. [Análisis de Flujo de Control](#6-análisis-de-flujo-de-control)
7. [Métricas de Código](#7-métricas-de-código)
8. [Herramientas de Análisis](#8-herramientas-de-análisis)

---

## 1. Principios del Análisis

### 1.1 El Arte de Leer Código Ancestral

```yaml
FILOSOFÍA_ANÁLISIS:
  objetivo: "Comprender antes de transformar"

  principios:
    - Código legacy contiene sabiduría acumulada
    - Cada decisión tuvo razón en su contexto
    - El contexto histórico es crucial
    - Documentación puede estar desactualizada
    - El código es la única verdad definitiva

  niveles_comprensión:
    1_sintáctico: "¿Qué dice el código?"
    2_semántico: "¿Qué significa?"
    3_pragmático: "¿Por qué se escribió así?"
    4_histórico: "¿Qué restricciones existían?"
```

### 1.2 Metodología de Análisis

```
PROCESO DE ANÁLISIS SISTEMÁTICO
================================

┌─────────────────┐
│  1. INVENTARIO  │  ← Catálogo de artefactos
├─────────────────┤
│  2. ESTRUCTURA  │  ← Arquitectura macro
├─────────────────┤
│  3. DEPENDENCIAS│  ← Grafos de llamadas
├─────────────────┤
│  4. DATOS       │  ← Estructuras y flujo
├─────────────────┤
│  5. CONTROL     │  ← Lógica y decisiones
├─────────────────┤
│  6. PATRONES    │  ← Identificar idiomas
├─────────────────┤
│  7. ANOMALÍAS   │  ← Casos especiales
├─────────────────┤
│  8. DOCUMENTAR  │  ← Conocimiento extraído
└─────────────────┘
```

### 1.3 Desafíos del Código Legacy

```yaml
DESAFÍOS_COMUNES:

  COBOL:
    - GOTO spaghetti
    - PERFORM THRU sin estructura
    - Copybooks anidados
    - Lógica en JCL
    - Código muerto acumulado
    - Variables con nombres crípticos (WS01-A1)

  FORTRAN:
    - COMMON blocks compartidos
    - EQUIVALENCE ocultos
    - GOTOs computados
    - Arrays con límites implícitos
    - ENTRY points múltiples

  ASSEMBLY:
    - Auto-modificación de código
    - Direccionamiento indirecto
    - Interrupciones y handlers
    - Timing-dependent code

  C_LEGACY:
    - Macros complejos
    - Punteros void*
    - Casting implícito
    - Buffer overflows potenciales
```

---

## 2. Análisis Léxico

### 2.1 Tokenización por Lenguaje

```python
"""
ANÁLISIS LÉXICO - Descomposición en tokens
==========================================

El primer paso es identificar los elementos atómicos del código.
"""

# Tokens comunes en lenguajes legacy
TOKEN_TYPES = {
    'KEYWORD': ['IF', 'THEN', 'ELSE', 'DO', 'WHILE', 'PERFORM', 'CALL'],
    'IDENTIFIER': r'[A-Z][A-Z0-9\-]*',
    'NUMBER': r'\d+(\.\d+)?',
    'STRING': r'"[^"]*"',
    'OPERATOR': ['+', '-', '*', '/', '=', '<', '>', '**'],
    'DELIMITER': ['.', ',', ';', '(', ')', ':'],
    'COMMENT': {
        'COBOL': r'^\*.*$|^\s{6}\*.*$',
        'FORTRAN': r'^[Cc\*].*$|!.*$',
        'ASM': r';.*$',
    }
}

class LexerCobol:
    """Analizador léxico para COBOL"""

    def __init__(self, source: str):
        self.source = source
        self.pos = 0
        self.tokens = []

    def tokenize(self):
        """Descomponer código en tokens"""
        lines = self.source.split('\n')

        for line_num, line in enumerate(lines, 1):
            # COBOL: columnas 7-72 son código
            if len(line) < 7:
                continue

            indicator = line[6] if len(line) > 6 else ' '

            # Comentario
            if indicator == '*':
                self.tokens.append({
                    'type': 'COMMENT',
                    'value': line[7:72].rstrip(),
                    'line': line_num
                })
                continue

            # Continuación
            if indicator == '-':
                # Anexar a token anterior
                pass

            # Código
            code = line[7:72] if len(line) > 7 else ''
            self._tokenize_line(code, line_num)

        return self.tokens

    def _tokenize_line(self, line: str, line_num: int):
        """Tokenizar una línea de código COBOL"""
        i = 0
        while i < len(line):
            # Saltar espacios
            if line[i].isspace():
                i += 1
                continue

            # String literal
            if line[i] == '"' or line[i] == "'":
                end = line.find(line[i], i + 1)
                self.tokens.append({
                    'type': 'STRING',
                    'value': line[i:end+1],
                    'line': line_num
                })
                i = end + 1
                continue

            # Número
            if line[i].isdigit():
                j = i
                while j < len(line) and (line[j].isdigit() or line[j] == '.'):
                    j += 1
                self.tokens.append({
                    'type': 'NUMBER',
                    'value': line[i:j],
                    'line': line_num
                })
                i = j
                continue

            # Identificador o palabra clave
            if line[i].isalpha():
                j = i
                while j < len(line) and (line[j].isalnum() or line[j] == '-'):
                    j += 1
                word = line[i:j].upper()
                token_type = 'KEYWORD' if word in COBOL_KEYWORDS else 'IDENTIFIER'
                self.tokens.append({
                    'type': token_type,
                    'value': word,
                    'line': line_num
                })
                i = j
                continue

            # Operadores y delimitadores
            self.tokens.append({
                'type': 'SYMBOL',
                'value': line[i],
                'line': line_num
            })
            i += 1

COBOL_KEYWORDS = {
    'IDENTIFICATION', 'DIVISION', 'PROGRAM-ID', 'ENVIRONMENT',
    'CONFIGURATION', 'DATA', 'WORKING-STORAGE', 'SECTION',
    'PROCEDURE', 'MOVE', 'TO', 'ADD', 'SUBTRACT', 'MULTIPLY',
    'DIVIDE', 'IF', 'ELSE', 'END-IF', 'PERFORM', 'UNTIL',
    'VARYING', 'CALL', 'USING', 'STOP', 'RUN', 'DISPLAY',
    'ACCEPT', 'OPEN', 'CLOSE', 'READ', 'WRITE', 'REWRITE'
}
```

### 2.2 Normalización de Código

```python
"""
NORMALIZACIÓN - Preparar código para análisis
=============================================
"""

class Normalizer:
    """Normaliza código legacy para análisis uniforme"""

    @staticmethod
    def normalize_cobol(source: str) -> str:
        """Normalizar código COBOL"""
        lines = source.split('\n')
        normalized = []

        for line in lines:
            # Expandir tabuladores
            line = line.expandtabs(4)

            # Asegurar 80 columnas
            if len(line) < 80:
                line = line.ljust(80)

            # Eliminar números de secuencia (cols 1-6)
            code = line[6:72]

            # Convertir a mayúsculas (COBOL es case-insensitive)
            code = code.upper()

            normalized.append(code)

        return '\n'.join(normalized)

    @staticmethod
    def normalize_fortran(source: str) -> str:
        """Normalizar código FORTRAN"""
        lines = source.split('\n')
        normalized = []
        continuation_line = ""

        for line in lines:
            # FORTRAN 77: columna 6 para continuación
            if len(line) > 5 and line[5] not in ' 0':
                # Línea de continuación
                continuation_line += line[6:72].strip()
                continue

            if continuation_line:
                normalized.append(continuation_line)
                continuation_line = ""

            # Comentario
            if line and line[0] in 'Cc*!':
                normalized.append('!' + line[1:])
                continue

            # Código normal
            if len(line) > 6:
                normalized.append(line[6:72].strip().upper())

        return '\n'.join(normalized)

    @staticmethod
    def expand_copybooks(source: str, copybook_dir: str) -> str:
        """Expandir COPY statements en COBOL"""
        import re
        import os

        pattern = r'COPY\s+(\S+)\s*\.'

        def replace_copy(match):
            copybook_name = match.group(1)
            copybook_path = os.path.join(copybook_dir, copybook_name + '.cpy')

            if os.path.exists(copybook_path):
                with open(copybook_path, 'r') as f:
                    return f'*> BEGIN COPY {copybook_name}\n{f.read()}\n*> END COPY {copybook_name}'
            else:
                return f'*> COPY {copybook_name} NOT FOUND'

        return re.sub(pattern, replace_copy, source, flags=re.IGNORECASE)
```

---

## 3. Análisis Sintáctico

### 3.1 Construcción del AST

```python
"""
ANÁLISIS SINTÁCTICO - Construcción de Árbol de Sintaxis Abstracta
================================================================
"""

from dataclasses import dataclass
from typing import List, Optional, Union

@dataclass
class ASTNode:
    """Nodo base del AST"""
    node_type: str
    line: int
    children: List['ASTNode'] = None

    def __post_init__(self):
        if self.children is None:
            self.children = []

@dataclass
class ProgramNode(ASTNode):
    """Nodo raíz del programa"""
    name: str
    divisions: List['DivisionNode']

@dataclass
class DivisionNode(ASTNode):
    """División COBOL"""
    name: str  # IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE
    sections: List['SectionNode']

@dataclass
class SectionNode(ASTNode):
    """Sección dentro de una división"""
    name: str
    paragraphs: List['ParagraphNode']

@dataclass
class ParagraphNode(ASTNode):
    """Párrafo (subrutina) en PROCEDURE DIVISION"""
    name: str
    statements: List['StatementNode']

@dataclass
class StatementNode(ASTNode):
    """Statement individual"""
    verb: str  # MOVE, IF, PERFORM, etc.
    operands: List[Union[str, 'ExpressionNode']]

@dataclass
class ExpressionNode(ASTNode):
    """Expresión aritmética o condicional"""
    operator: str
    left: Union[str, 'ExpressionNode']
    right: Union[str, 'ExpressionNode']

@dataclass
class DataItemNode(ASTNode):
    """Definición de dato"""
    level: int
    name: str
    picture: Optional[str]
    value: Optional[str]
    occurs: Optional[int]
    redefines: Optional[str]


class CobolParser:
    """Parser para COBOL"""

    def __init__(self, tokens: List[dict]):
        self.tokens = tokens
        self.pos = 0

    def parse(self) -> ProgramNode:
        """Parsear programa completo"""
        divisions = []
        program_name = None

        while not self.is_at_end():
            if self.match('IDENTIFICATION'):
                self.expect('DIVISION')
                self.expect('.')
                program_name = self._parse_identification()
                divisions.append(DivisionNode(
                    node_type='DIVISION',
                    line=self.current_line(),
                    name='IDENTIFICATION',
                    sections=[]
                ))

            elif self.match('ENVIRONMENT'):
                self.expect('DIVISION')
                self.expect('.')
                divisions.append(self._parse_environment())

            elif self.match('DATA'):
                self.expect('DIVISION')
                self.expect('.')
                divisions.append(self._parse_data())

            elif self.match('PROCEDURE'):
                self.expect('DIVISION')
                self.expect('.')
                divisions.append(self._parse_procedure())

            else:
                self.advance()

        return ProgramNode(
            node_type='PROGRAM',
            line=1,
            name=program_name or 'UNKNOWN',
            divisions=divisions
        )

    def _parse_procedure(self) -> DivisionNode:
        """Parsear PROCEDURE DIVISION"""
        sections = []
        current_paragraph = None
        statements = []

        while not self.is_at_end():
            # Nuevo párrafo
            if self._is_paragraph_start():
                if current_paragraph:
                    sections.append(ParagraphNode(
                        node_type='PARAGRAPH',
                        line=current_paragraph['line'],
                        name=current_paragraph['name'],
                        statements=statements
                    ))
                    statements = []

                current_paragraph = {
                    'name': self.current_value(),
                    'line': self.current_line()
                }
                self.advance()
                self.expect('.')
                continue

            # Statement
            stmt = self._parse_statement()
            if stmt:
                statements.append(stmt)

        # Último párrafo
        if current_paragraph:
            sections.append(ParagraphNode(
                node_type='PARAGRAPH',
                line=current_paragraph['line'],
                name=current_paragraph['name'],
                statements=statements
            ))

        return DivisionNode(
            node_type='DIVISION',
            line=self.current_line(),
            name='PROCEDURE',
            sections=sections
        )

    def _parse_statement(self) -> Optional[StatementNode]:
        """Parsear un statement"""
        if self.check('MOVE'):
            return self._parse_move()
        elif self.check('IF'):
            return self._parse_if()
        elif self.check('PERFORM'):
            return self._parse_perform()
        elif self.check('CALL'):
            return self._parse_call()
        # ... más statements
        return None

    def _parse_move(self) -> StatementNode:
        """Parsear MOVE statement"""
        self.expect('MOVE')
        source = self.current_value()
        self.advance()
        self.expect('TO')
        targets = []
        while not self.check('.'):
            targets.append(self.current_value())
            self.advance()
        self.expect('.')

        return StatementNode(
            node_type='STATEMENT',
            line=self.current_line(),
            verb='MOVE',
            operands=[source] + targets
        )

    # Métodos auxiliares
    def match(self, *types) -> bool:
        for t in types:
            if self.check(t):
                self.advance()
                return True
        return False

    def check(self, token_type: str) -> bool:
        if self.is_at_end():
            return False
        return self.tokens[self.pos]['value'] == token_type

    def advance(self):
        if not self.is_at_end():
            self.pos += 1

    def expect(self, token_type: str):
        if not self.match(token_type):
            raise SyntaxError(f"Expected {token_type}")

    def is_at_end(self) -> bool:
        return self.pos >= len(self.tokens)

    def current_value(self) -> str:
        return self.tokens[self.pos]['value']

    def current_line(self) -> int:
        return self.tokens[self.pos]['line']
```

### 3.2 Visitor Pattern para AST

```python
"""
VISITOR PATTERN - Recorrer y procesar el AST
============================================
"""

from abc import ABC, abstractmethod

class ASTVisitor(ABC):
    """Visitor base para recorrer AST"""

    def visit(self, node: ASTNode):
        """Despachar a método específico según tipo de nodo"""
        method_name = f'visit_{node.node_type.lower()}'
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node: ASTNode):
        """Visita genérica - recorrer hijos"""
        for child in node.children:
            self.visit(child)


class CallGraphBuilder(ASTVisitor):
    """Construir grafo de llamadas"""

    def __init__(self):
        self.current_paragraph = None
        self.calls = []  # (caller, callee)

    def visit_paragraph(self, node: ParagraphNode):
        self.current_paragraph = node.name
        for stmt in node.statements:
            self.visit(stmt)

    def visit_statement(self, node: StatementNode):
        if node.verb == 'PERFORM':
            target = node.operands[0]
            self.calls.append((self.current_paragraph, target))
        elif node.verb == 'CALL':
            target = node.operands[0]
            self.calls.append((self.current_paragraph, f'EXTERNAL:{target}'))


class VariableCollector(ASTVisitor):
    """Recolectar todas las variables"""

    def __init__(self):
        self.variables = {}  # name -> info

    def visit_dataitem(self, node: DataItemNode):
        self.variables[node.name] = {
            'level': node.level,
            'picture': node.picture,
            'line': node.line,
            'occurs': node.occurs,
            'redefines': node.redefines
        }


class ComplexityCalculator(ASTVisitor):
    """Calcular complejidad ciclomática"""

    def __init__(self):
        self.complexity = 1  # Base

    def visit_statement(self, node: StatementNode):
        # Cada decisión aumenta complejidad
        if node.verb in ('IF', 'EVALUATE'):
            self.complexity += 1
        elif node.verb == 'PERFORM' and 'UNTIL' in str(node.operands):
            self.complexity += 1
```

---

## 4. Análisis Semántico

### 4.1 Tabla de Símbolos

```python
"""
ANÁLISIS SEMÁNTICO - Significado y tipos
========================================
"""

from enum import Enum
from dataclasses import dataclass
from typing import Dict, List, Optional

class DataType(Enum):
    """Tipos de datos abstractos"""
    NUMERIC = 'numeric'
    ALPHANUMERIC = 'alphanumeric'
    ALPHABETIC = 'alphabetic'
    GROUP = 'group'
    INDEX = 'index'
    POINTER = 'pointer'

@dataclass
class Symbol:
    """Entrada en tabla de símbolos"""
    name: str
    data_type: DataType
    size: int
    decimal_places: int = 0
    level: int = 1
    parent: Optional[str] = None
    is_signed: bool = False
    is_array: bool = False
    array_size: int = 1
    redefines: Optional[str] = None
    usage: str = 'DISPLAY'  # DISPLAY, COMP, COMP-3, etc.
    line_defined: int = 0
    lines_referenced: List[int] = None

class SymbolTable:
    """Tabla de símbolos con scopes"""

    def __init__(self):
        self.symbols: Dict[str, Symbol] = {}
        self.hierarchy: Dict[str, List[str]] = {}  # padre -> hijos

    def define(self, symbol: Symbol):
        """Agregar símbolo a la tabla"""
        if symbol.name in self.symbols:
            raise SemanticError(f"Symbol {symbol.name} already defined")

        self.symbols[symbol.name] = symbol

        # Construir jerarquía
        if symbol.parent:
            if symbol.parent not in self.hierarchy:
                self.hierarchy[symbol.parent] = []
            self.hierarchy[symbol.parent].append(symbol.name)

    def lookup(self, name: str) -> Optional[Symbol]:
        """Buscar símbolo"""
        return self.symbols.get(name)

    def reference(self, name: str, line: int):
        """Registrar referencia a símbolo"""
        symbol = self.lookup(name)
        if symbol:
            if symbol.lines_referenced is None:
                symbol.lines_referenced = []
            symbol.lines_referenced.append(line)

    def get_type(self, name: str) -> Optional[DataType]:
        """Obtener tipo de símbolo"""
        symbol = self.lookup(name)
        return symbol.data_type if symbol else None

    def get_size(self, name: str) -> int:
        """Obtener tamaño total incluyendo hijos"""
        symbol = self.lookup(name)
        if not symbol:
            return 0

        if symbol.name in self.hierarchy:
            # Es grupo - sumar tamaños de hijos
            total = sum(self.get_size(child)
                       for child in self.hierarchy[symbol.name])
            return total * symbol.array_size
        else:
            return symbol.size * symbol.array_size


class PictureParser:
    """Parsear PICTURE clauses de COBOL"""

    @staticmethod
    def parse(picture: str) -> tuple:
        """
        Retorna (tipo, tamaño, decimales, signed)

        Ejemplos:
            9(5)     -> (NUMERIC, 5, 0, False)
            9(5)V99  -> (NUMERIC, 7, 2, False)
            S9(7)V99 -> (NUMERIC, 9, 2, True)
            X(20)    -> (ALPHANUMERIC, 20, 0, False)
            A(10)    -> (ALPHABETIC, 10, 0, False)
        """
        pic = picture.upper().replace(' ', '')

        signed = pic.startswith('S')
        if signed:
            pic = pic[1:]

        data_type = DataType.NUMERIC
        if 'X' in pic:
            data_type = DataType.ALPHANUMERIC
        elif 'A' in pic:
            data_type = DataType.ALPHABETIC

        # Expandir repeticiones
        import re
        expanded = re.sub(r'(\w)\((\d+)\)',
                         lambda m: m.group(1) * int(m.group(2)),
                         pic)

        # Contar dígitos y decimales
        total_size = len(expanded.replace('V', '').replace('.', ''))

        decimal_pos = expanded.find('V')
        if decimal_pos == -1:
            decimal_pos = expanded.find('.')

        decimals = 0
        if decimal_pos != -1:
            decimals = len(expanded) - decimal_pos - 1

        return (data_type, total_size, decimals, signed)
```

### 4.2 Verificación de Tipos

```python
"""
VERIFICACIÓN DE TIPOS
=====================
"""

class TypeChecker(ASTVisitor):
    """Verificar compatibilidad de tipos"""

    def __init__(self, symbol_table: SymbolTable):
        self.symbols = symbol_table
        self.errors = []

    def visit_statement(self, node: StatementNode):
        if node.verb == 'MOVE':
            self._check_move(node)
        elif node.verb == 'ADD':
            self._check_arithmetic(node)
        elif node.verb == 'COMPUTE':
            self._check_compute(node)

    def _check_move(self, node: StatementNode):
        """Verificar MOVE: source TO target"""
        source = node.operands[0]
        targets = node.operands[1:]

        source_type = self._get_type(source)

        for target in targets:
            target_type = self._get_type(target)

            if not self._compatible_move(source_type, target_type):
                self.errors.append({
                    'type': 'TYPE_MISMATCH',
                    'line': node.line,
                    'message': f"Incompatible MOVE from {source} ({source_type}) to {target} ({target_type})"
                })

    def _check_arithmetic(self, node: StatementNode):
        """Verificar operaciones aritméticas"""
        for operand in node.operands:
            op_type = self._get_type(operand)
            if op_type and op_type != DataType.NUMERIC:
                self.errors.append({
                    'type': 'TYPE_ERROR',
                    'line': node.line,
                    'message': f"Arithmetic operation requires numeric type, got {op_type} for {operand}"
                })

    def _get_type(self, name: str) -> Optional[DataType]:
        """Obtener tipo de variable o literal"""
        # Literal numérico
        if name.replace('.', '').replace('-', '').isdigit():
            return DataType.NUMERIC
        # Literal alfanumérico
        if name.startswith('"') or name.startswith("'"):
            return DataType.ALPHANUMERIC
        # Variable
        return self.symbols.get_type(name)

    def _compatible_move(self, source: DataType, target: DataType) -> bool:
        """Verificar si MOVE es válido según tipos"""
        if source is None or target is None:
            return True  # No podemos verificar

        # Reglas de compatibilidad COBOL
        compatible = {
            DataType.NUMERIC: {DataType.NUMERIC, DataType.ALPHANUMERIC},
            DataType.ALPHANUMERIC: {DataType.ALPHANUMERIC, DataType.NUMERIC},
            DataType.ALPHABETIC: {DataType.ALPHABETIC, DataType.ALPHANUMERIC},
        }

        return target in compatible.get(source, set())
```

---

## 5. Análisis de Flujo de Datos

### 5.1 Definición y Uso de Variables

```python
"""
ANÁLISIS DE FLUJO DE DATOS
==========================

Tracking de dónde se definen (escriben) y usan (leen) las variables.
"""

from dataclasses import dataclass
from typing import Set, Dict, List
from enum import Enum

class AccessType(Enum):
    READ = 'read'
    WRITE = 'write'
    BOTH = 'both'

@dataclass
class DataFlowInfo:
    """Información de flujo de datos para un statement"""
    line: int
    statement_type: str
    gen: Set[str]   # Variables definidas (escritas)
    kill: Set[str]  # Definiciones previas eliminadas
    use: Set[str]   # Variables usadas (leídas)

class DataFlowAnalyzer(ASTVisitor):
    """Analizar flujo de datos"""

    def __init__(self):
        self.flow_info: List[DataFlowInfo] = []
        self.variable_access: Dict[str, List[tuple]] = {}  # var -> [(line, access_type)]

    def analyze_statement(self, node: StatementNode) -> DataFlowInfo:
        """Analizar un statement"""
        gen = set()
        use = set()

        if node.verb == 'MOVE':
            # MOVE source TO target1 target2
            use.add(self._extract_var(node.operands[0]))
            for target in node.operands[1:]:
                gen.add(self._extract_var(target))

        elif node.verb in ('ADD', 'SUBTRACT'):
            # ADD a b TO c GIVING d
            for i, op in enumerate(node.operands):
                var = self._extract_var(op)
                if i == len(node.operands) - 1:  # Último es destino
                    gen.add(var)
                    if 'GIVING' not in str(node.operands):
                        use.add(var)  # También se lee si no hay GIVING
                else:
                    use.add(var)

        elif node.verb == 'READ':
            # READ file INTO record
            if len(node.operands) > 1:
                gen.add(self._extract_var(node.operands[1]))

        elif node.verb == 'ACCEPT':
            gen.add(self._extract_var(node.operands[0]))

        elif node.verb == 'DISPLAY':
            for op in node.operands:
                use.add(self._extract_var(op))

        elif node.verb == 'IF':
            # Condición - todas las variables son leídas
            for op in node.operands:
                if self._is_variable(op):
                    use.add(self._extract_var(op))

        # Registrar accesos
        for var in gen:
            if var:
                self._record_access(var, node.line, AccessType.WRITE)
        for var in use:
            if var:
                self._record_access(var, node.line, AccessType.READ)

        return DataFlowInfo(
            line=node.line,
            statement_type=node.verb,
            gen=gen,
            kill=gen,  # Simplificado
            use=use
        )

    def _extract_var(self, operand) -> str:
        """Extraer nombre de variable"""
        if isinstance(operand, str):
            # Limpiar calificadores (OF, IN)
            parts = operand.split()
            return parts[0] if parts else operand
        return str(operand)

    def _is_variable(self, operand) -> bool:
        """Verificar si es variable (no literal)"""
        s = str(operand)
        return not (s.isdigit() or s.startswith('"') or s.startswith("'"))

    def _record_access(self, var: str, line: int, access_type: AccessType):
        """Registrar acceso a variable"""
        if var not in self.variable_access:
            self.variable_access[var] = []
        self.variable_access[var].append((line, access_type))

    def find_unused_variables(self, symbol_table: SymbolTable) -> List[str]:
        """Encontrar variables definidas pero nunca usadas"""
        unused = []
        for var_name in symbol_table.symbols:
            if var_name not in self.variable_access:
                unused.append(var_name)
            else:
                accesses = self.variable_access[var_name]
                has_read = any(a[1] == AccessType.READ for a in accesses)
                if not has_read:
                    unused.append(var_name)
        return unused

    def find_uninitialized_reads(self) -> List[tuple]:
        """Encontrar lecturas antes de escritura"""
        issues = []
        for var, accesses in self.variable_access.items():
            first_access = min(accesses, key=lambda x: x[0])
            if first_access[1] == AccessType.READ:
                issues.append((var, first_access[0]))
        return issues
```

### 5.2 Reaching Definitions

```python
"""
REACHING DEFINITIONS
====================

¿Qué definiciones de variables pueden llegar a cada punto del programa?
"""

class ReachingDefinitions:
    """Análisis de reaching definitions"""

    def __init__(self, cfg: 'ControlFlowGraph'):
        self.cfg = cfg
        self.in_sets: Dict[int, Set] = {}   # block_id -> set of definitions
        self.out_sets: Dict[int, Set] = {}

    def analyze(self):
        """Calcular reaching definitions usando iteración de punto fijo"""
        # Inicializar
        for block in self.cfg.blocks:
            self.in_sets[block.id] = set()
            self.out_sets[block.id] = set()

        changed = True
        iterations = 0
        max_iterations = 1000

        while changed and iterations < max_iterations:
            changed = False
            iterations += 1

            for block in self.cfg.blocks:
                # IN[B] = union de OUT[P] para todos predecesores P
                new_in = set()
                for pred in self.cfg.predecessors(block.id):
                    new_in |= self.out_sets[pred]

                # OUT[B] = GEN[B] union (IN[B] - KILL[B])
                new_out = block.gen | (new_in - block.kill)

                if new_out != self.out_sets[block.id]:
                    changed = True
                    self.out_sets[block.id] = new_out
                    self.in_sets[block.id] = new_in

        return iterations

    def definitions_at(self, block_id: int) -> Set:
        """Obtener definiciones que llegan a un bloque"""
        return self.in_sets.get(block_id, set())
```

---

## 6. Análisis de Flujo de Control

### 6.1 Grafo de Flujo de Control (CFG)

```python
"""
CONTROL FLOW GRAPH
==================
"""

from dataclasses import dataclass
from typing import List, Set, Optional

@dataclass
class BasicBlock:
    """Bloque básico - secuencia sin bifurcaciones"""
    id: int
    statements: List[StatementNode]
    entry_line: int
    exit_line: int

    # Para análisis de datos
    gen: Set[str] = None    # Definiciones generadas
    kill: Set[str] = None   # Definiciones eliminadas

@dataclass
class CFGEdge:
    """Arista en el CFG"""
    from_block: int
    to_block: int
    condition: Optional[str] = None  # Para branches condicionales

class ControlFlowGraph:
    """Grafo de flujo de control"""

    def __init__(self):
        self.blocks: List[BasicBlock] = []
        self.edges: List[CFGEdge] = []
        self.entry_block: int = 0
        self.exit_blocks: List[int] = []

    def add_block(self, block: BasicBlock):
        """Agregar bloque básico"""
        self.blocks.append(block)

    def add_edge(self, from_id: int, to_id: int, condition: str = None):
        """Agregar arista"""
        self.edges.append(CFGEdge(from_id, to_id, condition))

    def predecessors(self, block_id: int) -> List[int]:
        """Obtener predecesores de un bloque"""
        return [e.from_block for e in self.edges if e.to_block == block_id]

    def successors(self, block_id: int) -> List[int]:
        """Obtener sucesores de un bloque"""
        return [e.to_block for e in self.edges if e.from_block == block_id]

    def dominators(self) -> Dict[int, Set[int]]:
        """Calcular dominadores de cada bloque"""
        dom = {b.id: set(b.id for b in self.blocks) for b in self.blocks}
        dom[self.entry_block] = {self.entry_block}

        changed = True
        while changed:
            changed = False
            for block in self.blocks:
                if block.id == self.entry_block:
                    continue

                preds = self.predecessors(block.id)
                if not preds:
                    continue

                new_dom = set.intersection(*[dom[p] for p in preds])
                new_dom.add(block.id)

                if new_dom != dom[block.id]:
                    dom[block.id] = new_dom
                    changed = True

        return dom

    def find_loops(self) -> List[tuple]:
        """Encontrar loops naturales"""
        dom = self.dominators()
        loops = []

        for edge in self.edges:
            # Back edge: to_block domina from_block
            if edge.to_block in dom[edge.from_block]:
                # Encontrar cuerpo del loop
                loop_body = self._find_loop_body(edge.to_block, edge.from_block)
                loops.append((edge.to_block, loop_body))

        return loops

    def _find_loop_body(self, header: int, tail: int) -> Set[int]:
        """Encontrar todos los bloques en un loop"""
        body = {header, tail}
        stack = [tail]

        while stack:
            block = stack.pop()
            for pred in self.predecessors(block):
                if pred not in body:
                    body.add(pred)
                    stack.append(pred)

        return body


class CFGBuilder:
    """Construir CFG desde AST"""

    def __init__(self):
        self.cfg = ControlFlowGraph()
        self.block_counter = 0

    def build(self, procedure_node: DivisionNode) -> ControlFlowGraph:
        """Construir CFG de PROCEDURE DIVISION"""
        current_statements = []
        entry_line = 1

        for section in procedure_node.sections:
            for para in section.paragraphs:
                for stmt in para.statements:
                    if self._is_leader(stmt):
                        # Terminar bloque anterior
                        if current_statements:
                            block = self._create_block(
                                current_statements,
                                entry_line
                            )
                            self.cfg.add_block(block)

                        current_statements = [stmt]
                        entry_line = stmt.line
                    else:
                        current_statements.append(stmt)

                    # Agregar edges según tipo de statement
                    self._handle_control_flow(stmt)

        # Último bloque
        if current_statements:
            block = self._create_block(current_statements, entry_line)
            self.cfg.add_block(block)

        return self.cfg

    def _is_leader(self, stmt: StatementNode) -> bool:
        """¿Este statement comienza un nuevo bloque?"""
        return stmt.verb in ('IF', 'EVALUATE', 'PERFORM', 'GO')

    def _create_block(self, statements: List, entry_line: int) -> BasicBlock:
        """Crear bloque básico"""
        block = BasicBlock(
            id=self.block_counter,
            statements=statements,
            entry_line=entry_line,
            exit_line=statements[-1].line if statements else entry_line
        )
        self.block_counter += 1
        return block
```

---

## 7. Métricas de Código

### 7.1 Métricas de Complejidad

```python
"""
MÉTRICAS DE CÓDIGO LEGACY
=========================
"""

from dataclasses import dataclass
from typing import Dict

@dataclass
class CodeMetrics:
    """Métricas de un programa"""
    loc: int                    # Lines of Code
    sloc: int                   # Source Lines of Code (sin comentarios)
    comments: int               # Líneas de comentario
    blank: int                  # Líneas en blanco

    cyclomatic: int             # Complejidad ciclomática
    max_nesting: int            # Máximo nivel de anidamiento
    num_procedures: int         # Número de procedimientos
    num_variables: int          # Variables declaradas

    halstead_vocabulary: int    # n1 + n2
    halstead_length: int        # N1 + N2
    halstead_difficulty: float  # (n1/2) * (N2/n2)
    halstead_effort: float      # D * V

    maintainability_index: float

class MetricsCalculator:
    """Calcular métricas de código"""

    def __init__(self, ast: ASTNode, symbol_table: SymbolTable, cfg: ControlFlowGraph):
        self.ast = ast
        self.symbols = symbol_table
        self.cfg = cfg

    def calculate_all(self) -> CodeMetrics:
        """Calcular todas las métricas"""
        # Líneas de código
        loc_metrics = self._calculate_loc()

        # Complejidad ciclomática: E - N + 2P
        # E = edges, N = nodes, P = connected components
        cyclomatic = len(self.cfg.edges) - len(self.cfg.blocks) + 2

        # Halstead
        halstead = self._calculate_halstead()

        # Maintainability Index
        mi = 171 - 5.2 * log(halstead['volume']) \
             - 0.23 * cyclomatic \
             - 16.2 * log(loc_metrics['sloc'])
        mi = max(0, min(100, mi * 100 / 171))  # Normalizar 0-100

        return CodeMetrics(
            loc=loc_metrics['loc'],
            sloc=loc_metrics['sloc'],
            comments=loc_metrics['comments'],
            blank=loc_metrics['blank'],
            cyclomatic=cyclomatic,
            max_nesting=self._max_nesting(),
            num_procedures=self._count_procedures(),
            num_variables=len(self.symbols.symbols),
            halstead_vocabulary=halstead['vocabulary'],
            halstead_length=halstead['length'],
            halstead_difficulty=halstead['difficulty'],
            halstead_effort=halstead['effort'],
            maintainability_index=mi
        )

    def _calculate_halstead(self) -> Dict:
        """Métricas de Halstead"""
        operators = {}  # operador -> count
        operands = {}   # operando -> count

        # Recorrer AST contando operadores y operandos
        self._count_halstead(self.ast, operators, operands)

        n1 = len(operators)  # Operadores únicos
        n2 = len(operands)   # Operandos únicos
        N1 = sum(operators.values())  # Total operadores
        N2 = sum(operands.values())   # Total operandos

        vocabulary = n1 + n2
        length = N1 + N2
        volume = length * log2(vocabulary) if vocabulary > 0 else 0
        difficulty = (n1 / 2) * (N2 / n2) if n2 > 0 else 0
        effort = difficulty * volume

        return {
            'vocabulary': vocabulary,
            'length': length,
            'volume': volume,
            'difficulty': difficulty,
            'effort': effort
        }

    def _count_halstead(self, node: ASTNode, operators: Dict, operands: Dict):
        """Contar operadores y operandos recursivamente"""
        if isinstance(node, StatementNode):
            # Verbo es operador
            operators[node.verb] = operators.get(node.verb, 0) + 1

            # Operandos
            for op in node.operands:
                if isinstance(op, str):
                    operands[op] = operands.get(op, 0) + 1

        for child in node.children:
            self._count_halstead(child, operators, operands)


def calculate_technical_debt(metrics: CodeMetrics) -> Dict:
    """Estimar deuda técnica"""
    issues = []
    debt_minutes = 0

    # Complejidad excesiva
    if metrics.cyclomatic > 10:
        issues.append({
            'type': 'HIGH_COMPLEXITY',
            'severity': 'major',
            'message': f'Cyclomatic complexity {metrics.cyclomatic} exceeds threshold 10'
        })
        debt_minutes += (metrics.cyclomatic - 10) * 10

    # Anidamiento profundo
    if metrics.max_nesting > 4:
        issues.append({
            'type': 'DEEP_NESTING',
            'severity': 'minor',
            'message': f'Maximum nesting {metrics.max_nesting} exceeds 4'
        })
        debt_minutes += (metrics.max_nesting - 4) * 5

    # Baja mantenibilidad
    if metrics.maintainability_index < 20:
        issues.append({
            'type': 'LOW_MAINTAINABILITY',
            'severity': 'critical',
            'message': f'Maintainability index {metrics.maintainability_index:.1f} is critically low'
        })
        debt_minutes += 60
    elif metrics.maintainability_index < 40:
        issues.append({
            'type': 'LOW_MAINTAINABILITY',
            'severity': 'major',
            'message': f'Maintainability index {metrics.maintainability_index:.1f} is low'
        })
        debt_minutes += 30

    return {
        'issues': issues,
        'total_debt_minutes': debt_minutes,
        'debt_rating': _debt_rating(debt_minutes)
    }

def _debt_rating(minutes: int) -> str:
    """Clasificación de deuda técnica"""
    if minutes < 30:
        return 'A'
    elif minutes < 60:
        return 'B'
    elif minutes < 120:
        return 'C'
    elif minutes < 240:
        return 'D'
    else:
        return 'E'
```

---

## 8. Herramientas de Análisis

### 8.1 Analizador Integrado

```python
"""
HERRAMIENTA DE ANÁLISIS INTEGRADA
=================================
"""

import json
from datetime import datetime

class LegacyCodeAnalyzer:
    """Analizador completo de código legacy"""

    def __init__(self, language: str):
        self.language = language.upper()
        self.source = None
        self.tokens = None
        self.ast = None
        self.symbol_table = None
        self.cfg = None
        self.metrics = None

    def analyze(self, source_code: str) -> Dict:
        """Ejecutar análisis completo"""
        self.source = source_code

        # Pipeline de análisis
        self._tokenize()
        self._parse()
        self._build_symbols()
        self._check_types()
        self._build_cfg()
        self._analyze_data_flow()
        self._calculate_metrics()

        return self._generate_report()

    def _tokenize(self):
        """Análisis léxico"""
        if self.language == 'COBOL':
            lexer = LexerCobol(self.source)
        elif self.language == 'FORTRAN':
            lexer = LexerFortran(self.source)
        else:
            raise ValueError(f"Unsupported language: {self.language}")

        self.tokens = lexer.tokenize()

    def _parse(self):
        """Análisis sintáctico"""
        if self.language == 'COBOL':
            parser = CobolParser(self.tokens)
        self.ast = parser.parse()

    def _build_symbols(self):
        """Construir tabla de símbolos"""
        self.symbol_table = SymbolTable()
        collector = VariableCollector()
        collector.visit(self.ast)
        # Poblar tabla...

    def _check_types(self):
        """Verificación de tipos"""
        checker = TypeChecker(self.symbol_table)
        checker.visit(self.ast)
        self.type_errors = checker.errors

    def _build_cfg(self):
        """Construir CFG"""
        builder = CFGBuilder()
        # Encontrar PROCEDURE DIVISION
        for div in self.ast.divisions:
            if div.name == 'PROCEDURE':
                self.cfg = builder.build(div)

    def _analyze_data_flow(self):
        """Análisis de flujo de datos"""
        self.data_flow = DataFlowAnalyzer()
        self.reaching_defs = ReachingDefinitions(self.cfg)
        self.reaching_defs.analyze()

    def _calculate_metrics(self):
        """Calcular métricas"""
        calc = MetricsCalculator(self.ast, self.symbol_table, self.cfg)
        self.metrics = calc.calculate_all()

    def _generate_report(self) -> Dict:
        """Generar reporte de análisis"""
        return {
            'timestamp': datetime.now().isoformat(),
            'language': self.language,
            'metrics': {
                'loc': self.metrics.loc,
                'sloc': self.metrics.sloc,
                'cyclomatic_complexity': self.metrics.cyclomatic,
                'maintainability_index': self.metrics.maintainability_index,
                'num_procedures': self.metrics.num_procedures,
                'num_variables': self.metrics.num_variables
            },
            'issues': {
                'type_errors': self.type_errors,
                'unused_variables': self.data_flow.find_unused_variables(self.symbol_table),
                'uninitialized_reads': self.data_flow.find_uninitialized_reads()
            },
            'structure': {
                'cfg_blocks': len(self.cfg.blocks),
                'cfg_edges': len(self.cfg.edges),
                'loops': len(self.cfg.find_loops())
            },
            'technical_debt': calculate_technical_debt(self.metrics)
        }


# Uso
if __name__ == '__main__':
    analyzer = LegacyCodeAnalyzer('COBOL')

    with open('program.cbl', 'r') as f:
        source = f.read()

    report = analyzer.analyze(source)
    print(json.dumps(report, indent=2))
```

---

## Referencia Rápida

### Tipos de Análisis

| Análisis | Propósito | Salida |
|----------|-----------|--------|
| Léxico | Tokenización | Lista de tokens |
| Sintáctico | Estructura | AST |
| Semántico | Significado | Tabla de símbolos |
| Flujo de datos | Variables | Def-Use chains |
| Flujo de control | Ejecución | CFG |
| Métricas | Calidad | Indicadores |

### Métricas Clave

| Métrica | Umbral Bueno | Umbral Malo |
|---------|--------------|-------------|
| Complejidad Ciclomática | < 10 | > 20 |
| Índice Mantenibilidad | > 60 | < 20 |
| Anidamiento Máximo | < 4 | > 6 |

### Herramientas Externas

- **COBOL**: Micro Focus Analyzer, Raincode
- **FORTRAN**: ftnchek, forcheck
- **C**: cppcheck, splint
- **General**: SonarQube, Understand

---

## Conclusión

El análisis de código legacy es el primer paso hacia cualquier modernización. Sin comprensión profunda, la traducción es imposible. Las técnicas presentadas - desde tokenización hasta análisis de flujo - forman el fundamento para entender sistemas que han evolucionado durante décadas.

**Puntos clave:**
1. Normalizar antes de analizar
2. Construir representaciones intermedias (AST, CFG)
3. Métricas objetivas guían prioridades
4. El contexto histórico es invaluable
5. Documentar todo conocimiento extraído

---

*"El arqueólogo del código no destruye - documenta, comprende, y solo entonces transforma con respeto por aquellos que construyeron antes."*
