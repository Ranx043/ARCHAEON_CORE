---
title: "ARSENAL Reference: COBOL PICTURE Clause"
category: reference
domain: mainframe
language: COBOL
version: 1.0.0
last_updated: 2025-12-31
tags: [cobol, picture, data-description, mainframe, legacy]
---

# ARSENAL_REF_PICTURE
## Complete COBOL PICTURE Clause Reference

---

# TABLE OF CONTENTS

1. [PICTURE Symbol Reference](#picture-symbol-reference)
2. [USAGE Clause](#usage-clause)
3. [Level Numbers](#level-numbers)
4. [Editing Symbols](#editing-symbols)
5. [Examples and Memory Layout](#examples-and-memory-layout)
6. [Common Patterns](#common-patterns)

---

# PICTURE SYMBOL REFERENCE

## Basic Data Symbols

| Symbol | Description | Valid Positions | Category |
|--------|-------------|-----------------|----------|
| `9` | Numeric digit (0-9) | Anywhere | Numeric |
| `X` | Any character | Anywhere | Alphanumeric |
| `A` | Alphabetic (A-Z, space) | Anywhere | Alphabetic |
| `V` | Implied decimal point | One per PIC | Numeric |
| `S` | Sign (+ or -) | First position only | Numeric |
| `P` | Assumed decimal scaling | Left or right | Numeric |

### Symbol Details

#### 9 - Numeric Digit
```cobol
01 WS-AMOUNT    PIC 9(5).        *> 00000 to 99999
01 WS-DECIMAL   PIC 9(3)V99.     *> 000.00 to 999.99
01 WS-SIGNED    PIC S9(4).       *> -9999 to +9999
```

#### X - Alphanumeric Character
```cobol
01 WS-NAME      PIC X(30).       *> Any 30 characters
01 WS-CODE      PIC X(10).       *> Alphanumeric code
01 WS-MIXED     PIC X(50).       *> Letters, numbers, symbols
```

#### A - Alphabetic Character
```cobol
01 WS-ALPHA     PIC A(20).       *> Letters and spaces only
01 WS-INITIAL   PIC A.           *> Single letter
```

#### V - Implied Decimal Point
```cobol
01 WS-RATE      PIC 9(2)V9(4).   *> e.g., 12.3456 stored as 123456
01 WS-PRICE     PIC 9(5)V99.     *> e.g., 12345.67 stored as 1234567
01 WS-PERCENT   PIC V9(3).       *> e.g., .123 stored as 123
```

#### S - Sign
```cobol
01 WS-BALANCE   PIC S9(7)V99.    *> Signed decimal
01 WS-TEMP      PIC S9(3).       *> -999 to +999
01 WS-DELTA     PIC S9(5)V9(3).  *> Signed with decimals
```

#### P - Decimal Scaling Position
```cobol
01 WS-LARGE     PIC 9(3)PPP.     *> 123000 (3 implied zeros right)
01 WS-SMALL     PIC PPP9(3).     *> .000123 (3 implied zeros left)
01 WS-MILLIONS  PIC 9(2)P(6).    *> 12000000
```

## Repetition Notation

| Notation | Equivalent | Bytes |
|----------|------------|-------|
| `9(5)` | `99999` | 5 |
| `X(10)` | `XXXXXXXXXX` | 10 |
| `9(3)V9(2)` | `999V99` | 5 |
| `S9(7)V9(2)` | `S9999999V99` | 9 (+sign) |

---

# USAGE CLAUSE

## USAGE Types

| USAGE | Description | Storage | Performance |
|-------|-------------|---------|-------------|
| `DISPLAY` | Human-readable (default) | 1 byte/digit | Slow |
| `COMP` / `BINARY` | Binary integer | 2/4/8 bytes | Fast |
| `COMP-1` | Single-precision float | 4 bytes | Fast |
| `COMP-2` | Double-precision float | 8 bytes | Fast |
| `COMP-3` / `PACKED-DECIMAL` | BCD packed | (n+1)/2 bytes | Medium |
| `COMP-4` | Binary (same as COMP) | 2/4/8 bytes | Fast |
| `COMP-5` | Native binary | 2/4/8 bytes | Fastest |

## DISPLAY (Default)

```cobol
*> Each digit = 1 byte (EBCDIC/ASCII)
01 WS-DISPLAY-NUM    PIC 9(5).
   *> Value: 12345
   *> Memory: F1 F2 F3 F4 F5 (EBCDIC)
   *> Memory: 31 32 33 34 35 (ASCII)

01 WS-DISPLAY-SIGNED PIC S9(5).
   *> Value: -12345
   *> Memory: F1 F2 F3 F4 D5 (trailing sign)
   *>                    ^-- D = negative
```

### Trailing Sign Values (EBCDIC)
| Digit | Positive | Negative |
|-------|----------|----------|
| 0 | C0 or F0 | D0 |
| 1 | C1 or F1 | D1 |
| 2 | C2 or F2 | D2 |
| 3 | C3 or F3 | D3 |
| 4 | C4 or F4 | D4 |
| 5 | C5 or F5 | D5 |
| 6 | C6 or F6 | D6 |
| 7 | C7 or F7 | D7 |
| 8 | C8 or F8 | D8 |
| 9 | C9 or F9 | D9 |

## COMP / BINARY

```cobol
*> Binary storage - efficient for calculations
01 WS-COMP-SMALL     PIC S9(4) COMP.
   *> 1-4 digits: 2 bytes (halfword)
   *> Range: -9999 to +9999

01 WS-COMP-MEDIUM    PIC S9(9) COMP.
   *> 5-9 digits: 4 bytes (fullword)
   *> Range: -999999999 to +999999999

01 WS-COMP-LARGE     PIC S9(18) COMP.
   *> 10-18 digits: 8 bytes (doubleword)
   *> Range: +/- 999999999999999999
```

### COMP Storage Summary
| Digits | Bytes | Min Value | Max Value |
|--------|-------|-----------|-----------|
| 1-4 | 2 | -32,768 | 32,767 |
| 5-9 | 4 | -2,147,483,648 | 2,147,483,647 |
| 10-18 | 8 | -9,223,372,036,854,775,808 | 9,223,372,036,854,775,807 |

## COMP-3 / PACKED-DECIMAL

```cobol
*> BCD: 2 digits per byte + sign nibble
01 WS-PACKED-5       PIC S9(5) COMP-3.
   *> Value: -12345
   *> Bytes: (5+1)/2 = 3 bytes
   *> Memory: 12 34 5D
   *>              ^-- D = negative sign

01 WS-PACKED-7       PIC S9(7) COMP-3.
   *> Value: +1234567
   *> Bytes: (7+1)/2 = 4 bytes
   *> Memory: 01 23 45 67 C
   *>                    ^-- C = positive sign
```

### COMP-3 Sign Nibble
| Nibble | Meaning |
|--------|---------|
| C | Positive |
| D | Negative |
| F | Unsigned positive |
| A, E | Alternate positive |
| B | Alternate negative |

### COMP-3 Storage Calculation
```
Bytes = (Number of 9s + 1) / 2, rounded up
```

| PIC | Digits | Bytes | Example |
|-----|--------|-------|---------|
| S9(3) COMP-3 | 3 | 2 | 12 3C |
| S9(5) COMP-3 | 5 | 3 | 01 23 45C |
| S9(7) COMP-3 | 7 | 4 | 01 23 45 67C |
| S9(9) COMP-3 | 9 | 5 | 01 23 45 67 89C |

## COMP-1 and COMP-2

```cobol
*> Single-precision floating point (4 bytes)
01 WS-FLOAT-SINGLE   COMP-1.
   *> IEEE 754 single precision
   *> Approx. 7 significant digits

*> Double-precision floating point (8 bytes)
01 WS-FLOAT-DOUBLE   COMP-2.
   *> IEEE 754 double precision
   *> Approx. 15 significant digits
```

---

# LEVEL NUMBERS

## Standard Level Numbers

| Level | Purpose | Description |
|-------|---------|-------------|
| 01 | Record level | Top-level data item |
| 02-49 | Group/Elementary | Subordinate items |
| 66 | RENAMES | Alternate grouping |
| 77 | Independent | Standalone elementary item |
| 88 | Condition name | Boolean condition |

## Level 01 - Record Definition

```cobol
01 WS-CUSTOMER-RECORD.
   05 WS-CUST-ID          PIC 9(8).
   05 WS-CUST-NAME        PIC X(30).
   05 WS-CUST-ADDRESS.
      10 WS-STREET        PIC X(40).
      10 WS-CITY          PIC X(20).
      10 WS-STATE         PIC X(2).
      10 WS-ZIP           PIC 9(5).
```

## Levels 02-49 - Hierarchy

```cobol
01 WS-DATE-RECORD.
   02 WS-DATE-FULL.
      03 WS-YEAR          PIC 9(4).
      03 WS-MONTH         PIC 9(2).
      03 WS-DAY           PIC 9(2).
   02 WS-DATE-FORMATTED   PIC X(10).

*> Group item WS-DATE-FULL contains 8 bytes
*> Elementary items have explicit PICs
```

## Level 66 - RENAMES

```cobol
01 WS-FULL-NAME.
   05 WS-FIRST-NAME       PIC X(15).
   05 WS-MIDDLE-INIT      PIC X(1).
   05 WS-LAST-NAME        PIC X(20).

66 WS-FIRST-AND-MIDDLE RENAMES WS-FIRST-NAME
                        THRU WS-MIDDLE-INIT.
66 WS-NAME-NO-MIDDLE   RENAMES WS-FIRST-NAME
                        THRU WS-FIRST-NAME.

*> WS-FIRST-AND-MIDDLE refers to first 16 bytes
```

## Level 77 - Independent Items

```cobol
77 WS-COUNTER            PIC 9(5)   VALUE ZEROS.
77 WS-TEMP-AMOUNT        PIC S9(7)V99 COMP-3.
77 WS-EOF-FLAG           PIC X      VALUE 'N'.

*> Cannot be subdivided
*> Cannot be part of a group
*> Useful for standalone variables
```

## Level 88 - Condition Names

```cobol
01 WS-STATUS-CODE        PIC X(2).
   88 STATUS-OK          VALUE '00'.
   88 STATUS-NOT-FOUND   VALUE '01'.
   88 STATUS-ERROR       VALUE '99'.
   88 STATUS-WARNING     VALUE '10' THRU '19'.
   88 STATUS-VALID       VALUE '00' '10' '20'.

01 WS-GENDER             PIC X.
   88 MALE               VALUE 'M'.
   88 FEMALE             VALUE 'F'.
   88 OTHER              VALUE 'O' 'U' 'X'.

*> Usage:
IF STATUS-OK
   DISPLAY "Success"
END-IF

SET STATUS-ERROR TO TRUE    *> Sets WS-STATUS-CODE to '99'
```

---

# EDITING SYMBOLS

## Numeric Editing Symbols

| Symbol | Description | Example PIC | Input | Output |
|--------|-------------|-------------|-------|--------|
| `Z` | Zero suppress | Z(4)9 | 00123 | "  123" |
| `*` | Asterisk fill | *(5)9 | 00123 | "**123" |
| `.` | Decimal point | 9(3).99 | 12345 | "123.45" |
| `,` | Comma | 9(3),9(3) | 123456 | "123,456" |
| `$` | Currency sign | $9(5) | 12345 | "$12345" |
| `+` | Plus sign | +9(4) | -1234 | "-1234" |
| `-` | Minus sign | 9(4)- | -1234 | "1234-" |
| `CR` | Credit | 9(4)CR | -1234 | "1234CR" |
| `DB` | Debit | 9(4)DB | -1234 | "1234DB" |
| `B` | Blank insertion | 9(3)B9(3) | 123456 | "123 456" |
| `0` | Zero insertion | 9(3)09(3) | 123456 | "1230456" |
| `/` | Slash insertion | 9(2)/9(2)/9(4) | 12252024 | "12/25/2024" |

## Zero Suppression (Z)

```cobol
01 WS-AMOUNT-EDIT    PIC Z(6)9.
   *> 0000123 displays as "    123"
   *> 0000000 displays as "      0"

01 WS-DECIMAL-EDIT   PIC Z(4)9.99.
   *> 0012345 displays as "  123.45"
   *> 0000099 displays as "    0.99"

01 WS-FULL-SUPPRESS  PIC Z(7).
   *> 0000000 displays as "       " (all spaces)
   *> 0000123 displays as "    123"
```

## Asterisk Fill (*)

```cobol
01 WS-CHECK-AMOUNT   PIC **(6)9.99.
   *> 0012345 displays as "***123.45"
   *> Used for check protection
   *> 0000099 displays as "*****0.99"

01 WS-SECURE-AMT     PIC $**(5)9.99.
   *> 0012345 displays as "$**123.45"
```

## Currency and Sign

```cobol
01 WS-PRICE-EDIT     PIC $Z(5)9.99.
   *> 0012345 displays as "$  123.45"

01 WS-FLOAT-DOLLAR   PIC $$$$$9.99.
   *> 0012345 displays as "  $123.45"
   *> Floating $ moves right

01 WS-SIGNED-EDIT    PIC +Z(5)9.99.
   *> +0012345 displays as "+  123.45"
   *> -0012345 displays as "-  123.45"

01 WS-TRAIL-SIGN     PIC Z(5)9.99-.
   *> Trailing sign (space if positive)
   *> -0012345 displays as "  123.45-"
```

## Credit and Debit

```cobol
01 WS-CREDIT-AMT     PIC Z(5)9.99CR.
   *> -0012345 displays as "  123.45CR"
   *> +0012345 displays as "  123.45  "

01 WS-DEBIT-AMT      PIC Z(5)9.99DB.
   *> -0012345 displays as "  123.45DB"
```

## Insertion Characters

```cobol
*> Date formatting
01 WS-DATE-EDIT      PIC 99/99/9999.
   *> 12252024 displays as "12/25/2024"

*> Phone number
01 WS-PHONE-EDIT     PIC (999)B999-9999.
   *> 5551234567 displays as "(555) 123-4567"

*> Social Security
01 WS-SSN-EDIT       PIC 999-99-9999.
   *> 123456789 displays as "123-45-6789"

*> Account number with spaces
01 WS-ACCT-EDIT      PIC 9(4)B9(4)B9(4).
   *> 123456789012 displays as "1234 5678 9012"
```

---

# EXAMPLES AND MEMORY LAYOUT

## Complete Record Example

```cobol
01 WS-EMPLOYEE-RECORD.
   05 WS-EMP-ID           PIC 9(6).
   05 WS-EMP-NAME.
      10 WS-FIRST-NAME    PIC X(15).
      10 WS-LAST-NAME     PIC X(20).
   05 WS-SALARY           PIC S9(7)V99 COMP-3.
   05 WS-DEPT-CODE        PIC X(4).
   05 WS-HIRE-DATE.
      10 WS-HIRE-YEAR     PIC 9(4).
      10 WS-HIRE-MONTH    PIC 9(2).
      10 WS-HIRE-DAY      PIC 9(2).
   05 WS-STATUS           PIC X.
      88 ACTIVE           VALUE 'A'.
      88 INACTIVE         VALUE 'I'.
      88 TERMINATED       VALUE 'T'.
```

### Memory Layout
```
Offset  Length  Field
------  ------  -----
0       6       WS-EMP-ID (DISPLAY)
6       15      WS-FIRST-NAME
21      20      WS-LAST-NAME
41      5       WS-SALARY (COMP-3: (7+2+1)/2 = 5)
46      4       WS-DEPT-CODE
50      4       WS-HIRE-YEAR
54      2       WS-HIRE-MONTH
56      2       WS-HIRE-DAY
58      1       WS-STATUS
------  ------
Total:  59 bytes
```

## Numeric Type Comparison

```cobol
01 WS-COMPARISONS.
   05 WS-DISPLAY-7        PIC S9(7)V99.
      *> Storage: 9 bytes + sign in last digit
      *> Value 1234567.89 = F1F2F3F4F5F6F7F8C9

   05 WS-COMP3-7          PIC S9(7)V99 COMP-3.
      *> Storage: (7+2+1)/2 = 5 bytes
      *> Value 1234567.89 = 01 23 45 67 89 C

   05 WS-BINARY-7         PIC S9(7)V99 COMP.
      *> Storage: 4 bytes (fullword)
      *> Value stored as binary integer

   05 WS-DISPLAY-18       PIC S9(16)V99.
      *> Storage: 18 bytes

   05 WS-COMP3-18         PIC S9(16)V99 COMP-3.
      *> Storage: (16+2+1)/2 = 10 bytes (rounded up)

   05 WS-BINARY-18        PIC S9(16)V99 COMP.
      *> Storage: 8 bytes (doubleword)
```

## REDEFINES Examples

```cobol
01 WS-DATE-AREA.
   05 WS-DATE-NUMERIC     PIC 9(8).
   05 WS-DATE-PARTS REDEFINES WS-DATE-NUMERIC.
      10 WS-YEAR          PIC 9(4).
      10 WS-MONTH         PIC 9(2).
      10 WS-DAY           PIC 9(2).
   05 WS-DATE-ALPHA REDEFINES WS-DATE-NUMERIC PIC X(8).

*> All three refer to same 8 bytes
*> MOVE 20241225 TO WS-DATE-NUMERIC
*> WS-YEAR = 2024, WS-MONTH = 12, WS-DAY = 25

01 WS-NUMERIC-ALPHA.
   05 WS-AMOUNT           PIC S9(9)V99 COMP-3.
   05 WS-AMOUNT-X REDEFINES WS-AMOUNT PIC X(6).

*> Allows byte-level access to packed decimal
```

## OCCURS and Tables

```cobol
01 WS-MONTHLY-SALES.
   05 WS-MONTH-DATA OCCURS 12 TIMES.
      10 WS-MONTH-NAME    PIC X(10).
      10 WS-SALES-AMT     PIC S9(9)V99 COMP-3.
      10 WS-UNITS-SOLD    PIC S9(7) COMP.

*> Each occurrence: 10 + 6 + 4 = 20 bytes
*> Total: 12 x 20 = 240 bytes

01 WS-VARIABLE-TABLE.
   05 WS-ITEM-COUNT       PIC S9(4) COMP.
   05 WS-ITEM OCCURS 1 TO 100 TIMES
              DEPENDING ON WS-ITEM-COUNT.
      10 WS-ITEM-CODE     PIC X(10).
      10 WS-ITEM-QTY      PIC S9(5) COMP-3.

*> Variable-length table
```

---

# COMMON PATTERNS

## Currency Fields

```cobol
*> Storage (internal calculation)
01 WS-AMOUNT-STORE        PIC S9(13)V99 COMP-3.

*> Display (report output)
01 WS-AMOUNT-DISPLAY      PIC $$$,$$$,$$$,$$9.99-.

*> Edited for checks
01 WS-CHECK-AMT           PIC $***,***,**9.99.
```

## Date Fields

```cobol
*> ISO date storage
01 WS-DATE-ISO.
   05 WS-ISO-YEAR         PIC 9(4).
   05 WS-ISO-MONTH        PIC 9(2).
   05 WS-ISO-DAY          PIC 9(2).

*> Julian date
01 WS-DATE-JULIAN.
   05 WS-JUL-YEAR         PIC 9(4).
   05 WS-JUL-DAY          PIC 9(3).

*> Formatted display
01 WS-DATE-DISPLAY        PIC 99/99/9999.

*> With named months
01 WS-DATE-FORMATTED      PIC X(18).
   *> "December 25, 2024"
```

## Name Fields

```cobol
01 WS-PERSON-NAME.
   05 WS-NAME-LAST        PIC X(30).
   05 WS-NAME-FIRST       PIC X(20).
   05 WS-NAME-MIDDLE      PIC X(20).
   05 WS-NAME-SUFFIX      PIC X(5).

01 WS-NAME-FORMATTED      PIC X(80).
```

## Counters and Accumulators

```cobol
*> Loop counter (binary for speed)
77 WS-INDEX               PIC S9(4) COMP VALUE 0.

*> Record counter
77 WS-REC-COUNT           PIC S9(9) COMP VALUE 0.

*> Running total (packed for precision)
77 WS-TOTAL-AMT           PIC S9(13)V99 COMP-3 VALUE 0.

*> Large accumulator
77 WS-GRAND-TOTAL         PIC S9(15)V99 COMP-3 VALUE 0.
```

## Status and Flag Fields

```cobol
01 WS-PROCESS-FLAGS.
   05 WS-EOF-FLAG         PIC X VALUE 'N'.
      88 END-OF-FILE      VALUE 'Y'.
      88 NOT-END-OF-FILE  VALUE 'N'.

   05 WS-ERROR-FLAG       PIC X VALUE 'N'.
      88 ERROR-FOUND      VALUE 'Y'.
      88 NO-ERROR         VALUE 'N'.

   05 WS-FILE-STATUS      PIC XX.
      88 FILE-OK          VALUE '00'.
      88 FILE-EOF         VALUE '10'.
      88 FILE-NOT-FOUND   VALUE '35'.
      88 FILE-ERROR       VALUE '30' THRU '99'.
```

## Conversion Patterns

### Numeric to Display
```cobol
MOVE WS-COMP3-AMOUNT TO WS-DISPLAY-AMOUNT.
*> Automatic conversion with de-editing
```

### Display to Numeric
```cobol
MOVE WS-INPUT-TEXT TO WS-NUMERIC-FIELD.
*> COBOL handles conversion
*> Use FUNCTION NUMVAL for complex cases
```

### String to Number
```cobol
COMPUTE WS-NUMERIC = FUNCTION NUMVAL(WS-STRING).
COMPUTE WS-CURRENCY = FUNCTION NUMVAL-C(WS-MONEY-STRING).
```

## Report Line Definitions

```cobol
01 WS-DETAIL-LINE.
   05 FILLER              PIC X(5)  VALUE SPACES.
   05 WS-DL-ITEM          PIC X(30).
   05 FILLER              PIC X(3)  VALUE SPACES.
   05 WS-DL-QTY           PIC Z(5)9.
   05 FILLER              PIC X(3)  VALUE SPACES.
   05 WS-DL-PRICE         PIC $$$,$$9.99.
   05 FILLER              PIC X(3)  VALUE SPACES.
   05 WS-DL-TOTAL         PIC $$$,$$$,$$9.99-.

01 WS-TOTAL-LINE.
   05 FILLER              PIC X(38) VALUE SPACES.
   05 FILLER              PIC X(12) VALUE 'GRAND TOTAL:'.
   05 FILLER              PIC X(3)  VALUE SPACES.
   05 WS-TL-GRAND         PIC $,$$$,$$$,$$9.99-.
```

---

# QUICK REFERENCE TABLES

## Storage Size Quick Reference

| PIC | DISPLAY | COMP-3 | COMP |
|-----|---------|--------|------|
| S9(1) | 1 | 1 | 2 |
| S9(2) | 2 | 2 | 2 |
| S9(3) | 3 | 2 | 2 |
| S9(4) | 4 | 3 | 2 |
| S9(5) | 5 | 3 | 4 |
| S9(7) | 7 | 4 | 4 |
| S9(9) | 9 | 5 | 4 |
| S9(11) | 11 | 6 | 8 |
| S9(15) | 15 | 8 | 8 |
| S9(18) | 18 | 10 | 8 |

## Editing Symbol Quick Reference

| Symbol | Suppresses Zero | Inserts | Floats |
|--------|-----------------|---------|--------|
| Z | Yes | Space | No |
| * | Yes | Asterisk | No |
| $ | Leftmost only | $ | Yes if multiple |
| + | No | +/- | Yes if multiple |
| - | No | Space/- | Yes if multiple |
| . | No | Period | No |
| , | Yes (before) | Comma | No |
| B | No | Space | No |
| / | No | Slash | No |
| 0 | No | Zero | No |

---

*ARCHAEON Arsenal - COBOL PICTURE Reference v1.0*
*"The mainframe speaks in EBCDIC, but its wisdom transcends encoding"*
