---
title: "ARCHAEON Arsenal - COBOL Snippets"
version: "1.0.0"
category: "ARSENAL/SNIPPETS"
language: "COBOL"
purpose: "Practical COBOL patterns for enterprise and mainframe programming"
created: "2025-12-31"
tags: ["cobol", "mainframe", "legacy", "enterprise", "jcl", "vsam", "batch"]
complexity: "Intermediate to Advanced"
---

# ARCHAEON ARSENAL: COBOL Snippets

> **Mission**: Provide production-ready COBOL patterns for enterprise systems,
> batch processing, file handling, and mainframe development.

---

## Table of Contents

1. [Program Structure Templates](#program-structure-templates)
2. [File Handling Patterns](#file-handling-patterns)
3. [String Manipulation](#string-manipulation)
4. [Table Operations](#table-operations)
5. [Date Handling](#date-handling)
6. [Error Handling](#error-handling)
7. [JCL Snippets](#jcl-snippets)
8. [Copybook Patterns](#copybook-patterns)

---

## Program Structure Templates

### Standard Batch Program Template

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE01.
       AUTHOR. ARCHAEON-ARSENAL.
       DATE-WRITTEN. 2025-12-31.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM: SAMPLE01                                              *
      * PURPOSE: Standard batch processing template                    *
      * INPUT:   INPUT-FILE - Sequential input file                    *
      * OUTPUT:  OUTPUT-FILE - Sequential output file                  *
      *          REPORT-FILE - Report listing                          *
      *================================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZOS.
       OBJECT-COMPUTER. IBM-ZOS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE
               ASSIGN TO INFILE
               FILE STATUS IS WS-INPUT-STATUS.

           SELECT OUTPUT-FILE
               ASSIGN TO OUTFILE
               FILE STATUS IS WS-OUTPUT-STATUS.

           SELECT REPORT-FILE
               ASSIGN TO RPTFILE
               FILE STATUS IS WS-REPORT-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  INPUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  INPUT-RECORD                    PIC X(80).

       FD  OUTPUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS.
       01  OUTPUT-RECORD                   PIC X(100).

       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS.
       01  REPORT-RECORD                   PIC X(132).

       WORKING-STORAGE SECTION.

       01  WS-FILE-STATUS.
           05  WS-INPUT-STATUS             PIC XX VALUE SPACES.
           05  WS-OUTPUT-STATUS            PIC XX VALUE SPACES.
           05  WS-REPORT-STATUS            PIC XX VALUE SPACES.

       01  WS-SWITCHES.
           05  WS-END-OF-FILE-SW           PIC X VALUE 'N'.
               88  END-OF-FILE                   VALUE 'Y'.
               88  NOT-END-OF-FILE               VALUE 'N'.

       01  WS-COUNTERS.
           05  WS-RECORDS-READ             PIC 9(9) VALUE ZEROS.
           05  WS-RECORDS-WRITTEN          PIC 9(9) VALUE ZEROS.
           05  WS-RECORDS-REJECTED         PIC 9(9) VALUE ZEROS.

       01  WS-CURRENT-DATE-DATA.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR         PIC 9(4).
               10  WS-CURRENT-MONTH        PIC 9(2).
               10  WS-CURRENT-DAY          PIC 9(2).
           05  WS-CURRENT-TIME.
               10  WS-CURRENT-HOUR         PIC 9(2).
               10  WS-CURRENT-MINUTE       PIC 9(2).
               10  WS-CURRENT-SECOND       PIC 9(2).
               10  WS-CURRENT-HUND-SEC     PIC 9(2).

       PROCEDURE DIVISION.

       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-FILE
               UNTIL END-OF-FILE
           PERFORM 3000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           OPEN INPUT  INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
                       REPORT-FILE
           PERFORM 1100-CHECK-FILE-STATUS
           PERFORM 2100-READ-INPUT
           .

       1100-CHECK-FILE-STATUS.
           IF WS-INPUT-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING INPUT FILE: ' WS-INPUT-STATUS
               PERFORM 9999-ABEND-ROUTINE
           END-IF
           IF WS-OUTPUT-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING OUTPUT FILE: ' WS-OUTPUT-STATUS
               PERFORM 9999-ABEND-ROUTINE
           END-IF
           .

       2000-PROCESS-FILE.
           ADD 1 TO WS-RECORDS-READ
           PERFORM 2200-PROCESS-RECORD
           PERFORM 2100-READ-INPUT
           .

       2100-READ-INPUT.
           READ INPUT-FILE
               AT END
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   CONTINUE
           END-READ
           .

       2200-PROCESS-RECORD.
           MOVE INPUT-RECORD TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           IF WS-OUTPUT-STATUS = '00'
               ADD 1 TO WS-RECORDS-WRITTEN
           ELSE
               ADD 1 TO WS-RECORDS-REJECTED
           END-IF
           .

       3000-TERMINATE.
           PERFORM 3100-PRINT-SUMMARY
           CLOSE INPUT-FILE
                 OUTPUT-FILE
                 REPORT-FILE
           .

       3100-PRINT-SUMMARY.
           DISPLAY '========================================'
           DISPLAY 'PROGRAM: SAMPLE01 - PROCESSING COMPLETE'
           DISPLAY '========================================'
           DISPLAY 'RECORDS READ:     ' WS-RECORDS-READ
           DISPLAY 'RECORDS WRITTEN:  ' WS-RECORDS-WRITTEN
           DISPLAY 'RECORDS REJECTED: ' WS-RECORDS-REJECTED
           DISPLAY '========================================'
           .

       9999-ABEND-ROUTINE.
           DISPLAY 'PROGRAM ABENDING - CHECK ERROR MESSAGES'
           STOP RUN.
```

---

## File Handling Patterns

### Sequential File Processing

```cobol
      *================================================================*
      * SEQUENTIAL FILE READ/WRITE PATTERN                             *
      *================================================================*

       FILE-CONTROL.
           SELECT SEQ-INPUT-FILE
               ASSIGN TO SEQIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-SEQ-IN-STATUS.

           SELECT SEQ-OUTPUT-FILE
               ASSIGN TO SEQOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-SEQ-OUT-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  SEQ-INPUT-FILE
           RECORDING MODE IS V
           RECORD CONTAINS 10 TO 500 CHARACTERS.
       01  SEQ-INPUT-REC.
           05  SEQ-IN-LENGTH               PIC S9(4) COMP.
           05  SEQ-IN-DATA                 PIC X(496).

       FD  SEQ-OUTPUT-FILE
           RECORDING MODE IS V
           RECORD CONTAINS 10 TO 500 CHARACTERS.
       01  SEQ-OUTPUT-REC                  PIC X(500).

       WORKING-STORAGE SECTION.
       01  WS-SEQ-IN-STATUS                PIC XX.
       01  WS-SEQ-OUT-STATUS               PIC XX.

       PROCEDURE DIVISION.

       READ-SEQ-FILE.
           READ SEQ-INPUT-FILE INTO WS-INPUT-AREA
               AT END
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   PERFORM PROCESS-SEQ-RECORD
           END-READ
           .

       WRITE-SEQ-FILE.
           WRITE SEQ-OUTPUT-REC FROM WS-OUTPUT-AREA
           IF WS-SEQ-OUT-STATUS NOT = '00'
               PERFORM HANDLE-WRITE-ERROR
           END-IF
           .
```

### VSAM KSDS (Indexed) File Operations

```cobol
      *================================================================*
      * VSAM KSDS (KEY-SEQUENCED DATA SET) OPERATIONS                  *
      *================================================================*

       FILE-CONTROL.
           SELECT CUSTOMER-FILE
               ASSIGN TO CUSTFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-KEY
               ALTERNATE RECORD KEY IS CUST-NAME
                   WITH DUPLICATES
               FILE STATUS IS WS-CUST-STATUS
                              WS-VSAM-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05  CUST-KEY                    PIC X(10).
           05  CUST-NAME                   PIC X(30).
           05  CUST-ADDRESS                PIC X(50).
           05  CUST-PHONE                  PIC X(15).
           05  CUST-BALANCE                PIC S9(9)V99 COMP-3.
           05  CUST-STATUS                 PIC X.

       WORKING-STORAGE SECTION.

       01  WS-CUST-STATUS                  PIC XX.
           88  CUST-SUCCESS                      VALUE '00'.
           88  CUST-DUPLICATE                    VALUE '22'.
           88  CUST-NOT-FOUND                    VALUE '23'.
           88  CUST-END-OF-FILE                  VALUE '10'.

       01  WS-VSAM-STATUS.
           05  WS-VSAM-RETURN-CODE         PIC 99.
           05  WS-VSAM-COMPONENT-CODE      PIC 99.
           05  WS-VSAM-REASON-CODE         PIC 99.

       PROCEDURE DIVISION.

      *--- SEQUENTIAL READ ---
       READ-CUSTOMER-SEQUENTIAL.
           READ CUSTOMER-FILE NEXT RECORD
               AT END
                   SET CUST-END-OF-FILE TO TRUE
               NOT AT END
                   PERFORM PROCESS-CUSTOMER
           END-READ
           .

      *--- RANDOM READ BY PRIMARY KEY ---
       READ-CUSTOMER-BY-KEY.
           MOVE WS-SEARCH-KEY TO CUST-KEY
           READ CUSTOMER-FILE
               INVALID KEY
                   SET CUST-NOT-FOUND TO TRUE
                   PERFORM HANDLE-NOT-FOUND
               NOT INVALID KEY
                   PERFORM PROCESS-CUSTOMER
           END-READ
           .

      *--- RANDOM READ BY ALTERNATE KEY ---
       READ-CUSTOMER-BY-NAME.
           MOVE WS-SEARCH-NAME TO CUST-NAME
           READ CUSTOMER-FILE
               KEY IS CUST-NAME
               INVALID KEY
                   SET CUST-NOT-FOUND TO TRUE
               NOT INVALID KEY
                   PERFORM PROCESS-CUSTOMER
           END-READ
           .

      *--- INSERT NEW RECORD ---
       ADD-CUSTOMER.
           MOVE WS-NEW-CUSTOMER TO CUSTOMER-RECORD
           WRITE CUSTOMER-RECORD
           EVALUATE TRUE
               WHEN CUST-SUCCESS
                   ADD 1 TO WS-RECORDS-ADDED
               WHEN CUST-DUPLICATE
                   PERFORM HANDLE-DUPLICATE
               WHEN OTHER
                   PERFORM HANDLE-VSAM-ERROR
           END-EVALUATE
           .

      *--- UPDATE EXISTING RECORD ---
       UPDATE-CUSTOMER.
           MOVE WS-UPDATE-KEY TO CUST-KEY
           READ CUSTOMER-FILE
               INVALID KEY
                   PERFORM HANDLE-NOT-FOUND
               NOT INVALID KEY
                   MOVE WS-NEW-BALANCE TO CUST-BALANCE
                   REWRITE CUSTOMER-RECORD
                   IF NOT CUST-SUCCESS
                       PERFORM HANDLE-VSAM-ERROR
                   END-IF
           END-READ
           .

      *--- DELETE RECORD ---
       DELETE-CUSTOMER.
           MOVE WS-DELETE-KEY TO CUST-KEY
           READ CUSTOMER-FILE
               INVALID KEY
                   PERFORM HANDLE-NOT-FOUND
               NOT INVALID KEY
                   DELETE CUSTOMER-FILE
                   IF NOT CUST-SUCCESS
                       PERFORM HANDLE-VSAM-ERROR
                   END-IF
           END-READ
           .

      *--- POSITION FOR BROWSE ---
       START-BROWSE-AT-KEY.
           MOVE WS-START-KEY TO CUST-KEY
           START CUSTOMER-FILE
               KEY IS >= CUST-KEY
               INVALID KEY
                   SET CUST-NOT-FOUND TO TRUE
           END-START
           .

       HANDLE-VSAM-ERROR.
           DISPLAY 'VSAM ERROR - FILE STATUS: ' WS-CUST-STATUS
           DISPLAY 'VSAM RETURN CODE: ' WS-VSAM-RETURN-CODE
           DISPLAY 'VSAM COMPONENT:   ' WS-VSAM-COMPONENT-CODE
           DISPLAY 'VSAM REASON:      ' WS-VSAM-REASON-CODE
           .
```

### VSAM RRDS (Relative Record) Operations

```cobol
      *================================================================*
      * VSAM RRDS (RELATIVE RECORD DATA SET) OPERATIONS                *
      *================================================================*

       FILE-CONTROL.
           SELECT RELATIVE-FILE
               ASSIGN TO RELFILE
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS DYNAMIC
               RELATIVE KEY IS WS-RELATIVE-KEY
               FILE STATUS IS WS-REL-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  RELATIVE-FILE.
       01  RELATIVE-RECORD                 PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-REL-STATUS                   PIC XX.
       01  WS-RELATIVE-KEY                 PIC 9(8) COMP.

       PROCEDURE DIVISION.

      *--- READ BY RELATIVE RECORD NUMBER ---
       READ-RELATIVE-RECORD.
           MOVE 12345 TO WS-RELATIVE-KEY
           READ RELATIVE-FILE
               INVALID KEY
                   PERFORM HANDLE-NOT-FOUND
           END-READ
           .

      *--- WRITE TO SPECIFIC SLOT ---
       WRITE-RELATIVE-RECORD.
           MOVE 12345 TO WS-RELATIVE-KEY
           WRITE RELATIVE-RECORD
               INVALID KEY
                   PERFORM HANDLE-SLOT-ERROR
           END-WRITE
           .
```

---

## String Manipulation

### STRING Statement Patterns

```cobol
       WORKING-STORAGE SECTION.

       01  WS-STRING-FIELDS.
           05  WS-FIRST-NAME               PIC X(20) VALUE 'JOHN'.
           05  WS-MIDDLE-INIT              PIC X     VALUE 'Q'.
           05  WS-LAST-NAME                PIC X(30) VALUE 'PUBLIC'.
           05  WS-FULL-NAME                PIC X(60) VALUE SPACES.
           05  WS-STRING-PTR               PIC 99    VALUE 1.

       01  WS-ADDRESS-PARTS.
           05  WS-STREET                   PIC X(30) VALUE '123 MAIN ST'.
           05  WS-CITY                     PIC X(20) VALUE 'ANYTOWN'.
           05  WS-STATE                    PIC XX    VALUE 'NY'.
           05  WS-ZIP                      PIC X(10) VALUE '12345-6789'.
           05  WS-FULL-ADDRESS             PIC X(100) VALUE SPACES.

       PROCEDURE DIVISION.

      *--- BASIC STRING CONCATENATION ---
       BUILD-FULL-NAME.
           INITIALIZE WS-FULL-NAME
           MOVE 1 TO WS-STRING-PTR

           STRING WS-FIRST-NAME DELIMITED BY '  '
                  ' '           DELIMITED BY SIZE
                  WS-MIDDLE-INIT DELIMITED BY SIZE
                  '. '          DELIMITED BY SIZE
                  WS-LAST-NAME  DELIMITED BY '  '
               INTO WS-FULL-NAME
               WITH POINTER WS-STRING-PTR
               ON OVERFLOW
                   DISPLAY 'STRING OVERFLOW - NAME TOO LONG'
           END-STRING
           .

      *--- BUILD FORMATTED ADDRESS ---
       BUILD-FULL-ADDRESS.
           INITIALIZE WS-FULL-ADDRESS
           MOVE 1 TO WS-STRING-PTR

           STRING WS-STREET    DELIMITED BY '  '
                  ', '         DELIMITED BY SIZE
                  WS-CITY      DELIMITED BY '  '
                  ', '         DELIMITED BY SIZE
                  WS-STATE     DELIMITED BY SIZE
                  ' '          DELIMITED BY SIZE
                  WS-ZIP       DELIMITED BY '  '
               INTO WS-FULL-ADDRESS
               WITH POINTER WS-STRING-PTR
           END-STRING
           .

      *--- STRING WITH MULTIPLE SOURCES ---
       BUILD-CSV-RECORD.
           INITIALIZE WS-CSV-OUTPUT
           MOVE 1 TO WS-STRING-PTR

           STRING WS-FIELD-1   DELIMITED BY SPACE
                  ','          DELIMITED BY SIZE
                  WS-FIELD-2   DELIMITED BY SPACE
                  ','          DELIMITED BY SIZE
                  WS-FIELD-3   DELIMITED BY SPACE
               INTO WS-CSV-OUTPUT
               WITH POINTER WS-STRING-PTR
           END-STRING
           .
```

### UNSTRING Statement Patterns

```cobol
       WORKING-STORAGE SECTION.

       01  WS-INPUT-LINE                   PIC X(200).
       01  WS-DELIMITER                    PIC X VALUE ','.

       01  WS-PARSED-FIELDS.
           05  WS-FIELD-1                  PIC X(30).
           05  WS-FIELD-2                  PIC X(30).
           05  WS-FIELD-3                  PIC X(30).
           05  WS-FIELD-4                  PIC X(30).
           05  WS-FIELD-5                  PIC X(30).

       01  WS-FIELD-COUNTS.
           05  WS-COUNT-1                  PIC 99.
           05  WS-COUNT-2                  PIC 99.
           05  WS-COUNT-3                  PIC 99.
           05  WS-COUNT-4                  PIC 99.
           05  WS-COUNT-5                  PIC 99.

       01  WS-DELIM-FOUND                  PIC X(5).
       01  WS-UNSTRING-PTR                 PIC 999 VALUE 1.
       01  WS-FIELDS-EXTRACTED             PIC 99  VALUE 0.

       PROCEDURE DIVISION.

      *--- PARSE CSV LINE ---
       PARSE-CSV-LINE.
           INITIALIZE WS-PARSED-FIELDS
           MOVE 1 TO WS-UNSTRING-PTR
           MOVE 0 TO WS-FIELDS-EXTRACTED

           UNSTRING WS-INPUT-LINE
               DELIMITED BY ',' OR ALL SPACES
               INTO WS-FIELD-1  COUNT IN WS-COUNT-1
                    WS-FIELD-2  COUNT IN WS-COUNT-2
                    WS-FIELD-3  COUNT IN WS-COUNT-3
                    WS-FIELD-4  COUNT IN WS-COUNT-4
                    WS-FIELD-5  COUNT IN WS-COUNT-5
               WITH POINTER WS-UNSTRING-PTR
               TALLYING IN WS-FIELDS-EXTRACTED
               ON OVERFLOW
                   DISPLAY 'MORE FIELDS THAN EXPECTED'
           END-UNSTRING
           .

      *--- PARSE NAME INTO PARTS ---
       PARSE-FULL-NAME.
           MOVE WS-FULL-NAME TO WS-INPUT-LINE
           INITIALIZE WS-FIRST-NAME WS-LAST-NAME

           UNSTRING WS-INPUT-LINE
               DELIMITED BY ALL SPACES OR ','
               INTO WS-FIRST-NAME
                    WS-LAST-NAME
           END-UNSTRING
           .

      *--- PARSE WITH DELIMITER CAPTURE ---
       PARSE-WITH-DELIMITERS.
           UNSTRING WS-INPUT-LINE
               DELIMITED BY '/' OR '-' OR '.'
               INTO WS-PART-1  DELIMITER IN WS-DELIM-1
                    WS-PART-2  DELIMITER IN WS-DELIM-2
                    WS-PART-3
           END-UNSTRING
           .
```

### INSPECT Statement Patterns

```cobol
       WORKING-STORAGE SECTION.

       01  WS-TEXT-STRING                  PIC X(100).
       01  WS-TALLY-COUNT                  PIC 9(5) VALUE 0.
       01  WS-CHAR-TO-FIND                 PIC X VALUE 'A'.

       PROCEDURE DIVISION.

      *--- COUNT OCCURRENCES ---
       COUNT-CHARACTER.
           MOVE ZEROS TO WS-TALLY-COUNT
           INSPECT WS-TEXT-STRING
               TALLYING WS-TALLY-COUNT
               FOR ALL WS-CHAR-TO-FIND
           DISPLAY 'COUNT OF ' WS-CHAR-TO-FIND ': ' WS-TALLY-COUNT
           .

      *--- COUNT LEADING CHARACTERS ---
       COUNT-LEADING-ZEROS.
           MOVE ZEROS TO WS-TALLY-COUNT
           INSPECT WS-NUMERIC-STRING
               TALLYING WS-TALLY-COUNT
               FOR LEADING '0'
           .

      *--- COUNT BEFORE INITIAL ---
       COUNT-BEFORE-SPACE.
           MOVE ZEROS TO WS-TALLY-COUNT
           INSPECT WS-TEXT-STRING
               TALLYING WS-TALLY-COUNT
               FOR CHARACTERS BEFORE INITIAL SPACE
           .

      *--- REPLACE ALL OCCURRENCES ---
       REPLACE-CHARACTERS.
           INSPECT WS-TEXT-STRING
               REPLACING ALL 'OLD' BY 'NEW'
           .

      *--- REPLACE LEADING ---
       REMOVE-LEADING-ZEROS.
           INSPECT WS-NUMERIC-STRING
               REPLACING LEADING '0' BY SPACE
           .

      *--- REPLACE FIRST OCCURRENCE ---
       REPLACE-FIRST.
           INSPECT WS-TEXT-STRING
               REPLACING FIRST 'ABC' BY 'XYZ'
           .

      *--- CONVERT TO UPPERCASE ---
       CONVERT-TO-UPPER.
           INSPECT WS-TEXT-STRING
               CONVERTING 'abcdefghijklmnopqrstuvwxyz'
               TO         'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
           .

      *--- CONVERT TO LOWERCASE ---
       CONVERT-TO-LOWER.
           INSPECT WS-TEXT-STRING
               CONVERTING 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
               TO         'abcdefghijklmnopqrstuvwxyz'
           .

      *--- REPLACE SPECIAL CHARACTERS ---
       SANITIZE-STRING.
           INSPECT WS-TEXT-STRING
               REPLACING ALL X'00' BY SPACE
                         ALL X'0A' BY SPACE
                         ALL X'0D' BY SPACE
           .

      *--- COMBINED TALLYING AND REPLACING ---
       COUNT-AND-REPLACE.
           MOVE ZEROS TO WS-TALLY-COUNT
           INSPECT WS-TEXT-STRING
               TALLYING WS-TALLY-COUNT FOR ALL 'ERROR'
               REPLACING ALL 'ERROR' BY 'FIXED'
           DISPLAY 'ERRORS FIXED: ' WS-TALLY-COUNT
           .
```

---

## Table Operations

### SEARCH Statement (Sequential)

```cobol
       WORKING-STORAGE SECTION.

       01  WS-STATE-TABLE.
           05  WS-STATE-ENTRY OCCURS 50 TIMES
               INDEXED BY WS-STATE-IDX.
               10  WS-STATE-CODE           PIC XX.
               10  WS-STATE-NAME           PIC X(20).
               10  WS-STATE-TAX-RATE       PIC V999.

       01  WS-STATE-VALUES.
           05  FILLER PIC X(25) VALUE 'ALALABAMA            075'.
           05  FILLER PIC X(25) VALUE 'AKAALASKA            000'.
           05  FILLER PIC X(25) VALUE 'AZARIZONA            056'.
           05  FILLER PIC X(25) VALUE 'ARARKANSAS           065'.
           05  FILLER PIC X(25) VALUE 'CACALIFORNIA         073'.
      *    ... more states ...

       01  WS-STATE-VALUES-REDEF REDEFINES WS-STATE-VALUES.
           05  WS-STATE-INIT OCCURS 50 TIMES.
               10  WS-INIT-CODE            PIC XX.
               10  WS-INIT-NAME            PIC X(20).
               10  WS-INIT-RATE            PIC 999.

       01  WS-SEARCH-CODE                  PIC XX.
       01  WS-FOUND-SW                     PIC X VALUE 'N'.
           88  FOUND                             VALUE 'Y'.
           88  NOT-FOUND                         VALUE 'N'.

       PROCEDURE DIVISION.

      *--- LOAD TABLE FROM DATA ---
       LOAD-STATE-TABLE.
           PERFORM VARYING WS-STATE-IDX FROM 1 BY 1
               UNTIL WS-STATE-IDX > 50
               MOVE WS-INIT-CODE(WS-STATE-IDX)
                   TO WS-STATE-CODE(WS-STATE-IDX)
               MOVE WS-INIT-NAME(WS-STATE-IDX)
                   TO WS-STATE-NAME(WS-STATE-IDX)
               DIVIDE WS-INIT-RATE(WS-STATE-IDX) BY 1000
                   GIVING WS-STATE-TAX-RATE(WS-STATE-IDX)
           END-PERFORM
           .

      *--- SEQUENTIAL SEARCH ---
       SEARCH-STATE-TABLE.
           SET NOT-FOUND TO TRUE
           SET WS-STATE-IDX TO 1

           SEARCH WS-STATE-ENTRY
               AT END
                   DISPLAY 'STATE NOT FOUND: ' WS-SEARCH-CODE
               WHEN WS-STATE-CODE(WS-STATE-IDX) = WS-SEARCH-CODE
                   SET FOUND TO TRUE
                   DISPLAY 'FOUND: ' WS-STATE-NAME(WS-STATE-IDX)
                   DISPLAY 'TAX RATE: ' WS-STATE-TAX-RATE(WS-STATE-IDX)
           END-SEARCH
           .
```

### SEARCH ALL (Binary Search)

```cobol
       WORKING-STORAGE SECTION.

       01  WS-PRODUCT-TABLE.
           05  WS-PRODUCT-COUNT            PIC 999 VALUE 0.
           05  WS-PRODUCT-ENTRY OCCURS 1000 TIMES
               ASCENDING KEY IS WS-PROD-CODE
               INDEXED BY WS-PROD-IDX.
               10  WS-PROD-CODE            PIC X(10).
               10  WS-PROD-DESC            PIC X(40).
               10  WS-PROD-PRICE           PIC S9(7)V99 COMP-3.
               10  WS-PROD-QTY-ON-HAND     PIC S9(7) COMP-3.

       01  WS-SEARCH-PROD-CODE             PIC X(10).

       PROCEDURE DIVISION.

      *--- BINARY SEARCH (TABLE MUST BE SORTED) ---
       SEARCH-PRODUCT-BINARY.
           SEARCH ALL WS-PRODUCT-ENTRY
               AT END
                   PERFORM PRODUCT-NOT-FOUND
               WHEN WS-PROD-CODE(WS-PROD-IDX) = WS-SEARCH-PROD-CODE
                   PERFORM PRODUCT-FOUND
           END-SEARCH
           .

       PRODUCT-FOUND.
           DISPLAY 'PRODUCT: ' WS-PROD-DESC(WS-PROD-IDX)
           DISPLAY 'PRICE:   ' WS-PROD-PRICE(WS-PROD-IDX)
           .

       PRODUCT-NOT-FOUND.
           DISPLAY 'PRODUCT CODE NOT FOUND: ' WS-SEARCH-PROD-CODE
           .
```

### Multi-Dimensional Tables

```cobol
       WORKING-STORAGE SECTION.

       01  WS-SALES-TABLE.
           05  WS-REGION OCCURS 4 TIMES
               INDEXED BY WS-REG-IDX.
               10  WS-MONTH OCCURS 12 TIMES
                   INDEXED BY WS-MON-IDX.
                   15  WS-PRODUCT OCCURS 100 TIMES
                       INDEXED BY WS-PRD-IDX.
                       20  WS-SALES-AMT    PIC S9(9)V99 COMP-3.
                       20  WS-SALES-QTY    PIC S9(7) COMP-3.

       01  WS-REGION-TOTALS.
           05  WS-REG-TOTAL OCCURS 4 TIMES  PIC S9(11)V99 COMP-3.

       PROCEDURE DIVISION.

      *--- CALCULATE REGION TOTALS ---
       CALC-REGION-TOTALS.
           INITIALIZE WS-REGION-TOTALS

           PERFORM VARYING WS-REG-IDX FROM 1 BY 1
               UNTIL WS-REG-IDX > 4
               PERFORM VARYING WS-MON-IDX FROM 1 BY 1
                   UNTIL WS-MON-IDX > 12
                   PERFORM VARYING WS-PRD-IDX FROM 1 BY 1
                       UNTIL WS-PRD-IDX > 100
                       ADD WS-SALES-AMT(WS-REG-IDX, WS-MON-IDX,
                           WS-PRD-IDX)
                           TO WS-REG-TOTAL(WS-REG-IDX)
                   END-PERFORM
               END-PERFORM
           END-PERFORM
           .
```

---

## Date Handling

### Date Manipulation Patterns

```cobol
       WORKING-STORAGE SECTION.

       01  WS-DATE-FIELDS.
           05  WS-CURRENT-DATE-INT         PIC 9(8).
           05  WS-CURRENT-DATE-INT-R REDEFINES WS-CURRENT-DATE-INT.
               10  WS-CURR-YYYY            PIC 9(4).
               10  WS-CURR-MM              PIC 9(2).
               10  WS-CURR-DD              PIC 9(2).
           05  WS-FORMATTED-DATE           PIC X(10).
           05  WS-JULIAN-DATE              PIC 9(7).

       01  WS-DATE-CALC-FIELDS.
           05  WS-INTEGER-DATE-1           PIC S9(9) COMP.
           05  WS-INTEGER-DATE-2           PIC S9(9) COMP.
           05  WS-DAYS-DIFFERENCE          PIC S9(9) COMP.

       01  WS-DAY-OF-WEEK                  PIC 9.
           88  IS-SUNDAY                         VALUE 1.
           88  IS-MONDAY                         VALUE 2.
           88  IS-SATURDAY                       VALUE 7.

       01  WS-DAY-NAMES.
           05  FILLER PIC X(9) VALUE 'SUNDAY   '.
           05  FILLER PIC X(9) VALUE 'MONDAY   '.
           05  FILLER PIC X(9) VALUE 'TUESDAY  '.
           05  FILLER PIC X(9) VALUE 'WEDNESDAY'.
           05  FILLER PIC X(9) VALUE 'THURSDAY '.
           05  FILLER PIC X(9) VALUE 'FRIDAY   '.
           05  FILLER PIC X(9) VALUE 'SATURDAY '.
       01  WS-DAY-NAME-TABLE REDEFINES WS-DAY-NAMES.
           05  WS-DAY-NAME OCCURS 7 TIMES  PIC X(9).

       PROCEDURE DIVISION.

      *--- GET CURRENT DATE ---
       GET-SYSTEM-DATE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE-INT
           .

      *--- FORMAT DATE AS MM/DD/YYYY ---
       FORMAT-DATE-US.
           STRING WS-CURR-MM   DELIMITED BY SIZE
                  '/'          DELIMITED BY SIZE
                  WS-CURR-DD   DELIMITED BY SIZE
                  '/'          DELIMITED BY SIZE
                  WS-CURR-YYYY DELIMITED BY SIZE
               INTO WS-FORMATTED-DATE
           END-STRING
           .

      *--- CONVERT TO INTEGER DATE (DAYS SINCE EPOCH) ---
       CONVERT-TO-INTEGER-DATE.
           COMPUTE WS-INTEGER-DATE-1 =
               FUNCTION INTEGER-OF-DATE(WS-CURRENT-DATE-INT)
           .

      *--- CALCULATE DAYS BETWEEN DATES ---
       CALC-DATE-DIFFERENCE.
           COMPUTE WS-INTEGER-DATE-1 =
               FUNCTION INTEGER-OF-DATE(WS-START-DATE)
           COMPUTE WS-INTEGER-DATE-2 =
               FUNCTION INTEGER-OF-DATE(WS-END-DATE)
           COMPUTE WS-DAYS-DIFFERENCE =
               WS-INTEGER-DATE-2 - WS-INTEGER-DATE-1
           .

      *--- ADD DAYS TO DATE ---
       ADD-DAYS-TO-DATE.
           COMPUTE WS-INTEGER-DATE-1 =
               FUNCTION INTEGER-OF-DATE(WS-BASE-DATE)
           ADD WS-DAYS-TO-ADD TO WS-INTEGER-DATE-1
           COMPUTE WS-RESULT-DATE =
               FUNCTION DATE-OF-INTEGER(WS-INTEGER-DATE-1)
           .

      *--- GET DAY OF WEEK ---
       GET-DAY-OF-WEEK.
           COMPUTE WS-INTEGER-DATE-1 =
               FUNCTION INTEGER-OF-DATE(WS-CHECK-DATE)
           COMPUTE WS-DAY-OF-WEEK =
               FUNCTION MOD(WS-INTEGER-DATE-1, 7) + 1
           DISPLAY 'DAY OF WEEK: ' WS-DAY-NAME(WS-DAY-OF-WEEK)
           .

      *--- CONVERT GREGORIAN TO JULIAN ---
       CONVERT-TO-JULIAN.
           MOVE FUNCTION DAY-OF-INTEGER(
               FUNCTION INTEGER-OF-DATE(WS-GREGORIAN-DATE))
               TO WS-JULIAN-DATE
           .

      *--- CHECK IF LEAP YEAR ---
       CHECK-LEAP-YEAR.
           IF FUNCTION MOD(WS-CURR-YYYY, 400) = 0
               SET IS-LEAP-YEAR TO TRUE
           ELSE IF FUNCTION MOD(WS-CURR-YYYY, 100) = 0
               SET IS-NOT-LEAP-YEAR TO TRUE
           ELSE IF FUNCTION MOD(WS-CURR-YYYY, 4) = 0
               SET IS-LEAP-YEAR TO TRUE
           ELSE
               SET IS-NOT-LEAP-YEAR TO TRUE
           END-IF
           .

      *--- GET LAST DAY OF MONTH ---
       GET-LAST-DAY-OF-MONTH.
           EVALUATE WS-CURR-MM
               WHEN 1  WHEN 3  WHEN 5  WHEN 7
               WHEN 8  WHEN 10 WHEN 12
                   MOVE 31 TO WS-LAST-DAY
               WHEN 4  WHEN 6  WHEN 9  WHEN 11
                   MOVE 30 TO WS-LAST-DAY
               WHEN 2
                   IF IS-LEAP-YEAR
                       MOVE 29 TO WS-LAST-DAY
                   ELSE
                       MOVE 28 TO WS-LAST-DAY
                   END-IF
           END-EVALUATE
           .
```

---

## Error Handling

### Comprehensive Error Handling Pattern

```cobol
       WORKING-STORAGE SECTION.

       01  WS-ERROR-HANDLING.
           05  WS-ERROR-CODE               PIC S9(4) COMP VALUE 0.
           05  WS-ERROR-MSG                PIC X(80) VALUE SPACES.
           05  WS-SEVERITY                 PIC X VALUE SPACES.
               88  SEVERITY-INFO                 VALUE 'I'.
               88  SEVERITY-WARNING              VALUE 'W'.
               88  SEVERITY-ERROR                VALUE 'E'.
               88  SEVERITY-FATAL                VALUE 'F'.
           05  WS-ERROR-COUNT              PIC 9(5) COMP VALUE 0.
           05  WS-MAX-ERRORS               PIC 9(5) COMP VALUE 100.

       01  WS-RETURN-CODES.
           05  WS-RETURN-CODE              PIC S9(4) COMP VALUE 0.
               88  RC-SUCCESS                    VALUE 0.
               88  RC-WARNING                    VALUE 4.
               88  RC-ERROR                      VALUE 8.
               88  RC-FATAL                      VALUE 12.
               88  RC-ABEND                      VALUE 16.

       01  WS-FILE-STATUS-TABLE.
           05  WS-FS-ENTRY OCCURS 20 TIMES
               INDEXED BY WS-FS-IDX.
               10  WS-FS-CODE              PIC XX.
               10  WS-FS-MSG               PIC X(50).

       01  WS-FS-VALUES.
           05  FILLER PIC X(52) VALUE '00SUCCESSFUL COMPLETION                           '.
           05  FILLER PIC X(52) VALUE '02DUPLICATE KEY - RECORD WRITTEN                  '.
           05  FILLER PIC X(52) VALUE '04RECORD LENGTH ERROR                             '.
           05  FILLER PIC X(52) VALUE '05FILE NOT PRESENT - OPTIONAL FILE                '.
           05  FILLER PIC X(52) VALUE '10END OF FILE REACHED                             '.
           05  FILLER PIC X(52) VALUE '21SEQUENCE ERROR ON SEQUENTIAL WRITE              '.
           05  FILLER PIC X(52) VALUE '22DUPLICATE KEY - RECORD NOT WRITTEN              '.
           05  FILLER PIC X(52) VALUE '23RECORD NOT FOUND                                '.
           05  FILLER PIC X(52) VALUE '24KEY BOUNDARY VIOLATION                          '.
           05  FILLER PIC X(52) VALUE '30PERMANENT I/O ERROR                             '.
           05  FILLER PIC X(52) VALUE '34BOUNDARY VIOLATION                              '.
           05  FILLER PIC X(52) VALUE '35FILE NOT FOUND                                  '.
           05  FILLER PIC X(52) VALUE '37OPEN MODE NOT PERMITTED                         '.
           05  FILLER PIC X(52) VALUE '38FILE LOCKED                                     '.
           05  FILLER PIC X(52) VALUE '39ATTRIBUTE CONFLICT                              '.
           05  FILLER PIC X(52) VALUE '41FILE ALREADY OPEN                               '.
           05  FILLER PIC X(52) VALUE '42FILE NOT OPEN                                   '.
           05  FILLER PIC X(52) VALUE '43READ NOT DONE BEFORE REWRITE/DELETE             '.
           05  FILLER PIC X(52) VALUE '44BOUNDARY VIOLATION ON REWRITE                   '.
           05  FILLER PIC X(52) VALUE '46READ AFTER END OF FILE                          '.
       01  WS-FS-VALUES-REDEF REDEFINES WS-FS-VALUES.
           05  WS-FS-INIT OCCURS 20 TIMES.
               10  WS-FSI-CODE             PIC XX.
               10  WS-FSI-MSG              PIC X(50).

       PROCEDURE DIVISION.

      *--- INITIALIZE ERROR TABLE ---
       INIT-ERROR-TABLE.
           PERFORM VARYING WS-FS-IDX FROM 1 BY 1
               UNTIL WS-FS-IDX > 20
               MOVE WS-FSI-CODE(WS-FS-IDX)
                   TO WS-FS-CODE(WS-FS-IDX)
               MOVE WS-FSI-MSG(WS-FS-IDX)
                   TO WS-FS-MSG(WS-FS-IDX)
           END-PERFORM
           .

      *--- CHECK FILE STATUS ---
       CHECK-FILE-STATUS.
           IF WS-FILE-STATUS NOT = '00' AND '10'
               PERFORM GET-FILE-STATUS-MESSAGE
               PERFORM LOG-ERROR
               IF WS-FILE-STATUS(1:1) = '3' OR '4' OR '9'
                   SET SEVERITY-FATAL TO TRUE
                   PERFORM ABEND-PROGRAM
               ELSE
                   SET SEVERITY-ERROR TO TRUE
               END-IF
           END-IF
           .

      *--- GET FILE STATUS MESSAGE ---
       GET-FILE-STATUS-MESSAGE.
           SET WS-FS-IDX TO 1
           SEARCH WS-FS-ENTRY
               AT END
                   MOVE 'UNKNOWN FILE STATUS' TO WS-ERROR-MSG
               WHEN WS-FS-CODE(WS-FS-IDX) = WS-FILE-STATUS
                   MOVE WS-FS-MSG(WS-FS-IDX) TO WS-ERROR-MSG
           END-SEARCH
           .

      *--- LOG ERROR ---
       LOG-ERROR.
           ADD 1 TO WS-ERROR-COUNT
           DISPLAY WS-SEVERITY ' ERROR #' WS-ERROR-COUNT ': '
               WS-ERROR-MSG

           IF WS-ERROR-COUNT >= WS-MAX-ERRORS
               DISPLAY 'MAXIMUM ERROR COUNT REACHED'
               PERFORM ABEND-PROGRAM
           END-IF
           .

      *--- ABEND PROGRAM ---
       ABEND-PROGRAM.
           DISPLAY '****************************************'
           DISPLAY 'PROGRAM ABENDING'
           DISPLAY 'RETURN CODE: ' WS-RETURN-CODE
           DISPLAY 'ERROR COUNT: ' WS-ERROR-COUNT
           DISPLAY '****************************************'
           MOVE WS-RETURN-CODE TO RETURN-CODE
           STOP RUN.

      *--- ON SIZE ERROR HANDLING ---
       SAFE-COMPUTE.
           COMPUTE WS-RESULT = WS-VALUE-A / WS-VALUE-B
               ON SIZE ERROR
                   SET SEVERITY-ERROR TO TRUE
                   MOVE 'ARITHMETIC OVERFLOW/UNDERFLOW'
                       TO WS-ERROR-MSG
                   PERFORM LOG-ERROR
                   MOVE 0 TO WS-RESULT
           END-COMPUTE
           .
```

---

## JCL Snippets

### Standard Batch Job

```jcl
//JOBNAME  JOB (ACCT),'DESCRIPTION',
//         CLASS=A,MSGCLASS=X,
//         MSGLEVEL=(1,1),
//         NOTIFY=&SYSUID,
//         REGION=0M
//*
//*================================================================*
//* JOB: JOBNAME                                                   *
//* PURPOSE: Standard batch processing job template                *
//*================================================================*
//*
//JOBLIB   DD DSN=YOUR.LOAD.LIBRARY,DISP=SHR
//*
//*================================================================*
//* STEP010 - DELETE OUTPUT FILES IF EXIST                         *
//*================================================================*
//STEP010  EXEC PGM=IEFBR14
//OUTFILE  DD DSN=YOUR.OUTPUT.FILE,
//            DISP=(MOD,DELETE,DELETE),
//            SPACE=(TRK,0)
//*
//*================================================================*
//* STEP020 - EXECUTE COBOL PROGRAM                                *
//*================================================================*
//STEP020  EXEC PGM=PROGNAME,COND=(0,NE)
//STEPLIB  DD DSN=YOUR.LOAD.LIBRARY,DISP=SHR
//INFILE   DD DSN=YOUR.INPUT.FILE,DISP=SHR
//OUTFILE  DD DSN=YOUR.OUTPUT.FILE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//RPTFILE  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  Control cards here if needed
/*
//*
//*================================================================*
//* STEP030 - SORT OUTPUT FILE                                     *
//*================================================================*
//STEP030  EXEC PGM=SORT,COND=(0,NE)
//SORTIN   DD DSN=YOUR.OUTPUT.FILE,DISP=SHR
//SORTOUT  DD DSN=YOUR.SORTED.FILE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//SYSOUT   DD SYSOUT=*
//SYSIN    DD *
  SORT FIELDS=(1,10,CH,A)
/*
//
```

### VSAM File Definition

```jcl
//*================================================================*
//* DEFINE VSAM KSDS CLUSTER                                       *
//*================================================================*
//DEFVSAM  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE YOUR.VSAM.FILE -
         CLUSTER -
         PURGE
  SET MAXCC=0
  DEFINE CLUSTER -
         (NAME(YOUR.VSAM.FILE) -
          INDEXED -
          KEYS(10 0) -
          RECORDSIZE(200 500) -
          SHAREOPTIONS(2 3) -
          SPEED -
          FREESPACE(20 10)) -
         DATA -
         (NAME(YOUR.VSAM.FILE.DATA) -
          CYLINDERS(10 5)) -
         INDEX -
         (NAME(YOUR.VSAM.FILE.INDEX) -
          TRACKS(5 1))
/*
//*================================================================*
//* DEFINE ALTERNATE INDEX                                         *
//*================================================================*
//DEFAIX   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE ALTERNATEINDEX -
         (NAME(YOUR.VSAM.FILE.AIX1) -
          RELATE(YOUR.VSAM.FILE) -
          KEYS(30 10) -
          UNIQUEKEY -
          UPGRADE) -
         DATA -
         (NAME(YOUR.VSAM.FILE.AIX1.DATA) -
          CYLINDERS(2 1)) -
         INDEX -
         (NAME(YOUR.VSAM.FILE.AIX1.INDEX) -
          TRACKS(2 1))
  DEFINE PATH -
         (NAME(YOUR.VSAM.FILE.PATH1) -
          PATHENTRY(YOUR.VSAM.FILE.AIX1))
  BLDINDEX -
         INDATASET(YOUR.VSAM.FILE) -
         OUTDATASET(YOUR.VSAM.FILE.AIX1)
/*
```

### PROC with Parameters

```jcl
//*================================================================*
//* PROCEDURE: MYPROC - REUSABLE BATCH PROCESSING PROCEDURE        *
//*================================================================*
//MYPROC   PROC ENV='PROD',
//              HLQ='PROD.DATA',
//              REGION='0M'
//*
//STEP010  EXEC PGM=MYPROG,REGION=&REGION
//STEPLIB  DD DSN=&ENV..LOAD.LIBRARY,DISP=SHR
//INFILE   DD DSN=&HLQ..INPUT.FILE,DISP=SHR
//OUTFILE  DD DSN=&HLQ..OUTPUT.FILE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5),RLSE)
//SYSOUT   DD SYSOUT=*
//         PEND
//*
//*--- EXECUTE PROCEDURE ---
//RUNPROD  EXEC MYPROC,ENV='PROD',HLQ='PROD.DATA'
//*
//RUNTEST  EXEC MYPROC,ENV='TEST',HLQ='TEST.DATA'
```

---

## Copybook Patterns

### Standard Record Layout Copybook

```cobol
      *================================================================*
      * COPYBOOK: CUSTREC                                              *
      * PURPOSE:  Customer master record layout                        *
      * VERSION:  2.0                                                  *
      * MODIFIED: 2025-12-31                                           *
      *================================================================*

       01  CUSTOMER-RECORD.
           05  CUST-KEY.
               10  CUST-REGION             PIC XX.
                   88  CUST-REGION-EAST          VALUE 'EA'.
                   88  CUST-REGION-WEST          VALUE 'WE'.
                   88  CUST-REGION-NORTH         VALUE 'NO'.
                   88  CUST-REGION-SOUTH         VALUE 'SO'.
               10  CUST-NUMBER             PIC 9(8).
           05  CUST-NAME-DATA.
               10  CUST-LAST-NAME          PIC X(30).
               10  CUST-FIRST-NAME         PIC X(20).
               10  CUST-MIDDLE-INIT        PIC X.
           05  CUST-ADDRESS-DATA.
               10  CUST-STREET-1           PIC X(30).
               10  CUST-STREET-2           PIC X(30).
               10  CUST-CITY               PIC X(25).
               10  CUST-STATE              PIC XX.
               10  CUST-ZIP.
                   15  CUST-ZIP-5          PIC X(5).
                   15  CUST-ZIP-4          PIC X(4).
               10  CUST-COUNTRY            PIC X(3).
           05  CUST-CONTACT-DATA.
               10  CUST-PHONE              PIC X(15).
               10  CUST-EMAIL              PIC X(50).
           05  CUST-FINANCIAL-DATA.
               10  CUST-CREDIT-LIMIT       PIC S9(9)V99 COMP-3.
               10  CUST-CURRENT-BALANCE    PIC S9(9)V99 COMP-3.
               10  CUST-YTD-PURCHASES      PIC S9(11)V99 COMP-3.
               10  CUST-YTD-PAYMENTS       PIC S9(11)V99 COMP-3.
           05  CUST-STATUS-DATA.
               10  CUST-STATUS             PIC X.
                   88  CUST-ACTIVE               VALUE 'A'.
                   88  CUST-INACTIVE             VALUE 'I'.
                   88  CUST-SUSPENDED            VALUE 'S'.
                   88  CUST-CLOSED               VALUE 'C'.
               10  CUST-TYPE               PIC XX.
                   88  CUST-TYPE-RETAIL          VALUE 'RT'.
                   88  CUST-TYPE-WHOLESALE       VALUE 'WH'.
                   88  CUST-TYPE-GOVT            VALUE 'GV'.
           05  CUST-DATE-DATA.
               10  CUST-CREATE-DATE        PIC 9(8).
               10  CUST-LAST-ACTIVITY-DATE PIC 9(8).
               10  CUST-LAST-PAYMENT-DATE  PIC 9(8).
           05  FILLER                      PIC X(20).
```

---

*ARCHAEON Arsenal - COBOL Division*
*"The enterprise runs on COBOL - 220+ billion lines and counting"*
