#lang eopl

;; █████████████████████████████████████████████████████████
;; PROYECTO FINAL: LOGICFUSION
;; Módulo 1: Arquitectura Base, Léxico y Gramática Global
;; █████████████████████████████████████████████████████████

;; █████████████████████████████████████████████████████████
;; PUNTO 2.1 Y 2.2.1: ESPECIFICACIÓN LÉXICA (Scanner Spec)
;; █████████████████████████████████████████████████████████
(define scanner-spec-logicfusion
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit "_" "?"))) symbol)
    (entero (digit (arbno digit)) number)
    (entero ("-" digit (arbno digit)) number)
    (flotante (digit (arbno digit) "." digit (arbno digit)) number)
    (flotante ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (cadena ("\"" (arbno (not "\"")) "\"") string)))

;; █████████████████████████████████████████████████████████
;; PUNTO 2.2.1: BLOQUES PRINCIPALES Y ESTRUCTURAS GLOBALES
;; █████████████████████████████████████████████████████████
(define grammar-spec-logicfusion
  '(;; Bloques Principales: Estrategia de bloques GLOBALS y PROGRAM
    (program ("GLOBALS" "{" (arbno global-decl) "}" "PROGRAM" "{" (arbno proc-decl) "}")
             un-programa)

    ;; Especificación de Tipos (Primitivos y Compuestos)
    (type-spec ("int") int-type)
    (type-spec ("float") float-type)
    (type-spec ("string") string-type)
    (type-spec ("bool") bool-type)
    (type-spec ("void") void-type)
    (type-spec ("sat") sat-type)
    (type-spec ("list" "<" type-spec ">") list-type)
    (type-spec ("vector" "<" type-spec ">") vector-type)
    (type-spec ("dict" "<" type-spec "," type-spec ">") dict-type)
    (type-spec ("proc") proc-type)

    ;; Definición de parámetro tipado (Tipo + Nombre)
    (param-decl (type-spec identifier) un-param-decl)

    ;; Estructuras Globales: Declaraciones de variables, constantes y funciones
    (global-decl (type-spec identifier "=" expression ";") var-global-decl)
    (global-decl ("const" type-spec identifier "=" expression ";") const-global-decl)
    (global-decl ("proc" identifier "=" "lambda" "(" (separated-list param-decl ",") ")" "{" "return" expression "}" ";")
                 proc-global-decl)

    ;; Bloques Principales: Estructura de funciones y proc main()
    (proc-decl ("proc" identifier "(" (separated-list param-decl ",") ")" "{" "return" expression "}")
               main-proc-decl)

    ;; Expresiones básicas y Literales
    (expression (entero) entero-lit)
    (expression (flotante) flotante-lit)
    (expression (cadena) cadena-lit)
    (expression ("True") true-exp)
    (expression ("False") false-exp)
    (expression (identifier) var-exp)

    ;; Invocación de Funciones/Procedimientos
    (expression (identifier "(" (separated-list expression ",") ")") app-exp)

    ;; Estructuras Globales: Secuenciación mediante el bloque BLOCK
    (expression ("BLOCK" "{" expression (arbno ";" expression) "}") block-exp)))


;; █████████████████████████████████████████████████████████
;; CONSTRUCCIÓN AUTOMÁTICA DEL PARSER CON SLLGEN
;; █████████████████████████████████████████████████████████
(sllgen:make-define-datatypes scanner-spec-logicfusion grammar-spec-logicfusion)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-logicfusion grammar-spec-logicfusion))

;; █████████████████████████████████████████████████████████
;; PUNTO 2: GESTIÓN DEL REPOSITORIO - PRUEBAS CON SCAN&PARSE
;; █████████████████████████████████████████████████████████
(define prueba-completa
  "GLOBALS {
    int a = 10;
    float pi = 3.1415;
    string nombre = \"Robinson\";
    bool docente? = True;
    const int maximo = 100;
    proc square = lambda (int x) { return BLOCK { a; x } };
  }
  PROGRAM {
    proc main () { return square(a) }
  }")

;; Para probar en la consola de DrRacket:
;; (scan&parse prueba-completa)
