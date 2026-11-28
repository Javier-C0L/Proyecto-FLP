#lang eopl

;; =============================================================================
;; Parte 1 – Javier.
;; Parte corregida y ajustada.
;; =============================================================================

;; 1. Especificación léxica.

(define scanner-spec-logicfusion
  '(
    (white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)

    (identifier (letter (arbno (or letter digit "_" "?"))) symbol)

    (entero (digit (arbno digit)) number)
    (entero ("-" digit (arbno digit)) number)

    (flotante (digit (arbno digit) "." digit (arbno digit)) number)
    (flotante ("-" digit (arbno digit) "." digit (arbno digit)) number)

    (cadena (#\" (arbno (not #\")) #\") string)
  )
)

;; =============================================================================
;; 2. Especificación gramatical.

(define grammar-spec-logicfusion ;; Programa principal.
  '(
    (program
      ("GLOBALS" "{" (arbno global-decl) "}" "PROGRAM" "{" (arbno proc-decl) "}")
      un-programa)

    ;; Especificación de tipos de variable (no tocar).

    (type-spec ("int") int-type)
    (type-spec ("float") float-type)
    (type-spec ("string") string-type)
    (type-spec ("bool") bool-type)
    (type-spec ("void") void-type)
    (type-spec ("sat") sat-type)
    (type-spec ("list" "<" type-spec ">") list-type)
    (type-spec ("vector" "<" type-spec ">") vector-type)
    (type-spec ("dict" "<" type-spec "," type-spec ">") dict-type)
    (type-spec ("function") proc-type)

    ;; Parámetros tipados.

    (param-decl
      (type-spec identifier)
      un-param-decl)

    ;; Declaraciones globales.

    (global-decl
      (type-spec identifier "=" expression ";")
      var-global-decl)

    (global-decl
      ("const" type-spec identifier "=" expression ";")
      const-global-decl)

    (global-decl
      ("proc" identifier "=" "lambda" "(" (separated-list param-decl ",") ")" "{" "return" expression "}" ";")
      proc-global-decl)

    ;; Declaraciones de procedimientos

    (proc-decl
      ("proc" identifier "(" (separated-list param-decl ",") ")" "{" "return" expression "}")
      proc-header-decl)

    ;; =========================================================================
    ;; Expresiones separadas por tipo, mantener el orden para mejor lectura.
    ;; (No tocar a menos de que sea absolutamente necesario (🙏).

    ;; Literales

    (expression (entero) entero-lit)
    (expression (flotante) flotante-lit)
    (expression (cadena) cadena-lit)
    (expression ("True") true-exp)
    (expression ("False") false-exp)

    ;; Identificadores: variables, llamadas y asignaciones.
    ;; Se reúne en un solo lugar para evitar errores comunes al unir los aportes.
    ;; (Por favor, mantener el orden).

    (expression
      (identifier identifier-tail)
      identifier-exp)

    (identifier-tail
      ("(" (separated-list expression ",") ")")
      call-tail)

    (identifier-tail
      ("->" expression)
      assign-tail)

    (identifier-tail
      ()
      variable-tail)

    ;; Declaraciones locales.

    (local-decl
      (type-spec identifier "=" expression ";")
      var-local-decl)

    (local-decl
      ("const" type-spec identifier "=" expression ";")
      const-local-decl)

    (local-decl
      ("proc" identifier "=" "lambda"
       "(" (separated-list param-decl ",") ")"
       "{" "return" expression "}" ";")
      proc-local-decl)

    ;; Bloques de secuenciación.

    (expression
      ("BLOCK" "{" expression (arbno ";" expression) "}")
      block-exp)

    ;; Bloques con declaraciones locales.

    (expression
      ("LOCALS" "{"
       (arbno local-decl)
       "}" "{"
       expression (arbno ";" expression)
       "}")
      locals-exp)

    ;; Estructuras de control.
    ;; if siempre lleva else. switch siempre lleva default.
    ;;
    ;; if (cond) { exp; exp } else { exp }
    ;; switch (exp) { case exp : exp default : exp }
    ;; while (cond) { exp; exp }
    ;; for (inicio; cond; actualizacion) { exp }
    ;; lambda (tipo nombre, ...) { return exp }

    (expression
      ("if" "(" expression ")" "{"
       expression (arbno ";" expression)
       "}" "else" "{"
       expression (arbno ";" expression)
       "}")
      if-exp)

    (expression
      ("switch" "(" expression ")" "{"
       (arbno switch-case)
       switch-default
       "}")
      switch-exp)

    (switch-case
      ("case" expression ":" expression)
      un-case)

    (switch-default
      ("default" ":" expression)
      un-default)

    (expression
      ("while" "(" expression ")" "{"
       expression (arbno ";" expression)
       "}")
      while-exp)

    (expression
      ("for" "(" expression ";" expression ";" expression ")" "{"
       expression (arbno ";" expression)
       "}")
      for-exp)

    ;; lambda como expresión. Así el procedimiento es un valor.
    (expression
      ("lambda" "(" (separated-list param-decl ",") ")"
       "{" "return" expression "}")
      lambda-exp)

    ;; Operadores binarios. 
    ;; Si, por x o y razón, hay que manipular la estructura, avisar al grupo entero.

    (expression
      ("(" expression binary-operator expression ")")
      binary-exp)

    (binary-operator ("+") plus-op)
    (binary-operator ("-") minus-op)
    (binary-operator ("*") times-op)
    (binary-operator ("/") divide-op)
    (binary-operator ("==") equal-op)
    (binary-operator ("!=") not-equal-op)
    (binary-operator (">") greater-op)
    (binary-operator ("<") less-op)

    ;; Impresión por consola.

    (expression
      ("print" "(" (separated-list expression ",") ")")
      print-exp)
  )
)

;; ===========================================================================
;; 3. Datatypes y parser (No crear redundancias).

(sllgen:make-define-datatypes
  scanner-spec-logicfusion
  grammar-spec-logicfusion)

(define show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes
      scanner-spec-logicfusion
      grammar-spec-logicfusion)))

(define scan&parse
  (sllgen:make-string-parser
    scanner-spec-logicfusion
    grammar-spec-logicfusion))

(define just-scan
  (sllgen:make-string-scanner
    scanner-spec-logicfusion
    grammar-spec-logicfusion))

;; =============================================================================
;; 4. Pruebas de la parte 1.

(define prueba-jav-1
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

;; =============================================================================
;; 5.Ejecución de prueba.
;; =============================================================================

;; Ejecutar:
;; (scan&parse prueba-jav-1)


;; =============================================================================
;; Parte 2 – Mafe.
;;
;; IMPORTANTE:
;; Cuidado con los datatypes. No los toquen (!).
;; =============================================================================


;; =============================================================================
;; 1. TAD ambientes.

(define-datatype environment environment?

  (empty-env-record)

  (extended-env-record
   (syms (list-of symbol?))
   (refs (list-of vector?))
   (types (list-of (lambda (x) #t)))
   (mutables (list-of boolean?))
   (amb-padre environment?))
)

(define empty-env
  (lambda ()
    (empty-env-record)))


;; =============================================================================
;; 2. Valores expresados.

(define-datatype exp-val exp-val?

  (num-val
   (val number?))

  (bool-val
   (val boolean?))

  (string-val
   (val string?))

  (proc-val
   (params (list-of param-decl?))  ;; Me apareció error, revisar.
   (body expression?)
   (saved-env environment?))

  (void-val)
)


;; ===============================================================================
;; 3. Tipos (NO tocar, deje eso quieto que ya funciona 😭).

(define type-spec->symbol
  (lambda (type)

    (cases type-spec type

      (int-type ()
        'int)

      (float-type ()
        'float)

      (string-type ()
        'string)

      (bool-type ()
        'bool)

      (void-type ()
        'void)

      (sat-type ()
        'sat)

      (list-type (inner)
        (list 'list (type-spec->symbol inner)))

      (vector-type (inner)
        (list 'vector (type-spec->symbol inner)))

      (dict-type (key-type value-type)
        (list 'dict
              (type-spec->symbol key-type)
              (type-spec->symbol value-type)))

      (proc-type ()
        'proc)
    )
  )
)


;; =============================================================================
;; 4. Verificación de tipos. No mover.

(define check-type-compat
  (lambda (expected-type val)

    (let ((type-sym
           (if (type-spec? expected-type)
               (type-spec->symbol expected-type)
               expected-type)))

      (cond

        ((eqv? type-sym 'void)
         #t)

        ((eqv? type-sym 'int)
         (cases exp-val val
           (num-val (n)
             (integer? n))
           (else #f))) ;; Corroboro usando booleanos.
        ((eqv? type-sym 'float)
         (cases exp-val val
           (num-val (n)
             (real? n))
           (else #f)))

        ((eqv? type-sym 'string)
         (cases exp-val val
           (string-val (s)
             #t)
           (else #f)))

        ((eqv? type-sym 'bool)
         (cases exp-val val
           (bool-val (b)
             #t)
           (else #f)))

        ((eqv? type-sym 'proc)
         (cases exp-val val
           (proc-val (p b e)
             #t)
           (else #f))) ;; Evaluación de procedimiento (revisar).

        (else #t)
      )
    )
  )
)


;; ===========================================================================
;; 5. Algunas funciones auxiliares para los ambientes (Cuidado con las redundancias).

;; Buscar la posición de un símbolo en una lista.
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls)
       #f)
      ((pred (car ls))
       0)
      (else
       (let ((result
              (list-index pred (cdr ls))))
         (if (number? result)
             (+ result 1)
             #f)))
    )
  )
)


(define list-find-position
  (lambda (sym los)

    (list-index
     (lambda (s)
       (eqv? s sym))
     los)
  )
)

;; Ambiente con una sola variable.

(define extend-env
  (lambda (id ref type mutable? env)

    (extended-env-record
     (list id)
     (list ref)
     (list type)
     (list mutable?)
     env)
  )
)

;; Buscar una variable en el ambiente actual y sus padres.

(define apply-env-ref
  (lambda (env search-var)

    (cases environment env

      (empty-env-record ()

        (eopl:error
         'apply-env-ref
         "Variable no declarada en el alcance: ~s"
         search-var)
      )

      (extended-env-record
       (syms refs types mutables amb-padre)

       (let ((pos
              (list-find-position search-var syms)))

         (if (number? pos)

             (list
              (list-ref refs pos)
              (list-ref types pos)
              (list-ref mutables pos))

             (apply-env-ref amb-padre search-var))
       )
      )
    )
  )
)


;; Obtener el valor almacenado en una variable.

(define lookup-var-val
  (lambda (env search-var)

    (let ((binding
           (apply-env-ref env search-var))) ;; (Revisar).

      (vector-ref
       (car binding)
       0)
    )
  )
)


;; Obtener los parámetros de una declaración.

(define get-param-symbols
  (lambda (params)

    (map
     (lambda (pd) ;; (Revisar).

       (cases param-decl pd

         (un-param-decl (type id)
           id)
       )
     )
     params)
  )
)


;; ====================================================================
;; 6. Evaluación de mutables.

(define eval-assign
  (lambda (id-sym expr env)

    (let ((binding
           (apply-env-ref env id-sym)))

      (let ((ref
             (car binding))
            (type
             (cadr binding))
            (mutable?
             (caddr binding)))

        (if (not mutable?)
            (eopl:error
             'eval-assign
             "No se puede modificar la constante inmutable: ~s"
             id-sym)

            (let ((val
                   (eval-expression expr env)))

              (if (check-type-compat type val)

                  (begin
                    (vector-set! ref 0 val)
                    val)

                  (eopl:error
                   'eval-assign
                   "Error de tipo: valor incompatible para la variable ~s"
                   id-sym)
              )
            )
        )
      )
    )
  )
)

;; =========================================================================
;; 7. Evaluación de declaraciones globales.

(define eval-global-decls
  (lambda (decls env)

    (if (null? decls)
        env
        (let ((decl
               (car decls)))
          (let ((new-env
                 (cases global-decl decl

                   ;; Variable global

                   (var-global-decl
                    (type id expr)

                    (let ((val
                           (eval-expression expr env)))
                      (if (check-type-compat type val)
                          (extend-env
                           id
                           (vector val)
                           type
                           #t
                           env)
                          (eopl:error
                           'eval-global-decls
                           "Error de tipo en variable global: ~s"
                           id)
                      )
                    )
                   )

                   ;; Constante global

                   (const-global-decl
                    (type id expr)

                    (let ((val
                           (eval-expression expr env)))

                      (if (check-type-compat type val)

                          (extend-env
                           id
                           (vector val)
                           type
                           #f
                           env)

                          (eopl:error
                           'eval-global-decls
                           "Error de tipo en constante global: ~s"
                           id)
                      )
                    )
                   )

                   ;; Procedimiento global

                   (proc-global-decl
                    (id params body)

                    (let* ((param-syms
                            (get-param-symbols params))

                           (ref
                            (vector #f))

                           (new-env
                            (extend-env
                             id ref 'proc #t env))

                           (proc
                            (proc-val
                             param-syms
                             body
                             new-env)))

                      ;; Permite que el procedimiento
                      ;; pueda reconocerse a sí mismo (Corrección de error omg).

                      (vector-set! ref 0 proc)

                      new-env
                    )
                   )
                 )
               ))

            (eval-global-decls
             (cdr decls)
             new-env)
          )
        )
    )
  )
)


;; ==========================================================================
;; 8. Evaluación de declaraciones locales.

(define eval-local-decl
  (lambda (decl env)
    (cases local-decl decl

      ;; Variable local

      (var-local-decl
       (type id expr)

       (let ((val
              (eval-expression expr env)))

         (if (check-type-compat type val)

             (extend-env
              id
              (vector val)
              type
              #t
              env)

             (eopl:error
              'eval-local-decl
              "Error de tipo en declaracion local: ~s"
              id)
         )
       )
      )

      ;; Constante local

      (const-local-decl
       (type id expr)

       (let ((val
              (eval-expression expr env)))

         (if (check-type-compat type val)

             (extend-env
              id
              (vector val)
              type
              #f
              env)

             (eopl:error
              'eval-local-decl
              "Error de tipo en constante local: ~s"
              id)
         )
       )
      )

      ;; Procedimiento local

      (proc-local-decl
       (id params body)

       (let* ((param-syms
               (get-param-symbols params))

              (ref
               (vector #f))

              (new-env
               (extend-env
                id ref 'proc #t env))

              (proc
               (proc-val
                param-syms
                body
                new-env)))

         (vector-set! ref 0 proc)

         new-env
       )
      )
    )
  )
)


;; =============================================================================
;; 9. Evaluación de secuencias y bloques (Revisar y juntar con mod 1).

(define eval-expression-sequence
  (lambda (exps env)

    (cond

      ((null? exps)
       (void-val))

      ((null? (cdr exps))
       (eval-expression (car exps) env))

      (else

       (begin

         (eval-expression (car exps) env)

         (eval-expression-sequence
          (cdr exps)
          env)
       )
      )
    )
  )
)


(define eval-locals-exp
  (lambda (decls exps env)

    (let loop ((rest-decls decls)
               (current-env env))

      (if (null? rest-decls)

          (eval-expression-sequence
           exps
           current-env)

          (loop
           (cdr rest-decls)

           (eval-local-decl
            (car rest-decls)
            current-env)
          )
      )
    )
  )
)

;; ===========================================================================
;; 10. Aplicación de procedimientos.

(define apply-procedure
  (lambda (proc args)

    (cases exp-val proc

      (proc-val
       (params body saved-env)

       (if (= (length params) (length args))

           (let ((refs
                  (map
                   (lambda (arg)
                     (vector arg))
                   args))

                 (types
                  (map
                   (lambda (param)
                     'void)
                   params))

                 (mutables
                  (map
                   (lambda (param)
                     #t)
                   params)))

             (let ((new-env

                    (extended-env-record
                     params
                     refs
                     types
                     mutables
                     saved-env)))

               (eval-expression
                body
                new-env)
             )
           )

           (eopl:error
            'apply-procedure
            "Numero incorrecto de argumentos para la función.")
       )
      )

      (else

       (eopl:error
        'apply-procedure
        "Intento de aplicar un valor no funcional: ~s"
        proc)
      )
    )
  )
)


;; =====================================================================
;; 11. Funciones auxiliares (Ojo con la redundanciaaa).

;; Extraer un número de un valor expresado.

(define expval->number
  (lambda (val)

    (cases exp-val val

      (num-val (n)
        n)

      (else
       (eopl:error
        'expval->number
        "Se esperaba un valor numérico, ojo."))
    )
  )
)


;; Extraer un booleano de un valor expresado.

(define expval->boolean
  (lambda (val)

    (cases exp-val val

      (bool-val (b)
        b)

      (else
       (eopl:error
        'expval->boolean
        "Se esperaba un valor booleano."))
    )
  )
)


;; =============================================================================
;; 12. Conversión de valores a cadenas.

(define exp-val->string
  (lambda (val)

    (cases exp-val val

      (num-val (n)
        (number->string n))

      (bool-val (b)
        (if b "True" "False"))

      (string-val (s)
        s)

      (proc-val (params body env)
        "#<procedimiento>")

      (void-val ()
        "void")
    )
  )
)

;; =============================================================================
;; 13. Evaluación de expresiones.

(define eval-expression
  (lambda (exp env)

    (cases expression exp

      ;; -----------------------------------------------------------------------
      ;; Expresiones literales.

      (entero-lit (n)
        (num-val n))

      (flotante-lit (n)
        (num-val n))

      (cadena-lit (s)
        (string-val s))

      (true-exp ()
        (bool-val #t))

      (false-exp ()
        (bool-val #f))


      ;; -----------------------------------------------------------------------
      ;; Identificadores, variables, llamadas y asignaciones (Revisar asignaciones).

      (identifier-exp
       (id tail)

       (cases identifier-tail tail

         ;; Variable.

         (variable-tail ()
           (lookup-var-val env id)
         )
         ;; Asignación mutable

         (assign-tail (expr)

           (eval-assign
            id
            expr
            env)
         )

         ;; Llamada a procedimiento.

         (call-tail (args)
           (let ((proc
                  (lookup-var-val env id))

                 (arg-vals
                  (map
                   (lambda (arg)
                     (eval-expression arg env))
                   args)))

             (apply-procedure
              proc
              arg-vals)
           )
         )
       )
      )
      ;; -----------------------------------------------------------------------
      ;; Block

      (block-exp
       (first-exp rest-exps)

       (eval-expression-sequence
        (cons first-exp rest-exps)
        env)
      )


      ;; -----------------------------------------------------------------------
      ;; Bloque locals.

      (locals-exp
       (decls first-exp rest-exps)

       (eval-locals-exp
        decls
        (cons first-exp rest-exps)
        env)
      )


      ;; -----------------------------------------------------------------------
      ;; Operaciones aritméticas y relacionales (OJO CON LOS SÍMBOLOS).

      (binary-exp
       (left operator right)

       (let ((left-val (eval-expression left env))
             (right-val (eval-expression right env)))

         (cases binary-operator operator

           (plus-op ()
             (num-val
              (+ (expval->number left-val)
                 (expval->number right-val))))

           (minus-op ()
             (num-val
              (- (expval->number left-val)
                 (expval->number right-val))))

           (times-op ()
             (num-val
              (* (expval->number left-val)
                 (expval->number right-val))))

           (divide-op ()
             (let ((denominator (expval->number right-val)))
               (if (zero? denominator)
                   (eopl:error 'eval-expression
                               "No puedes dividir entre cero, u silly.") 
                   (num-val
                    (/ (expval->number left-val) denominator)))))

           (equal-op ()
             (bool-val (equal? left-val right-val)))

           (not-equal-op ()
             (bool-val (not (equal? left-val right-val))))

           (greater-op ()
             (bool-val
              (> (expval->number left-val)
                 (expval->number right-val))))

           (less-op ()
             (bool-val
              (< (expval->number left-val)
                 (expval->number right-val))))
         )
       )
       )


      ;; ---------------------------------------------------------------------
      ;; Consolaaaaa.

      (print-exp
       (args)

       (let ((vals
              (map
               (lambda (arg)
                 (eval-expression arg env))
               args)))

         (for-each
          (lambda (val)

            (display
             (exp-val->string val))

            (display " ")
          )

          vals
         )

         (newline)

         (if (null? vals)

             (void-val)

             (car (reverse vals))
         )
       )
      )
    )
  )
)


;; =========================================================================
;; 14. Evaluación de procedimientos de programa.

(define eval-proc-decls
  (lambda (decls env)

    (if (null? decls)

        env

        (let ((decl
               (car decls)))

          (let ((new-env

                 (cases proc-decl decl

                   (proc-header-decl
                    (id params body)

                    (let* ((param-syms
                            (get-param-symbols params))

                           (ref
                            (vector #f))

                           (recursive-env
                            (extend-env
                             id ref 'proc #t env))

                           (proc
                            (proc-val
                             param-syms
                             body
                             recursive-env)))

                      (vector-set! ref 0 proc)

                      recursive-env
                    )
                   )
                 )
               ))

            (eval-proc-decls
             (cdr decls)
             new-env)
          )
        )
    )
  )
)


;; =============================================================================
;; 15. Ejecución del programa (🙏).

(define run-program
  (lambda (parsed-program)

    (cases program parsed-program

      (un-programa
       (global-decls proc-decls)

       (let* ((global-env
               (eval-global-decls
                global-decls
                (empty-env)))

              (program-env
               (eval-proc-decls
                proc-decls
                global-env)))

         ;; El procedimiento principal debe llamarse main
         ;; y no debe recibir argumentos, respetar la regla
         ;; al hacer pruebas POR FAVOR.

         (let ((main-proc
                (lookup-var-val
                 program-env
                 'main)))

           (apply-procedure
            main-proc
            '())
         )
       )
      )
    )
  )
)
;; =============================================================================
;; 16. Ejecución desde una cadena de texto.

(define execute-logicfusion
  (lambda (source-code)

    (run-program
     (scan&parse source-code))
  )
)

