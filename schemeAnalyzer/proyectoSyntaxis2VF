#lang racket

;se lee el archivo
(define source (open-input-file "/home/ivansotomayor/Desktop/SintaxisV2/clojure.txt"))

;automata (ver en pdf el diseño)
(define automata
     ;Estado 0
  '(((0 espacio 0) (0 saltolinea 0) (0 digito 1) (0 letra 7) (0 simbid 7) (0 comillas 8) (0 puntoComa 9) (0 simbolo 10) (0 simboloRecursivo 106) (0 masmenos 12) (0 * 13)
     ;Estado 1
     (1 espacio 100) (1 saltolinea 100) (1 punto 2) (1 letra 7) (1 simbid 7) (1 masmenos 7) (1 * 7)
     ;Estado 2
     (2 digito 3)
     ;Estado 3
     (3 digito 3) (3 espacio 101) (3 saltolinea 101) (3 letra 4)
     ;Estado 4
     (4 masmenos 5) (4 digito 6)
     ;Estado 5
     (5 digito 6)
     ;Estado 6
     (6 digito 6) (6 espacio 101) (6 saltolinea 101)
     ;Estado 7
     (7 letra 7) (7 digito 7) (7 simbid 7) (7 masmenos 7) (7 * 7) (7 espacio 102) (7 saltolinea 102)
     ;Estado 8
     (8 letra 8) (8 espacio 8) (8 comillas 103)
     ;Estado 9
     (9 letra 9) (9 digito 9) (9 simbolo 9) (9 simboloRecursivo 9) (9 comillas 9)
     (9 punto 9) (9 puntoComa 9) (9 espacio 9) (9 desconocido 9) (9 * 9) (9 masmenos 9)
     (9 simbid 9) (9 saltolinea 104)
     ;Estado 10
     (10 simbolo 11) (10 espacio 105) (10 saltolinea 105)
     ;Estado 11
     (11 espacio 105) (11 saltolinea 105)
     ;Estado 12
     (12 espacio 105) (12 saltolinea 105)
     ;Estado 13
     (13 espacio 105) (13 saltolinea 105)
     ;extras
     (106 espacio 106) (106 saltolinea 106))))

;se lee el archivo y se almacena en una lista
(define lectura
  (lambda (archivo)
    (cond
      ((eof-object? (peek-char archivo)) '() )
      (else (cons (read-char archivo) (lectura archivo))))))

;funcion de filtro que nos clasifica los caracteres segun su tipo.
(define filter
  (lambda (character)
    (cond
      ((equal? character "") "")
      ;digitos, letras, espacios y saltos de linea
      ((char-alphabetic? character) 'letra)
      ((char-numeric? character) 'digito)
      ((equal? #\space character) 'espacio) 
      ((equal? #\newline character) 'saltolinea)
      ((equal? #\. character) 'punto)
      ;parentesis, corchetes, square brakets  
      ((equal? #\( character) 'simboloRecursivo )
      ((equal? #\) character) 'simboloRecursivo )
      ((equal? #\[ character) 'simboloRecursivo )
      ((equal? #\] character) 'simboloRecursivo )
      ((equal? #\{ character) 'simboloRecursivo )
      ((equal? #\} character) 'simboloRecursivo )
      ((equal? #\' character) 'simboloRecursivo )
      ;simbolos que son tomados en cuenta por el lexico
      ((equal? #\/ character) 'simbolo)
      ((equal? #\# character) 'simbolo)
      ((equal? #\= character) 'simbolo)
      ((equal? #\> character) 'simbolo)
      ((equal? #\< character) 'simbolo)
      ((equal? #\% character) 'simbolo)
      ((equal? #\: character) 'simbolo)
      ;simbolos para identificadores
      ((equal? #\* character) '* )
      ((equal? #\+ character) 'masmenos)
      ((equal? #\! character) 'simbid)
      ((equal? #\- character) 'masmenos)
      ((equal? #\_ character) 'simbid)
      ((equal? #\? character) 'simbid)
      ;simbolos independientres
      ((equal? #\; character) 'puntoComa)
      ((equal? #\" character) 'comillas)
      ;simbolos desconocidos
      ((equal? #\, character) 'desconocido)
      ((equal? #\& character) 'desconocido))))

;Identificador de lexemas por el automata
(define (sig-edo edoact simbolo transiciones) 
  (cond ((null? transiciones) 0)  
        ((and (= edoact (caar transiciones)) (eq? simbolo (car (cdr (car transiciones))))) (car(cdr(cdr(car transiciones)))))
        (else (sig-edo edoact simbolo (cdr transiciones)))))

;See imprime al html reconociendo elementos en el automata
(define htmlfile (open-output-file "/home/ivansotomayor/Desktop/SintaxisV2/index.html" #:exists 'replace))

(define palabrasReservadas (list "def" "if" "cond" "defn" "fn" "else" "true" "false" "nil" "cons" "first" "second" "rest" "next"
"concat" "map" "apply" "filter" "reduce" "pmap" "let" "seq" "conj" "do" "and" "or" "not" "not=" "quote" "empty?"
"take" "range" "doall" "time" "future" "delay" "promise"))

(define buscarResv
  (lambda (palabra)
    (cond
      ((equal? (filter-map(lambda(x) (equal? palabra x)) palabrasReservadas) '()) #f)
      ((equal? (filter-map(lambda(x) (equal? palabra x)) palabrasReservadas) '(#t)) #t))))

(define (estados edoact lista transiciones word)
  (cond
    ((null? lista) edoact  (close-output-port htmlfile))
    (else(let([char (car lista)])
           (cond
             ;numeros enteros
             ((and (equal? (sig-edo edoact (filter char) transiciones) 1) (equal? (car(cdr lista)) #\space)) (write-string (string-append "<span style='color:DarkSlateGrey'>" (string char) " " "</span>") htmlfile) (estados 0 (cdr lista) transiciones ""))
             ((and (equal? (sig-edo edoact (filter char) transiciones) 1) (equal? (car(cdr lista)) #\newline)) (write-string (string-append "<span style='color:DarkSlateGrey'>" (string char) " " "</span>") htmlfile) (estados 0 (cdr lista) transiciones ""))
             ((equal? (sig-edo edoact (filter char) transiciones) 1) (write-string (string-append "<span style='color:DarkSlateGrey'>" (string char) "</span>") htmlfile) (estados 0 (cdr lista) transiciones ""))
             ;numeros flotantes
             ((equal? (sig-edo edoact (filter char) transiciones) 101) (write-string (string-append "<span style='color:Purple'>" word "</span>") htmlfile) (estados 0 (cdr lista) transiciones ""))
             ;identificadores                                          ;palabras reservadas
             ((equal? (sig-edo edoact (filter char) transiciones) 102) (cond
                                                                         ((equal? (buscarResv word) #t) (write-string (string-append "<span style='color:Green'>" word (string char) "</span>") htmlfile) (estados 0 (cdr lista) transiciones ""))
                                                                         (else(write-string (string-append "<span style='color:Red'>" word (string char) "</span>") htmlfile) (estados 0 (cdr lista) transiciones ""))))
             ;strings
             ((equal? (sig-edo edoact (filter char) transiciones) 103) (write-string (string-append "<span style='color:Teal'>" word (string char) "</span>") htmlfile) (estados 0 (cdr lista) transiciones ""))
             ;comentarios
             ((equal? (sig-edo edoact (filter char) transiciones) 104) (write-string (string-append "<span style='color:Orange'>" word "</span>" "<br>") htmlfile) (estados 0 (cdr lista) transiciones ""))
             ;simbolos
             ((equal? (sig-edo edoact (filter char) transiciones) 105) (write-string (string-append "<span style='color:Indigo'>" word (string char) "</span>") htmlfile) (estados 0 (cdr lista) transiciones ""))
             ;parentesis, corchetes, llaves
             ((equal? (sig-edo edoact (filter char) transiciones) 106) (write-string (string-append "<span style='color:Magenta'>" (string char) "</span>") htmlfile) (estados 0 (cdr lista) transiciones ""))
             ;saltos de linea
             ((equal? char #\newline) (write-string (string-append "<br>" (string char)) htmlfile) (estados 0 (cdr lista) transiciones "")) 
             (else (estados (sig-edo edoact (filter char) transiciones) (cdr lista) transiciones (string-append word (string char)))))))))


                
                       
               
         
 
               
                                   








      

     
      
      

      




    











  









