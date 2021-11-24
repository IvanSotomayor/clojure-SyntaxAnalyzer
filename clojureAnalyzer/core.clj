(ns sintaxisv3.core
  (:gen-class))

(def automata
     ;Estado 0
  '((
     (0 espacio 0) (0 saltolinea 0) (0 digito 1) (0 letra 7) (0 simbid 7) 
     (0 comillas 8) (0 puntoComa 9) (0 simbolo 10) (0 simboloRecursivo 106) (0 masmenos 12) (0 * 13)
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
     (106 espacio 106) (106 saltolinea 106)
    ))
)

;Lee el archivo txt y almacena su contenido en un vector
(def stringlista (slurp "paraLeer.txt"))
(require '[clojure.string :as str])

(defn charlista [lista]
  (str/split lista #"")
)

;funcion de filtro que nos clasifica los caracteres segun su tipo.
(defn filtro [caracter]
  (cond
    (= caracter "") ""
    ;digitos, letras, espacios y saltos de linea
    (re-seq #"[a-zA-Z]" caracter) 'letra
    (re-seq #"[0-9]" caracter) 'digito
    (= caracter " ") 'espacio
    (= caracter "\n") 'saltolinea
    (= caracter ".") 'punto
    ;parentesis, corchetes, square brackets y comilla
    (= caracter "(") 'simboloRecursivo
    (= caracter ")") 'simboloRecursivo
    (= caracter "{") 'simboloRecursivo
    (= caracter "}") 'simboloRecursivo
    (= caracter "[") 'simboloRecursivo
    (= caracter "]") 'simboloRecursivo
    (= caracter "'") 'simboloRecursivo
    ;simbolos que son tomasos en cuenta por el lexico de clojure
    (= caracter "/") 'simbolo
    (= caracter "#") 'simbolo
    (= caracter "=") 'simbolo
    (= caracter ">") 'simbolo
    (= caracter "<") 'simbolo
    (= caracter "%") 'simbolo
    (= caracter ":") 'simbolo
    ;simbolos para identificadores
    (= caracter "*") '*
    (= caracter "+") 'masmenos
    (= caracter "!") 'simbid
    (= caracter "-") 'masmenos
    (= caracter "_") 'simbid
    (= caracter "?") 'simbid
    ;simbolos idependientes
    (= caracter ";") 'puntoComa
    (= caracter "\"") 'comillas
    ;simbolos desconocidos
    (= caracter ",") 'desconocido
    (= caracter "&") 'desconocido
  )
)

;lista de palabras reservadas
(def palabrasReservadas (list "def" "if" "cond" "defn" "fn" "else" "true" "false" "nil" "cons" "first" "second" "rest" "next"
"concat" "map" "apply" "filter" "reduce" "pmap" "let" "seq" "conj" "do" "and" "or" "not" "not=" "quote" "empty?"
"take" "range" "doall" "time" "future" "delay" "promise"))


(defn buscarResv [palabra lista]
  (cond
    (= (first lista) palabra) true
    (and (empty? (rest lista)) (not(= (first lista) palabra))) false 
    :else (buscarResv palabra (rest lista))
  )
)

;Identificador de lexemas por el automata
(defn sig-edo [edoact simbolo transiciones]
  (cond
    (empty? transiciones) 0
    (and (= edoact (first(first transiciones)))
         (= simbolo (first(rest(first transiciones))))
    )(first(rest(rest(first transiciones))))
    :else(sig-edo edoact simbolo (rest transiciones))
  )
)

;Se imprime al html reconociendo elementos en el automata

;nombre del archivo de salida
(def archivo "ResaltadorV3.html")

(defn estados [edoact lista transiciones word]
  (cond
    (empty? lista) (spit archivo "" :append true)
    :else
    (let [caracter (first lista)]
      (cond
        
        ;numeros entero, si encuentra un numero y un espacio ejemplo (+ 1 2)
        (and (= (sig-edo edoact (filtro caracter) transiciones) 1) (= (first (rest lista)) " "))
        (when true
          (spit archivo (str "<span style='color:DarkSlateGrey'>" caracter " " "</span>") :append true)
          (estados 0 (rest lista) transiciones ""))

        ;numero entero, si encuentra un numero y un salto de linea... ejemplo : 1\n
        (and (= (sig-edo edoact (filtro caracter) transiciones) 1) (= (first (rest lista)) "\n"))
        (when true
          (spit archivo (str "<span style='color:DarkSlateGrey'>" caracter " " "</span>") :append true)
          (estados 0 (rest lista) transiciones ""))

        ;numero si no tiene espacio ni salto de linea despues
        (= (sig-edo edoact (filtro caracter) transiciones) 1)
        (when true
          (spit archivo (str "<span style='color:DarkSlateGrey'>" caracter "</span>") :append true)
          (estados 0 (rest lista) transiciones ""))

        ;numeros flotantes
        (= (sig-edo edoact (filtro caracter) transiciones) 101)
        (when true
          (spit archivo (str "<span style='color:Purple'>" caracter "</span>") :append true)
          (estados 0 (rest lista) transiciones ""))

        ;identificadores
        (= (sig-edo edoact (filtro caracter) transiciones) 102)
        (cond
          (= (buscarResv word palabrasReservadas) true)
          (when true
            (spit archivo (str "<span style='color:Green'>" word caracter "</span>") :append true)
            (estados 0 (rest lista) transiciones ""))
          :else
          (when true
            (spit archivo (str "<span style='color:Red'>" word caracter "</span>") :append true)
            (estados 0 (rest lista) transiciones "")))

        ;strings
        (= (sig-edo edoact (filtro caracter) transiciones) 103)
        (when true
          (spit archivo (str "<span style='color:Teal'>" word caracter "</span>") :append true)
          (estados 0 (rest lista) transiciones ""))

        ;comentarios
        (= (sig-edo edoact (filtro caracter) transiciones) 104)
        (when true
          (spit archivo (str "<span style='color:Orange'>" word "</span>" "<br>") :append true)
          (estados 0 (rest lista) transiciones ""))

        ;simbolos
        (= (sig-edo edoact (filtro caracter) transiciones) 105)
        (when true
          (spit archivo (str "<span style='color:Indigo'>" word caracter "</span>") :append true)
          (estados 0 (rest lista) transiciones ""))

        ;parentesis, corchetes, llaves
        (= (sig-edo edoact (filtro caracter) transiciones) 106)
        (when true
          (spit archivo (str "<span style='color:Magenta'>" caracter "</span>") :append true)
          (estados 0 (rest lista) transiciones ""))

        ; saltos de line
       

        :else (estados (sig-edo edoact (filtro caracter) transiciones) (rest lista) transiciones (str word caracter)))
    )
  )
)

(charlista stringlista)

(estados 0 (charlista stringlista) (first automata) "")


;lectura de archivos

(defn archivos [file]
  (str/split-lines (slurp file)))

(map (fn [archivo](estados 0 (charlista (slurp archivo)) (first automata) "")) (archivos "directorios.txt"))





















