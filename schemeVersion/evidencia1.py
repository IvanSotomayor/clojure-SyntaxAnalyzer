#Ivan Ortega Sotomayor A01383282
#Implementación de la solución a la situación problema: Resaltador de sintaxis v1.0 (Regex)

import re
import time
start_time = time.time()

# regex
palabras_reservadas = "(define|lambda|if|cond|else|true|false|nil|car|cdr|cons|list|apply|map|let|begin|null\?|eq\?|set)"
identificadores = "[^\(\)\[\]\{\}\;\,\‘\“\#]"
constantesEnterasDecimales = "[0-9]+"
enterasNegativas = "\-[0-9]+"
constantesFloat = "[0-9]+\.[0-9]+"
floatnegativas  = "\-[0-9]+\.[0-9]+"
simbolosEspeciales = "(\+|\-|\*|\/|\<|\>|\=|\<\>|\(|\)|\‘)"
comentarios = "\;.*"
letraOnumero = "[a-zA-Z0-9]"
soloLetras = "[a-zA-z]"
masOmenos = "(\+|\-)"

#palabras reservadas verificacion
lista_reservadas = ['define','lambda','if','cond','else','true','false','nilcar','cdr','cons','list','apply','map','let','begin','null?','eq?','set']

#crear html
html = open("index.html", "w")
html.write("<!DOCTYPE html>\n")
html.write("<html lang='en'>\n")
html.write("<head>\n")
html.write("    <meta charset='UTF-8'>\n")
html.write("    <meta http-equiv='X-UA-Compatible' content='IE=edge'>\n")
html.write("    <meta name='viewport' content='width=device-width, initial-scale=1.0'>\n")
html.write("    <link rel='stylesheet' href='./style.css'>\n")
html.write("    <title>SchemeSyntaxis</title>\n")
html.write("</head>\n")
html.write("<body>\n")

#logica del programa
archivotxt = open('schemeSyntaxis.txt')
caracter = archivotxt.readline(1)
stringFormado = ""


while caracter != "":

    #simbolos especiales
    if(re.match(simbolosEspeciales, caracter)):
        html.write("<simbolo>" + caracter + "</simbolo>")

    if(caracter == "\n"):
        html.write("<br>")
        html.write("\n")

    if(caracter == " "):
        html.write(" ")
    
    #comentarios
    if(re.match(comentarios, caracter)):
        while(caracter != "\n"):
            stringFormado += caracter
            caracter = archivotxt.readline(1)
        if(caracter == "\n"):
            html.write("<br>")
            html.write("\n")
        html.write("<comentarios>" + stringFormado + "<comentarios/>")
        stringFormado = ""
    
    #identificadores
    if(re.match(identificadores, caracter)):
        if(re.match(letraOnumero, caracter)):
            while(caracter != ' ' and caracter != '\n' and caracter != '(' and caracter != ')'):
                stringFormado += caracter
                caracter = archivotxt.readline(1)
            
            #palabras reservadas
            if(re.match(palabras_reservadas, stringFormado)):
                for i in range(len(lista_reservadas)):
                    if(stringFormado == lista_reservadas[i]):
                        html.write("<reservada>" + stringFormado + "<reservada/>")
                        stringFormado = ""
                        break

            #enteros y decimales
            if(re.match(constantesEnterasDecimales, stringFormado)):
                for letra in stringFormado:
                    if(letra == "."):
                        html.write("<flotantes>" + stringFormado + "<flotantes/>")
                        stringFormado = ""
                html.write("<enteras>" + stringFormado + "<enteras/>")
                stringFormado = ""    

            #identificadores        
            if(re.match(identificadores, stringFormado)):
                html.write("<identificador>" + stringFormado + "<identificador/>")
                stringFormado = ""
    
            #ifs por si sobro algun simbolo, espacio o salto de linea al final del renglon
            if(re.match(simbolosEspeciales, caracter)):
                html.write("<simbolo>" + caracter + "</simbolo>")
            elif(caracter == "\n"):
                html.write("<br>")
                html.write("\n")
            elif(caracter == " "):
                html.write(" ")

        #diferenciar entre negativas y + - como simbolos
        elif(re.match(masOmenos,caracter)):
            while(caracter != ' ' and caracter != '\n' and caracter != '(' and caracter != ')'):
                stringFormado += caracter
                caracter = archivotxt.readline(1)
            if(re.match(enterasNegativas,stringFormado)):
                html.write("<EnterasNega>" + stringFormado +"<EnterasNega/>")            
    if(not re.match(identificadores, caracter) and (not re.match(simbolosEspeciales, caracter))):
        html.write("<error>" + caracter + "<erro/>")

    #erorres
    caracter = archivotxt.readline(1)
    

#cerrar html tags
html.write("</body>\n")
html.write("</html>\n")
#cerrar el doc html que se creo
html.close()

#crear css y llenarlo
css = open("style.css", "w")

css.write("simbolo{\n")
css.write("    color:blue;\n")
css.write("}\n")

css.write("reservada{\n")
css.write("    color:green;\n")
css.write("}\n")

css.write("identificador{\n")
css.write("    color:pink;\n")
css.write("}\n")

css.write("enteras{\n")
css.write("    color:orange;\n")
css.write("}\n")

css.write("enterasNega{\n")
css.write("    color:chocolate;\n")
css.write("}\n")

css.write("flotantes{\n")
css.write("    color:darkmagenta;\n")
css.write("}\n")

css.write("comentarios{\n")
css.write("    color:lawngreen;\n")
css.write("}\n")

css.write("error{\n")
css.write("    color:red;\n")
css.write("}\n")

#cerrar css
css.close()

print("ejecutado con exito")


print("--- %s seconds ---" % (time.time() - start_time))