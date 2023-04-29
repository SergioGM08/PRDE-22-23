{- Sergio González Montero; Antonio... -}

type Posicion = Int 
type Posiciones = [Posicion]
type Valor = Int
type Valores = [Valor]

data Tablero = T Posiciones Posiciones
               deriving (Show,Read, Eq)
               
--Para el inicio del juego:
tableroVacio :: Tablero
tableroVacio = T [] []

--Tablero lleno, acabará en tablas si no ha habido ganador previo
tableroLleno :: Tablero -> Bool 
tableroLleno (T x o) = length x + length o  == 9

--Juega primero 'X', principio del turno n
juega1X :: Tablero -> Bool
juega1X (T x o) = length x == length o

--Juega segundo 'O', fin del turno n
juega2O :: Tablero -> Bool
juega2O (T x o) = length x /= length o

--Juega primero 'O', principio del turno n
juega1O :: Tablero -> Bool
juega1O (T x o) = length x == length o

--Juega segundo 'X', fin del turno n
juega2X :: Tablero -> Bool
juega2X (T x o) = length x /= length o

{-  Si es turno de 'X', se guarda la posición marcada en la primera lista
    Si es turno de 'O', se guarda la posición marcada en la segunda lista   -}
marca1 :: Tablero -> Posicion -> Tablero
marca1 (T x o) pos
        |juega1X (T x o) == True && marcaPermitida (T x o) pos == True = T (pos:x) o
        |juega2O (T x o) == True && marcaPermitida (T x o) pos == True = T x (pos:o)

marca2 :: Tablero -> Posicion -> Tablero
marca2 (T x o) pos
        |juega2X (T x o) == True && marcaPermitida (T x o) pos == True = T (pos:x) o
        |juega1O (T x o) == True && marcaPermitida (T x o) pos == True = T x (pos:o)
        
--Decide si se puede marcar la posición elegida
marcaPermitida :: Tablero -> Posicion -> Bool
marcaPermitida (T x o) pos
        |pos < 1 || pos > 9 = False
        |pertenece pos x o == False = True
        |otherwise = False
        
--Auxiliares para marcaPermitida
pertenece :: Posicion -> Posiciones -> Posiciones -> Bool 
pertenece pos [] [] = False
pertenece pos (xs:xss) [] = perteneceAux pos (last(xs:xss))
pertenece pos [] (os:oss) = perteneceAux pos (last(os:oss))
pertenece pos (xs:xss) (os:oss)
    |pos == xs || pos == os = True
    |otherwise = pertenece pos xss oss

perteneceAux :: Posicion -> Posicion -> Bool 
perteneceAux pos xss
    |pos == xss = True
    |otherwise = False
    
--Todas las posiciones guardadas en lista q están en lista p
contPos :: Posiciones -> Posiciones -> Bool 
contPos p q = all (`elem` p) q

--Comprueba si las posiciones marcadas por los jugadores son combinación ganadora
hayRaya :: Posiciones -> Bool 
hayRaya pos = contPos pos [1, 2, 3]|| contPos pos [1, 4, 7]|| contPos pos [1, 5, 9]|| contPos pos [4, 5, 6]|| contPos pos [2, 5, 8]|| contPos pos [3, 5, 7]|| contPos pos [7, 8, 9]|| contPos pos [3, 6, 9]

--Comprueba si 'X' o 'O' tienen raya
hayGanador :: Tablero -> Bool
hayGanador (T x o) = hayRaya x || hayRaya o

--Da qué hay en qué posición, dado un tablero dado
queHay :: Posicion -> Tablero -> String
queHay pos (T x o)
        |pos `elem` x = "X"
        |pos `elem` o = "O"
        |otherwise = show pos
        
--Da qué hay en la linea, de arriba hacia abajo, de un tablero dado
dameLinea :: Int -> Tablero -> [String]
dameLinea n (T x o) = [queHay (n*3 - 2) (T x o), queHay (n*3 - 1) (T x o), queHay (n*3) (T x o)]
        
--Da el tablero actual
dameTablero :: Tablero -> String
dameTablero (T x o) = show (dameLinea 3 (T x o)) ++ "\n ---+---+--- \n" ++ show(dameLinea 2 (T x o)) ++ "\n ---+---+--- \n" ++ show (dameLinea 1 (T x o))

--Dibuja el tablero
pintaTablero ::  Tablero -> IO()
pintaTablero (T x o) = putStrLn("\n" ++ (dameTablero (T x o)))

--ENTRADA SALIDA
leeInt :: IO Int
leeInt = do c <- getLine
            return (read c)
leeIntEnRango :: Int -> Int -> IO Int
leeIntEnRango menor mayor = do putStr ("\nElija modalidad: ")
                               n <- leeInt
                               if (n < menor) || (n > mayor)
                                   then do putStr "Entrada incorrecta. Repita, por favor.\n"
                                           leeIntEnRango menor mayor
                                   else return n

{- Menú principal en el que se decide si jugar contra otra persona, contra la máquina o salir del juego -}

menuPrincipal :: IO ()
menuPrincipal = do  putStrLn ("--------------------------------\n" ++
                                " HA HABIERTO EL JUEGO 3 EN RAYA\n" ++
                                "--------------------------------")
                    putStrLn ("\nEl objetivo es poner tres marcas X/O en fila antes " ++
                             "que el \ncontrincante consiga hacer lo mismo con sus marcas O/X. ")
                    putStrLn "Elija la dificultad de la maquina, juegue con otra persona o salga del juego: \n"
                    putStrLn "1.- Facil."
                    putStrLn "2.- Medio."
                    putStrLn "3.- Dificil."
                    putStrLn "4.- Jugador 1 X vs Jugador 2 O."
                    putStrLn "5.- Jugador 1 O vs Jugador 2 X."
                    putStrLn "6.- Salir."
                    opcion <- leeIntEnRango 1 6
                    case opcion of
                        1 -> do jugadorLvl1 tableroVacio
                                menuPrincipal
                        2 -> do jugadorLvl2 tableroVacio
                                menuPrincipal
                        3 -> do jugadorLvl3 tableroVacio
                                menuPrincipal
                        4 -> do jugador1X tableroVacio
                                menuPrincipal
                        5 -> do jugador1O tableroVacio
                                menuPrincipal
                        6 -> putStrLn "Gracias por jugar."

--Empieza el jugador, se guarda en un txt el tablero cada vez que el jugador marca
jugadorLvl1 :: Tablero -> IO ()
jugadorLvl1 (T x o) = do putStr "\n------------------------\nMarcar X en la posicion: "
                         unLugar <- getLine
                         let lugar :: Posicion
                             lugar = read unLugar 
                         if  marcaPermitida (T x o) lugar == False
                             then do putStrLn "\nMarca no permitida"
                                     jugadorLvl1 (T x o)
                             else do let (T xs os) = marca1 (T x o) lugar
                                     putStr "Guarde el tablero en: "
                                     tableroGuardado <- getLine
                                     appendFile tableroGuardado (dameTablero (T xs os))
                                     appendFile tableroGuardado "\n\n*************\n\n"
                                     pintaTablero (T xs os)
                                     if  hayGanador (T xs os)
                                         then do putStrLn ("\nEl jugador gana en " ++ show(length xs) ++ " movimientos.")
                                                 appendFile tableroGuardado ("El jugador gana en " ++ show(length xs) ++ " movimientos.\n")
                                         else if (tableroLleno (T xs os) == True)
                                                 then do putStrLn "\nHay empate"
                                                         appendFile tableroGuardado "\nHay empate"
                                                 else maquinaLvl1 (T xs os)
--La máquina responde al movimiento de jugador
maquinaLvl1 :: Tablero -> IO ()
maquinaLvl1 (T x o) = do putStr "\nLa maquina ha marcado O: \n"
                         let (T xs os) = minimaxMain 2 expande valora (T x o)
                         pintaTablero(T xs os)
                         if hayGanador (T xs os)
                            then do putStrLn ("\nLa maquina gana en " ++ show(length os) ++ " movimientos.")
                            else jugadorLvl1 (T xs os)

--Empieza el jugador, se guarda en un txt el tablero cada vez que el jugador marca
jugadorLvl2 :: Tablero -> IO ()
jugadorLvl2 (T x o) = do putStr "\n------------------------\nMarcar X en la posicion: "
                         unLugar <- getLine
                         let lugar :: Posicion
                             lugar = read unLugar 
                         if  marcaPermitida (T x o) lugar == False
                             then do putStrLn "\nMarca no permitida"
                                     jugadorLvl2 (T x o)
                             else do let (T xs os) = marca1 (T x o) lugar
                                     putStr "Guarde el tablero en: "
                                     tableroGuardado <- getLine
                                     appendFile tableroGuardado (dameTablero (T xs os))
                                     appendFile tableroGuardado "\n\n*************\n\n"
                                     pintaTablero (T xs os)
                                     if  hayGanador (T xs os)
                                         then do putStrLn ("\nEl jugador gana en " ++ show(length xs) ++ " movimientos.")
                                                 appendFile tableroGuardado ("El jugador gana en " ++ show(length xs) ++ " movimientos.\n")
                                         else if (tableroLleno (T xs os) == True)
                                                 then do putStrLn "\nHay empate"
                                                         appendFile tableroGuardado "\nHay empate"
                                                 else maquinaLvl2 (T xs os)
--La máquina responde al movimiento de jugador
maquinaLvl2 :: Tablero -> IO ()
maquinaLvl2 (T x o) = do putStr "\nLa maquina ha marcado O: \n"
                         let (T xs os) = minimaxMain 4 expande valora (T x o)
                         pintaTablero(T xs os)
                         if hayGanador (T xs os)
                            then do putStrLn ("\nLa maquina gana en " ++ show(length os) ++ " movimientos.")
                            else jugadorLvl2 (T xs os)
                         
--Empieza el jugador, se guarda en un txt el tablero cada vez que el jugador marca
jugadorLvl3 :: Tablero -> IO ()
jugadorLvl3 (T x o) = do putStr "\n------------------------\nMarcar X en la posicion: "
                         unLugar <- getLine
                         let lugar :: Posicion
                             lugar = read unLugar 
                         if  marcaPermitida (T x o) lugar == False
                             then do putStrLn "\nMarca no permitida"
                                     jugadorLvl3 (T x o)
                             else do let (T xs os) = marca1 (T x o) lugar
                                     putStr "Guarde el tablero en: "
                                     tableroGuardado <- getLine
                                     appendFile tableroGuardado (dameTablero (T xs os))
                                     appendFile tableroGuardado "\n\n*************\n\n"
                                     pintaTablero (T xs os)
                                     if  hayGanador (T xs os)
                                         then do putStrLn ("\nEl jugador gana en " ++ show(length xs) ++ " movimientos.")
                                                 appendFile tableroGuardado ("El jugador gana en " ++ show(length xs) ++ " movimientos.\n")
                                         else if (tableroLleno (T xs os) == True)
                                                 then do putStrLn "\nHay empate"
                                                         appendFile tableroGuardado "\nHay empate"
                                                 else maquinaLvl3 (T xs os)
--La máquina responde al movimiento de jugador
maquinaLvl3 :: Tablero -> IO ()
maquinaLvl3 (T x o) = do putStr "\nLa maquina ha marcado O: \n"
                         let (T xs os) = minimaxMain 6 expande valora (T x o)
                         pintaTablero(T xs os)
                         if hayGanador (T xs os)
                            then do putStrLn ("\nLa maquina gana en " ++ show(length os) ++ " movimientos.")
                            else jugadorLvl3 (T xs os)

{-	Para jugar jugador 1 vs jugador 2, comienza jugador 1.
	Se guarda en un .txt el tablero cada vez que el jugador1 1 marca,
    o bien en un .txt distinto o en el mismo .txt dependiendo del nombre elegido  -}

--El jugador 1 marca con X	
jugador1X :: Tablero -> IO ()
jugador1X (T x o) = do putStr "\n------------------------\nMarcar X en la posicion: "
                       unLugar <- getLine
                       let lugar :: Posicion
                           lugar = read unLugar 
                       if  marcaPermitida (T x o) lugar == False
                           then do putStrLn "\nMarca no permitida"
                                   jugador1X (T x o)
                           else do let (T xs os) = marca1 (T x o) lugar
                                   putStr "Guarde el tablero en:  "
                                   tableroGuardado <- getLine
                                   appendFile tableroGuardado (dameTablero (T xs os))
                                   appendFile tableroGuardado "\n\n*************\n"
                                   pintaTablero (T xs os)
                                   if  hayGanador (T xs os)
                                       then do putStrLn ("\nEl jugador 1 gana en " ++ show(length xs) ++ " movimientos.")
                                               appendFile tableroGuardado ("El jugador 1 gana en " ++ show(length xs) ++ " movimientos.")
                                       else if (tableroLleno (T xs os) == True)
                                               then do putStrLn "\nHay empate"
                                                       appendFile tableroGuardado "\nHay empate"
                                               else jugador2O (T xs os)

--El jugador 2, con O, responde al movimiento de jugador 1
jugador2O :: Tablero -> IO ()
jugador2O (T x o) = do putStr "\nMarcar O en la posicion: "
                       unLugar <- getLine
                       let lugar :: Posicion
                           lugar = read unLugar 
                       if  marcaPermitida (T x o) lugar == False
                           then do putStrLn "\nMarca no permitida"
                                   jugador2O (T x o)
                           else do let (T xs os) = marca1 (T x o) lugar
                                   pintaTablero (T xs os)
                                   if hayGanador (T xs os)
                                   then do putStrLn "\nEl jugador 2 gana"
                                           pintaTablero (T xs os)
                                   else jugador1X (T xs os)
--El jugador 1 marca con O
jugador1O :: Tablero -> IO ()
jugador1O (T x o) = do putStr "\n------------------------\nMarcar O en la posicion: "
                       unLugar <- getLine
                       let lugar :: Posicion
                           lugar = read unLugar 
                       if  marcaPermitida (T x o) lugar == False
                           then do putStrLn "\nMarca no permitida"
                                   jugador1O (T x o)
                           else do let (T xs os) = marca2 (T x o) lugar
                                   putStr "Guarde el tablero en: "
                                   tableroGuardado <- getLine
                                   appendFile tableroGuardado (dameTablero (T xs os))
                                   appendFile tableroGuardado "\n\n*************\n\n"
                                   pintaTablero (T xs os)
                                   if  hayGanador (T xs os)
                                       then do putStrLn ("\nEl jugador 1 gana en " ++ show(length os) ++ " movimientos.")
                                               appendFile tableroGuardado ("El jugador 1 gana en " ++ show(length os) ++ " movimientos.")
                                       else if (tableroLleno (T xs os) == True)
                                               then do putStrLn "\nHay empate"
                                                       appendFile tableroGuardado "\nHay empate"
                                               else jugador2X (T xs os)

--El jugador 2, con O, responde al movimiento de jugador 1
jugador2X :: Tablero -> IO ()
jugador2X (T x o) = do putStr "\nMarcar X en la posicion: "
                       unLugar <- getLine
                       let lugar :: Posicion
                           lugar = read unLugar 
                       if  marcaPermitida (T x o) lugar == False
                           then do putStrLn "\nMarca no permitida"
                                   jugador2X (T x o)
                           else do let (T xs os) = marca2 (T x o) lugar
                                   putStrLn (dameTablero (T xs os))
                                   if hayGanador (T xs os)
                                   then do putStrLn "\nEl jugador 2 gana"
                                           pintaTablero (T xs os)
                                   else jugador1O (T xs os)

-- MINIMAX:

{-  Lista de todas las posiciones libres en el momento
    para expander del minimax -}

posicionesLibres :: Tablero -> Posiciones
posicionesLibres (T x o) = eliminaAux [1..9] xo
    where  xo = x++o

eliminaAux :: Posiciones -> Posiciones -> Posiciones
eliminaAux (n:ns) xs
    |xs == [] = [1..9]
    |ns == [] && (filter (==9) xs) == [] = [9]
    |ns == [] && (filter (==9) xs) /= [] = []
    |n `elem` xs = eliminaAux ns xs
    |otherwise = n:(eliminaAux ns xs)

{-  Todos los posibles tableros inmediatos pero no sigue si el
    tablero es ganador, añade las posibilidades a realizar  -}
expande :: Tablero -> [Tablero]
expande (T x o)
    |hayGanador (T x o) = []
    |otherwise = map (marca1 (T x o)) (posicionesLibres (T x o))
    
{- Valora el tablero -}
valora :: Tablero -> Valor
valora (T x o)
    |ganaX (T x o) = -10
    |ganaO (T x o) = 10
    |otherwise = 0
    
--Si gana el jugador que marca 'X'   
ganaX :: Tablero -> Bool
ganaX (T x o) = hayRaya x

--Si gana el jugador que marca 'O'  
ganaO :: Tablero -> Bool
ganaO (T x o) = hayRaya o

--Algoritmo minimax
minimax :: Int -> (Tablero-> [Tablero]) -> (Tablero->Valor) -> (Valores->Valor) -> (Valores->Valor) -> Tablero -> Valor
minimax prof expandir valorar peor mejor (T x o)
   |(prof ==0) || (null siguientes) = valorar (T x o)
   |otherwise = mejor (map (minimax (prof-1) expandir valorar mejor peor) siguientes)
   where siguientes = expandir (T x o)


minimaxMain :: Int -> (Tablero-> [Tablero]) -> (Tablero->Valor) -> Tablero -> Tablero
minimaxMain prof expandir valorar (T x o)
    |(prof ==0) || (null siguientes) = (T x o)
    |otherwise = snd (maximum' sigVals)
    where siguientes = expandir (T x o)
          valoraciones = map (minimax (prof-1) expandir valorar maximum minimum) siguientes
          sigVals = zip valoraciones siguientes

maximum' :: [(Valor,Tablero)] -> (Valor,Tablero)
maximum' = foldr1 max'

max' :: (Valor,Tablero) -> (Valor,Tablero) -> (Valor,Tablero)
max' (v1,(T x1 o1)) (v2,(T x2 o2))
    |(v1 >= v2) = (v1,(T x1 o1))
    |otherwise = (v2,(T x2 o2))
