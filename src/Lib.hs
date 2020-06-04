module Lib where
import Text.Show.Functions

laVerdad = True

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Float,
  precisionJugador :: Float
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Float,
  precision :: Float,
  altura :: Float
} deriving (Eq, Show)

type Puntos = Int

{-Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b
-}

--PUNTO 1--

type Palo = (Habilidad -> Tiro)

putter :: Palo
putter habilidad = UnTiro 10 (modificarPrecisionJugador 2 habilidad)  0

madera :: Palo
madera habilidad = UnTiro 100 (modificarPrecisionJugador 0.5 habilidad) 0

hierros :: Float -> Palo
hierros n habilidad = UnTiro (modificarFuerzaJugador n habilidad) (modificarPrecisionJugador (1 / n) habilidad) (n - 3)

modificarPrecisionJugador :: Float -> Habilidad -> Float
modificarPrecisionJugador n  = (*n).precisionJugador 

modificarFuerzaJugador :: Float -> Habilidad -> Float
modificarFuerzaJugador n = (*n).fuerzaJugador

type Palos = [Palo]


--PUNTO 2--

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

--PUNTO 3--



