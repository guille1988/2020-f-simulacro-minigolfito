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


between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b


--PUNTO 1--

type Palo = (Habilidad -> Tiro)

putter :: Palo
putter habilidad = UnTiro 10 (modificarPrecisionJugador 2 habilidad)  0

madera :: Palo
madera habilidad = UnTiro 100 (modificarPrecisionJugador 0.5 habilidad) 0

hierros :: Float -> Palo
hierros n habilidad = UnTiro (modificarFuerzaJugador n habilidad) (modificarPrecisionJugador (1 / n) habilidad) ( max 0 (n - 3))

modificarPrecisionJugador :: Float -> Habilidad -> Float
modificarPrecisionJugador n  = (*n).precisionJugador 

modificarFuerzaJugador :: Float -> Habilidad -> Float
modificarFuerzaJugador n = (*n).fuerzaJugador

type Palos = [Palo]


--PUNTO 2--

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

--PUNTO 3--

{-
Lo que nos interesa de los distintos obstáculos es si un tiro puede superarlo, 
y en el caso de poder superarlo, cómo se ve afectado dicho tiro por el obstáculo. 
En principio necesitamos representar los siguientes obstáculos:
Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, 
independientemente de la velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica, 
la precisión pasa a ser 100 y la altura 0.
Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. 
Luego de superar una laguna el tiro llega con la misma velocidad y precisión, 
pero una altura equivalente a la altura original dividida por el largo de la laguna.
Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. 
Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.
-}

type Obstaculo = (Tiro -> Tiro)

tunelConRampita :: Obstaculo
tunelConRampita  tiro 
 | (condicionParaSuperarloEnPrecision (>90) tiro) && (condicionParaSuperarloEnAltura (==0) tiro) = efectoTunelConRampita tiro
 | otherwise = tiroDetenido

condicionParaSuperarloEnVelocidad :: (Float -> Bool) -> Tiro -> Bool
condicionParaSuperarloEnVelocidad condicion = condicion.velocidad

condicionParaSuperarloEnPrecision :: (Float -> Bool) -> Tiro -> Bool
condicionParaSuperarloEnPrecision condicion = condicion.precision

condicionParaSuperarloEnAltura :: (Float -> Bool) -> Tiro -> Bool
condicionParaSuperarloEnAltura condicion = condicion.altura

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita  tiro = UnTiro {velocidad = (velocidad tiro) * 2 , precision = 100, altura = 0}

tiroDetenido = UnTiro 0 0 0 

laguna :: Float -> Tiro -> Tiro
laguna largoLaguna tiro
  | (condicionParaSuperarloEnVelocidad (>80) tiro) && condicionParaSuperarloEnAltura (between 1 5) tiro =  (efectoLaguna largoLaguna) tiro
  | otherwise = tiroDetenido

efectoLaguna :: Float -> Tiro -> Tiro
efectoLaguna largoLaguna tiro = UnTiro {velocidad = velocidad tiro, precision = precision tiro, altura = ((/largoLaguna).altura) tiro}

hoyo :: Obstaculo
hoyo tiro
 | (condicionParaSuperarloEnVelocidad (between 5 20) tiro) && (condicionParaSuperarloEnPrecision (>95) tiro) && (condicionParaSuperarloEnAltura (==0) tiro) = tiroDetenido
 | otherwise = tiro
 
--PUNTO 4--

