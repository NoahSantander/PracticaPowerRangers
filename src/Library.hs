module Library where
import PdePreludat

-- Defino mis alias
type Habilidad = String
type Habilidades = [Habilidad]
type EsBuena = Bool
type Persona = (Habilidades, EsBuena)
type Color = String
type NivelDePelea = Number
type Colores = [Color]
type Personas = [Persona]
type Equipo = [PowerRanger]
type CantidadDePelo = Number
type ChicaSuperPoderosa = (Color, CantidadDePelo)
type ChicasSuperPoderosas = [ChicaSuperPoderosa]

-- Defino mis tipos
data PowerRanger = UnPowerRanger {
    color :: Color,
    habilidades :: Habilidades,
    nivelDePelea :: NivelDePelea
} deriving Show

-- Defino la función 
potenciarHabilidad :: Habilidad -> Habilidad
potenciarHabilidad habilidadAPotenciar = "super"++( head habilidadAPotenciar:drop 1 habilidadAPotenciar)

calcularNivelDePelea :: Habilidades -> NivelDePelea
calcularNivelDePelea habilidades = sum (map (length) habilidades)

convertirEnPowerRanger :: Color -> Persona -> PowerRanger
convertirEnPowerRanger color persona = UnPowerRanger color (map (potenciarHabilidad) (fst persona)) (calcularNivelDePelea (fst persona))

-- Defino la función 
formarEquipoRanger :: Colores -> Personas -> Equipo
formarEquipoRanger _ [] = []
formarEquipoRanger (color:restoColores) (persona:restoPersonas) 
    | snd persona = convertirEnPowerRanger color persona:formarEquipoRanger restoColores restoPersonas
    | otherwise = formarEquipoRanger (color:restoColores) restoPersonas

-- Defino la función
findOrElse :: (a -> Bool) -> a -> [a] -> a
findOrElse condicion valor lista 
    | any (condicion) lista = find (condicion) lista
    | otherwise = valor

-- Defino la función
esElRojo :: PowerRanger -> Bool
esElRojo = (=="rojo").color

rangerLider :: Equipo -> PowerRanger
rangerLider equipo = findOrElse esElRojo (head equipo) 

-- Defino la funcióm 
maximumBy :: Ord b => [a] -> (a->b) -> a
maximumBy [x] _ = x
maximumBy (x:xs) funcion 
    | funcion x > funcion (head xs) = maximumBy (x:drop 1 xs) funcion
    | otherwise = maximumBy xs funcion

-- Defino la función
rangerMasPoderoso :: Equipo -> PowerRanger
rangerMasPoderoso equipo = maximumBy equipo (nivelDePelea) 

-- Defino la función 
rangerHabilidoso :: PowerRanger -> Bool
rangerHabilidoso = (>5).length.habilidades

-- Inicializo la constante 
alfa5 = UnPowerRanger "matalico" ["reparar cosas", (repeat "ay ")] 0
-- alfa5 no es un ranger habilidoso ya que solo tiene 2 habilidades
-- alfa5 si no fuera un ranger, tampoco podriamos calcular su nivel de pelea al convertirse en uno ya que la función diverge

-- Defino la función
esLaRoja :: ChicasSuperPoderosas -> Bool
esLaRoja = (=="rojo").fst

chicaLider :: ChicasSuperPoderosas -> ChicaSuperPoderosa
chicaLider equipo = findOrElse esLaRoja (head equipo)