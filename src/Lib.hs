module Lib where
import Text.Show.Functions

--Modelado
type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto { 
  ambientes :: Int,
  superficie :: Int,
  precio :: Int,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
}
ordenarSegun :: (a->a->Bool)->[a]->[a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
  (ordenarSegun criterio . filter (not . criterio x)) xs ++
  [x] ++
  (ordenarSegun criterio . filter (criterio x)) xs

between cotaInferior cotaSuperior valor =
  valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
  Depto 3 80 7500 "Palermo", 
  Depto 1 45 3500 "Villa Urquiza", 
  Depto 2 50 5000 "Palermo", 
  Depto 1 45 5500 "Recoleta"]


--1)
--a)
mayor :: (Ord b)=>(a->b)->a->a->Bool
mayor unaFuncion unValor = (<(unaFuncion unValor)).unaFuncion

menor :: (Ord b)=>(a->b)->a->a->Bool
menor unaFuncion unValor = (>(unaFuncion unValor)).unaFuncion

--b)

--ordenarSegun (mayor length)  ["hola","mar"]
