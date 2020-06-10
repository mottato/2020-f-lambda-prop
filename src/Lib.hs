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

--2)
--a)
ubicadoEn :: [Barrio]->Requisito
ubicadoEn barrios depto = elem (barrio depto) barrios

--b)
cumpleRango ::(Ord b)=>(Depto->b)->b->b->Depto->Bool
cumpleRango unaFuncion unNumero otroNumero unDepto = between unNumero otroNumero (unaFuncion unDepto)

--3)
--a)
cumpleBusqueda :: Depto->Busqueda->Bool
cumpleBusqueda unDepto = all (\requisito->requisito unDepto)

--b)
buscar :: Busqueda->(Depto->Depto->Bool)->[Depto]->[Depto]
buscar unaBusqueda unCriterio = (ordenarSegun unCriterio).filter (flip cumpleBusqueda unaBusqueda) 

--4)

mailsDePersonasInteresadas :: Depto->[Persona]->[Mail]
mailsDePersonasInteresadas unDepto = map mail.filter (estaInteresado unDepto) 

estaInteresado :: Depto->Persona->Bool
estaInteresado unDepto unaPersona = any (cumpleBusqueda unDepto) (busquedas unaPersona)
