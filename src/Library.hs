module Library where
import PdePreludat

type Habilidad = String
type Planeta = String
type Material = String
type Universo = [Personaje]

data Gema = Gema {
    poder :: String
} deriving Show

data Guantelete = Guantelete {
    material :: Material, 
    gemas :: [Gema]
} deriving Show

data Personaje = Personaje {
    nombre :: String,
    edad :: Number,
    energia :: Number,
    habilidades :: [Habilidad],
    planetaDondeViven :: Planeta
} deriving (Show,Eq)

chasquearUniverso :: Guantelete -> Universo -> Universo
chasquearUniverso guantelete universo 
    | esGuanteleteCompleto guantelete = reducirUniverso universo
    | otherwise = universo

esGuanteleteCompleto :: Guantelete -> Bool
esGuanteleteCompleto guantelete = (length.gemas) guantelete == 6 && material guantelete == "uru"

reducirUniverso :: Universo -> Universo
reducirUniverso universo = take (div (length universo) 2) universo

-- Punto 2: (3 puntos) Resolver utilizando únicamente orden superior.
-- Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen 
-- menos de 45 años.
-- Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes que 
-- tienen más de una habilidad.

universoAptoParaPendex :: Universo -> Bool
universoAptoParaPendex = any ((<45).edad)

energiaTotalDeUniverso :: Universo -> Number
energiaTotalDeUniverso universo = sum (map energia (filter ((1<).length.habilidades) universo))