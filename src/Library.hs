module Library where
import PdePreludat

type Habilidad = String
type Planeta = String
type Material = String
type Universo = [Personaje]
type Gema = Personaje -> Personaje

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
esGuanteleteCompleto guantelete = (( == 6).length.gemas) guantelete && ((== "uru").material) guantelete 

reducirUniverso :: Universo -> Universo
reducirUniverso universo = take (div (length universo) 2) universo

-- Punto 2

universoAptoParaPendex :: Universo -> Bool
universoAptoParaPendex = any $ (<45).edad

energiaTotalDeUniverso :: Universo -> Number
energiaTotalDeUniverso = sum.map energia.filter ((1<).length.habilidades)

-- Punto 3
sacarEnergia :: Number -> Gema
sacarEnergia cantidad personaje = personaje {
    energia = energia personaje - cantidad
}

laMente :: Number -> Gema
laMente = sacarEnergia

elAlma :: Habilidad -> Gema
elAlma habilidadAEliminar rival = sacarEnergia 10 rival {
    habilidades = sacarHabilidadSiExiste rival habilidadAEliminar
}

sacarHabilidadSiExiste :: Personaje -> Habilidad -> [Habilidad]
sacarHabilidadSiExiste personaje habilidadAEliminar = filter (/= habilidadAEliminar) $ habilidades personaje

elEspacio :: Planeta -> Gema
elEspacio nuevoPlaneta rival = sacarEnergia 20 rival {
    planetaDondeViven = nuevoPlaneta
}

elPoder :: Gema
elPoder rival = sacarEnergia (energia rival) rival {
    habilidades = quitarHabilidadesSiCorresponde rival
}

quitarHabilidadesSiCorresponde :: Personaje -> [Habilidad]
quitarHabilidadesSiCorresponde personaje | ((<=2).length.habilidades) personaje = []
                                         | otherwise = habilidades personaje

elTiempo :: Gema
elTiempo rival = sacarEnergia 50 rival {
    edad = disminuirEdadCorrespondiente rival
} 

disminuirEdadCorrespondiente :: Personaje -> Number
disminuirEdadCorrespondiente personaje = max 18 $ div (edad personaje) 2

gemaLoca :: Gema -> Gema
gemaLoca gema = (gema.gema)


-- Punto 4
punisher :: Guantelete
punisher = Guantelete{
    material = "goma",
    gemas = [elAlma "usar Mjolnir",gemaLoca (elAlma "programacion en Haskell")]
}

-- Punto 5

utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas enemigo = foldr ($) enemigo gemas

-- Punto 6
gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa = (compararRoboEnergia.gemas)

compararRoboEnergia :: [Gema] -> Personaje -> Gema
compararRoboEnergia [unaGema] _ = unaGema
compararRoboEnergia (primerGema:segundaGema:gemas) victima 
    | (energia.primerGema) victima >= (energia.segundaGema) victima = compararRoboEnergia (primerGema:gemas) victima 
    | otherwise = compararRoboEnergia (segundaGema:gemas) victima

unPersonaje = Personaje {
    nombre = "ironMan",
    edad = 30,
    energia = 100,
    habilidades = [],
    planetaDondeViven = "Andromeda"
}

unGuantelete = Guantelete {
    material = "unMat",
    gemas = [elAlma "usar Mjolnir",elTiempo]
}    

-- Punto 7
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas elTiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

-- Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:
-- gemaMasPoderosa punisher guanteleteDeLocos
-- usoLasTresPrimerasGemas guanteleteDeLocos punisher
