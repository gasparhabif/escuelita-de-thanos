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
esGuanteleteCompleto guantelete = (length.gemas) guantelete == 6 && material guantelete == "uru"

reducirUniverso :: Universo -> Universo
reducirUniverso universo = take (div (length universo) 2) universo

-- Punto 2: (3 puntos) Resolver utilizando únicamente orden superior.

universoAptoParaPendex :: Universo -> Bool
universoAptoParaPendex = any ((<45).edad)

energiaTotalDeUniverso :: Universo -> Number
energiaTotalDeUniverso universo = sum (map energia (filter ((1<).length.habilidades) universo))

laMente :: Number -> Gema
laMente cantidad rival = rival {
    energia = energia rival - cantidad
} 

elAlma :: Habilidad -> Gema
elAlma habilidadAEliminar rival = rival {
    energia = energia rival - 10,
    habilidades = sacarHabilidadSiExiste rival habilidadAEliminar
}

sacarHabilidadSiExiste :: Personaje -> Habilidad -> [Habilidad]
sacarHabilidadSiExiste personaje habilidadAEliminar = filter (/= habilidadAEliminar) (habilidades personaje)

elEspacio :: Planeta -> Gema
elEspacio nuevoPlaneta rival = rival {
    energia = energia rival - 20,
    planetaDondeViven = nuevoPlaneta
}

elPoder :: Gema
elPoder rival = rival {
    energia = 0,
    habilidades = quitarHabilidadesSiCorresponde rival
}

quitarHabilidadesSiCorresponde :: Personaje -> [Habilidad]
quitarHabilidadesSiCorresponde personaje | ((<=2).length.habilidades) personaje = []
                                         | otherwise = habilidades personaje

elTiempo :: Gema
elTiempo rival = rival {
    energia = energia rival - 50,
    edad = disminuirEdadCorrespondiente rival
} 

disminuirEdadCorrespondiente :: Personaje -> Number
disminuirEdadCorrespondiente personaje | (div (edad personaje) 2) > 18 = div (edad personaje) 2
                                       | otherwise = 18

-- La gema loca que permite manipular el poder de una gema y la ejecuta 2 veces contra un rival.
gemaLoca :: Personaje -> Gema -> Personaje
gemaLoca rival gema = (gema.gema) rival


-- Punto 3: (3 puntos) Implementar las gemas del infinito, evitando lógica duplicada. 

-- Punto 4: (1 punto) Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma que quita la habilidad de “usar Mjolnir” y 
-- la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell”.


-- Punto 5: (2 puntos). No se puede utilizar recursividad. Generar la función utilizar  que dado una lista de gemas y un enemigo 
-- ejecuta el poder de cada una de las gemas que lo componen contra el personaje dado. Indicar cómo se produce el “efecto de lado” 
-- sobre la víctima.


-- Punto 6: (2 puntos). Resolver utilizando recursividad. Definir la función gemaMasPoderosa que dado un guantelete y una persona
-- obtiene la gema del infinito que produce la pérdida más grande de energía sobre la víctima. 


-- Punto 7: (1 punto) Dada la función generadora de gemas y un guantelete de locos:
-- infinitasGemas :: Gema -> [Gema]
-- infinitasGemas gema = gema:(infinitasGemas gema)

-- guanteleteDeLocos :: Guantelete
-- guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo)

-- Y la función 
-- usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
-- usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

-- Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:
-- gemaMasPoderosa punisher guanteleteDeLocos
-- usoLasTresPrimerasGemas guanteleteDeLocos punisher
