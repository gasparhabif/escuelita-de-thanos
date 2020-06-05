import PdePreludat
import Library
import Test.Hspec

gema1 = Gema {
    poder = "Nidea"
}

guanteleteCompleto = Guantelete {
    material = "uru",
    gemas = [gema1,gema1,gema1,gema1,gema1,gema1]
}

guanteleteIncompleto1 = Guantelete {
    material = "materialMalo",
    gemas = [gema1,gema1,gema1,gema1,gema1,gema1]
}

guanteleteIncompleto2 = Guantelete {
    material = "uru",
    gemas = [gema1,gema1]
}

personaje1 = Personaje {
    nombre = "ironMan",
    edad = 30,
    energia = 100,
    habilidades = [],
    planetaDondeViven = "Andromeda"
}
personaje2 = Personaje{
    nombre = "drStrange",
    edad = 30,
    energia = 100,
    habilidades = [],
    planetaDondeViven = "Andromeda"
}

main :: IO ()
main = hspec $ do
  -- Tests Punto 1
  describe "Tests de Chasquear Universo: " $ do
    it "Un guantelete completo" $ do
      chasquearUniverso guanteleteCompleto [personaje1,personaje2,personaje1] `shouldBe` [personaje1]
    it "Un guantelete con material incorrecto" $ do
      chasquearUniverso guanteleteIncompleto1 [personaje1,personaje2] `shouldBe` [personaje1,personaje2]
    it "Un guantelete con gemas incorrectas" $ do
      chasquearUniverso guanteleteIncompleto2 [personaje1,personaje2] `shouldBe` [personaje1,personaje2]

