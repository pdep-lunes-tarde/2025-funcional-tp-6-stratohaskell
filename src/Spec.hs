module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    --parte 1
    describe "TP 5" $ do
        it "test de prueba" $ do
            2 + 2 `shouldBe` 4
    describe "agrandar" $ do
        it "Dada una hamburguesa con carne, devuelve la hamburguesa con una carne mas, es decir 2 carnes" $ do
            agrandar cuartoDeLibra `shouldBe` Hamburguesa {precioBase =20, ingredientes = [Carne,Pan,Carne,Cheddar,Pan]}
        it "Dada una hamburguesa con pollo, devuelve la hamburguesa con un pollo mas, es decir 2 pollos" $ do
            agrandar hamburguesaDePollo `shouldBe` Hamburguesa {precioBase =30,ingredientes=[Pollo,Pan,Pollo,Pan]}
        it "Si se recibe una hamburguesa mixta (con carne y pollo), se devuelve la hamburguesa con una carne mas" $ do
            agrandar hamburguesaMixta `shouldBe` Hamburguesa {precioBase =20, ingredientes = [Carne,Pan,Carne,Pollo,Cheddar,Pan]}

    describe "agregarIngrediente" $ do
        it "dada un ingrediente y una hamburguesa, se devuelve la hamburguesa con el ingrediente agregado" $ do
            agregarIngrediente  Panceta cuartoDeLibra `shouldBe` Hamburguesa {precioBase =20, ingredientes = [Panceta,Pan,Carne,Cheddar,Pan]}

    describe "descuento" $ do
        it "dado un porcentaje y una Hamburguesa, devuelve la hamburguesa con el precio base reducido segun el porcentaje" $ do
            descuento 25 cuartoDeLibra `shouldBe` Hamburguesa {precioBase =15, ingredientes = [Pan,Carne,Cheddar,Pan]}
    describe "pdepBurger" $ do
        it "La pdepBurger debe tener como precio final 110" $ do
            precioFinal pdepBurger `shouldBe` 110
    
    --parte 2

    describe "dobleCuarto " $ do
        it "el precioFinal de doble cuarto debería ser 84" $ do
            precioFinal dobleCuarto `shouldBe` 84
            
    describe "bigPdep " $ do
        it "el precioFinal de bigPdep debería ser 89" $ do
            precioFinal bigPdep `shouldBe` 89
    
    describe "delDia " $ do
        it "dada una hamburguesa se le tiene que agregar papas y un 30 porciento de descuento en el precio base" $ do
            precioFinal (delDia dobleCuarto) `shouldBe` 88

--parte 3

    describe "hacerVeggie" $ do
        it "dada una hamburguesa, cambia los ingredientes por unos aptos para veganos" $ do
            hacerVeggie pdepBurger `shouldBe` Hamburguesa{precioBase= 16, ingredientes =[BaconDeTofu,QuesoDeAlmendras,PatiVegano,PatiVegano,Pan,PatiVegano,QuesoDeAlmendras,Pan]}
    
    describe "cambiarPan" $ do
        it "dada una hamburguesa, devuelve la misma cambiando los Panes normales por panes intergrales" $ do
            cambiarPan cuartoDeLibra `shouldBe` Hamburguesa {precioBase =20, ingredientes = [PanIntegral,Carne,Cheddar,PanIntegral]}
        
    describe "dobleCuartoVegano" $ do
        it "el doble cuarto vegano debe ser un doble cuarto de libra hecho veggie y con pan integral" $ do
            dobleCuartoVegano `shouldBe` Hamburguesa {precioBase=20,ingredientes =[PatiVegano,QuesoDeAlmendras,PanIntegral,PatiVegano,QuesoDeAlmendras,PanIntegral]}
     