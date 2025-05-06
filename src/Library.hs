module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas | PatiVegano
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)
--parte 1
cuartoDeLibra = Hamburguesa {precioBase =20, ingredientes = [Pan,Carne,Cheddar,Pan]}
hamburguesaDePollo = Hamburguesa {precioBase =30,ingredientes=[Pan,Pollo,Pan]}

hamburguesaMixta =Hamburguesa {precioBase =20, ingredientes = [Pan,Carne,Pollo,Cheddar,Pan]}

tieneCarne:: Hamburguesa -> Bool
tieneCarne ham = any (==Carne)  (ingredientes ham)
tienePollo::  Hamburguesa -> Bool
tienePollo ham = any (==Pollo)  (ingredientes ham)

precioFinal:: Hamburguesa ->Number
precioFinal ham = precioBase ham + sum (map precioIngrediente (ingredientes ham))

agrandar:: Hamburguesa->Hamburguesa
agrandar hamburguesa
    | tienePatiVegano
    | tieneCarne hamburguesa = agregarIngrediente Carne hamburguesa
    | tienePollo hamburguesa =agregarIngrediente Pollo hamburguesa

agregarIngrediente :: Ingrediente ->Hamburguesa->Hamburguesa
agregarIngrediente ingrediente ham = ham{ingredientes= ingrediente: ingredientes ham}

descuento:: Number->Hamburguesa->Hamburguesa
descuento porcentaje ham = ham {precioBase = precioBase ham - precioBase ham* (porcentaje/100)}

pdepBurger :: Hamburguesa
pdepBurger = (agregarIngrediente Panceta. agregarIngrediente Cheddar . agrandar. agrandar. descuento 20) cuartoDeLibra

--parte 2

dobleCuarto :: Hamburguesa
dobleCuarto = (agrandar . agregarIngrediente Cheddar) cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

delDia :: Hamburguesa->Hamburguesa
delDia ham = (agregarIngrediente Papas . descuento 30) ham

