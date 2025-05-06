module Library where
import PdePreludat


data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas | PatiVegano | BaconDeTofu | PanIntegral
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
precioIngrediente BaconDeTofu = 12
precioIngrediente PanIntegral = 3


data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)
--parte 1
cuartoDeLibra = Hamburguesa {precioBase =20, ingredientes = [Pan,Carne,Cheddar,Pan]}
hamburguesaDePollo = Hamburguesa {precioBase =30,ingredientes=[Pan,Pollo,Pan]}

hamburguesaMixta =Hamburguesa {precioBase =20, ingredientes = [Pan,Carne,Pollo,Cheddar,Pan]}


tieneIngrediente::Ingrediente->Hamburguesa->Bool
tieneIngrediente ingrediente ham = any (==ingrediente) (ingredientes ham)

precioFinal:: Hamburguesa ->Number
precioFinal ham = precioBase ham + sum (map precioIngrediente (ingredientes ham))

agrandar:: Hamburguesa->Hamburguesa
agrandar hamburguesa
    | tieneIngrediente PatiVegano hamburguesa = agregarIngrediente PatiVegano hamburguesa
    | tieneIngrediente Carne hamburguesa = agregarIngrediente Carne hamburguesa
    | tieneIngrediente Pollo hamburguesa =agregarIngrediente Pollo hamburguesa

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

--parte 3

reemplazar::Ingrediente ->Ingrediente
reemplazar ingrediente
    |ingrediente == Carne ||ingrediente == Pollo = PatiVegano
    |ingrediente == Panceta = BaconDeTofu
    |ingrediente == Cheddar = QuesoDeAlmendras
    |otherwise = ingrediente

reemplazarPan::Ingrediente ->Ingrediente
reemplazarPan ingrediente 
    | ingrediente == Pan = PanIntegral
    | otherwise = ingrediente

hacerVeggie :: Hamburguesa->Hamburguesa
hacerVeggie ham = ham{ingredientes = map reemplazar (ingredientes ham)}

cambiarPan :: Hamburguesa->Hamburguesa
cambiarPan ham = ham {ingredientes = map reemplazarPan (ingredientes ham)}

dobleCuartoVegano ::Hamburguesa
dobleCuartoVegano = (hacerVeggie.cambiarPan) dobleCuarto

