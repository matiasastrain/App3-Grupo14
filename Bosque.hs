
module Bosque (
    Posicion,
    obtenerValor,
    dentroMatriz,
    esTrampa,
    esDiagonal,
    destino
) where

-- Tipo Posicion
type Posicion = (Int, Int)

-- Obtener el valor de una celda dada una posición
-- | Asume que la posición fue validada previamente con 'dentroMatriz'.
obtenerValor :: [[Int]] -> Posicion -> Int
obtenerValor bosque (x, y) = (bosque !! x) !! y

-- Verifica si una posición está dentro de los límites válidos de la matriz
dentroMatriz :: [[Int]] -> Posicion -> Bool
dentroMatriz [] _ = False  -- matriz vacía no tiene límites válidos
dentroMatriz bosque@(fila:_) (x, y) =
    x >= 0 && y >= 0 && x < length bosque && y < length fila

-- Determina si una celda es una trampa
esTrampa :: [[Int]] -> Posicion -> Bool
esTrampa bosque pos = obtenerValor bosque pos == 0

-- Verifica si el último movimiento fue en diagonal abajo-derecha
esDiagonal :: Posicion -> [Posicion] -> Bool
esDiagonal pos camino
    | null camino = False
    | otherwise =
        let (x1, y1) = last camino
            (x2, y2) = pos
        in (x2 - x1 == 1) && (y2 - y1 == 1)

-- Retorna la posición final de la matriz (abajo a la derecha)
destino :: [[Int]] -> Posicion
-- matriz vacía: posición por defecto
destino [] = (0, 0)  
destino (fila:resto) = (length (fila:resto) - 1, length fila - 1)

