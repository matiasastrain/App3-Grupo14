module Caminos (caminoConMaximaEnergia) where

import Bosque (
    Posicion,
    obtenerValor,
    dentroMatriz,
    esTrampa,
    esDiagonal,
    destino
  )

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- Tipos alias
type Camino = [Posicion]
type Energia = Int
type TablaMemoizacion = Map (Posicion, Energia) [(Camino, Energia)]
-- estructura para la poda
type RegistroMejorEnergia = Map Posicion Energia  

-- Movimientos válidos
movimientosPosibles :: [Posicion]
movimientosPosibles = [(1, 0), (0, 1), (1, 1), (-1, 0), (0, -1)]

-- Sumar dos posiciones
sumarPosiciones :: Posicion -> Posicion -> Posicion
sumarPosiciones (x, y) (dx, dy) = (x + dx, y + dy)

-- Punto de entrada con estructura inicial
caminoConMaximaEnergia :: [[Int]] -> Energia -> (Camino, Energia)
caminoConMaximaEnergia bosque energiaInicial =
  let (caminosEvaluados, _, _) = buscarUsandoMemoizacionYPoda bosque energiaInicial (0, 0) [] Map.empty Map.empty
  in seleccionarCaminoConMasEnergia caminosEvaluados


buscarUsandoMemoizacionYPoda ::
  [[Int]] ->
  Energia ->
  Posicion ->
  Camino ->
  TablaMemoizacion ->
  RegistroMejorEnergia ->
  ([(Camino, Energia)], TablaMemoizacion, RegistroMejorEnergia)

buscarUsandoMemoizacionYPoda bosque energiaActual posicionActual caminoActual tablaMemoizacion mejoresEnergia
  | not (dentroMatriz bosque posicionActual) = ([], tablaMemoizacion, mejoresEnergia)
  | posicionActual `elem` caminoActual = ([], tablaMemoizacion, mejoresEnergia)
  | energiaCalculada < 0 = ([], tablaMemoizacion, mejoresEnergia)
  | maybe False (>= energiaCalculada) (Map.lookup posicionActual mejoresEnergia) = ([], tablaMemoizacion, mejoresEnergia)
  | posicionActual == destino bosque = ([(caminoActualActualizado, energiaCalculada)], tablaMemoizacion, Map.insert posicionActual energiaCalculada mejoresEnergia)
  | otherwise =
      case Map.lookup (posicionActual, energiaActual) tablaMemoizacion of
        Just resultadoMemoizado ->
          (map (\(caminoMemorizado, energiaMemorizada) -> (caminoActual ++ caminoMemorizado, energiaMemorizada)) resultadoMemoizado, tablaMemoizacion, mejoresEnergia)
        Nothing ->
          let (caminosFinales, nuevaTablaMemo, nuevoRegistroEnergia) =
                foldl procesarMovimiento ([], tablaMemoizacion, Map.insert posicionActual energiaCalculada mejoresEnergia) movimientosPosibles
              resultadoARegistrar = map (\(caminoParcial, energiaRestante) -> (drop (length caminoActual) caminoParcial, energiaRestante)) caminosFinales
              tablaActualizada = Map.insert (posicionActual, energiaActual) resultadoARegistrar nuevaTablaMemo
          in (caminosFinales, tablaActualizada, nuevoRegistroEnergia)
  where
    valorCelda = obtenerValor bosque posicionActual
    penalizacionPorTrampa = if esTrampa bosque posicionActual then 3 else 0
    penalizacionPorDiagonal = if esDiagonal posicionActual caminoActual then 2 else 0
    energiaCalculada = energiaActual + valorCelda - penalizacionPorTrampa - penalizacionPorDiagonal
    caminoActualActualizado = caminoActual ++ [posicionActual]

    procesarMovimiento :: ([(Camino, Energia)], TablaMemoizacion, RegistroMejorEnergia) -> Posicion -> ([(Camino, Energia)], TablaMemoizacion, RegistroMejorEnergia)
    procesarMovimiento (acumulado, tabla, registro) movimiento =
      let siguientePosicion = sumarPosiciones posicionActual movimiento
          (resultados, tablaActualizada, registroActualizado) = buscarUsandoMemoizacionYPoda bosque energiaCalculada siguientePosicion caminoActualActualizado tabla registro
      in (acumulado ++ resultados, tablaActualizada, registroActualizado)

-- Escoge el mejor camino entre todos
seleccionarCaminoConMasEnergia :: [(Camino, Energia)] -> (Camino, Energia)
seleccionarCaminoConMasEnergia [] = ([], 0)
seleccionarCaminoConMasEnergia (caminoInicial:otrosCaminos) = foldl compararEnergia caminoInicial otrosCaminos
  where
    compararEnergia camino1 camino2 =
      let (_, energia1) = camino1
          (_, energia2) = camino2
      in if energia2 > energia1 then camino2 else camino1

