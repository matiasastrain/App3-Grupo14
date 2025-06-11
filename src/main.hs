import System.IO (hFlush, stdout)
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Caminos (caminoConMaximaEnergia)
import Bosque ()
-- Función principal: menú de opciones
main :: IO ()
main = do
  putStrLn "¿Qué deseas hacer?"
  putStrLn "1. Ingresar la matriz manualmente"
  putStrLn "2. Generar matriz aleatoria"
  opcion <- getLine

  case opcion of
    "1" -> do
      -- Opción 1: pedir la matriz manualmente
      (matriz, _, _) <- pedirMatrizManual
      guardarMatriz "bosque.txt" matriz
      putStrLn "\nMatriz guardada como 'bosque.txt':"
      mapM_ print matriz
      energia <- pedirEnergia
      ejecutarJuego matriz energia

    "2" -> do
      -- Opción 2: generar una matriz aleatoria
      (filas, columnas) <- pedirDimensiones
      matriz <- matrizAleatoria filas columnas
      guardarMatriz "bosque.txt" matriz
      putStrLn "\nMatriz aleatoria generada y guardada como 'bosque.txt':"
      mapM_ print matriz
      energia <- pedirEnergia
      ejecutarJuego matriz energia

    _  -> do
      putStrLn "Opción inválida. Intenta nuevamente.\n"
      main

-- Ejecuta el cálculo del camino con matriz y energía dados
ejecutarJuego :: [[Int]] -> Int -> IO ()
ejecutarJuego matriz energia = do
  let (camino, energiaFinal) = caminoConMaximaEnergia matriz energia
  if null camino
    then putStrLn "No hay camino válido posible con esa energía."
    else do
      putStrLn "\nCamino óptimo:"
      mapM_ print camino
      putStrLn $ "\nEnergía final: " ++ show energiaFinal


-- Pide al usuario la energía inicial
pedirEnergia :: IO Int
pedirEnergia = do
  putStr "\nIngresa la energía inicial (≥ 0): "
  hFlush stdout
  energia <- read <$> getLine
  if energia < 0
    then do
      putStrLn "La energía no puede ser negativa. Intenta de nuevo."
      pedirEnergia
    else return energia


-- Pide dimensiones (filas y columnas) al usuario
pedirDimensiones :: IO (Int, Int)
pedirDimensiones = do
  putStr "Número de filas: "
  hFlush stdout
  f <- read <$> getLine
  putStr "Número de columnas: "
  hFlush stdout
  c <- read <$> getLine
  return (f, c)

-- Permite ingresar la matriz fila por fila desde consola
pedirMatrizManual :: IO ([[Int]], Int, Int)
pedirMatrizManual = do
  putStrLn "\nDefine las dimensiones de la matriz:"
  (filas, columnas) <- pedirDimensiones
  putStrLn "\nIngresa los valores de la matriz fila por fila (separados por espacio):"
  matriz <- replicateM filas $ do
    putStr "> "
    hFlush stdout
    fmap (map read . words) getLine
  return (matriz, filas, columnas)

-- Genera una matriz aleatoria con valores entre -5 y 5
matrizAleatoria :: Int -> Int -> IO [[Int]]
matrizAleatoria filas columnas =
  replicateM filas (replicateM columnas (randomRIO (-5, 5)))

-- Guarda una matriz en un archivo de texto
guardarMatriz :: FilePath -> [[Int]] -> IO ()
guardarMatriz archivo matriz = writeFile archivo (formatearMatriz matriz)

-- Convierte una matriz a texto plano (para guardar en archivo)
formatearMatriz :: [[Int]] -> String
formatearMatriz = unlines . map (unwords . map show)
