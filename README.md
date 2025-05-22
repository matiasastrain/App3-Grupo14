# **TICS200: App #3 – El Bosque de las Runas Mágicas (Paradigma Funcional)**

## **Profesores**
- **María Loreto Arriagada**  
  loreto.arriagada.v@edu.uai.cl

- **Paulina González**  
  paulina.gonzalez.p@edu.uai.cl

- **Justo Vargas**  
  justo.vargas@edu.uai.cl

## **Ayudante**
- **Diego Duhalde**  
  dduhalde@alumnos.uai.cl

---

## 1. Objetivos

1. Comprender el paradigma de Programación Funcional
2. Practicar **herramientas de apoyo** a la programación: GIT (fork, pull requests, commits balanceados), debuggers, etc.

---

## 2. Enunciado

Un mago quiere atravesar un bosque encantado lleno de runas que modifican su energía. Cada celda del bosque contiene una runa con un valor (positivo o negativo). El mago parte desde la esquina superior izquierda y quiere llegar a la esquina inferior derecha maximizando su energía restante.

### 2.1 Requerimientos funcionales

1. El bosque es una matriz de enteros (puede ser de tamaño NxN).
2. El mago solo puede moverse a la derecha o hacia abajo.
3. En cada celda, suma (o resta) el valor de la runa a su energía.
4. El mago comienza con una energía inicial (por ejemplo, 10).
5. El recorrido final debe ser uno de los caminos posibles que deje al mago con la mayor energía posible al final.
6. Si en algún momento la energía es menor que 0, el camino se invalida.
   

### 2.2 Supuestos y detalles

Entrada:
  1. Matriz de runas: [[2, -3, 1], [-5, 4, -2], [1, 3, 2]]
  2. Energía inicial: 10
     
Salida esperada:
  1. La lista con las coordenadas del camino válido con mayor energía final.
  2. La energía final.

### 2.3 Restricciones de implementación

  1. Usar programación funcional pura (sin variables mutables, ni bucles).
  2. Solución basada en recursión y/o map/filter/reduce según el lenguaje.
  3. Lenguaje: Haskell

## 3. Bonus y Detalles de la Entrega

- Agregar runas especiales: si una celda es "T" (teletransportador), puede enviar al mago a otra coordenada fija si la energía es suficiente.
- Opción de diagonales si el mago encuentra una runa "D" (doble salto).
  
- **Fecha de entrega**: Sábado 31 de Mayo a las 23:59.
- Por cada día de atraso se descuenta 1 punto, comenzando a las 00:00 del día siguiente.
  - Ejemplo: si entregan a las 00:00 del día siguiente, la nota máxima es 6.0.

## 4. Formato de Entrega (vía repositorio GitHub)

### Repositorio de trabajo

- Deberán crear un repositorio para el grupo que se llame App3 en GitHub (o el que indique el curso).
- Asegurarse que el repositorio sea privado al grupo de trabajo.
- En ese repositorio, agregar a todos los integrantes del grupo como colaboradores, y dar acceso a dicho repositorio al profesor y al ayudante.

### Commits balanceados y Pull Request

- Cada integrante del grupo debe tener aproximadamente la misma cantidad de commits.
- Se evaluará la participación equitativa a través del historial de commits.
- La entrega oficial por medio de WEBC indicando la URL del repo (o como indique la asignatura).

### Estructura del repositorio


### Compilación y ejecución

## 5. Rúbrica de Evaluación

| Criterio | Peso | Descripción |
|----------|------|-------------|
| 1. Funcionamiento general | 30% | <ul><li>El proyecto compila y se ejecuta correctamente.</li>
| 2. Paradigma Funcional | 30% | <ul><li>Uso adecuado de clases, encapsulamiento, herencia, interfaces y colecciones.</li><li>Organización en paquetes clara y lógica.</li><li>Aplicación de principios de OOP (mínima duplicación de código, coherencia en la jerarquía de clases, etc.).</li></ul> |
| 3. Informe de diseño y reflexiones finales | 10% | <ul><li><strong>Informe de diseño</strong>: explica la arquitectura, justifica las decisiones (7%).</li><li><strong>Reflexiones</strong>: aprendizajes, desafíos, etc. (5%).</li><li><strong>Uso de IA</strong>: transparencia y validación (3%).</li></ul> |
| 4. Uso de Git (commits y pull request) / Organización del repositorio/Presentación | 20% | <ul><li>Commits equilibrados entre integrantes (aporte individual visible).</li><li>Estructura del repositorio clara, con README que indique cómo compilar/ejecutar.</li></ul> |
| 5. Presentación | 10% | <ul><li>Presentar en clases la solución la app</li><li>Presentar la app funcionando.</li></ul> |
| **Total** | **100%** |  |

### Penalizaciones y Bonus

- **Atrasos**: Resta 1 punto al máximo posible por día de atraso (comenzando a las 00:00 del día siguiente).
- **Grupos con más de 5 integrantes**: penalización (no se admite grupo de 6).
- **Grupos con menos integrantes**: puede existir un pequeño bonus, según políticas de la asignatura.

## 6. Ejemplo de Uso


## 7. Conclusión

Este App #3 busca afianzar conocimientos del paradigma funcional con la incorporación de un nuevo lenguale de programación como es Haskell. Aseguren  un uso equilibrado de GIT para evidenciar la contribución de cada integrante.
