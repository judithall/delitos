# Reglas del Repositorio (Manuscrito Quarto)

## Estructura y Compilación
- El proyecto se compila con `quarto render`.
- Nunca inventes ni redondees manualmente coeficientes, errores estándar o p-valores; todo valor en el texto debe provenir directamente de la ejecución de los scripts sobre los datos verificados.
- Respeta estrictamente el alcance inferencial: unidad de análisis agregada (N = 29 centros), evitando falacia ecológica y lenguaje causal.

## Flujo de Trabajo
- Crea siempre una rama nueva antes de modificar archivos `.qmd` o scripts.
- Verifica que `quarto render` termine sin errores tras cualquier edición.
