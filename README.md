# Modelado de nicho ecológico de Sobralia turkeliae

> Research Package generado por **[Ressearch AI](https://ressearchai.app)** · 2026-07-07T16:27:00.16343+00:00


> ⚠️ **Paquete incompleto**: 1 archivo(s) no se pudo(eron) incluir.
> Las entradas afectadas también constan en `MANIFEST.json.incomplete`.
>
> - `data/` — datos externos requeridos — ver data/DATA_SETUP.md


## ⚠️ Aviso de asistencia por IA

Este documento fue elaborado con asistencia de Ressearch AI. Su contenido puede incluir material generado o asistido por inteligencia artificial y debe validarse por una persona responsable antes de su publicación, uso clínico, regulatorio o académico. Marco normativo: Ley N.° 31814 del Perú y Reglamento DS 115-2025-PCM.

Marco normativo aplicable: **Ley N.° 31814** (Perú) y su Reglamento **Decreto Supremo N.° 115-2025-PCM**, así como la **NTP-ISO/IEC 42001:2025** (sistema de gestión de IA). Autoridad competente: Secretaría de Gobierno y Transformación Digital (SGTD) de la PCM. Más detalle en la [Tarjeta de Sistema de IA de Ressearch AI](https://ressearchai.app/legal/sistema-de-ia).

## Pregunta de investigación

OK Modelado de nicho ecológico de Sobralia turkeliae

## Objetivos

- Obtener y limpiar los datos de presencia de Sobralia turkeliae.
- Seleccionar las variables bioclimáticas óptimas mediante el Factor de Inflación de Varianza (VIF).
- Optimizar los parámetros del modelo MaxEnt utilizando validación cruzada espacial k-folds.
- Generar los productos del modelo de distribución de especies (ASCII, GeoTIFF y mapa).
- Analizar e interpretar el nicho ecológico y la biogeografía de Sobralia turkeliae.
- Validar los hallazgos del modelo mediante una búsqueda y revisión de literatura científica.

## Contenido del paquete

| Path | Contenido |
|------|-----------|
| `MANIFEST.json` | Inventario + SHA-256 de cada archivo |
| `ro-crate-metadata.json` | RO-Crate 1.1 (FAIR) |
| `codemeta.json` | CodeMeta 2.0 (software metadata) |
| `CITATION.cff` | Cómo citar este paquete |
| `LICENSE` / `LICENSE-DATA` | Licencias de código y datos |
| `Makefile` | `make reproduce` corre todo el pipeline |
| `Dockerfile` | Contenedor reproducible (`make docker-run`) |
| `code/pipeline.py` | Pipeline Python (2 ejecución(es)) |
| `code/pipeline.R` | Pipeline R (17 ejecución(es)) |
| `code/executions/` | Scripts individuales por ejecución |
| `code/execution_log.jsonl` | Log estructurado (uno-por-línea) |
| `figures/` | 6 figura(s) |
| `tables/` | 2 tabla(s) |
| `databases/` (canvas) | 4 dataset artifact(s) |
| `RESSEARCH_AGENTS.md` | Constitución + journal del agente |
| `provenance/` | Grafo PROV-O + changelog de artifacts |
| `environment/` | requirements.txt · environment.yml · r_packages.txt · renv.lock |
| `metadata/` | project.json · research_brief.json · data_availability.md |

## Cómo reproducir

Opción 1 — **Local** (recomendado si tienes Python/R instalado):

```bash
make reproduce
```

Opción 2 — **Contenedor reproducible** (si no quieres tocar tu sistema):

```bash
make docker-run
```

Opción 3 — **Manual**:

```bash
python -m venv .venv && source .venv/bin/activate
pip install -r environment/requirements.txt
cd code && python pipeline.py && cd ..
Rscript -e "renv::restore(lockfile='environment/renv.lock')"
cd code && Rscript pipeline.R && cd ..
make verify   # confirma checksums contra MANIFEST.json
```

## Integridad

Cada archivo del paquete tiene su SHA-256 en `MANIFEST.json`. Para validar:

```bash
make verify
```

## Estándares cumplidos

- **RO-Crate 1.1** — empaquetado FAIR (`ro-crate-metadata.json`).
- **W3C PROV-O** — grafo de proveniencia (`provenance/provenance.json`).
- **CodeMeta 2.0** — metadata de software (`codemeta.json`).
- **CITATION.cff** — citación interoperable con Zenodo, GitHub, Zotero.

## Licencias

- **Código**: MIT (ver `LICENSE`)
- **Datos / figuras / tablas**: CC-BY-4.0 (ver `LICENSE-DATA`)

## Reproducibilidad estricta

El pipeline inyecta un **random seed determinístico** derivado del proyecto;
los paths del sandbox (E2B `/home/user/`) se reescriben a relativos al
package root antes de exportar; el ZIP contiene un Dockerfile que fija un
entorno cerrado. Si necesitas paridad bit-exacta con el entorno original,
revisa los profiles del sandbox en `code/execution_log.jsonl` (campo
`profile`) y reproduce las imágenes desde el repo de Ressearch AI.

## Limitaciones conocidas

Los pasos se ejecutaron de forma interactiva y pueden depender de **estado en
memoria del kernel** entre turnos que **no se recrea** al correr el pipeline
linealmente. Si un paso falla con `NameError` (Python) u `object '...' not
found` (R), reordená los pasos o recomputá las dependencias del paso previo.
El pipeline instala un hook de error que imprimirá esta misma guía a `stderr`
cuando detecte ese tipo de fallo, sin ocultar el traceback original.

---

Si depositas este paquete en Zenodo, automaticamente recibe un DOI citable
desde tu paper.
