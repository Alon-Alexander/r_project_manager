# Microbiome Project Organizer (R Package) — Product Summary

## 1) What this package is
This is an R package that enforces and supports a standardized folder structure for microbiome research projects.

The main goal is that researchers can focus only on *research work* (data prep, modeling, figures), while the package automatically handles:
- consistent organization of analysis code + results
- reproducible references to inputs
- discovery/loading of outputs from previous analyses
- provenance tracking (what produced what, using which inputs/parameters)

The package is intended to be used **in every step of day-to-day work**, not just at the end for archiving.

Implementation style: **R6-first** (core objects are R6 classes).


---

## 2) Core concepts and objects

### Project
A “project” is a folder on disk containing:
- global metadata
- input definitions
- one or more analyses
- optional registries/indexes for fast searching

In R, this is represented by an `PMProject` R6 object.

### Analysis
An “analysis” is a user-facing folder where work happens (prep, stats, figures, etc.).

Each analysis folder contains:
- a narrative README
- code (scripts / notebooks)
- results (tables, plots, models)
- optional cache for intermediate steps

In R, this is represented by an `PMAnalysis` R6 object.

### Artifacts
An “artifact” is any saved output from an analysis, such as:
- tables (tsv/csv)
- serialized objects (rds/qs)
- plots (png/pdf)
- intermediate cached objects

Artifacts are represented as records (data.frame/tibble rows) and optionally as an `PMArtifact` R6 object later.


---

## 3) Folder structure (source of truth)

### Project root
microbiome_project/
├── README.md
├── project.yaml # committed: semantic input definitions (portable)
├── inputs.local.yaml # NOT committed: local path mapping (machine-specific)
└── analyses/ # day-to-day work lives here

### Each analysis folder
analyses/a001_data_preparation/
├── README.md # narrative notes, user edited
├── code/ # scripts/functions
├── results/ # final outputs
├── cache/ # intermediate outputs (optional)
└── logs/

### Git rules
- `project.yaml` is committed.
- `inputs.local.yaml` is gitignored.
- analysis `results/` are git ignored.
- `cache/` is gitignored or optional.

Recommended `.gitignore` includes:
- `inputs.local.yaml`
- `analyses/**/cache/`
- `analyses/**/results/`


---

## 4) Inputs design (two-layer system)

### `project.yaml` (portable, committed)
Defines input *names* and expectations. No machine-specific paths.

Example keys:
- `inputs.feature_table` (type biom)
- `inputs.sample_metadata` (type tsv)
- `expected.required_columns` for validation
- optional fingerprint fields (md5, size, etc.)

### `inputs.local.yaml` (local, ignored)
Maps canonical input names to absolute paths for each user.

Example:
```yaml
paths:
  feature_table: "/Users/name/data/feature_table.biom"
  sample_metadata: "/Users/name/data/metadata.tsv"
```

---

## 5) Main user workflows (what package must make easy)
Note: This section is a draft and API might heavily change.

Create/open project
pm_project(path)

pm_project_init(path)

Configure inputs
pm_inputs_set(project, feature_table="...", sample_metadata="...")

pm_inputs_check(project) (existence + format + columns)

Create/open analysis
pm_analysis_create(project, "data_preparation") (auto-scaffold folder)

pm_analysis(project, "a001_data_preparation")

Resolve inputs in analysis code
pm_inputs(project) or pm_inputs(analysis)

pm_design(analysis) (if analysis defines a design matrix later)

Record outputs (core ergonomics feature)
These functions must auto-pick storage paths and update artifacts.jsonl:

pm_record_table(analysis, name, x, kind=...)

pm_record_rds(analysis, name, obj, stage=c("cache","output"))

pm_record_plot(analysis, name, gg, formats=...)

Discover and load outputs
pm_artifacts(analysis) list artifacts in that analysis

pm_load(analysis, name=..., kind=..., latest=TRUE)

pm_find_artifacts(project, ...) across analyses

Use outputs from another analysis (dependency chaining)
pm_use(analysis, from=other_analysis, name=..., kind=...)

pm_deps(analysis) resolves dependencies to artifact paths/objects

This enables the standard multi-analysis chain:
data preparation → modeling → figures

---

### 6) Product goals / non-goals
Goals
Researchers never manually choose output paths.

Everything saved is discoverable and reloadable.

Easy chaining across analyses.

Works even when input files live in different locations per person.

Works with minimal friction (sane defaults + “fix-it” error messages).

Non-goals (for MVP)
Full workflow execution engine

Enforcing specific microbiome tools (QIIME2/phyloseq/etc.)

Heavy database infrastructure

Perfect provenance capture across all external tools

---

8) Implementation constraints (important)
Package is implemented primarily via R6 classes

PMProject

PMAnalysis

optionally PMArtifact later

Prefer simple filesystem storage:

YAML config

JSONL artifact registry

plain results files (tsv/rds/png/pdf)

The package should remain useful even without optional dependencies (arrow/qs/etc.).