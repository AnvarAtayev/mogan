# Mogan Markdown Plugin

A plugin for exporting TeXmacs documents to Markdown. Supports two flavours:
- **Vanilla** — standard Markdown
- **Hugo** — Markdown with Hugo shortcodes and YAML frontmatter

## Directory Structure

```
TeXmacs/plugins/markdown/
├── packages/markdown/                  # TeXmacs style macros for .tm documents
│   ├── markdown.ts                     # Generic markdown helper macros
│   └── hugo.ts                         # Hugo-specific macros
├── progs/
│   ├── convert/markdown/               # Core conversion pipeline (Scheme)
│   │   ├── tmmarkdown.scm              # Stage 1: TeXmacs stree → markdown stree
│   │   ├── markdownout.scm             # Stage 2: markdown stree → markdown string
│   │   ├── markdown-utils.scm          # Shared utilities (wrapping, YAML, encoding)
│   │   ├── markdown-smart-ref.scm      # Smart reference type table
│   │   ├── markdown-menus.scm          # UI menu integration
│   │   └── markdownout-test.scm        # Unit tests for markdownout
│   └── data/markdown/
│       └── markdown.scm                # Format registration and preferences
└── tests/
    ├── run.scm                         # Batch test runner
    ├── markdown-integration-test.scm   # Integration test framework
    ├── vanilla/                        # Vanilla test pairs (.tm + expected .md)
    └── hugo/                           # Hugo test pairs (.tm + expected .md)
```

## Conversion Pipeline

Conversion happens in two stages with a Markdown S-expression as the intermediate representation:

```
.tm file
  └─→ parse-texmacs + tree→stree
        └─→ texmacs->markdown*   [tmmarkdown.scm]    TeXmacs stree → Markdown stree
              └─→ serialize-markdown*  [markdownout.scm]   Markdown stree → string
                    └─→ .md file
```

The two-stage design keeps semantic conversion (what things mean) separate from serialization (how they look), and makes the intermediate tree inspectable for debugging.

## File Reference

### `progs/data/markdown/markdown.scm` — Format Registration

The entry point for the plugin. Registers converters with the TeXmacs conversion system and defines user-facing preferences:

| Preference | Default | Description |
|---|---|---|
| `texmacs->markdown:flavour` | `"vanilla"` | Output flavour: `"vanilla"` or `"hugo"` |
| `texmacs->markdown:paragraph-width` | `79` | Word-wrap column limit (`#f` to disable) |
| `texmacs->markdown:numbered-sections` | `"on"` | Auto-number section headings |
| `texmacs->markdown:table-format` | `"html"` | Render tables as `"html"` or raw Markdown |
| `texmacs->markdown:auto-export` | `"off"` | Auto-save to `.md` on document save |

---

### `progs/convert/markdown/tmmarkdown.scm` — Stage 1: Semantic Conversion

Converts a TeXmacs S-expression tree into a Markdown S-expression tree. This stage handles all semantic decisions — what each TeXmacs tag means in Markdown.

**Key responsibilities:**

- **Counter system** — hierarchical numbering for sections, figures, equations, and tables. Child counters reset automatically when their parent increments.
- **Label tracking** — builds a table mapping label IDs to their counter values, used later for cross-reference resolution.
- **Tag dispatch** — a table of ~110 TeXmacs tags mapped to handlers. Examples:
  - `section / subsection / subsubsection` → `h1 / h2 / h3` with counter
  - `strong / em / tt` → inline style tags
  - `itemize / enumerate` → list structures
  - `equation / eqnarray` → math environments
  - `theorem / lemma / definition` → `std-env` with numbered caption
  - `big-figure / small-figure` → `figure` with caption
- **Math conversion** — calls `texmacs->latex` for inline and display math, extracts any embedded labels.
- **Hugo shortcodes** — passes `hugo-short` tags through to Stage 2.

Output format: `(markdown (labels (id . value) ...) <body>)`

---

### `progs/convert/markdown/markdownout.scm` — Stage 2: Serialization

Converts the Markdown S-expression from Stage 1 into a Markdown string. This stage handles all formatting decisions — how things look on the page.

**Key responsibilities:**

- **Global state** — indentation level, list item markers, document language, and collected footnotes are tracked in a hash table (`md-globals`) and restored after scoped changes via `with-md-globals`.
- **Word wrapping** — `md-paragraph` wraps text at the configured column width, using separate prefixes for the first line and continuation lines. Skipped for code blocks and math.
- **List rendering** — `md-list` manages `- ` / `1. ` prefixes and indentation. Multi-paragraph list items use blank-line separation and indented continuation paragraphs (`md-subpara`).
- **Style distribution** — `add-style-to` pushes inline styles (`**bold**`, `*italic*`) down into block elements. It is idempotent (duplicate styles collapse) and never improperly wraps structured environments.
- **Math** — display equations use `\[...\]` delimiters. Numbered equations generate HTML anchor links for cross-referencing.
- **Tables** — serialized as HTML `<table>` elements.
- **Cross-references** — `md-reference` and `md-eqref` emit Markdown links pointing to anchors built from the label table.
- **Hugo features** — YAML frontmatter written in a prelude, footnotes collected and emitted in a postlude, `{{<cite>}}` and `{{<toc>}}` shortcodes.

---

### `progs/convert/markdown/markdown-utils.scm` — Shared Utilities

Utilities used by both conversion stages:

- **`adjust-width` / `adjust-width*`** — word-wraps a string, supporting different prefixes for the first line vs continuation lines. Avoids breaking Markdown syntax at line starts (characters like `- + * > :`).
- **`md-split-lines`** — S7-compatible line splitting that preserves empty lines (unlike S7's `string-split`).
- **Encoding** — `tm-encoding->md-encoding` and `md-encoding->tm-encoding` handle Cork ↔ UTF-8 conversion.
- **`sanitize-selector`** — makes strings safe for use as HTML `id` attributes.
- **S-tree transforms** — `replace-fun`, `stree-contains?` for recursive pattern replacement.
- **YAML serialization** — `serialize-yaml`, `dict->yaml`, `string->yaml` for Hugo frontmatter generation.
- **Auto-export** — `autoexport-on?`, `save-buffer` hook integration.

---

### `progs/convert/markdown/markdown-smart-ref.scm` — Smart Reference Types

A data-only file mapping ~90 reference type names to their display strings. Used by Stage 1 to generate text like "Theorem 3.2" or "Section 2" automatically from smart references.

Examples:
- `thm-ref`, `theorem-ref` → `"Theorem"`
- `sec-ref`, `section-ref` → `"Section"`
- `eq-ref`, `eqn-ref` → `"make-eqref"` (special equation handling)
- `fig-ref` → `"Figure"`

---

### `progs/convert/markdown/markdown-menus.scm` — UI Menus

Integrates the plugin into the TeXmacs menu system:

- Export menu items for vanilla and Hugo Markdown
- Preferences submenu for flavour, paragraph width, section numbering, table format, and auto-export
- Developer tools: `copy-mdtree` (inspect intermediate stree), `run-tests`

---

### `progs/convert/markdown/markdownout-test.scm` — Unit Tests

Unit tests for `add-style-to`, the style distribution function in Stage 2. Covers:

- **Idempotency**: `(em (em x))` → `(em x)`
- **Distribution**: `(em (document a b))` → `(document (em a) (em b))`
- **Non-stylable blocks**: `(em (std-env ...))` wraps the entire node
- **Exemptions**: math, labels, and footnotes are left unstyled

---

### `packages/markdown/markdown.ts` — Generic Markdown Macros

TeXmacs style macros for use inside `.tm` documents:

- **`md-alt-image(img, alt)`** — display one image in TeXmacs and a different one in Markdown export
- **`eqnarray-lab(lab)`** / **`eqnarray-lab*`** — equation labels with reference anchors for `eqnarray` environments

---

### `packages/markdown/hugo.ts` — Hugo Macros

TeXmacs macros for Hugo-specific content in `.tm` documents:

- **`hugo-short(name, args...)`** — embed an arbitrary Hugo shortcode
- **`hugo-front(key, val, ...)`** — set YAML frontmatter fields directly from the document
- **`dict(...)`** — construct a YAML dictionary value
- **`pdf-name`** — sets the PDF download filename

---

## Tests

Tests use paired `.tm` (input) and `.md` (expected output) files.

### Integration test framework (`markdown-integration-test.scm`)

Runs end-to-end conversion for each test pair:
1. Load and parse the `.tm` file to a stree
2. Extract the document language from the stree
3. Run the full pipeline: `texmacs->markdown` → `serialize-markdown-document`
4. Compare the result against the expected `.md` file

### Vanilla tests (`tests/vanilla/`)

Run with 79-character paragraph width, vanilla flavour.

| Test | What it covers |
|---|---|
| `code` | Inline and fenced code blocks (Python, Shell, C++, Scheme) |
| `eqnarray` | Equation arrays with labels |
| `itemize` | Lists with multi-paragraph items |
| `itemize-styled` | Lists with inline styling |
| `quotations` | Block quotes |
| `sections` | Header hierarchy with auto-numbering |
| `simple-math` | Inline and display math |
| `std-environments` | Theorem, definition, lemma environments |
| `styles` | Bold, italic, monospace, strikethrough |
| `titles` | Document title, authors, date |
| `encoding` | Character encoding |
| `label` | Anchor labels and cross-references |
| `numbered-equations` | Equation numbering and linking |
| `references` | Cross-reference links |
| `specific` | Medium-specific content filtering |

### Hugo tests (`tests/hugo/`)

| Test | What it covers |
|---|---|
| `abstract` | Abstract in YAML frontmatter |
| `doc-data` | Author metadata |
| `frontmatter` | Custom YAML frontmatter fields |
| `marginal-figure` | Side figures as Hugo shortcodes |
| `marginal-note` | Sidenotes as Hugo shortcodes |
| `tm-figures` | Figures as Hugo shortcodes |
| `wide-figure` | Full-width figure shortcodes |

### Running tests

```scheme
; From within Mogan/TeXmacs:
(run-tests)

; Or headless:
./build/macosx/arm64/release/MoganSTEM \
  -headless \
  -b /tmp/md-run-tests2.scm \
  -x "(regtest-run2)" \
  -q
```

Test output is written to the hourly log at:
`~/Library/Application Support/moganlab/system/YYYYMMDDHH.log`

## S7 Compatibility Notes

The plugin runs on S7 Scheme (used by Mogan), not Guile. Several Guile/SRFI-13 functions are missing from S7 and have been reimplemented:

| Function | Location | Reason |
|---|---|---|
| `string-capitalize` | `tmmarkdown.scm` | Not in S7 |
| `md-split-lines` | `markdown-utils.scm` | S7 `string-split` collapses consecutive delimiters |
