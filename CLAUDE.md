# pcproject

A browser-only tool for projecting user-uploaded PLINK genotype data onto a
precomputed PCA (principal component analysis) of reference populations, and
plotting the result. No backend — everything (PLINK parsing, SNP overlap
matching, projection math) runs client-side in the browser.

Deployed via GitHub Pages at http://www.stephanschiffels.de/pcproject/
(a project page under the user's custom domain, so all app-internal
references — script imports, `fetch()` calls — must be relative paths, never
absolute (`/app.js` resolves to the domain root, not `/pcproject/`)).

## Stack

- **PureScript** (Halogen) compiled to a single JS module via Spago/esbuild.
- **Spago multi-package workspace** (`spago.yaml` at repo root holds only the
  `workspace:`/package-set config; each real package lives under `packages/*`
  with its own `spago.yaml`). This is groundwork for eventually publishing
  pieces of this app as standalone npm packages (a `@popgen-toolbox` scope) —
  see git history around the `refactor/monorepo-packages` branch for the
  rationale.
- `docs/` (repo root, **not** under `packages/`) is both the GitHub Pages
  source folder and the `npm run serve` target — deliberately left outside
  the package split because GitHub Pages' "source folder" setting only
  supports repo-root or `/docs`, not arbitrary nested paths.
- Charts via `chartjs` / `chartjs-halogen` (Chart.js wrapped for Halogen).

## Commands

- `npm run build` — `spago bundle -p webapp --outfile ../../docs/app.js
  --bundle-type module` (the `--outfile` path is relative to the *selected
  package's* directory, `packages/webapp`, hence `../../`)
- `npm run serve` — `http-server docs -c -1` (serves the same folder GitHub
  Pages serves, so this is a reliable local preview of the deployed site)
- `npm test` — `spago test` (runs the (currently placeholder) test suite for
  every package in the workspace)
- `npm run repl` — `spago repl`
- `spago build` (no `-p`) builds all three workspace packages; `spago build
  -p <name>` / `spago test -p <name>` scope to one.

After any change under `packages/*/src`, run `npm run build` before `npm run
serve` — `docs/app.js` is a committed build artifact, not generated on the fly.

## Package layout (`packages/`)

Three Spago packages, each environment-agnostic unless noted:

- **`genotype-io`** — `GenotypeIO.Plink` (+ `.js`): binary parsers for
  `.bed`/`.bim`/`.fam`. Pure functions over strings/`ArrayBuffer`s, no
  browser/DOM/network dependency — usable from Node or the browser alike.
- **`pca`** — depends on `genotype-io`. `Pca.SnpWeights` / `Pca.RefPosData`
  (parsers for the reference PCA bundle: per-SNP PC weights/frequencies,
  reference sample coordinates) and `Pca.Projection` (the actual math:
  `getOverlapMasks` matches SNPs between user data and reference weights,
  handling strand ambiguity and allele flips; then `projectSamples` projects
  genotypes onto the PCs). Also has no browser-specific dependency — this is
  the intended reusable "core."
- **`webapp`** — the Halogen UI (only package allowed to depend on
  `halogen`/`chartjs`/DOM). Depends on both `genotype-io` and `pca`.
  - `src/Main.purs` — entry point, mounts `App.Interface.component` into the
    page body.
  - `src/App/Interface.purs` — root Halogen component. Loads the reference
    PCA bundle on init (`LoadRefData`), holds uploaded user data, and
    triggers `RunProjection` whenever both are present. Owns the
    three-column layout (reference data box / projection monitor / user
    upload) and the two chart boxes below it.
  - `src/App/UserInputComponent.purs` — file upload widget (PLINK
    `.fam`/`.bim`/`.bed` triplet) and the "Load Example Data" button, which
    fetches a bundled example triplet from `docs/assets/` instead of
    requiring a user upload.
  - `src/App/RefChart.purs` / `RefChart.js` — scatter plot of reference
    population samples (Chart.js), grouped/colored by `popGroup`.
  - `src/App/ProjChart.purs` — scatter plot overlaying projected user
    samples (black) on top of a grayed-out reference layer; filters out
    samples with fewer than 20000 overlapping SNPs.
  - `src/App/Utils.purs` — `RemoteData e a` (`NotAsked | Loading | Failure e
    | Success a`), used throughout to drive loading/error UI state.

PureScript modules with FFI pair a `.purs` file with a same-named `.js` file
holding the JS implementation (binary parsing, typed-array math) — check the
`.js` file when a `.purs` file only has `foreign import` declarations. Module
names follow each package's own namespace (`GenotypeIO.*`, `Pca.*`) rather
than the old flat `PCproject.*` namespace from before the package split; the
app's own UI modules keep the `App.*` namespace since they aren't a published
library.

## Data flow

1. On load, `App.Interface` fetches the reference bundle from
   `docs/assets/`: SNP weights+frequencies (`.txt`), reference sample
   PCA coordinates (`.tsv`), and PCA parameters (`.json`, includes
   eigenvalues and default X/Y PC axes for plotting).
2. User either uploads a `.fam`/`.bim`/`.bed` triplet or clicks "Load Example
   Data" (fetches a bundled example triplet from `docs/assets/`).
3. Once both the reference bundle and user data are present,
   `RunProjection` overlaps SNPs, reduces weights to the overlap, projects
   genotypes onto PCs, and renders results in the projection chart.

## Known constraints / gotchas

- **Git LFS breaks GitHub Pages.** `docs/assets/*.bed` and `*.bim` are
  currently tracked by Git LFS (`.gitattributes`). GitHub Pages serves the
  *raw git blob*, not the LFS-resolved content — for LFS-tracked files that
  blob is just a small pointer stub (`version https://git-lfs.github.com/...`),
  so the deployed "Load Example Data" button fetches ~100 bytes of pointer
  text instead of the real multi-MB file and fails to parse. This is not a
  path bug — `git cat-file -p HEAD:docs/assets/<file>` on a clean checkout
  will show a pointer file if this is still true. Not yet fixed as of this
  writing; options are to de-LFS these files (commit real bytes, grows repo
  size) or host them externally with CORS enabled and fetch by absolute URL.
- Local working-tree files *are* the real, full-size data (LFS smudges them
  on checkout) — the mismatch only shows up in what's actually pushed/served,
  so `ls -la docs/assets` locally looks fine even when this bug is present.
- The example-data assets in `docs/assets/` are large (tens of MB); avoid
  reading them directly into context — use `ls -la`, `wc -l`, or targeted
  `grep`/`head` instead.
- `nextAnimationFrame` in `App/Interface.purs` is a deliberate hack: the
  actual projection math (`RunProjection`) runs synchronously on the JS
  thread, so two `requestAnimationFrame` round-trips are forced first to let
  Halogen flush the "Loading…" spinner to the DOM before the blocking
  computation starts. Fetch-based loading doesn't need this since `fetch` is
  naturally async and yields control on its own.
