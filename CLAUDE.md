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
- `docs/` is both the GitHub Pages source folder and the `npm run serve`
  target — `npm run build` writes `docs/app.js` directly, so there is no
  separate dev/prod asset layout to keep in sync.
- Charts via `chartjs` / `chartjs-halogen` (Chart.js wrapped for Halogen).

## Commands

- `npm run build` — `spago bundle --outfile docs/app.js --bundle-type module`
- `npm run serve` — `http-server docs -c -1` (serves the same folder GitHub
  Pages serves, so this is a reliable local preview of the deployed site)
- `npm test` — `spago test`
- `npm run repl` — `spago repl`

After any change under `src/`, run `npm run build` before `npm run serve` —
`docs/app.js` is a committed build artifact, not generated on the fly.

## Source layout (`src/`)

- `Main.purs` — entry point, mounts `App.Interface.component` into the page body.
- `App/Interface.purs` — root Halogen component. Loads the reference PCA
  bundle on init (`LoadRefData`), holds uploaded user data, and triggers
  `RunProjection` whenever both are present. Owns the three-column layout
  (reference data box / projection monitor / user upload) and the two chart
  boxes below it.
- `App/UserInputComponent.purs` — file upload widget (PLINK `.fam`/`.bim`/`.bed`
  triplet) and the "Load Example Data" button, which fetches a bundled example
  triplet from `docs/assets/` instead of requiring a user upload.
- `App/RefChart.purs` / `App/RefChart.js` — scatter plot of reference
  population samples (Chart.js), grouped/colored by `popGroup`.
- `App/ProjChart.purs` — scatter plot overlaying projected user samples (black)
  on top of a grayed-out reference layer; filters out samples with fewer than
  20000 overlapping SNPs.
- `App/Utils.purs` — `RemoteData e a` (`NotAsked | Loading | Failure e | Success a`),
  used throughout to drive loading/error UI state.
- `PCproject/PlinkData.purs` (+ `.js`) — binary parsers for `.bed`/`.bim`/`.fam`.
- `PCproject/SnpWeights.purs` / `RefPosData.purs` — parsers for the reference
  PCA bundle: per-SNP PC weights/frequencies, and reference sample coordinates.
- `PCproject/PCproject.purs` (+ `.js`) — the actual math: matching SNPs
  between the user's data and the reference weights (`getOverlapMasks`,
  handling strand ambiguity and allele flips), then projecting genotypes onto
  the PCs (`projectSamples`).

PureScript modules with FFI pair a `.purs` file with a same-named `.js` file
holding the JS implementation (binary parsing, typed-array math) — check the
`.js` file when a `.purs` file only has `foreign import` declarations.

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
