# @popgen-toolbox/pca

Project PLINK genotype data onto a precomputed PCA of reference
populations, and load hosted reference panels to project against. Built on
[@popgen-toolbox/genotype-io](https://www.npmjs.com/package/@popgen-toolbox/genotype-io).

Compiled from PureScript ([source](https://github.com/stschiff/pcproject/tree/main/packages/pca));
full generated API reference is on [Pursuit](https://pursuit.purescript.org)
once published there.

## Install

```bash
npm install @popgen-toolbox/pca
```

## Loading a reference panel

```js
import { panels, loadReferenceBundle } from "@popgen-toolbox/pca";

console.log(Object.keys(panels)); // e.g. ["WestEurasia_HiRes"]

const result = await loadReferenceBundle(panels["WestEurasia_HiRes"]);
if (result.constructor.name === "Left") {
  throw new Error(result.value0); // load failed - value0 is the error message
}
const ref = result.value0; // { snpWeights, refPosData, pcaParams }
```

`loadReferenceBundle` returns a real `Promise` directly, awaitable as
normal - no extra `()` needed. It resolves to an `Either`-shaped value (a
PureScript convention) rather than rejecting the promise on failure, so
check `result.constructor.name` before using it: `"Left"` means it failed
(message in `.value0`), `"Right"` means `.value0` is the loaded bundle.

## Full pipeline: projecting your own data

```js
import { readFamData, readBimData, readBedData } from "@popgen-toolbox/genotype-io";
import {
  panels, loadReferenceBundle,
  getOverlapMasks, reducePcWeights, extractAndTransposeGenotypes, projectSamples
} from "@popgen-toolbox/pca";

const fam = readFamData(famText);
const bim = readBimData(bimText);
const numSNPs = bim.snpIDs.length;
const numInds = fam.indNames.length;
const bed = readBedData(bedBuffer, numSNPs, numInds);

const result = await loadReferenceBundle(panels["WestEurasia_HiRes"]);
const ref = result.value0; // see note above about checking Left/Right first

const overlap = getOverlapMasks(bim, ref.snpWeights);
const reduced = reducePcWeights(ref.snpWeights, overlap);
const genotypes = extractAndTransposeGenotypes(bed, numSNPs, numInds, overlap);
const projected = projectSamples(genotypes, reduced.pcWeights, reduced.frequencies, numInds, reduced.numPCs, ref.pcaParams);

// projected[i] = { pcCoordinates: number[], nonMissingCount: number }
// corresponds to fam.indNames[i] / fam.popNames[i]
```

Every function above is a plain, directly-callable JS function - no
currying, no trailing `()` (except `await` on the one Promise-returning
call). Verified end-to-end against the published `WestEurasia_HiRes` panel
and a 35-sample test dataset: 413,151 overlapping SNPs, 10 PCs, all 35
samples projected.

## License

MIT
