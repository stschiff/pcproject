# @popgen-toolbox/genotype-io

Binary parsers for PLINK `.bed`/`.bim`/`.fam` genotype files. Pure functions
over strings and `ArrayBuffer`s — no browser/DOM or Node-specific
dependency, so it works the same in a browser tab, a Node script, or an
Observable notebook.

Compiled from PureScript ([source](https://github.com/stschiff/pcproject/tree/main/packages/genotype-io));
full generated API reference is on [Pursuit](https://pursuit.purescript.org)
once published there.

## Install

```bash
npm install @popgen-toolbox/genotype-io
```

## Usage

```js
import { readFamData, readBimData, readBedData } from "@popgen-toolbox/genotype-io";

const famText = await fetch(".../sample.fam").then(r => r.text());
const bimText = await fetch(".../sample.bim").then(r => r.text());
const bedBuffer = await fetch(".../sample.bed").then(r => r.arrayBuffer());

const fam = readFamData(famText);
const bim = readBimData(bimText);
const numSNPs = bim.snpIDs.length;
const numIndividuals = fam.indNames.length;
const bed = readBedData(bedBuffer, numSNPs, numIndividuals);
```

Plain functions, called the normal JS way — no currying, no extra `()` to
force execution. (These are compiled from PureScript, which would normally
mean both of those; this package wraps every exported function so you don't
need to know that.)

## Shapes returned

- `readFamData(text)` → `{ indNames: string[], popNames: string[] }`
- `readBimData(text)` → `{ snpIDs: string[], chromosomes: Uint8Array, positions: Uint32Array, alleles1: Uint8Array, alleles2: Uint8Array }`
- `readBedData(buffer, numSNPs, numInds)` → `Uint8Array`, one byte per
  genotype call (individual × SNP)

## License

MIT
