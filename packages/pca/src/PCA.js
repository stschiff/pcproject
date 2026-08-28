import dgels from '@rreusser/blapack/lapack/base/dgels';

export function getOverlapMasksImpl(sampleBimData, snpWeights) {
    const snpWeightMask = new Uint8Array(snpWeights.snpIDs.length);
    const plinkMask = new Uint8Array(sampleBimData.snpIDs.length);
    const flipMask = new Uint8Array(sampleBimData.snpIDs.length);
    let plinkIndex = 0;
    let removedStrandAmbiguous = 0;
    let removedInconsistent = 0;
    let nrIncluded = 0;
    let nrToBeFlipped = 0;

    for (let i = 0; i < snpWeights.snpIDs.length; i++) {
        while (sampleBimData.chromosomes[plinkIndex] < snpWeights.chromosomes[i] ||
                (sampleBimData.chromosomes[plinkIndex] == snpWeights.chromosomes[i] &&
                sampleBimData.positions[plinkIndex] < snpWeights.positions[i])) {
            plinkIndex++;
        }
        if (sampleBimData.chromosomes[plinkIndex] === snpWeights.chromosomes[i] &&
            sampleBimData.positions[plinkIndex] === snpWeights.positions[i]) {
            const pa1 = String.fromCharCode(sampleBimData.alleles1[plinkIndex]);
            const pa2 = String.fromCharCode(sampleBimData.alleles2[plinkIndex]);
            const sa1 = String.fromCharCode(snpWeights.alleles1[i]);
            const sa2 = String.fromCharCode(snpWeights.alleles2[i]);
            if (!strandAmbiguous(sa1, sa2)) {
                if (isConsistent(sa1, pa1) && isConsistent(sa2, pa2) ||
                    isConsistent(sa1, complement(pa1)) && isConsistent(sa2, complement(pa2))) {
                    snpWeightMask[i] = 1;
                    plinkMask[plinkIndex] = 1;
                    nrIncluded++;
                } else if (isConsistent(sa1, pa2) && isConsistent(sa2, pa1) ||
                            isConsistent(sa1, complement(pa2)) && isConsistent(sa2, complement(pa1))) {
                    snpWeightMask[i] = 1;
                    plinkMask[plinkIndex] = 1;
                    flipMask[plinkIndex] = 1;
                    nrIncluded++;
                    nrToBeFlipped++;
                } else {
                    removedInconsistent++;
                }
            }
            else {
                removedStrandAmbiguous++;
            }
        }
        if(plinkIndex >= sampleBimData.snpIDs.length) {
            break;
        }
    }
    return { snpWeightMask, plinkMask, flipMask, removedStrandAmbiguous,
                removedInconsistent, nrIncluded, nrToBeFlipped };
}

function strandAmbiguous(a1, a2) {
    // Bug 1 fix: returns true when the pair IS ambiguous (A/T or C/G), false otherwise
    return (a1 + a2 === 'AT' ||
            a1 + a2 === 'TA' ||
            a1 + a2 === 'CG' ||
            a1 + a2 === 'GC');
}

function isConsistent(a1, a2) {
    return (  a1 === a2
//           || (a1 !== 'N' && a2 === 'N')
//           || (a1 === 'N' && a2 !== 'N')
            );
}

function complement(a) {
    switch (a) {
        case 'A':
            return 'T';
        case 'T':
            return 'A';
        case 'C':
            return 'G';
        case 'G':
            return 'C';
        default:
            return a; // Return the same character for non-ACGT bases
    }
}

export function reducePcWeightsImpl(snpWeights, overlap) {
    if (snpWeights.snpIDs.length == overlap.nrIncluded) {
        return snpWeights; // no reduction needed
    } else {
        let reducedIndex = 0;
        const pcWeights = new Float32Array(overlap.nrIncluded * snpWeights.numPCs);
        const frequencies = new Float32Array(overlap.nrIncluded);
        const snpIDs = new Array(overlap.nrIncluded);
        const chromosomes = new Uint8Array(overlap.nrIncluded);
        const positions = new Uint32Array(overlap.nrIncluded);
        for(let i = 0; i < snpWeights.snpIDs.length; i++) {
            if(overlap.snpWeightMask[i]) {
                for(let j = 0; j < snpWeights.numPCs; j++)
                    pcWeights[reducedIndex * snpWeights.numPCs + j] = snpWeights.pcWeights[i * snpWeights.numPCs + j];
                frequencies[reducedIndex] = snpWeights.frequencies[i];
                chromosomes[reducedIndex] = snpWeights.chromosomes[i];
                positions[reducedIndex] = snpWeights.positions[i];
                snpIDs[reducedIndex] = snpWeights.snpIDs[i];
                reducedIndex++;
            }
        }
        const ret = {pcWeights, frequencies, snpIDs, chromosomes, positions, numPCs: snpWeights.numPCs};
        console.log(`Reduced SNP weights: ${ret.snpIDs.length} SNPs, ${ret.numPCs} PCs`);
        return ret;
    }
}

export function extractAndTransposeGenotypesImpl(plinkBedDat, numSNPs, numInds, overlap) {
    const newGenotypeMatrix = new Uint8Array(numInds * overlap.nrIncluded); // we transpose the output
    let reducedIndex = 0;
    for(let i = 0; i < numSNPs; i++) {
        if(overlap.plinkMask[i]) {
            for(let j = 0; j < numInds; j++) {
                const srcGeno = plinkBedDat[i * numInds + j];
                const targetGeno = overlap.flipMask[i] ? flip(srcGeno) : srcGeno;
                newGenotypeMatrix[j * overlap.nrIncluded + reducedIndex] = targetGeno; //transpose
            }
            reducedIndex++;
        }
    }
    console.log(`Extracted and transposed genotypes: ${numInds} individuals, ${overlap.nrIncluded} SNPs`);
    return newGenotypeMatrix;
}

function flip(geno) {
    if(geno == 3) // missing
        return 3;
    else
        return 2 - geno;
}

export function projectSamplesImpl(transposedGenotypeMatrix, pcWeights, frequencies,
                        numInds, numPCs, { nScale, yScale, eigenValues }) {
    let ret = [];
    const numSNPs = frequencies.length;
    const aBuf = new Float64Array(pcWeights.length);
    const bBuf = new Float64Array(numSNPs);
    for (let i = 0; i < numInds; i++) {
        let reducedIndex = 0;
        for (let j = 0; j < numSNPs; j++) {
            const g = transposedGenotypeMatrix[i * numSNPs + j];
            const f = frequencies[j];
            if (g !== 3) { // not missing
                const gRef = 2 - g; //all allele frequencies in smartPCA are for the reference allele
                const fRef = 1 - f;
                const centeredGenoRef = gRef - 2 * fRef;
                bBuf[reducedIndex] = centeredGenoRef / Math.sqrt(fRef * (1 - fRef));
                for (let k = 0; k < numPCs; k++) {
                    aBuf[reducedIndex * numPCs + k] =
                        pcWeights[j * numPCs + k] /
                            Math.sqrt(nScale * eigenValues[k] * yScale);
                }
                reducedIndex++;
            }
        }
        const nonMissing = reducedIndex;
        const M = nonMissing;
        dgels('row-major', 'no-transpose', M, numPCs, 1, aBuf, numPCs, bBuf, 1);
        ret.push({
            pcCoordinates: Array.from(
                bBuf.subarray(0, numPCs).map((x, k) =>
                    x / (yScale * eigenValues[k]))),
            nonMissingCount: nonMissing
        });
    }
    console.log(`Projected ${numInds} samples onto ${numPCs} PCs`);
    return ret;
}

export function readRefPosData(content) {
    const lines = content.trim().split('\n');
    const numSamples = lines.length - 1;
    let samples = new Array(numSamples);
    const numFields = lines[1].trim().split(/\s+/).length;
    const numPCs = numFields - 3;
    if (numPCs < 1) {
        throw new Error(`Expected at least 4 columns per line (sampleID, PCs, popName and popGroup), but found ${numFields} in the first line.`);
    }
    for (let i = 0; i < numSamples; i++) {
        const fields = lines[i + 1].trim().split(/\s+/);
        if (fields.length !== numFields) {
            throw new Error(`Inconsistent number of columns in line ${i + 2}: expected ${numFields}, found ${fields.length}`);
        }
        samples[i] = {
            sampleID: fields[0],
            popName: fields[1],
            popGroup: fields[numFields - 1],
            pcValues: new Array(numPCs)
        };
        if (i < 10) {
            console.log(samples[i]);
        }
        for (let j = 0; j < numPCs; j++) {
            samples[i].pcValues[j] = parseFloat(fields[2 + j]);
            if (isNaN(samples[i].pcValues[j])) {
                throw new Error(`Invalid PC value for sample ${samples[i].sampleID} PC${j + 1}: ${fields[1 + j]}`);
            }
        }
    }
    console.log(`First sample: ${samples[0]}`);
    console.log(`First sample: ${samples[0].sampleID}, PCs: ${samples[0].pcValues.join(', ')}, popName: ${samples[0].popName}, popGroup: ${samples[0].popGroup}`);
    console.log(`Loaded ${numSamples} samples with ${numPCs} PCs from reference position file.`);
    return { samples, numSamples, numPCs };
}

export function readSnpWeights(snpWeightText) {
    const lines = snpWeightText.trim().split('\n');
    const numSNPs = lines.length;
    let snpIDs = new Array(numSNPs);
    const chromosomes = new Uint8Array(numSNPs);
    const positions = new Uint32Array(numSNPs);
    const alleles1 = new Uint8Array(numSNPs);
    const alleles2 = new Uint8Array(numSNPs);
    const firstLineFields = lines[0].trim().split(/\s+/);
    if (firstLineFields.length < 7) {
        throw new Error(`For SnpWeights expected at least 7 columns per line (snpIDs, chrom, pos, allele1, allele2, and at least one PC and one frequency), but found ${firstLineFields.length} in the first line.`);
    }
    const numPCs = firstLineFields.length - 6;
    const pcWeights = new Float32Array(numSNPs * numPCs);
    const frequencies = new Float32Array(numSNPs);
    for (let i = 0; i < numSNPs; i++) {
        const fields = lines[i].trim().split(/\s+/);
        snpIDs[i] = fields[0];
        chromosomes[i] = parseInt(fields[1]);
        positions[i] = parseInt(fields[2]);
        alleles1[i] = fields[3].charCodeAt(0);
        alleles2[i] = fields[4].charCodeAt(0);
        if (fields.length !== numPCs + 6) {
            throw new Error(`Inconsistent number of columns in line ${i + 1}:
                                expected ${numPCs + 6}, found ${fields.length}`);
        }
        for (let j = 0; j < numPCs; j++) {
            pcWeights[i * numPCs + j] = parseFloat(fields[5 + j]);
            if (isNaN(pcWeights[i * numPCs + j])) {
                throw new Error(`Invalid weight for SNP ${snpIDs[i]} PC${j + 1}: ${fields[5 + j]}`);
            }
        }
        frequencies[i] = parseFloat(fields[fields.length - 1]);
    }
    console.log(`Loaded ${numSNPs} SNPs with ${numPCs} PCs from weight file.`);
    return { snpIDs, chromosomes, positions, alleles1, alleles2, pcWeights, frequencies, numSNPs, numPCs };
}
