import dgels from '@rreusser/blapack/lapack/base/dgels';

function readBimData(bimText: string): BimData {
    const lines = bimText.trim().split('\n');
    const nrSNPs = lines.length;
    let chromosomes = new Uint8Array(nrSNPs);
    let positions = new Uint32Array(nrSNPs);
    let snpIDs = new Array(nrSNPs);
    let alleles1 = new Uint8Array(nrSNPs);
    let alleles2 = new Uint8Array(nrSNPs);
    for (let i = 0; i < nrSNPs; i++) {
        const fields = lines[i].trim().split(/\s+/);
        chromosomes[i] = parseInt(fields[0]);
        if (isNaN(chromosomes[i]) || chromosomes[i] < 1 || chromosomes[i] > 25) {
            throw new Error(`Invalid chromosome for SNP ${snpIDs[i]}: ${fields[0]}`);
        }
        snpIDs[i] = fields[1];
        positions[i] = parseInt(fields[3]);
        if (isNaN(positions[i])) {
            throw new Error(`Invalid position for SNP ${snpIDs[i]}: ${fields[3]}`);
        }
        alleles1[i] = fields[4].charCodeAt(0);
        alleles2[i] = fields[5].charCodeAt(0);
    }
    console.log(`Loaded ${nrSNPs} SNPs from BIM file.`);
    return { snpIDs, chromosomes, positions, alleles1, alleles2 };
}

function readFamData(famText: string): FamData {
    const lines = famText.trim().split('\n');
    const nrSamples = lines.length;
    let popNames = new Array(nrSamples);
    let indNames = new Array(nrSamples);
    for (let i = 0; i < nrSamples; i++) {
        const fields = lines[i].trim().split(/\s+/);
        popNames[i] = fields[0];
        indNames[i] = fields[1];
    }
    console.log(`Loaded ${nrSamples} individuals from FAM file.`);
    return { indNames, popNames };
}
  
function readBedData(bedArrayBuffer: ArrayBuffer, numSnps: number, numInds: number): Uint8Array {
    const bytes = new Uint8Array(bedArrayBuffer);
    if (bytes.length < 3 || bytes[0] !== 0b01101100 || bytes[1] !== 0b00011011 || bytes[2] !== 0b00000001) {
        throw new Error("Invalid .bed file: incorrect magic numbers");
    }
    let returnArray = new Uint8Array(numSnps * numInds);
    let blockSize = Math.ceil(numInds / 4);
    for (let i = 0; i < numSnps; i++) {
        for (let j = 0; j < numInds; j++) {
            const byteIndex = 3 + i * blockSize + Math.floor(j / 4);
            const bitOffset = (j % 4) * 2;
            const genotypeBits = (bytes[byteIndex] >> bitOffset) & 0b11;
            switch (genotypeBits) {
                case 0b00:
                    returnArray[i * numInds + j] = 0; break; // Homozygous reference
                case 0b10:
                    returnArray[i * numInds + j] = 1; break; // Heterozygous
                case 0b11:
                    returnArray[i * numInds + j] = 2; break; // Homozygous alternate
                case 0b01:
                    returnArray[i * numInds + j] = 3; break; // Missing genotype
            }
        }
    }
    console.log(`Loaded ${numSnps * numInds} genotypes from BED file.`);
    return returnArray;
}

function readSnpWeights(snpWeightText: string): SnpWeights {
    const lines = snpWeightText.trim().split('\n');
    const numSNPs = lines.length;
    let snpIDs = new Array(numSNPs);
    const chromosomes = new Uint8Array(numSNPs);
    const positions = new Uint32Array(numSNPs);
    const alleles1 = new Uint8Array(numSNPs);
    const alleles2 = new Uint8Array(numSNPs);
    const frequencies = new Float32Array(numSNPs);
    let firstLineFields = lines[0].trim().split(/\s+/);
    if (firstLineFields.length < 7) {
        throw new Error(`For SnpWeights expected at least 7 columns per line (snpIDs, chroms, pos, allele1, allele2, at least one PC and a frequency), but found ${firstLineFields.length} in the first line.`);
    }
    let numPCs = firstLineFields.length - 6; // subtracting 6 for snpID, chrom, pos, allele1, allele2, frequency
    let pcWeights = new Float32Array(numSNPs * numPCs);
    for (let i = 0; i < numSNPs; i++) {
        const fields = lines[i].trim().split(/\s+/);
        snpIDs[i] = fields[0];
        chromosomes[i] = parseInt(fields[1]);
        positions[i] = parseInt(fields[2]);
        alleles1[i] = fields[3].charCodeAt(0);
        alleles2[i] = fields[4].charCodeAt(0);
        frequencies[i] = parseFloat(fields[fields.length - 1]);
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
    }
    console.log(`Loaded ${numSNPs} SNPs with ${numPCs} PCs from weight file.`);
    return { snpIDs, chromosomes, positions, alleles1, alleles2, pcWeights, numPCs, frequencies };
}

function getOverlapMasks(sampleBimData: BimData, snpWeights: SnpWeights): OverlapMasks {
    let snpWeightMask = new Uint8Array(snpWeights.snpIDs.length);
    let plinkMask = new Uint8Array(sampleBimData.snpIDs.length);
    let flipMask = new Uint8Array(sampleBimData.snpIDs.length);
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

function strandAmbiguous(a1: string, a2: string): boolean {
    // Bug 1 fix: returns true when the pair IS ambiguous (A/T or C/G), false otherwise
    return (a1 + a2 === 'AT' ||
            a1 + a2 === 'TA' ||
            a1 + a2 === 'CG' ||
            a1 + a2 === 'GC');
}

function isConsistent(a1: string, a2: string): boolean {
    return (  a1 === a2
//           || (a1 !== 'N' && a2 === 'N')
//           || (a1 === 'N' && a2 !== 'N')
            );
}

function complement(a: string): string {
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

function reducePcWeights(snpWeights: SnpWeights, overlap: OverlapMasks): SnpWeights {
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
        return {pcWeights, frequencies, snpIDs, chromosomes, positions, numPCs: snpWeights.numPCs};
    }
}

function extractAndTransposeGenotypes(plinkBedDat: Uint8Array, numSNPs: number, numInds: number, overlap: OverlapMasks): Uint8Array {
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
    return newGenotypeMatrix;
}

function flip(geno: number): number {
    if(geno == 3) // missing
        return 3;
    else
        return 2 - geno;
}

function projectSamples(
        transposedGenotypeMatrix: Uint8Array,
        pcWeights: Float32Array,
        frequencies: Float32Array,
        numInds: number,
        numPCs: number,
        nScale: number, // this is the number of SNPs in the PCA, not the reduced number in the weight file here.
        yScale: number,
        eigenValues: number[]    
    ): ProjectionResult[] {
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
    return ret;
}

export { readBimData, readFamData, readBedData, readSnpWeights, getOverlapMasks, reducePcWeights,
       extractAndTransposeGenotypes, projectSamples, dgels };