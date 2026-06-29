export function readBimData(bimText) {
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

export function readFamData(famText) {
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

export function readBedData(bedArrayBuffer, numSnps, numInds) {
    const bytes = new Uint8Array(bedArrayBuffer);
    if (bytes.length < 3 || bytes[0] !== 0b01101100 || bytes[1] !== 0b00011011 || bytes[2] !== 0b00000001) {
        throw new Error("Invalid .bed file: incorrect magic numbers");
    }
    let returnArray = new Float32Array(numSnps * numInds);
    let blockSize = Math.ceil(numInds / 4);
    let genotypeData = new Uint8Array(numSnps * blockSize);
    for (let i = 0; i < numSnps; i++) {
        for (let j = 0; j < numInds; j++) {
            const byteIndex = 3 + i * blockSize + Math.floor(j / 4);
            const bitOffset = (j % 4) * 2;
            const genotypeBits = (bytes[byteIndex] >> bitOffset) & 0b11;
            switch (genotypeBits) {
                case 0b00:
                    returnArray[i * numInds + j] = 0; // Homozygous reference
                case 0b10:
                    returnArray[i * numInds + j] = 0.5; // Heterozygous
                case 0b11:
                    returnArray[i * numInds + j] = 1; // Homozygous alternate
                case 0b01:
                    returnArray[i * numInds + j] = null; // Missing genotype
            }
        }
    }
    return returnArray;
}