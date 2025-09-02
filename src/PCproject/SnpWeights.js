export function readSnpWeights(snpWeightText) {
    const lines = snpWeightText.trim().split('\n');
    const numSNPs = lines.length;
    let snpIds = new Array(numSNPs);
    let chromosomes = new Array(numSNPs);
    let positions = new Uint16Array(numSNPs);
    let alleles1 = new Array(numSNPs);
    let alleles2 = new Array(numSNPs);
    let firstLineFields = lines[0].trim().split(/\s+/);
    if (firstLineFields.length < 6) {
        throw new Error(`For SnpWeights expected at least 6 columns per line (snpIds, chrom, pos, allele1, allele2, and at least one PC), but found ${firstLineFields.length} in the first line.`);
    }
    let numPCs = firstLineFields.length - 5;
    let pcWeights = new Float32Array(numSNPs * numPCs);
    for (let i = 0; i < numSNPs; i++) {
        const fields = lines[i].trim().split(/\s+/);
        snpIds[i] = fields[0];
        chromosomes[i] = fields[1];
        positions[i] = parseInt(fields[2]);
        if (isNaN(positions[i])) {
            throw new Error(`Invalid position for SNP ${snpIds[i]}: ${fields[2]}`);
        }
        alleles1[i] = fields[3];
        alleles2[i] = fields[4];
        if (fields.length !== numPCs + 5) {
            throw new Error(`Inconsistent number of columns in line ${i + 1}: expected ${numPCs + 5}, found ${fields.length}`);
        }
        for (let j = 0; j < numPCs; j++) {
            pcWeights[i * numPCs + j] = parseFloat(fields[5 + j]);
            if (isNaN(pcWeights[i * numPCs + j])) {
                throw new Error(`Invalid weight for SNP ${snpIds[i]} PC${j + 1}: ${fields[5 + j]}`);    
            }
        }
    }
    console.log(`Loaded ${numSNPs} SNPs with ${numPCs} PCs from weight file.`);
    return { snpIds, chromosomes, positions, alleles1, alleles2, pcWeights, numSNPs, numPCs };
}