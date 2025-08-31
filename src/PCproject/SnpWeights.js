export function readSnpWeights(snpWeightText) {
    const lines = snpWeightText.trim().split('\n');
    const numSnps = lines.length;
    let snpIds = new Array(nrSnps);
    let chromosomes = new Array(nrSnps);
    let positions = new Uint16Array(nrSnps);
    let alleles1 = new Array(nrSnps);
    let alleles2 = new Array(nrSnps);
    let firstLineFields = lines[0].trim().split(/\s+/);
    if (firstLineFields.length < 6) {
        throw new Error(`For SnpWeights expected at least 6 columns per line (snpIds, chrom, pos, allele1, allele2, and at least one PC), but found ${firstLineFields.length} in the first line.`);
    }
    let numPcs = firstLineFields.length - 5;
    let pcWeights = new Float32Array(nrSnps * numPcs);
    for (let i = 0; i < nrSnps; i++) {
        const fields = lines[i].trim().split(/\s+/);
        snpIds[i] = fields[0];
        chromosomes[i] = fields[1];
        positions[i] = parseInt(fields[2]);
        if (isNaN(positions[i])) {
            throw new Error(`Invalid position for SNP ${snpIds[i]}: ${fields[2]}`);
        }
        alleles1[i] = fields[3];
        alleles2[i] = fields[4];
        if (fields.length !== numPcs + 5) {
            throw new Error(`Inconsistent number of columns in line ${i + 1}: expected ${numPcs + 5}, found ${fields.length}`);
        }
        for (let j = 0; j < numPcs; j++) {
            pcWeights[i * numPcs + j] = parseFloat(fields[5 + j]);
            if (isNaN(weights[i * numPcs + j])) {
                throw new Error(`Invalid weight for SNP ${snpIds[i]} PC${j + 1}: ${fields[5 + j]}`);    
            }
        }
    }
    return { snpIds, chromosomes, positions, alleles1, alleles2, pcWeights, numSnps, numPcs };
}