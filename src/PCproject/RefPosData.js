export function readRefPosData(content) {
    const lines = content.trim().split('\n');
    const numSamples = lines.length;
    let sampleIDs = new Array(numSamples);
    const numFields = lines[0].trim().split(/\s+/).length;
    const numPCs = numFields - 2;
    let popNames = new Array(numSamples);
    let pcValues = new Float32Array(numSamples * numPCs);
    if (numPCs < 1) {
        throw new Error(`Expected at least 3 columns per line (sampleID, PCs and a popname), but found ${numFields} in the first line.`);
    }
    for (let i = 0; i < numSamples; i++) {
        const fields = lines[i].trim().split(/\s+/);
        if (fields.length !== numFields) {
            throw new Error(`Inconsistent number of columns in line ${i + 1}: expected ${numFields}, found ${fields.length}`);
        }
        sampleIDs[i] = fields[0];
        for (let j = 0; j < numPCs; j++) {
            pcValues[i * numPCs + j] = parseFloat(fields[1 + j]);
            if (isNaN(pcValues[i * numPCs + j])) {
                throw new Error(`Invalid PC value for sample ${sampleIDs[i]} PC${j + 1}: ${fields[1 + j]}`);    
            }
        }
        popNames[i] = fields[numFields - 1];
    }
    console.log(`Loaded ${numSamples} samples with ${numPCs} PCs from reference position file.`);
    return { sampleIDs, popNames, pcValues, numSamples, numPCs };
}
    