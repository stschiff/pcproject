export function readRefPosData(content) {
    const lines = content.trim().split('\n');
    const numSamples = lines.length;
    let samples = new Array(numSamples);
    const numFields = lines[0].trim().split(/\s+/).length;
    const numPCs = numFields - 3;
    if (numPCs < 1) {
        throw new Error(`Expected at least 4 columns per line (sampleID, PCs, popName and popGroup), but found ${numFields} in the first line.`);
    }
    for (let i = 0; i < numSamples; i++) {
        const fields = lines[i].trim().split(/\s+/);
        if (fields.length !== numFields) {
            throw new Error(`Inconsistent number of columns in line ${i + 1}: expected ${numFields}, found ${fields.length}`);
        }
        samples[i] = {
            sampleID: fields[0],
            popName: fields[numFields - 2],
            popGroup: fields[numFields - 1],
            pcValues: new Array(numPCs)
        };
        for (let j = 0; j < numPCs; j++) {
            samples[i].pcValues[j] = parseFloat(fields[1 + j]);
            if (isNaN(samples[i].pcValues[j])) {
                throw new Error(`Invalid PC value for sample ${samples[i].sampleID} PC${j + 1}: ${fields[1 + j]}`);    
            }
        }
    }
    console.log(`Loaded ${numSamples} samples with ${numPCs} PCs from reference position file.`);
    return { samples, numSamples, numPCs };
}
    