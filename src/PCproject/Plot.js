import * as Plot from "@observablehq/plot";

export const drawChart = refPosDat => pc1index => pc2index => () => {
    const xcol = `PC${pc1index + 1}`;
    const ycol = `PC${pc2index + 1}`;
    let plotDat = Array.from({length: refPosDat.numSamples}, (_, i) => ({
        [xcol]: refPosDat.pcValues[i * refPosDat.numPCs + pc1index],
        [ycol]: refPosDat.pcValues[i * refPosDat.numPCs + pc2index],
        pop: refPosDat.popNames[i],
        id: refPosDat.sampleIDs[i]
    }));
    const chart = Plot.plot({
        marks: [
            Plot.dot(plotDat, {x: xcol, y: ycol, fill: "pop", title: d => d.id, r: 3}),
        ]
    });
    const container = document.getElementById("chart-container");
    if (container) {
        container.innerHTML = ""; // Clear previous content
        container.appendChild(chart);
    } else {
        console.error("Container element not found");
    }   
}