import * as Plot from "@observablehq/plot";

export const drawChart = () => {
    let x = [1, 2, 3];
    let y = [4, 2, 5];
    let data = x.map((xi, i) => ({x: xi, y: y[i]}));

    const chart = Plot.plot({
        marks: [
            Plot.dot(data, {x: "x", y: "y"})
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