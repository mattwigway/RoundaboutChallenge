import('https://webr.r-wasm.org/v0.5.8/webr.mjs').then(
  async ({ WebR }) => {
    const webR = new WebR();
    await webR.init();

    await webR.installPackages(["ggplot2", "tibble", "ggforce", "svglite"])

    await webR.evalR("library(ggplot2)")
    await webR.evalR("library(tibble)")
    await webR.evalR("library(ggforce)")
    await webR.evalR("library(svglite)")

    const result = await fetch("R/draw_roundabout.R")
    const functions = await result.text()

    await webR.evalR(functions)

    async function update () {
        document.getElementById("calculating").style=""
        document.getElementById("outputs").style="display: none; visibility: hidden"

        const icd = Number(document.getElementById("icd").value)
        const island_pct = Number(document.getElementById("island-size").value)
        const island_prop = island_pct / 100
        const approach_radius = Number(document.getElementById("approach-radius").value)

        document.getElementById("icd-readout").textContent = "" + icd
        document.getElementById("island-size-readout").textContent = "" + island_pct
        document.getElementById("approach-radius-readout").textContent = "" + approach_radius

        const island_radius = icd / 2 * island_prop
        const roadway_width = icd / 2 - island_radius

        console.log({icd, island_prop, approach_radius, island_radius, roadway_width})

        await webR.objs.globalEnv.bind("island_radius", island_radius);
        await webR.objs.globalEnv.bind("approach_radius", approach_radius);
        await webR.objs.globalEnv.bind("roadway_width", roadway_width);
        await webR.evalR("results = draw_roundabout(island_radius, approach_radius, roadway_width)")

        // draw plot
        await webR.evalR("svglite('result.svg'); print(results$plot); dev.off()")

        const plotData = await webR.FS.readFile('/home/web_user/result.svg')
        const blob = new Blob([plotData], { type: 'image/svg+xml' })
        const url = URL.createObjectURL(blob)
        const cvs = document.getElementById("display")
        cvs.innerHTML = ""
        const img = document.createElement("img")
        img.src = url
        cvs.appendChild(img)

        const speedmph = await webR.evalRNumber("round(min(results$speeds$approach_speed, results$speeds$circ_speed) / 1.609)")
        document.getElementById("speed-mph").textContent = "" + speedmph
        document.getElementById("calculating").style="display: none; visibility: hidden"
        document.getElementById("outputs").style = ""
    }

    document.getElementById("icd").addEventListener("change", update);
    document.getElementById("approach-radius").addEventListener("change", update);
    document.getElementById("island-size").addEventListener("change", update);

    await update()

    document.getElementById("loading").remove()
    document.getElementById("app").style = ""
});