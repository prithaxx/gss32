console.log('IN HERE')

async function captureAndSave(selector = ".chart-container", filename = "chart_screenshot.png") {
    console.log('IN captureAndSave, selector:', selector);

    const element = document.querySelector(selector);
    if (!element) {
        console.error("Element not found:", selector);
        return;
    }

    // Wait until Shiny fully renders the chart
    await new Promise(resolve => {
        const observer = new MutationObserver((mutations, obs) => {
            if (element.querySelector("canvas, svg, img")) { // Ensure a renderable element exists
                obs.disconnect();
                resolve();
            }
        });
        observer.observe(element, { childList: true, subtree: true });

        // Timeout fallback (5s max wait)
        setTimeout(() => {
            observer.disconnect();
            resolve();
        }, 5000);
    });

    try {
        const canvas = await html2canvas(element, { scale: 2 });
        const imgData = canvas.toDataURL("image/png");

        // Create a download link
        const link = document.createElement("a");
        link.href = imgData;
        link.download = filename;
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
    } catch (error) {
        console.error("Error capturing chart:", error);
    }
}


Shiny.addCustomMessageHandler("captureChart",
  function(message) {
    console.log("Capturing chart with selector: ", message.selector);
    captureAndSave(message.selector || ".chart-container");
  }
);
