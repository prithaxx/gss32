console.log('IN HERE')

async function captureAndSave(selector = "chart-area", filename = "chart_screenshot.png") {
    console.log('IN captureAndSave');

    const element = document.querySelector(selector);
    if (!element) {
        console.error("Element not found:", selector);
        return;
    }

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
    captureAndSave(message.selector || "chart-area");
  }
);
