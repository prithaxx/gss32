const puppeteer = require('puppeteer');

async function captureChart(url, outputPath) {
    const browser = await puppeteer.launch();
    const page = await browser.newPage();

    await page.goto(url, { waitUntil: 'networkidle2' });
    await page.waitForTimeout(5000); 

    const chartElement = await page.$('#shiny-plot-output'); // Replace with actual chart div ID

    if (chartElement) {
        await chartElement.screenshot({ path: outputPath });
    } else {
        console.log("Chart not found! Taking full-page screenshot instead.");
        await page.screenshot({ path: outputPath });
    }

    await browser.close();
}

// Get arguments from R script (URL and output path)
const url = process.argv[2];
const outputPath = process.argv[3];

captureChart(url, outputPath);
