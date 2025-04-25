# Interactive Data Dashboards

## Project Overview

**Interactive Data Dashboards** is a project designed to explore the 2018 *General Social Survey on Caregiving and Care Receiving* from Statistics Canada. The dashboard focuses on a subset of the dataset, specifically responses from:

- **Care receivers** who are 65 years old or older
- **Caregivers** who provide assistance to individuals aged 65 or older

This demographic offers valuable insights into the experiences and challenges associated with providing or receiving care for older adults. The dataset includes information related to health, financial situations, quality of life, and other caregiving-relevant factors.

---

## Dashboard Sections

The dashboard is divided into three key sections, each providing unique perspectives on caregiving and care receiving:

1. **General Charts**  : Offers an overview and summary of the entire dataset.

2. **Receiver Response Charts**  : Focuses on data collected from care receivers aged 65 or older.

3. **Giver Response Charts**  : Presents data from caregivers who assist elderly individuals.

---

## Data Views

The dashboard supports three different data visualization formats:

- **Counts**  : Frequency of responses by group.

- **Percentages**  : Proportions of responses within groups.

- **Tables**  : Tabular format showing both frequency and proportions for groups.

---

## Key Features

### 1. Filtering
- Filters are available for both Receiver and Giver Response Charts to allow in-depth exploration of specific criteria.
- Applied filters are displayed under each chart for clarity.
- A **Reset** button restores charts to their original unfiltered state.

### 2. Group By
- Charts can be grouped by:
  - Sex
  - Age group
  - Alzheimer’s/ Dementia status
- Grouped data includes a filtered population count shown below each chart.

### 3. Sharing Data Vignettes
- Users can share interesting observations found while exploring the dashboard.
- By clicking **Share**, users can:
  - Add a short description of their insight
  - Optionally tag their vignette
- Shared charts are saved under the **Data Vignettes** section with their descriptions and tags.

---

## Future Features (Planned)

### 1. Preview of Shared Charts
- Users should be able to see a preview of chart tags and descriptions before opening.
- This feature was initiated but not completed due to technical complexity and limited resources.
- Technologies like **Puppeteer** or **html2canvas** could support this feature.

### 2. Sorting by Tags
- Allow filtering and sorting of shared vignettes based on their tags for better usability and organization.

### 3. Group By in General Charts
- Implement groupings like **caree**, **carer**, and **none** in General Charts for more detailed exploration.

### 4. UX Improvements (Nitpicks)
- Set appropriate character limits for custom tags.
- Increase size of the text area for vignette descriptions.

---

## Known Bugs

- The **filter by age of primary caree** in Giver Response Charts currently does not yield accurate results.

---

## Recommendations for Future Development

- **Refactor the `app.R` code** into modular files to reduce technical debt and improve maintainability:
  - Separate client and server logic
  - Modularize UI components and chart logic
- Consider having **two developers** on the project:
  - One focused on code refactoring and tech debt
  - The other focused on feature development

---

