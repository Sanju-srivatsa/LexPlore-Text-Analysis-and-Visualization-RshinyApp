# Lexplore: Your Text Analysis Companion

**Lexplore** is a web-based application designed to analyze and visualize text data interactively. With a user-friendly interface and dynamic visualizations, Lexplore is perfect for exploring patterns, trends, and insights in any written content, whether it's literature, feedback, or any text dataset.

---
## Why "Lexplore"?

The name **Lexplore** is derived from:
- **"Lex"**, meaning vocabulary or words.
- **"Explore"**, emphasizing the discovery of insights from text.

This app combines the power of text processing with visually engaging graphs, enabling users to better understand their data.

---
## About the App

Lexplore is built using R and Shiny, combining robust natural language processing (NLP) techniques with powerful visualization tools. The app is designed to be user-friendly, enabling both novice and experienced users to dive into text data analysis without requiring extensive programming knowledge.

With Lexplore, you can:
- Tokenize and process text data.
- Perform word frequency analysis to identify the most common words.
- Conduct sentiment analysis to understand the emotional tone of the text.
- Visualize insights using dynamic bar charts, word clouds, and interactive tables.
- Customize visualizations to suit your preferences.

The app supports multiple text input methods, allowing users to upload files, paste custom text, or analyze a default poem included with the app.

---
## 🌟 Features

### 1. **Flexible Text Input Options**
- **Upload a File:** Analyze any `.txt` file containing text data.
- **Paste Custom Text:** Copy and paste text directly into the app for instant analysis.
- **Default Poem Analysis:** Start exploring immediately with the built-in default poem, *The Tyger* by William Blake.

### 2. **Dynamic and Interactive Visualizations**
- **Word Frequency Analysis:** 
  - Displays the top N most frequent words.
  - Visualized as a bar chart with frequency labels for clarity.
- **Sentiment Analysis:**
  - Categorizes words into **positive** and **negative** sentiments.
  - Visualized with colorful bar charts to quickly identify emotional tone.
- **Word Cloud:** 
  - Highlights prominent words in a visually engaging format.
  - Adjustable word count to customize the cloud.
- **Interactive Keyword Table:**
  - View and sort words along with their frequencies in a searchable, paginated table.

### 3. **Customization Options**
- Adjust the number of words to display in visualizations (e.g., Top 10, Top 20, Top 50).
- Easily switch between different text inputs (uploaded files, pasted text, or default poem).

---

## 🎯 Purpose of the App

Lexplore is built to simplify text analysis and visualization for a wide range of users. Whether you’re a student analyzing literature, a researcher exploring textual datasets, or a business professional extracting insights from customer feedback, Lexplore helps you:

- **Understand Word Patterns:** Identify the most common words in your text.
- **Discover Emotional Tone:** Use sentiment analysis to gauge positivity or negativity.
- **Visualize Data Creatively:** Present text data in a visually appealing and insightful manner.
- **Gain Insights Quickly:** Save time by leveraging dynamic and automated text analysis.

---

## 🚀 How to Use the App

### 1. **Launch the App**
Access the app online via the following link:  
👉 [Lexplore on ShinyApps.io](https://sanjusrivatsa.shinyapps.io/Lexplore/)

### 2. **Input Your Text**
- **Option 1:** Upload a `.txt` file from your computer.
- **Option 2:** Paste custom text into the text input box.
- **Option 3:** Use the default poem by clicking the "Load Poem" button.

### 3. **Set Parameters**
- Adjust the slider to select the top N words to display in the analysis.

### 4. **Analyze**
- Click the "Analyze" button to generate insights.

### 5. **Explore Results**
- Navigate through the tabs:
  - **Word Frequency:** View the top words and their frequencies.
  - **Sentiment Analysis:** Understand the emotional tone of the text.
  - **Word Cloud:** See prominent words visualized in a creative format.
  - **Top Keywords:** Explore a detailed, interactive table of the top keywords.

---

## 🎨 Visual Highlights

### Word Frequency Chart
Displays the top N words in a bar chart format, with frequency labels for clarity.
<img width="945" alt="image" src="https://github.com/user-attachments/assets/e3e2cf62-5948-43c4-ba73-305ee66fff30" />

### Sentiment Analysis Chart
Visualizes the emotional tone of the text, categorized into positive and negative sentiments using vibrant colors.
<img width="952" alt="image" src="https://github.com/user-attachments/assets/8a55ad2d-19fb-4d49-b17f-273640d64faa" />

### Word Cloud
Generates an engaging cloud of the most frequently occurring words, with customizable word count.
<img width="946" alt="image" src="https://github.com/user-attachments/assets/c3da0f4e-c871-4d3b-9cc5-8fa964ffc37e" />

### Interactive Table
Offers a sortable and searchable table of the top keywords with their frequencies.
<img width="958" alt="image" src="https://github.com/user-attachments/assets/63f68a18-aceb-42bb-8cf4-8943302419d0" />


---

## 🛠️ Technical Details

### Built With:
- **R** and **Shiny** for the interactive app framework.
- **tidytext** and **tidyverse** for text processing and data manipulation.
- **ggplot2** for visualizations.
- **wordcloud** and **RColorBrewer** for creative word clouds.
- **DT** for interactive tables.

---

## 💡 Future Enhancements
- Support for additional file types (e.g., `.docx` and `.csv`).
- Export options for visualizations and analysis results.
- Advanced NLP techniques like topic modeling and named entity recognition.
- Multi-language support for text analysis.

---

## 📜 License
This project is licensed under the MIT License. Feel free to use and adapt the app for your needs.

---

## 📝 Acknowledgments
- Default text (*The Tyger* by William Blake) is in the public domain.
- This app leverages the power of R and Shiny to make text analysis accessible to everyone.

---

Explore your text like never before with **Lexplore**! 🌟  
[Launch the App](https://sanjusrivatsa.shinyapps.io/Lexplore/)

---

This README file provides a comprehensive overview of your app while maintaining simplicity and readability. Let me know if there’s anything else you’d like to include! 😊
