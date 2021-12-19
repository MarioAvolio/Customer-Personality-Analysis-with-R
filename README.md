<div id="top"></div>
<div id="top"></div>
<!--
*** Thanks for checking out the Best-README-Template. If you have a suggestion
*** that would make this better, please fork the repo and create a pull request
*** or simply open an issue with the tag "enhancement".
*** Don't forget to give the project a star!
*** Thanks again! Now go create something AMAZING! :D
-->



<!-- PROJECT SHIELDS -->
<!--
*** I'm using markdown "reference style" links for readability.
*** Reference links are enclosed in brackets [ ] instead of parentheses ( ).
*** See the bottom of this document for the declaration of the reference variables
*** for contributors-url, forks-url, etc. This is an optional, concise syntax you may use.
*** https://www.markdownguide.org/basic-syntax/#reference-style-links
-->
[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]



<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/MarioAvolio/Customer-Personality-Analysis-with-R">
    <img src="images/logo.png" alt="Logo" width="80" height="80">
  </a>

<h3 align="center">Customer Personality Analysis</h3>

  <p align="center">
    with R Program Language
    <br />
    <a href="https://github.com/MarioAvolio/Customer-Personality-Analysis-with-R"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/MarioAvolio/Customer-Personality-Analysis-with-R">View Demo</a>
    ·
    <a href="https://github.com/MarioAvolio/Customer-Personality-Analysis-with-R/issues">Report Bug</a>
    ·
    <a href="https://github.com/MarioAvolio/Customer-Personality-Analysis-with-R/issues">Request Feature</a>
  </p>
</div>



<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project

[![Product Name Screen Shot][product-screenshot]](https://www.kaggle.com/imakash3011/customer-personality-analysis)

### Context
#### Problem Statement
Customer Personality Analysis is a detailed analysis of a company’s ideal customers. It helps a business to better understand its customers and makes it easier for them to modify products according to the specific needs, behaviors and concerns of different types of customers.

Customer personality analysis helps a business to modify its product based on its target customers from different types of customer segments. For example, instead of spending money to market a new product to every customer in the company’s database, a company can analyze which customer segment is most likely to buy the product and then market the product only on that particular segment.
### Content
#### Attributes
##### People

- ID: Customer's unique identifier
- Year_Birth: Customer's birth year
- Education: Customer's education level
- Marital_Status: Customer's marital status
- Income: Customer's yearly household income
- Kidhome: Number of children in customer's household
- Teenhome: Number of teenagers in customer's household
- Dt_Customer: Date of customer's enrollment with the company
- Recency: Number of days since customer's last purchase
- Complain: 1 if the customer complained in the last 2 years, 0 otherwise

##### Products
- MntWines: Amount spent on wine in last 2 years
- MntFruits: Amount spent on fruits in last 2 years
- MntMeatProducts: Amount spent on meat in last 2 years
- MntFishProducts: Amount spent on fish in last 2 years
- MntSweetProducts: Amount spent on sweets in last 2 years
- MntGoldProds: Amount spent on gold in last 2 years

##### Promotion
- NumDealsPurchases: Number of purchases made with a discount
- AcceptedCmp1: 1 if customer accepted the offer in the 1st campaign, 0 otherwise
- AcceptedCmp2: 1 if customer accepted the offer in the 2nd campaign, 0 otherwise
- AcceptedCmp3: 1 if customer accepted the offer in the 3rd campaign, 0 otherwise
- AcceptedCmp4: 1 if customer accepted the offer in the 4th campaign, 0 otherwise
- AcceptedCmp5: 1 if customer accepted the offer in the 5th campaign, 0 otherwise
 - Response: 1 if customer accepted the offer in the last campaign, 0 otherwise
##### Place
- NumWebPurchases: Number of purchases made through the company’s website
- NumCatalogPurchases: Number of purchases made using a catalogue
- NumStorePurchases: Number of purchases made directly in stores
- NumWebVisitsMonth: Number of visits to company’s website in the last month

### Target
Need to perform clustering to summarize customer segments.



<p align="right">(<a href="#top">back to top</a>)</p>


### Built With

* [R](https://www.r-project.org/)
<p align="right">(<a href="#top">back to top</a>)</p>



<!-- GETTING STARTED -->
## Getting Started

This is an example of how you may give instructions on setting up your project locally.
To get a local copy up and running follow these simple example steps.

### Prerequisites
#### Tools
- R
- R Studio

#### Library
- caTools
```
install.packages('caTools')
```
- niniar
```
install.packages("naniar")
```
- lubridate
```
install.packages("lubridate")
```
- ggplot2
```
install.packages("ggplot2")
```
- ggcorrplot
```
install.packages("ggcorrplot")
```


### Installation
```sh
git clone https://github.com/MarioAvolio/Customer-Personality-Analysis-with-R.git
```
<p align="right">(<a href="#top">back to top</a>)</p>



<!-- USAGE EXAMPLES -->
## Usage
TODO

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- ROADMAP -->
## Roadmap

- [X] Importing Data 
- [X] EDA 
- [X] Pre-Processing Data 
- [X] PCA 
- [X] Analysis Algorithm (K-Means)

See the [open issues](https://github.com/MarioAvolio/Customer-Personality-Analysis-with-R/issues) for a full list of proposed features (and known issues).

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

If you have a suggestion that would make this better, please fork the repo and create a pull request. You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE` for more information.

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- CONTACT -->
## Contact

Project Link: [https://github.com/MarioAvolio/Customer-Personality-Analysis-with-R](https://github.com/MarioAvolio/Customer-Personality-Analysis-with-R)

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- ACKNOWLEDGMENTS -->
## Acknowledgments
The dataset for this project is provided by Dr. Omar Romero-Hernandez. 
* [Dataset](https://www.kaggle.com/imakash3011/customer-personality-analysis)

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/MarioAvolio/Customer-Personality-Analysis-with-R.svg?style=for-the-badge
[contributors-url]: https://github.com/MarioAvolio/Customer-Personality-Analysis-with-R/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/MarioAvolio/Customer-Personality-Analysis-with-R.svg?style=for-the-badge
[forks-url]: https://github.com/MarioAvolio/Customer-Personality-Analysis-with-R/network/members
[stars-shield]: https://img.shields.io/github/stars/MarioAvolio/Customer-Personality-Analysis-with-R.svg?style=for-the-badge
[stars-url]: https://github.com/MarioAvolio/Customer-Personality-Analysis-with-R/stargazers
[issues-shield]: https://img.shields.io/github/issues/MarioAvolio/Customer-Personality-Analysis-with-R.svg?style=for-the-badge
[issues-url]: https://github.com/MarioAvolio/Customer-Personality-Analysis-with-R/issues
[license-shield]: https://img.shields.io/github/license/MarioAvolio/Customer-Personality-Analysis-with-R.svg?style=for-the-badge
[license-url]: https://github.com/MarioAvolio/Customer-Personality-Analysis-with-R/blob/master/LICENSE
[product-screenshot]: images/screenshot.png
