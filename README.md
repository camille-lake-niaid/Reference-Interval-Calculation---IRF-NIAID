# Reference-Interval-Calculation---IRF-NIAID
A public repository for the project related to the publication: Calculating Veterinary Clinical Pathology Reference Intervals for Research at the Integrated Research Facility, NIAID: A Practical Application of Data Partitioning and Statistical Methodology.

Below is a general schema for signalment partitioning used in this project:

![image](https://github.com/user-attachments/assets/572b4342-388b-4f30-a413-8e6aa8ec3bfa)

Below is the general data flow and statistical calculations associated with the project:

![image](https://github.com/user-attachments/assets/53ee6e57-8228-438c-a7be-4b8a762bff81)

Files found in this repository:
1. Reference Ranges Outlier Detection: This quarto markdown document produces outliers, associated graphics, and identifying information for groups of analytes for the Reference Range Calculation project with the Integrated Research Facility.
2. Reference Ranges Statistical Outcomes: This R script intakes data and outputs reference ranges per group. Two separate excel files are produced: (1) Data Entry Component data frame - for inputting the reference ranges into the Study Data Browser using the custom-built data entry component, and (2) Comprehensive data frame - include additional information requested by the customers like confidence intervals around the reference ranges and summary statistics for groups where reference range calculation is not appropriate.
3. Rule and Statistical Logic: This excel file documents the specific rule-based and statistical logic used in this project to handle specific scenarios encountered over the course of development. 

A comprehensive set of reference ranges and summary statistics can be found in the supplemental material of the manuscript, here: []. 

