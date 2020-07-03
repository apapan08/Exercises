Statisitcal Programming - Sem 1, 2019 - Project 1
-----------

Due Monday, October 28th by 11:59 pm.

## Rules

1. Your solutions must be written up using the provided R Markdown file (`proj1.Rmd)`, this file must include your code and write up for each task.

2. This project is open book, open internet, closed other people. You may use *any* online or book based resource you would like, but you must include citations for any code that you use (directly or indirectly). You *may not* consult with anyone else about this exam other than the Lecturers or Tutors for this course - this includes posting anything online. You may post questions on Piazza, any general question is fine to be posted publicly - any question containing your code should be posted privately.

3. You have until Monday, October 28th at 11:59 pm to complete this project and turn it in via your personal Github repo - late work will be subject to the standard university late policy. Technical difficulties are not an excuse for late work - do not wait until the last minute to commit / push.

4. All of your answers *must* include a brief description / writeup of your approach. This includes both annotating / commenting your code *and* a separate written descriptions of all code / implementations. I should be able to suppress *all* code output in your document and still be able to read and make sense of your answers.

5. You may use any packages you like unless otherwise specified for a particular task.

6. The most important goal is to write code that can accomplish the given tasks, however grading will also be based on the quality of the code you write - elegant, efficient code will be given better marks and messy, slow code will be penalized.

<br />

## Data

For this exam you will be working with a data from the 2018 Formula 1 season. The data was downloaded from ergast.com in the form of a single large JSON file which contains information on the results of all 21 races from the 2018 season. Your repo should contain both the original json file (`f1_2018.json`) as well as an RDS binary file (`f1_2018.rds`) which can be read in using

```r
f1 = readRDS(file="data/f1_2018.rds")
```

The data is structured as a list of lists of lists of lists and so on, it is up to you to look at the data and figure out how it is structured and how best to get at the information you want.

<br />

## Task 1 - Tidy the data (20 marks)

Starting from the `f1` object create a tidy data frame from these data including the following columns:

* `name` - The name of the race (character type)
* `round` - Round of the race (integer type, between 1 and 21)
* `date` - Date of the race (date class)
* `driver` - Name of a driver, including first and last name (character type)
* `constructor` - Name of a driver's constructor, i.e. team (character type)
* `position` - Position (place) driver finished in for the race (integer type, `NA` if they did not finish for any reason)
* `points` - Number of points the driver earned for the race (integer type)

Print out at least 10 rows of this data frame, clearly showing the format and column types of your answer.

<br/>


## Task 2 - Drivers' Championship (30 marks)

Using the data frame from Task 1, construct a table showing the World Drivers' Championship standings for this F1 season. This table should *resemble* but not be identical to the results available on [Wikipedia](https://en.wikipedia.org/wiki/2018_Formula_One_World_Championship#World_Drivers'_Championship_standings). Your data frame should also have 23 columns: Driver name, finishing position for all 21 races, and finally their overall points total for the season. Failure to finish for any reason (did not start, did not finish, disqualified, retired, etc.) should be coded as an `NA`. Race finishes and points total should all have an integer type. The order of the race columns should follow the chronological order in which the races occured. Finally, your data frame should be sorted by points total, but you do not need to include any additional logic to handle ties. 

Print out a nicely formatted version of the *complete* table in your rendered `md` document. 

<br />

## Task 3 - Cumulative Constructors (30 marks)

Using the data frame from Task 1 (as a starting point), construct a table that contains the cumulative points earned by each of the 10 teams at the end of each of the 21 races of the 2018 season. For example Mercedes earned 22 points from the Australian Grand Prix, 33 from the Bahrain Grand Prix, and 30 from the China Grand Prix. Therefore the row for Mercedes in this data frame would contain the values 22, 55, 85 for the first three columns, corresponding to these races. Note - there is no need to take into account the exclusion, sale, and rentry of the Force India team, this can be treated as a single team for the purpose of this task.

Your final data frame should have 22 columns: Constructor name and one column for each of the 21 grand prix races. You results should be ordered by the constructors total points at the end of the season.

Print out a nicely formatted version of the *complete* table in your rendered `md` document.

<br />

## Task 4 - Visualization (20 marks)

Design a visualization that shows the performance of *both* drivers and teams over the course of the 2018 F1 season in terms of the points earned toward the drivers' and constructors' Championship standings. This exercise is meant to be purposefully open ended, come up with a visualization that tells a compelling story about this season and use your write up to justify your design choices. Your write up *must* include a discussion of what you are trying to convey as well as how your visualization achieves those goals.

You may use any visualization tools and/or packages you would like as long as your results are reproducible (e.g. I should be able to change the source data and that change would be reflected in the visualization).


