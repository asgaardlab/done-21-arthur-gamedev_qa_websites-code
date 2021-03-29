# Replication package for the paper "An Empirical Study of Q&A Websites for Game Developers"

This repository contains the datasets and scripts used to replicate the results from the paper "An Empirical Study of Q&A Websites for Game Developers".

## Replication data

The datasets used to replicate the results for the paper can be found in the [data directory](/data/). These are the datasets we obtained after running all of the notebooks in this repository.

Two of the studied websites are owned by companies (Epic and Unity) and we are not legally allowed to share the textual contents of the questions and answers as they are considered intellectual property. Therefore, instead of sharing the content of those posts, we included the URLs to all of the pages where the information used in the paper can be found, so that they can be crawled by future researchers.

This is not an issue for Stack Overflow and the Game Development Stack Exchange, since that data is provided by Stack Exchange in the [Stack Exchange Data Dump](https://archive.org/details/stackexchange).

**Survey data:** Unfortunately, our University's ethics board only allows us to share the survey responses in aggregated format, which is done in the paper. In this repository, we added the list of communities in which we shared the survey ([here](/data/surveyed_communities.csv)).

## Using this repository

If you are using the datasets provided in this repository, you just need to run the [analysis notebook](/code/analysis/paper_results.ipynb) to obtain the results as shown in the paper.

Otherwise, if you want to run the whole pipeline from the start, follow these steps:

1. Download the data from Unity Answers and the UE4 AnswerHub from their websites (you can use the URLs provided in our datasets to help). Parse the HTML pages and extract the required information.

2. Download the data from Stack Overflow and the Game Development Stack Exchange from [the Stack Exchange Data Dump](https://archive.org/details/stackexchange). Run the notebooks to process the XML files from the Stack Exchange data dump ([here](/code/process_xml/)). For Stack Overflow, run the [select\_gamedev\_posts.ipynb](/code/process_xml/stackoverflow/select_gamedev_posts.ipynb) first.

3. Run the [text processing notebook](/code/text_processing.ipynb).

4. Run the [topic modelling notebook](/code/topic_modelling.ipynb).

5. Run the [topic comparisons notebook](/code/topic_comparisons.ipynb).

6. Finally, run the [analysis notebook](/code/analysis/paper_results.ipynb) to get the results as shown on the paper.
