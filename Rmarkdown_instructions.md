---
author: "Nick Riches"
name: ArgAnAut
output:
  html_document:
    # css: style.css
    number_sections: no
    toc: yes
    toc_float:
      collapsed: yes
  pdf_document:
    toc: yes
  word_document:
    toc: yes
---
        


![](arganaut.png)
        
# Welcome to the ArgAnAut language analysis tool!

## What is ArgAnAut?

ArgAnAut is a tool for analysing argument structure use in clients with language difficulties. It is specifically designed for individuals with Aphasia. Once the therapist has coded the argument structure by hand, ArgAnAut provides a graphical summary of the data, comparing it to benchmarks derived from [Webster, J., Franklin, S., & Howard, D. (2007). An analysis of thematic and phrasal structure in people with aphasia: What more can we learn from the story of Cinderella? Journal of Neurolinguistics, 20(5), 363–394
](https://doi.org/10.1016/j.jneuroling.2007.02.002). 

## Why is it called ArgAnAut?

"Arg" comes from ARG-ument, "An" comes from AN-analysis, and "Aut" comes from AUT-omatic. Jason and the Argonauts was also one of my favourite films as a child.

# Usage

## Step 1 - Coding

Unfortunately argument structure needs to be hand-coded (I cannot think of a reliable automatic way to to this). The app uses a fairly simple scheme:

A = Argument
I = Implicit argument
X = Missing argument
V = Verb

Here are some examples...

1. Cinderella... A Ball... <mark>AX</mark> (Arguments with missing verbs)
2. Went to the ball. <mark>XVA</mark> (Missing argument in subject position, but location argument included after the verb)
3. She ran out <mark>AV</mark> and left behind her glass slipper IVA (Note implicit argument in the second clause)
4. Then she... home <mark>AXA</mark> (Sentence which has two arguments, but verb is not realised)
5. She went straight... <mark>AVX</mark> (Postverbal argument expressing location is omitted)

Make sure that (a) there are spaces on either side of the letter sequence (e.g.<mark> AXA </mark>, <mark> XVA </mark> etc) and (b) there are no spaces between each letter in the sequence.

This coding scheme does not take account of dependent clauses. If you have a sentence with a main clause and a dependent clause, you will need to code these separately, e.g.

6. She said <mark>AVA</mark> she was very sorry <mark>AVA</mark>

The second clause fits inside the second argument of the first clause, so that it has the structure [AV[AVA]]. Although we can't mark dependent clauses, they are likely to be relative rare in the speech of individuals with aphasia.

## Step 2 - Entering data

Go to `(2) Analyse data > Enter data`. You can either upload a Word file (.doc or .docx) or plain text file (.txt). Secondly, you may copy and paste text into a box.

Note that you may enter data *from more than one participant*. To do this, just put three equals signs "===" between transcripts. This may be interesting, because it can help you to prepare different profiles across clients.

## Step 3 - Interpreting performance

When you have entered the data, a figure like the following will appear...


![](graph.png)


This compares the individual's performance (or performance of multiple individuals) against the norms provided by Webster et al. These are shown using the red whisker plots. The mean is the dot in the middle, while the whiskers show the +/- 2 standard deviation limits). The bars show the following

(1) Percentage of predicates with one argument
(2) Percentage of predicates with two arguments
(3) Percentage of predicates with three arguments
(4) Percentage of predicats with an Unidentified Thematic Structure (verbs without any arguments, or arguments without any verbs)
(5) (NEW GRAPH) The mean number of arguments per proposition
(6) (NEW GRAPH) The percentage of arguments omitted



