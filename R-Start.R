#oh wonderful packages for qualitative analysis

#general package: tm

#documents come in varying forms: PDF, word, excel, csv....
#we`ll read in a pdf document first

library(pdftools)

#converting pdf in text document

txt <-pdf_text("Bamberg2009_Chapter_BeratungInDerArbeitsundOrganis.pdf")
txt

#or

read <- readPDF(control = list(text = "-layout"))
document <- Corpus(URISource("Bamberg2009_Chapter_BeratungInDerArbeitsundOrganis.pdf"), readerControl = list(reader = read))
doc <- content(document[[1]])
head(doc)

#saves text as strings in character vector of length equal to number of pages in pdf
# open text on each page:

cat(txt[1])

#table of contents, if available

toc <- pdf_toc("Bamberg2009_Chapter_BeratungInDerArbeitsundOrganis.pdf")
jsonlite::toJSON(toc, auto_unbox = TRUE, pretty = TRUE)

info <- pdf_info("Bamberg2009_Chapter_BeratungInDerArbeitsundOrganis.pdf")
jsonlite::toJSON(info, auto_unbox = TRUE, pretty = TRUE)

pdf_fonts("Bamberg2009_Chapter_BeratungInDerArbeitsundOrganis.pdf")

pdf_data("Bamberg2009_Chapter_BeratungInDerArbeitsundOrganis.pdf")

#normal txt.file

readPlain("Gerechtigkeit biblische Ansätze.txt")

#cleaning text up

library(tm)

stripWhitespace(txt)
txt1 <- strsplit(txt, "\n")
head(txt1)

remove

# course Dave Langer

# Install all required packages.
install.packages(c("ggplot2", "e1071", "caret", "quanteda", 
                   "irlba", "randomForest"))

# Load up the .CSV data and explore in RStudio.
spam.raw <- read.csv("spam.csv", stringsAsFactors = FALSE)
View(spam.raw)

# Clean up the data frame and view our handiwork.
spam.raw <- spam.raw[, 1:2]
names(spam.raw) <- c("Label", "Text")
View(spam.raw)

# Check data to see if there are missing values.
#! --> not, which --> is true
length(which(!complete.cases(spam.raw)))

# Convert our class label into a factor.
spam.raw$Label <- as.factor(spam.raw$Label)



# The first step, as always, is to explore the data.
# First, let's take a look at distibution of the class labels (i.e., ham vs. spam).
prop.table(table(spam.raw$Label))



# Next up, let's get a feel for the distribution of text lengths of the SMS 
# messages by adding a new feature for the length of each message.
spam.raw$TextLength <- nchar(spam.raw$Text)
summary(spam.raw$TextLength)



# Visualize distribution with ggplot2, adding segmentation for ham/spam.
library(ggplot2)

ggplot(spam.raw, aes(x = TextLength, fill = Label)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Text Count", x = "Length of Text",
       title = "Distribution of Text Lengths with Class Labels")
