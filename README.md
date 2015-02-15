# Workz
A quick marketing tool kit with a shiny application to support it

Workz is a marketing CRM (customer relationship management) project which is based on a simple model of Leads and Discussions.

The goals of the toolkit are:
* **Similicity** - Management, users and data-entry should be able to use it intuitively
* **Flexibility** - The toolkit can be used by any medium-sized organization which does not have full fledged CRM
* **General** - Developers and marketers are welcome to contribute to make this more general purpose

The module is combined with an Excel file for data-entry and a Shiny user interface to conduct data-analysis.

To run the app using Shiny in R, you may simply use the command:

```r
shiny::runGitHub("Workz","sachinsancheti1",ref = "masternew")
```
New update on 15 Feb 2015
* Ctrl + u - When pressed on an **Initiative cell**, will return all the events that have been scheduled for this Initiative
* Ctrl + i - When pressed on an **Initiative cell**, a new event will be prompted for creation
* Ctrl + o - Export to **Microsoft Outlook** will take place for events which were not earlier **Done**
