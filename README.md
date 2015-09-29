---
output:
  md_document:
    variant: markdown_github
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# rtmr #

To-do list apps can be great on the go, with nifty UIs, APIs and hooks into all kinds of services.

I couldn't find a wrapper for "Remember the Milk" in R, so here's my attempt at that, to allow the level of automation and statistical geekery to which R users are accustomed.

## Create Tasks ##

Start with the basics, creating tasks, but with an R twist: vectorize your add_tasks.


```r
tasks <- c("watch lectures ^wed =\"2 hours\"",
           "do reading ^thu =\"2 hours\"",
           "do problem set ^fri =\"4 hours\"",
           "review problem set ^sat =\"1 hour\"")
rtm_add_task(paste(tasks, "*weekly #MyClass"))
```

## Choose Tasks ##

Pick them randomly!


```r
quick_tasks <- rtm_search("list:Household timeEstimate\"< 30 minutes\"")
todays_tasks <- sample(quick_tasks, 3)

## add notes to one of them
notes_add(todays_tasks[1,],
          note_title = "get this plunger model",
          note_text = "XT-9000D... it has super suction power!")

## then complete them when done
complete(todays_tasks)
```

## Apply Some Rules to Tasks ##

Move tasks around, postpone them, using R functions.


```r
inbox <- rtm_search("list:Inbox")

rule_fun <- function(task) {
  if(grepl("narf", task[["name"]]))
    postpone(task)
  else if (grepl("zort", task[["name"]]))
    moveTo(task, "ImportantResearch")
  else if (grepl("poit", task[["name"]]))
    setTags(task, "#chainletter #plan")
}

## note: needs to be split by "task.id" b/c "id" and "name"
## might not be unique
plyr::d_ply(inbox, "task.id", rule_fun, .progress = "text")
```

## Visualization ##

ggplot2? DiagrammeR? Shiny?

