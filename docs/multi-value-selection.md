# Multi Value Selection

Code Review is currently using `completing-read-multiple` function which allows
us to choose more than 1 option for the header fields of your Pull Request e.g.
Labels, Milestones, Projects, and others.

It's important to keep in mind that Github (the only forge supported right now)
does not have an incremental update on these values therefore you must always
choose the complete set of values you want to have.

`completing-read-multiple` works by separating each value with a comma. You can
also see a list of candidates by pressing TAB.
