# ga-data-explorer
Shiny app for exploring web analytics data across multiple dimensions.

This is actually a collection of multiple Shiny apps:

* **ga-data-explorer** is limited to Google Analytics data. But, the user has to log in to Google and then select an Account/Property/View to use.

* **ga_explorer_demo** requires hardcoding (or putting in .Renviron) a view ID. So, it locks the specific data being used to a single view

* **adobe-data-explorer** uses Adobe Analytics data rather than Google Analytics data. Functionally, it's the same as **ga_explorer_demo**, but it uses the older Adobe Analytics auth functionality, so requires hardcoding or putting in the .Renviron file the Adobe account, ID, and secret.
