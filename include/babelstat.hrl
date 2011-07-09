%%The document as it is in the database
-record(babelstat,
	{
	  id,
	  rev,
	  type = babelstat,
	  date,
	  value,
	  metric,
	  scale,
	  frequency,
	  location,
	  category,
	  sub_category,
	  subject,
	  series_category,
	  title,
	  source,
	  calculation = null,
	  constant = false
	}).

-record(babelstat_query,
	{
	  category,
	  sub_category,
	  subject,
	  series_category,
	  title
	}).

-record(babelstat_filter,
	{
	  metric,
	  scale,
	  frequency,
	  from_date,
	  to_date
	}).

%%The output from shows
-record(babelstat_series,
	{
	  dates = [],
	  values = [],
	  metric,
	  scale,
	  frequency,
	  location,
	  category,
	  sub_category,
	  subject,
	  series_category,
	  title,
	  source,
	  legend
	}).
