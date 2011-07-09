%%The document as it is in the database
-record(babelstat,
	{
	  id,
	  rev,
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
	  calculation = undefined,
	  constant = false
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
