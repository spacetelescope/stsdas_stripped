#  Common block for wstatistics task

bool	allst			# Compute all statistical quantities?
bool	npix			# Display number of non-rejected pixels?
bool	minmax			# Compute minmax?
bool	mean			# Compute mean?
bool	median			# Compute midpt?
bool	mode			# Compute mode?
bool	stddev			# Compute standard deviation?
bool	skew			# Compute skew?
bool	kurt			# Compute kurtosis?

common	/cstats/ allst, npix, minmax, mean, median, mode, stddev, skew, kurt
