# TERMINAL.COM -- Global variables set by terminal object

pointer	terminal	# terminal's object descriptor
pointer	focus		# object with current focus
pointer	select		# object currently selected
pointer	logger		# object which acts as logger
pointer	help		# object which displays help

common /terminal/ terminal, focus, select, logger, help
