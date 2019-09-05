# MESSAGE.COM -- Global variables used for passing messages

pointer	queue			# queue of pending messages
pointer	msgbuf			# message buffer
int	frontq			# next message in queue
int	endq			# next unoccupied spot in queue
int	endbuf			# first char unoccupied in buffer
int	msgkind			# kind of pending message

common	/message/	queue, msgbuf, frontq, endq, endbuf, msgkind
