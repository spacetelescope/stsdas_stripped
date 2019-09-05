#################################################################################
#  DESCRIBE --	A shortcut task to see the "Description" section of a help 	#
#		file.								#
#										#
#	8/91	Initial code by RAShaw						#

procedure describe (taskname)

string	taskname = "" {prompt = "Enter task name > "}
string	mode = "al"

begin
	string	tasknm
	tasknm = taskname
	print ("\nDESCRIPTION for task: ", tasknm)
	help (tasknm, section="description")
end
