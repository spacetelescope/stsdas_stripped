#  EXAMPLES --	A shortcut task to see the "Examples" section of a help file.	#
#										#
#	8/91	Initial code by PAHodge and RAShaw				#

procedure examples (taskname)

string	taskname = "" {prompt = "Enter task name > "}
string	mode = "al"

begin
	string  tasknm
	tasknm = taskname
	print ("\nEXAMPLES for task: ", tasknm)
	help (tasknm, section="examples")
end
