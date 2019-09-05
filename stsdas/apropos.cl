procedure apropos ( topic )

string	topic {prompt = ">Apropos ? ", mode="ql"}
file	index = "stsdas$lib/apropos.db" {prompt=">index to search"}

begin
string csubject

	# remove case sensitivity
	csubject = "{"//topic//"}"
	match ( csubject, index, stop=no, meta=yes, print_file_names=no)

end
