procedure prochist( task )

string task  {prompt=">task name"}

begin

string comline, ttask

	ttask = task

	comline = "lpar "//ttask//" | parprint "//ttask//" STDIN"
    	print ( comline ) | cl ()
end
