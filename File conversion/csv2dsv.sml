(*We have used iterative over recursive style in both the functions*)
exception emptyInputFile
fun convertNewlines(infilename: string, newline1: string, outfilename: string, newline2: string)=
    let 
    val inputstream = TextIO.openIn infilename
    val outputstream= TextIO.openOut outfilename
    val n=String.size(newline1)
	and c= ref ""
	val count: int ref= ref 0(*Just a counter to recognize whether file is empty or not *)
	in
		(*Loop which takes charachter by charcter and analysies it*)
		while (c := TextIO.inputN(inputstream,1); !c<>"") do
            (	count:= 1;
				if (String.sub(!c,0)<>String.sub(newline1,0))(*If the first char of newline comes then check if the newline is there else continue printing*)
            	then (TextIO.output(outputstream,!c))
            	else             
                	(let 
                    	val a=(!c)^(TextIO.inputN(inputstream,n-1))
                	in 
		        		if (a=newline1)
                    	then (TextIO.output(outputstream,newline2))(*If there is an instance of newline1 change it into our newline2*)
                    	else (TextIO.output(outputstream,a))
                	end)
			);
		TextIO.closeIn inputstream;
		TextIO.closeOut outputstream;
		if (!count=0)then raise emptyInputFile else(())(*If counter is still at 0 which means there arent any char to read so empty file*)
    end;

fun unix2dos(infilename,outfilename)= convertNewlines(infilename,"\n",outfilename,"\r\n")
fun dos2unix(infilename, outfilename)= convertNewlines(infilename,"\r\n",outfilename,"\n")




exception UnevenFields of string
exception emptyInputFile
exception NotTerminatedFile (*If the file doesnt have a terminated newline symbol*)
exception NotClosedField(*If the Feild is started by enclosing in " but isnt closed*)
exception NotEscaped(*If something hasnt been escaped raise this error*)
fun convertDelimiters(infilename:string, delim1:char, outfilename:string, delim2:char)=
    let
    val inputstream = TextIO.openIn infilename
    val outputstream= TextIO.openOut outfilename
	val c= ref ""
	val counter : int ref = ref 1 (*A counter to maintain the no of commas*)
	val row: int ref=ref 0 (*Line no.*)
	val column: int ref=ref 0 (*column no value*)
	val icolumn: int ref=ref 0 (*initial no of fields that we take as refrence*)
	val str:string ref=ref ""
	val flag:bool ref= ref false(*the flag we need to check if delim2 occurs in the feild so we enclose in double quotes*)
	val prev:string ref=ref ""(*Maintain the previous char*)
	in
		(*Loop which takes charachter by charcter and analysies it*)
		while (c := TextIO.inputN(inputstream,1); !c<>"") do
            (
				(*If the element if delim so empty feild so increase the feild count and move*)
				if (String.sub(!c,0)=delim1) then (
					column := !column+1;
					TextIO.output1(outputstream,delim2)
				)
				else if (!c="\n" andalso !prev<>"" andalso String.sub(!prev,0)=delim1) then 
				(
					column := !column+1;
					if (!row=0) then 
						(
							icolumn := !column
						)
					else(
						(*Error for unequal field*)
						if (!column<> !icolumn) then 
							(
								print("Expected: "^(Int.toString(!icolumn))^" fields, Present: "^(Int.toString(!column)^" fields on Line "^(Int.toString(!row+1))));
								raise UnevenFields ""
							)
							else()
						);
					column :=0;
					row := !row+1;
					TextIO.output(outputstream,"\n")							
						
				)
				(*ignore the newline character if prev wasn't newline char *)
				else if (!c="\n") then (
					if (!prev="\n") then (TextIO.output(outputstream,"\n")) else ()
				)
				(*if we get a double quote means the field is enclosed*)
                else if !c="\"" then
				(	column := !column+1;
					TextIO.output(outputstream,!c);
					(*run the loop till we get end of the feild or some error occurs , end identified when there are even no of " in the feild*)
					while (c := TextIO.inputN(inputstream,1); !c<>"" andalso (((String.sub(!c,0)<>delim1 andalso !c<>"\n") orelse ((!counter) mod 2)=1))) do 
						(
							(*updating the counter of "*)
							if !c="\"" then
							(
								counter := !counter+1
							)
							(*throw error if unenclosed "*)
							else(if ((!counter) mod 2)<>1 then (print("Unescaped \" found at field at Column: "^(Int.toString(!column))^" Line: "^(Int.toString(!row+1))^" Please add a double quote to enclose it"); raise NotEscaped) else ());
							TextIO.output(outputstream,!c)
						);
					(*Error if the feild isnt closed or not terminated file *)
					if !c="" then (if !prev<>"\"" then (print("The Field at Column: "^(Int.toString(!column))^" Line: "^(Int.toString(!row+1))^" starts with a \" but is left unclosed"); raise NotClosedField) else print("The File Doesn't Terminate with line break"); raise NotTerminatedFile ) else ();
					counter := !counter+1;
					(*change delim*)
					if String.sub(!c,0)=delim1 then 
					(	TextIO.output1(outputstream,delim2)
					)
					else
					(	(*Get the initial column count*)
						if (!row=0) then 
						(
							icolumn := !column
						)
						else(
							(*Error for unequal field*)
							if (!column<> !icolumn) then 
							(
								print("Expected: "^(Int.toString(!icolumn))^" fields, Present: "^(Int.toString(!column)^" fields on Line "^(Int.toString(!row+1))));
								raise UnevenFields ""
							)
							else(())
						);
						column :=0;
						row := !row+1;
						TextIO.output(outputstream,"\n")
					)
				)
				(*This block for the unenclosed field*)
				else 
				(	column := !column+1;
					str:= !c;
					flag:=false;
					(*Run the loop till we reach a delimter or \n*)
					while(c:=TextIO.inputN(inputstream,1); ( !c<>"" andalso !c<>"\n") andalso (String.sub(!c,0)<>delim1 )) do 
					(
						(*If an " appears it in unenclosed so raise erro*)
						if (!c="\"") then (print("Unescaped \" found at field at Column: "^(Int.toString(!column))^" Line: "^(Int.toString(!row+1))^" Please enclose the field in double quotes"); raise NotEscaped) else ();
						(*If we find the delimeter 2 that means this has feild has to be escaped so flag to true*)
						if (String.sub(!c,0)=delim2) then (flag:=true) else(());
						str:=(!str)^(!c)
					);
					if !c="" then (print("The File Doesn't Terminate with line break"); raise NotTerminatedFile) else (());						
					(*flag is true meaning enclose it else not*)
					if (!flag) then (TextIO.output(outputstream,"\""); TextIO.output(outputstream,!str);TextIO.output(outputstream,"\"")) else (TextIO.output(outputstream,!str));
					(*change delim*)
					if String.sub(!c,0)=delim1 then
					(	TextIO.output1(outputstream,delim2)
					)
					else
					(	(*Get the initial column count*)
						if (!row=0) then
						(
							icolumn := !column
						)
						else (
							(*Error for unequal field*)
							if (!column<> !icolumn) then 
							(   
								print("Expected: "^(Int.toString(!icolumn))^" fields, Present: "^(Int.toString(!column)^" fields on Line "^(Int.toString(!row+1))));
								raise UnevenFields ""
							)
							else(())							
						);
						column :=0;
						row := !row+1;
						TextIO.output(outputstream,"\n")
					)
				);
				prev:= !c(*Updating the prev*)
            );
		TextIO.closeIn inputstream;
		TextIO.closeOut outputstream;
		if (!column=0 andalso !row=0) then (raise emptyInputFile) else(());(*If nothing in the raise the Empty file exception*)
		if (!column<>0) then (print("The File Doesn't Terminate with line break"); raise NotTerminatedFile) else (())(*if the file didnt terminate the column count would not be intialed to zero*)
	end

fun csv2tsv(infilename:string,outfilename:string)= convertDelimiters(infilename,#",",outfilename,#"\t")
fun tsv2csv(infilename:string,outfilename:string)= convertDelimiters(infilename,#"\t",outfilename,#",")
