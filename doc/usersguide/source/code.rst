.index Inside the code

Inside the code : TODO
#################

This section should be read by advanced users who want to help devlopment of Liam2. 
The python code is quite readable. 

The main prgrogram is the executable. It calls the simulation program, which is finally the definition of a class simulation. 
That class has a classmethod from_yaml that can read a yaml file. Check are done at that moment to see wether the input file 
is correct. 

Others programs are well named so it is easy to understand what they are for. 

One thing important to understand as it is used everywhere are the following concepts : 

- expr and Expr (EvaluableExpr)
- isinstance()
- method evaluate in class
- the Entity program
- context