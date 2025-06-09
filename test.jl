module test
import ModelingToolkit
using SymbolicUtils
using MacroTools
using Random
include("SLA.jl") 


SymbolicUtils.@syms i::Bool j::Bool m n

open("output.txt","w") do io
end

function printToTestFile(M::IMatrix.SymbolicMatrix,m::Int64,n::Int64,matName::String)

	open("output.txt","a+") do io
		write(io,"\n")
		write(io,string("=============Matrix ",matName,"========\n"))
		for ii=1:m
			for jj=1:n
				write(io,string(IMatrix.get(M,[ii,jj])))
				write(io,"  ")
			end
			write(io,"\n")
		end

		write(io,"\n")
		write(io,"Case tuples:\n")
		for case in M.cases
			write(io,string("case(v: ",case.value,",C: ",case.indices,")"))
			write(io,"\n")
		end
	end
	
end

#A and B are constructed using 5 conditions=================================

A5 = SymbolicMatrix([[1, "i==j"],[0, "j < 2"], [rand(1:9), "j == 2"], [rand(1:9), "j==3"], [rand(1:9), "j>3"]], m, n)
B5 = SymbolicMatrix([[1, "i==j-1"],[0, "i==j-2"], [rand(1:9), "j == 2"], [rand(1:9), "j<=5"], [rand(1:9), "j>5"]], m, n)

#A and B are constructed using 10 conditions--==================================

A10 = SymbolicMatrix([[0, "i>j"],[1, "i==j"], [rand(1:9), "j == 2"], [rand(1:9), "j==3"], [rand(1:9), "j==4"],[rand(1:9), "j == 5"],[rand(1:9), "j == 6"],[rand(1:9), "j == 7"],[rand(1:9), "j == 8"],[0, "i<=j"]], m, n)
B10 = SymbolicMatrix([[0, "i==j-1"],[0, "i==j+1"], [rand(1:9), "i==j+2"], [1, "i==j"], [0, "j < 2"], [rand(1:9), "j == 2"], [rand(1:9), "j == 3"], [rand(1:9), "j == 4"], [rand(1:9), "j<=6"], [rand(1:9), "j>6"]], m, n)

#A and B are constructed using 20 conditions===========================================

A20 = SymbolicMatrix([[0, "i>j"],[0, "j==1"], [rand(1:9), "j == 2"], [rand(1:9), "j==3"], [rand(1:9), "j==4"],[rand(1:9), "j == 5"],[rand(1:9), "j == 6"],[0, "j == 7"],[rand(1:9), "j == 8"],[rand(1:9), "j==9"],[rand(1:9), "j==10"],[rand(1:9), "j==11"], [rand(1:9), "j==12"], [0, "j==13"], [rand(1:9), "j==14"],[rand(1:9), "j==15"],[rand(1:9), "j==16"],[rand(1:9), "j==17"],[rand(1:9), "j==18"],[1, "i>=j"]], m, n)
B20 = SymbolicMatrix([[1, "i==j"],[0, "i==j+1"], [0, "i==j-1"], [rand(1:9), "j==1"], [rand(1:9), "j==2"], [rand(1:9), "j==3"], [rand(1:9), "j == 4"], [rand(1:9), "j == 5"], [rand(1:9), "j==6"], [0, "j==7"],[rand(1:9), "j==8"],[rand(1:9), "j==9"],[rand(1:9), "j == 10"], [rand(1:9), "j==11"], [rand(1:9), "j==12"],[0, "j == 13"],[rand(1:9), "j == 14"],[rand(1:9), "j == 15"],[rand(1:9), "j == 16"],[rand(1:9), "j==17"]], m, n)

printToTestFile(A5,10,10,"A1")
# printToTestFile(A5,100,100,"A2")
# printToTestFile(A5,1000,1000,"A3")
printToTestFile(A10,10,10,"A4")
# printToTestFile(A10,100,100,"A5")
# printToTestFile(A10,1000,1000,"A6")
printToTestFile(A20,10,10,"A7")
# printToTestFile(A20,100,100,"A8")
# printToTestFile(A20,1000,1000,"A9")


printToTestFile(B5,10,10,"B1")
# printToTestFile(B5,100,100,"B2")
# printToTestFile(B5,1000,1000,"B3")
printToTestFile(B10,10,10,"B4")
# printToTestFile(B10,100,100,"B5")
# printToTestFile(B10,1000,1000,"B6")
printToTestFile(B20,10,10,"B7")
# printToTestFile(B20,100,100,"B8")
# printToTestFile(B20,1000,1000,"B9")

#==C5 = A5 + B5

printToTestFile(C5,10,10,"C1")

C10 = A10 + B10
printToTestFile(C10,10,10,"C2")

C20 = A20 + B20
printToTestFile(C20,10,10,"C3")

test = IMatrix.matMultiplication(A5,B5)

printToTestFile(test,10,10,"mult")==#

end
