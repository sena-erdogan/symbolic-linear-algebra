module IMatrix
	import ModelingToolkit
	using SymbolicUtils
	using MacroTools

	mutable struct Constraint
		expr::Expr
		symbols::Array{Base.Symbol}
	end

	mutable struct CaseCondition
		system::Array{Constraint}
		symbols::Array{Base.Symbol}
	end
	  
	mutable struct CaseValue
		expr::Union{Expr,Number}
		free_variables::Array{Base.Symbol}
		bound_constants::Array{Union{Base.Symbol,SymbolicUtils.Sym{Number},Number}} 
	end
	  
	mutable struct IndexSet
		condition::CaseCondition
		dimension::Int16 # how many indices, typically 2 --> i,j

		function IndexSet(condition::CaseCondition, dimension::Int16)
			return new(condition, dimension)
		end

		function IndexSet(constr::Vector{Constraint}, dimension::Int16)
			return IndexSet(CaseCondition(rule_simplification(constr),[]), dimension)
		end
	end
	  
	mutable struct Case
		value::CaseValue
		indices::IndexSet
	end

	mutable struct SymbolicMatrix <: Number
		cases::Vector{Case}
		m::SymbolicUtils.Sym{Number}
		n::SymbolicUtils.Sym{Number}

		function SymbolicMatrix(cases::Vector{Case}, m::SymbolicUtils.Sym{Number}, n::SymbolicUtils.Sym{Number})
			return new(cases, m, n)
		end

		function SymbolicMatrix(list::Vector{Vector{Any}}, m::SymbolicUtils.Sym{Number}, n::SymbolicUtils.Sym{Number})
			cases = Vector{Case}()
			for case in list
				push!(cases, Case(CaseValue(case[1],[],[]),IndexSet(CaseCondition([Constraint((Meta.parse(case[2])),[Symbol(i),Symbol(j)])],[]),Int16(2))))
			end
			return new(cases, m, n)
		end
	end

	function Base.:*(a::Number, A::SymbolicMatrix)
		new_cases = Vector{Case}() #New matrix have Acase count of cases.
		
		for case in A.cases
			push!(new_cases, Case((a * case.value), case.indices))
		end
  
		C = SymbolicMatrix(new_cases,A.m,A.n) #Init new matrix
  
		return C
	  end

	function Base.:+(A::SymbolicMatrix, B::SymbolicMatrix)
		new_cases = Vector{Case}() #New matrix have Acase  x Bcase count of cases.
		
		for case_alpha in A.cases
		   for case_beta in B.cases
			  push!(new_cases, Case((case_alpha.value + case_beta.value), mix(case_alpha.indices, case_beta.indices)))
		   end
		end
  
		C = SymbolicMatrix(new_cases,A.m,A.n) #Init new matrix
  
		return C
	  end
	  
	  # TODO
	function simplify(IS::IndexSet) #using RULES

		#==for i in 1:(IS.dimension + 1)
			for case in IS.conditions
				for system in case.system
					if system.expr -> i=a & i=b & a!=b #natural
						if a!=b
							system.expr -> false
						end
					end
					
					if system.expr -> i=a & i!=a #natural
						system.expr -> false
					end

					if system.expr -> i=a & i!=a #not natural
						system.expr -> false
					end

					if system.expr -> i!=a & i!=a #natural
						system.expr -> i!=a
					end

					if system.expr -> i=a & i=a #natural
						system.expr -> i=a
					end

					#6-13

					if system.expr -> i=j-a & i=j-b & a!=b #natural
						if a!=b
							system.expr -> false
						end
					end

					if system.expr -> i=j+a & i=j+b & a!=b #natural
						if a!=b
							system.expr -> false
						end
					end

					if system.expr -> i=j-a & i=j+b & a!=b #natural
						if a!=b
							system.expr -> false
						end
					end

					if system.expr -> i=j+a & i=j-b & a!=b #natural
						if a!=b
							system.expr -> false
						end
					end

					if system.expr -> i=j & i=n #natural
						system.expr -> i=n & j=n
					end

					if system.expr -> i=n & j=n #natural
						system.expr -> i=n & j=n
					end

					if system.expr -> i=n & i=j #natural
						system.expr -> i=n & j=n
					end

					if system.expr -> j=n & i=j #natural
						system.expr -> i=n & j=n
					end

					if system.expr -> j=a*j & i=b*j & a!=b #natural
						system.expr -> false
					end

				end
			end
		end==#

		return IS

	end

	#here

	function Base.print(M::SymbolicMatrix, m::Int64, n::Int64)
		println("===========================================")
		for ii=1:m
			for jj=1:n
				print(get(M,[ii,jj]))
				print("  ")
			end
			println("")
		end
		println("===========================================")
	end

	function Base.print(case::CaseValue)
		print(case.expr , '\t')
	end

	function belongs(point::Vector{<:Union{Base.Symbol, SymbolicUtils.Sym{Number}, Number, Int64}}, indexSet::IndexSet)

		exp = :(1==1)

		for constraint in indexSet.condition.system
			exp = Expr(:call, :&, exp, deepcopy(constraint.expr))
		end
		
		s = indexSet.condition.system[1].symbols

		for i =1: length(s)
			exp = MacroTools.postwalk(exp -> exp == s[i] ? point[i] : exp, exp)
		end

		return eval(exp)

		#return eval(MacroTools.postwalk(exp -> exp == :i ? ii : exp, exp))
	end

	function get(matrix::SymbolicMatrix, indices::Vector{Int64})
		for case in matrix.cases
			if belongs(indices,case.indices)
				if typeof(case.value) <: Number
					return case.value
				else
					###assuming free_variables == indices for now###

					s = case.value.free_variables

					e = deepcopy(case.value.expr)
					for i =1: length(s)
						e = MacroTools.postwalk(e -> e == s[i] ? point[i] : e, e)
					end
					#println(e)
					return eval(e)
				end
			end
		end
		return nothing
	end

	function Base.:*(a::Number,A::CaseValue)
		return CaseValue(a * A.expr, A.free_variables, A.bound_constants)
	end

	function Base.:+(A::CaseValue, B::CaseValue)
		if typeof(A.expr)<:Number && typeof(B.expr)<:Number
			return CaseValue(A.expr + B.expr, hcat(A.free_variables, B.free_variables), hcat(A.bound_constants, B.bound_constants))
		end

		return CaseValue(Expr(A.expr) + Expr(B.expr), hcat(A.free_variables, B.free_variables), hcat(A.bound_constants, B.bound_constants))
	end

	function row(M::SymbolicMatrix,k::Any)

		@SymbolicUtils.syms i

		cases = Array{Case,1}(undef, length(M.cases))
		for temp =1: length(M.cases)
			cases[temp] = Case(M.cases[temp].value, mix(M.cases[temp].condition, i==k))
		end
		R = SymbolicVector(cases, true, M.n, k)
		return(R)
	end

	function col(M::SymbolicMatrix,k::Any)

		@SymbolicUtils.syms i

		cases = Array{Case,1}(undef, length(M.cases))
		for temp =1: length(M.cases)
		 cases[temp] = Case(M.cases[temp].value, mix(M.cases[temp].condition, j==k))
		end
		R = SymbolicVector(cases, false, M.m, k)
		return(R)
	end

	function mix(x::SymbolicUtils.Term{Bool},y::SymbolicUtils.Term{Bool})
		return SymbolicUtils.serial_simplifier((x) & (y))
	end

	function mix(x::IndexSet,y::IndexSet)
		if x.dimension != y.dimension
			throw(ErrorException("Dimensions not the same"))
		end
		return IndexSet(combine(x.condition, y.condition), x.dimension)
	end

	function combine(x::CaseCondition, y::CaseCondition)
		return CaseCondition(hcat(x.system, y.system), hcat(x.symbols, y.symbols))
	end

	function matMultiplication(A::SymbolicMatrix, B::SymbolicMatrix)

		if A.n != B.m
			println("ERROR : Matrices A and B cannot be Multiplied.")
			return
		end
		
		case_count_A = length(A.cases) #Get the case count of first matrix
		case_count_B = length(B.cases) #Get the case count of second matrix
		counter = 1
		new_cases = Array{Case,1}(undef, case_count_A * case_count_B) #New matrix have Acase  x Bcase count of cases.
		C = SymbolicMatrix(new_cases,A.m,B.n) #Init new matrix

		for case_alpha in A.cases
			for case_beta in B.cases
				new_cases[counter] = Case(innerProduct(row(A,i),col(B,j)), mix(case_alpha.condition, case_beta.condition))
				#new_cases[counter] = Case(Case(innerProduct(row(A,i),col(B,j)),case_beta.condition), case_alpha.condition)
				counter = counter + 1
			end
		end

		return (C)
	end

   function rule_simplification(C::Vector{Constraint})
	SymbolicUtils.@syms x y z i::Bool j::Bool
	RULES = [
		#=1=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~y::SymbolicUtils.isliteral(Integer)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~z::SymbolicUtils.isliteral(Integer))=>false),

		#=2=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~y::SymbolicUtils.isliteral(Integer)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) != ~y::SymbolicUtils.isliteral(Integer))=>false),

		#=3=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~y::SymbolicUtils.isliteral(SymbolicUtils.Sym)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) != ~y::SymbolicUtils.isliteral(SymbolicUtils.Sym))=>false),

		#=4=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) != ~y::SymbolicUtils.isliteral(Integer)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) != ~y::SymbolicUtils.isliteral(Integer))=>~x != ~y),

		#=5=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~y::SymbolicUtils.isliteral(Integer)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~y::SymbolicUtils.isliteral(Integer))=>~x == ~y),

		#=6=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) > ~y::SymbolicUtils.isliteral(Integer)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) > ~z::SymbolicUtils.isliteral(Integer))=> ~x > max(~y, ~z)),

		#=7=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) < ~y::SymbolicUtils.isliteral(Integer)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) < ~z::SymbolicUtils.isliteral(Integer))=> ~x < min(~y, ~z)),

		#=8=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) <= ~y::SymbolicUtils.isliteral(Integer)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) <= ~z::SymbolicUtils.isliteral(Integer))=> ~x <= min(~y, ~z)),

		#=9=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) >= ~y::SymbolicUtils.isliteral(Integer)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) >= ~z::SymbolicUtils.isliteral(Integer))=> ~x >= max(~y, ~z)),

		#=10=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) <= ~y::SymbolicUtils.isliteral(Integer)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) < ~z::SymbolicUtils.isliteral(Integer))=> ~x <= min(~y, ~z-1)),

		#=11=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) >= ~y::SymbolicUtils.isliteral(Integer)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) > ~z::SymbolicUtils.isliteral(Integer))=> ~x >= max(~y, ~z-1)),

		#=12=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) < ~y::SymbolicUtils.isliteral(Integer)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) <= ~z::SymbolicUtils.isliteral(Integer))=> ~x <= min(~y-1, ~z)),

		#=13=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) > ~y::SymbolicUtils.isliteral(Integer)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) >= ~z::SymbolicUtils.isliteral(Integer))=> ~x >= max(~y-1, ~z)) ,

		#=14=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~y::SymbolicUtils.isliteral(SymbolicUtils.Sym) - ~z::SymbolicUtils.isliteral(Integer)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~y::SymbolicUtils.isliteral(SymbolicUtils.Sym) - ~t::SymbolicUtils.isliteral(Integer))=> false),

		#=15=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~y::SymbolicUtils.isliteral(SymbolicUtils.Sym) + ~z::SymbolicUtils.isliteral(Integer)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~y::SymbolicUtils.isliteral(SymbolicUtils.Sym) + ~t::SymbolicUtils.isliteral(Integer))=> false),

		#=16=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~y::SymbolicUtils.isliteral(SymbolicUtils.Sym) - ~z::SymbolicUtils.isliteral(Integer)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~y::SymbolicUtils.isliteral(SymbolicUtils.Sym) + ~t::SymbolicUtils.isliteral(Integer))=> false),

		#=17=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~y::SymbolicUtils.isliteral(SymbolicUtils.Sym) + ~z::SymbolicUtils.isliteral(Integer)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~y::SymbolicUtils.isliteral(SymbolicUtils.Sym) - ~t::SymbolicUtils.isliteral(Integer))=> false),

		#=18=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~y::SymbolicUtils.isliteral(SymbolicUtils.Sym)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~z::SymbolicUtils.isliteral(Integer))=>(~x == ~z) & (~y == ~z)),

		#=19=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~y::SymbolicUtils.isliteral(SymbolicUtils.Sym)) & (~y::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~z::SymbolicUtils.isliteral(Integer))=>(~x == ~z) & (~y == ~z)),

		#=20=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~z::SymbolicUtils.isliteral(Integer)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~y::SymbolicUtils.isliteral(SymbolicUtils.Sym)) =>(~x == ~z) & (~y == ~z)),

		#=21=#SymbolicUtils.@rule((~y::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~z::SymbolicUtils.isliteral(Integer)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~y::SymbolicUtils.isliteral(SymbolicUtils.Sym)) =>(~x == ~z) & (~y == ~z)),

		#=22=#SymbolicUtils.@rule((~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~y::SymbolicUtils.isliteral(Integer) * ~z::SymbolicUtils.isliteral(SymbolicUtils.Sym)) & (~x::SymbolicUtils.isliteral(SymbolicUtils.Sym) == ~t::SymbolicUtils.isliteral(Integer) * ~z::SymbolicUtils.isliteral(SymbolicUtils.Sym)) => false)

		]

		rset = SymbolicUtils.Rewriters.Chain(RULES)

		counter = 1
		simp_cases = Vector{Constraint}() #New matrix have Acase  x Bcase count of cases.
		
		for case in C.cases
			simp = rset(case.condition)
			if simp !== nothing
				simp_cases[counter] = Case(case.value, simp)
			else
				simp_cases[counter] = Case(case.value, case.condition)
			end
			counter = counter + 1
		end

		A = SymbolicMatrix(simp_cases,C.m,C.n) #Init new matrix
		
		return A
	end

	SymbolicUtils.@syms i::Bool j::Bool m n

	A = SymbolicMatrix([[1, "i==j"],[0, "i!=j"]], m, n)
	B = SymbolicMatrix([[1, "i==j"],[0, "j < 2"], [rand(1:9), "j == 2"], [rand(1:9), "j==3"], [rand(1:9), "j>3"]], m, n)

	println("Printing Matrix A")
	print(A,10,10)

	println("Printing Matrix B")
	print(B,10,10)

	C = A + B
	println("Printing Matrix A + B which is C:")
	print(C,10,10)
	#==println("Conditions of C:")
	for case in C.cases
	   println(case)
	end==#

	C = 2 * C
	println("Multiplying Matrix C with 2:")
	print(C,10,10)

 end
