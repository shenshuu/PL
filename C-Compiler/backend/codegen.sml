open Parser;

signature Codegen =
sig
    type program
    type function
    type instruction
    type operand

    val generate_asm : Parser.program -> program
end
    

structure Codegen =
struct

type id = string
datatype program = Program of function
     and function = Function of (id * instruction list)
     and instruction = Mov of (operand * operand) | Ret
     and operand = Imm of int | Register

fun generate_instructions (Parser.Program func) =
    let
	fun generate_func_instructions (Parser.Function(name,body)) =
	    Function(name, [generate_stm_instructions body])
	and generate_stm_instructions (Parser.Statement exp) =
	    Mov(generate_exp_instructions exp, Register)
	and generate_exp_instructions (Parser.Const n) =
	    Imm n
    in
	Program(generate_func_instructions func)
    end
				    
end
