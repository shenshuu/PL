open Utils;
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

fun map_ast_nodes (Parser.Program node) =
    let
	fun map_func_node (Parser.Function(name,body)) =
	    Function(name, [Ret, map_stm_node body])
	and map_stm_node (Parser.Statement exp) =
	    Mov(map_exp_node exp, Register)
	and map_exp_node (Parser.Const n) =
	    Imm n
    in
	Program(map_func_node node)
    end

fun generate_asm (Program func) =
    let
	fun generate_func_instructions (node, depth) =
	    case node of
		Function(name, ins_list) => (let val header = ".globl _" ^ name ^ "\n" ^ "_" ^ name ^ ":\n"
						 val instructions = List.map (fn ins => generate_instructions(ins, depth+1)) ins_list
					     in header ^ Utils.string_multiply("  ", depth) ^ List.foldl (op ^) "" instructions end)
						
	and generate_instructions (node, depth) =
	    let
		val instructions = (
		    case node of
			(Mov(src, dst)) => "movl " ^ generate_operands src ^ ", " ^ generate_operands dst ^ "\n"
		      | Ret => "retq\n")
	    in
		Utils.string_multiply("  ", depth) ^ instructions
	    end
	and generate_operands (Imm n) =
	    "$" ^ Int.toString n
	  | generate_operands Register = "%eax"
    in
	generate_func_instructions (func, 0)
    end
end

fun write_to_file file asm =
    let
	val outfile = TextIO.openOut file
    in
	(TextIO.output(outfile, asm);
	 TextIO.flushOut(outfile);
	 TextIO.closeOut(outfile))
    end
