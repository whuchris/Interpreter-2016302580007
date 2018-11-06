#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "../include/KaleidoscopeJIT.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;
using namespace llvm::orc;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token {
	tok_eof = -1,

	// commands
	tok_FUNC = -2,
	tok_error = -3,

	// primary
	tok_identifier = -4,
	tok_number = -5,
	tok_text = -6,
	// control
	tok_IF = -7,
	tok_FI = -8,
	tok_THEN = -9,
	tok_ELSE = -10,
	tok_DO = -11,
	tok_WHILE = -12,
	tok_RETURN = -13,
	tok_PRINT = -14,

	tok_VAR = -15,
	tok_assign = -16,
	tok_var = -17,
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal;             // Filled in if tok_number
static std::string text;
/// gettok - Return the next token from standard input.
static int gettok() {
	static int LastChar = ' ';

	// Skip any whitespace.
	while (isspace(LastChar))
		LastChar = getchar();

	if (isalpha(LastChar)) {
		IdentifierStr = "";
		IdentifierStr += LastChar;
		while (isalnum((LastChar = getchar())))
			IdentifierStr += LastChar;

		if (IdentifierStr == "FUNC")
			return tok_FUNC;
		if (IdentifierStr == "IF")
			return tok_IF;
		if (IdentifierStr == "FI")
			return tok_FI;
		if (IdentifierStr == "THEN")
			return tok_THEN;
		if (IdentifierStr == "ELSE")
			return tok_ELSE;
		if (IdentifierStr == "RETURN")
			return tok_RETURN;
		if (IdentifierStr == "PRINT")
			return tok_PRINT;
		if (IdentifierStr == "VAR")
			return tok_VAR;
		return tok_identifier;
	}

	if (isdigit(LastChar)) { // Number: [0-9.]+
		std::string NumStr;
		do {
			NumStr += LastChar;
			LastChar = getchar();
		} while (isdigit(LastChar));

		NumVal = strtod(NumStr.c_str(), nullptr);
		return tok_number;
	}

	if (LastChar == '/') {
		// Comment until end of line.
		LastChar = getchar();
		if (LastChar == '/') {
			do
				LastChar = getchar();
			while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

			if (LastChar != EOF)
				return gettok();
		}
		else {
			return '/';
		}
		return tok_error;
	}

	// Check for end of file.  Don't eat the EOF.

	if (LastChar == '"') {
		text = "";
		while ((LastChar = getchar()) != '"' && LastChar != '\n')
			text += LastChar;
		if (LastChar == '"') {
			LastChar = getchar();
			return tok_text;
		}
		return tok_error;
	}

	if (LastChar == ':') {
		LastChar = getchar();
		if (LastChar == '=') {
			LastChar = getchar();
			return tok_assign;
		}
		return tok_error;
	}
	if (LastChar == EOF)
		return tok_eof;
	// Otherwise, just return the character as its ascii value.
	int ThisChar = LastChar;
	LastChar = getchar();
	return ThisChar;
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

namespace {

	/// ExprAST - Base class for all expression nodes.
	class ExprAST {
	public:
		virtual ~ExprAST() = default;

		virtual Value *codegen() = 0;
	};
	class StateAST {
	public:
		virtual ~StateAST() = default;
		virtual Value* codegen() = 0;
	};

	/// NumberExprAST - Expression class for numeric literals like "1.0".
	class NumberExprAST : public ExprAST {
		double Val;

	public:
		NumberExprAST(int Val) : Val(Val) {}

		Value *codegen() override;
	};

	/// VariableExprAST - Expression class for referencing a variable, like "a".
	class VariableExprAST : public ExprAST {
		std::string Name;

	public:
		VariableExprAST(const std::string &Name) : Name(Name) {}

		Value *codegen() override;
	};

	/// BinaryExprAST - Expression class for a binary operator.
	class BinaryExprAST : public ExprAST {
		char Op;
		std::unique_ptr<ExprAST> LHS, RHS;

	public:
		BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
			std::unique_ptr<ExprAST> RHS)
			: Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

		Value *codegen() override;
	};

	/// CallExprAST - Expression class for function calls.
	class CallExprAST : public ExprAST {
		std::string Callee;
		std::vector<std::unique_ptr<ExprAST>> Args;

	public:
		CallExprAST(const std::string &Callee,
			std::vector<std::unique_ptr<ExprAST>> Args)
			: Callee(Callee), Args(std::move(Args)) {}

		Value *codegen() override;
	};

	/// IfExprAST - Expression class for if/then/else.


	/// ForExprAST - Expression class for for/in.
	/// PrototypeAST - This class represents the "prototype" for a function,
	/// which captures its name, and its argument names (thus implicitly the number
	/// of arguments the function takes).
	class PrototypeAST {
		std::string Name;
		std::vector<std::string> Args;

	public:
		PrototypeAST(const std::string &Name, std::vector<std::string> Args)
			: Name(Name), Args(std::move(Args)) {}

		Function *codegen();
		const std::string &getName() const { return Name; }
	};

	/// FunctionAST - This class represents a function definition itself.
	class FunctionAST {
		std::unique_ptr<PrototypeAST> Proto;
		std::unique_ptr<ExprAST> Body;

	public:
		FunctionAST(std::unique_ptr<PrototypeAST> Proto,
			std::unique_ptr<ExprAST> Body)
			: Proto(std::move(Proto)), Body(std::move(Body)) {}

		Function *codegen();
	};

	/*class PRINTExprAST : public ExprAST {
	std::vector<std::unique_ptr<ExprAST>> Items;

	public:
	PRINTExprAST(std::vector<std::unique_ptr<ExprAST>> items)
	: Items(std::move(items)) {}
	Value *codegen() override;
	};
	class AssignExprAST :public ExprAST {
	std::unique_ptr<ExprAST> Exp;
	std::string Name;
	public:
	AssignExprAST(const std::string &Name, std::unique_ptr<ExprAST> Exp): Name(Name), Exp(std::move(Exp)) {}
	Value *codegen() override;
	};

	class RETURNExprAST : public ExprAST {
	std::unique_ptr<ExprAST> Expr;

	public:
	RETURNExprAST(std::unique_ptr<ExprAST> expr) : Expr(std::move(expr)) {}
	Value *codegen() override;
	};*/
	class BlockExprAST : public StateAST {
		std::vector<std::unique_ptr<StateAST>> Statement;
	public:
		BlockExprAST(std::vector<std::unique_ptr<StateAST>> Statement) : Statement(std::move(Statement)) {}
		Value *codegen() override;
	};
	class IfStateAST : public StateAST {
		std::unique_ptr<ExprAST> Cond;
		std::unique_ptr<StateAST> Then, Else;

	public:
		IfStateAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<StateAST> Then,
			std::unique_ptr<StateAST> Else)
			: Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}

		Value *codegen() override;
	};
} // end anonymous namespace

  //===----------------------------------------------------------------------===//
  // Parser
  //===----------------------------------------------------------------------===//

  /// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
  /// token the parser is looking at.  getNextToken reads another token from the
  /// lexer and updates CurTok with its results.
static int CurTok;
static int getNextToken() { return CurTok = gettok(); }

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() {
	if (!isascii(CurTok))
		return -1;

	// Make sure it's a declared binop.
	int TokPrec = BinopPrecedence[CurTok];
	if (TokPrec <= 0)
		return -1;
	return TokPrec;
}

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char *Str) {
	fprintf(stderr, "Error: %s\n", Str);
	return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
	LogError(Str);
	return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();
static std::unique_ptr<StateAST> ParseStatement();
/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
	auto Result = llvm::make_unique<NumberExprAST>(NumVal);
	getNextToken(); // consume the number
	return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
	getNextToken(); // eat (.
	auto V = ParseExpression();
	if (!V)
		return nullptr;

	if (CurTok != ')')
		return LogError("expected ')'");
	getNextToken(); // eat ).
	return V;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
	std::string IdName = IdentifierStr;

	getNextToken(); // eat identifier.

	if (CurTok != '(') // Simple variable ref.
		return llvm::make_unique<VariableExprAST>(IdName);

	// Call.
	getNextToken(); // eat (
	std::vector<std::unique_ptr<ExprAST>> Args;
	if (CurTok != ')') {
		while (true) {
			if (auto Arg = ParseExpression())
				Args.push_back(std::move(Arg));
			else
				return nullptr;

			if (CurTok == ')')
				break;

			if (CurTok != ',')
				return LogError("Expected ')' or ',' in argument list");
			getNextToken();
		}
	}

	// Eat the ')'.
	getNextToken();

	return llvm::make_unique<CallExprAST>(IdName, std::move(Args));
}
/// ifexpr ::= 'if' expression 'then' expression 'else' expression
static std::unique_ptr<StateAST> ParseIfExpr() {
	getNextToken(); // eat the if.

					// condition.
	if (auto E = ParseExpression()) {
		if (CurTok != tok_THEN) {
			LogError("expected then");
			return nullptr;
		}
		getNextToken();
		if (auto M = ParseStatement()) {
			if (CurTok == tok_ELSE) {
				if (auto Me = ParseStatement()) {
					if (CurTok == tok_FI) {
						getNextToken();
						return llvm::make_unique<IfStateAST>(std::move(E), std::move(Me));
					}
					else {
						LogError("expected fi");
						return nullptr;
					}
				}
				else {
					return nullptr;
				}
			}
			else if (CurTok == tok_FI) {
				getNextToken();
				return llvm::make_unique<IfStateAST>(std::move(E), std::move(M));
			}
			else {
				LogError("expected fi");
				return nullptr;
			}
		}
		return nullptr;
	}
	return nullptr;
}

/// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
///   ::= ifexpr
///   ::= forexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
	switch (CurTok) {
	default:
		return LogError("unknown token when expecting an expression");
	case tok_identifier:
		return ParseIdentifierExpr();
	case tok_number:
		return ParseNumberExpr();
	case '(':
		return ParseParenExpr();
	}
}

/// binoprhs
///   ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
	std::unique_ptr<ExprAST> LHS) {
	// If this is a binop, find its precedence.
	while (true) {
		int TokPrec = GetTokPrecedence();

		// If this is a binop that binds at least as tightly as the current binop,
		// consume it, otherwise we are done.
		if (TokPrec < ExprPrec)
			return LHS;

		// Okay, we know this is a binop.
		int BinOp = CurTok;
		getNextToken(); // eat binop

						// Parse the primary expression after the binary operator.
		auto RHS = ParsePrimary();
		if (!RHS)
			return nullptr;

		// If BinOp binds less tightly with RHS than the operator after RHS, let
		// the pending operator take RHS as its LHS.
		int NextPrec = GetTokPrecedence();
		if (TokPrec < NextPrec) {
			RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
			if (!RHS)
				return nullptr;
		}

		// Merge LHS/RHS.
		LHS =
			llvm::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
	}
}

/// expression
///   ::= primary binoprhs
///
static std::unique_ptr<ExprAST> ParseExpression() {
	auto LHS = ParsePrimary();
	if (!LHS)
		return nullptr;

	return ParseBinOpRHS(0, std::move(LHS));
}
/*static std::unique_ptr<AssignExprAST> ParseAssign() {
std::string Vname = IdentifierStr;
getNextToken();
if (CurTok != tok_assign) {
LogError("Expected ':=' in prototype");
return nullptr;
}
getNextToken();
if (auto E = ParseExpression())
return llvm::make_unique<AssignExprAST>(Vname, std::move(E));
return nullptr;
}*/
/// prototype
///   ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype() {
	if (CurTok != tok_identifier)
		return LogErrorP("Expected function name in prototype");

	std::string FnName = IdentifierStr;
	getNextToken();

	if (CurTok != '(')
		return LogErrorP("Expected '(' in prototype");

	std::vector<std::string> ArgNames;

	while (getNextToken() == tok_identifier)
		ArgNames.push_back(IdentifierStr);

	if (CurTok != ')')
		return LogErrorP("Expected ')' in prototype");

	// success.
	getNextToken(); // eat ')'.

	return llvm::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

/// definition ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseFunc() {
	getNextToken(); // eat FUNC.
	auto Proto = ParsePrototype();
	if (!Proto)
		return nullptr;
	if (CurTok != '{')
	{
		LogErrorP("Expected '{' in function");
		return nullptr;
	}
	getNextToken();

	auto E = ParseStatement();
	if (!E)
		return nullptr;
	if (CurTok != '}')
	{
		LogErrorP("Expected '}' in function");
		return nullptr;
	}
	getNextToken();

	return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
}
/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
	if (auto E = ParseExpression()) {
		// Make an anonymous proto.
		auto Proto = llvm::make_unique<PrototypeAST>("__anon_expr",
			std::vector<std::string>());
		return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
	}
	return nullptr;
}

/// external ::= 'extern' prototype
/*static std::unique_ptr<ExprAST> ParsePRINT() {
getNextToken();
return nullptr;
}
static std::unique_ptr<ExprAST> ParseRETURN() {

if (CurTok != tok_RETURN)
return LogError("Expected RETURN in Expression");
getNextToken();
return llvm::make_unique<RETURNExprAST>(std::move(ParseExpression()));
}
*/
static std::unique_ptr<StateAST>ParseStatement() {
	switch (CurTok) {
	case tok_IF:
		return ParseIfExpr();
		break;
	default:
		if (auto E = ParseExpression())
			return llvm::make_unique<StateAST>(std::move(CurTok), std::move(E));
		break;
	}
	return nullptr;
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;
static std::map<std::string, Value *> NamedValues;
static std::unique_ptr<legacy::FunctionPassManager> TheFPM;
static std::unique_ptr<KaleidoscopeJIT> TheJIT;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;

Value *LogErrorV(const char *Str) {
	LogError(Str);
	return nullptr;
}

Function *getFunction(std::string Name) {
	// First, see if the function has already been added to the current module.
	if (auto *F = TheModule->getFunction(Name))
		return F;

	// If not, check whether we can codegen the declaration from some existing
	// prototype.
	auto FI = FunctionProtos.find(Name);
	if (FI != FunctionProtos.end())
		return FI->second->codegen();

	// If no existing prototype exists, return null.
	return nullptr;
}

Value *NumberExprAST::codegen() {
	return ConstantInt::get(TheContext, APInt(32, Val, true));
}

Value *VariableExprAST::codegen() {
	// Look this variable up in the function.
	Value *V = NamedValues[Name];
	if (!V)
		return LogErrorV("Unknown variable name");
	return V;
}

Value *BinaryExprAST::codegen() {
	Value *L = LHS->codegen();
	Value *R = RHS->codegen();
	if (!L || !R)
		return nullptr;

	switch (Op) {
	case '+':
		return Builder.CreateFAdd(L, R, "addtmp");
	case '-':
		return Builder.CreateFSub(L, R, "subtmp");
	case '*':
		return Builder.CreateFMul(L, R, "multmp");
	case '/':
		L = Builder.CreateFCmpULT(L, R, "cmptmp");
		// Convert bool 0/1 to double 0.0 or 1.0
		return Builder.CreateUIToFP(L, Type::getInt32Ty(TheContext), "booltmp");
	default:
		return LogErrorV("invalid binary operator");
	}
}

Value *CallExprAST::codegen() {
	// Look up the name in the global module table.
	Function *CalleeF = getFunction(Callee);
	if (!CalleeF)
		return LogErrorV("Unknown function referenced");

	// If argument mismatch error.
	if (CalleeF->arg_size() != Args.size())
		return LogErrorV("Incorrect # arguments passed");

	std::vector<Value *> ArgsV;
	for (unsigned i = 0, e = Args.size(); i != e; ++i) {
		ArgsV.push_back(Args[i]->codegen());
		if (!ArgsV.back())
			return nullptr;
	}

	return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

Value *IfStateAST::codegen() {
	Value *CondV = Cond->codegen();
	if (!CondV)
		return nullptr;

	// Convert condition to a bool by comparing non-equal to 0.0.
	CondV = Builder.CreateFCmpONE(
		CondV, ConstantFP::get(TheContext, APFloat(0.0)), "ifcond");

	Function *TheFunction = Builder.GetInsertBlock()->getParent();

	// Create blocks for the then and else cases.  Insert the 'then' block at the
	// end of the function.
	BasicBlock *ThenBB = BasicBlock::Create(TheContext, "then", TheFunction);
	BasicBlock *ElseBB = BasicBlock::Create(TheContext, "else");
	BasicBlock *MergeBB = BasicBlock::Create(TheContext, "ifcont");

	Builder.CreateCondBr(CondV, ThenBB, ElseBB);

	// Emit then value.
	Builder.SetInsertPoint(ThenBB);

	Value *ThenV = Then->codegen();
	if (!ThenV)
		return nullptr;

	Builder.CreateBr(MergeBB);
	// Codegen of 'Then' can change the current block, update ThenBB for the PHI.
	ThenBB = Builder.GetInsertBlock();

	// Emit else block.
	TheFunction->getBasicBlockList().push_back(ElseBB);
	Builder.SetInsertPoint(ElseBB);

	Value *ElseV = Else->codegen();
	if (!ElseV)
		return nullptr;

	Builder.CreateBr(MergeBB);
	// Codegen of 'Else' can change the current block, update ElseBB for the PHI.
	ElseBB = Builder.GetInsertBlock();

	// Emit merge block.
	TheFunction->getBasicBlockList().push_back(MergeBB);
	Builder.SetInsertPoint(MergeBB);
	PHINode *PN = Builder.CreatePHI(Type::getDoubleTy(TheContext), 2, "iftmp");

	PN->addIncoming(ThenV, ThenBB);
	PN->addIncoming(ElseV, ElseBB);
	return PN;
}
/*Value *RETURNExprAST::codegen() {
Value *value = Expr->codegen();
return value;
}
Value *PRINTExprAST::codegen() {
return nullptr;
}
Value *AssignExprAST::codegen() {
Value *value = Exp->codegen();

Value *v = NamedValues[Name];
if (!v)
return LogErrorV("Unknown variable name");
NamedValues[Name] = value;
return Builder.CreateStore(value, TheModule->getGlobalVariable(this->Name));
}
Value *BlockExprAST::codegen() {
return nullptr;
}*/

Function *PrototypeAST::codegen() {
	// Make the function type:  double(double,double) etc.
	Function *TheFunction = TheModule->getFunction(Name);
	if (TheFunction)
		return (Function*)LogErrorV("Function cannot be redefined.");
	std::vector<Type *> Integers(Args.size(), Type::getInt32Ty(TheContext));
	FunctionType *FT =
		FunctionType::get(Type::getInt32Ty(TheContext), Integers, false);

	Function *F =
		Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

	// Set names for all arguments.
	unsigned Idx = 0;
	for (auto &Arg : F->args())
		Arg.setName(Args[Idx++]);

	return F;
}
Function *FunctionAST::codegen() {
	// Transfer ownership of the prototype to the FunctionProtos map, but keep a
	// reference to it for use below.
	auto &P = *Proto;
	FunctionProtos[Proto->getName()] = std::move(Proto);
	Function *TheFunction = getFunction(P.getName());
	if (!TheFunction)
		return nullptr;

	// Create a new basic block to start insertion into.
	BasicBlock *BB = BasicBlock::Create(TheContext, "entry", TheFunction);
	Builder.SetInsertPoint(BB);

	// Record the function arguments in the NamedValues map.
	NamedValues.clear();
	for (auto &Arg : TheFunction->args())
		NamedValues[Arg.getName()] = &Arg;

	/* if (Value *RetVal = Body->codegen()) {
	// Finish off the function.
	Builder.CreateRet(RetVal);

	// Validate the generated code, checking for consistency.
	verifyFunction(*TheFunction);

	// Run the optimizer on the function.
	TheFPM->run(*TheFunction);

	return TheFunction;
	}*/

	// Error reading body, remove function.
	TheFunction->eraseFromParent();
	return nullptr;
}

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

static void InitializeModuleAndPassManager() {
	// Open a new module.
	TheModule = llvm::make_unique<Module>("my cool jit", TheContext);
	TheModule->setDataLayout(TheJIT->getTargetMachine().createDataLayout());

	// Create a new pass manager attached to it.
	TheFPM = llvm::make_unique<legacy::FunctionPassManager>(TheModule.get());

	// Do simple "peephole" optimizations and bit-twiddling optzns.
	TheFPM->add(createInstructionCombiningPass());
	// Reassociate expressions.
	TheFPM->add(createReassociatePass());
	// Eliminate Common SubExpressions.
	TheFPM->add(createGVNPass());
	// Simplify the control flow graph (deleting unreachable blocks, etc).
	TheFPM->add(createCFGSimplificationPass());

	TheFPM->doInitialization();
}

static void HandleDefinition() {
	if (auto FnAST = ParseFunc()) {
		if (auto *FnIR = FnAST->codegen()) {
			fprintf(stderr, "Read function definition:");
			FnIR->print(errs());
			fprintf(stderr, "\n");
			TheJIT->addModule(std::move(TheModule));
			InitializeModuleAndPassManager();
		}
	}
	else {
		// Skip token for error recovery.
		getNextToken();
	}
}


static void HandleTopLevelExpression() {
	// Evaluate a top-level expression into an anonymous function.
	if (auto FnAST = ParseTopLevelExpr()) {
		if (FnAST->codegen()) {
			// JIT the module containing the anonymous expression, keeping a handle so
			// we can free it later.
			auto H = TheJIT->addModule(std::move(TheModule));
			InitializeModuleAndPassManager();

			// Search the JIT for the __anon_expr symbol.
			auto ExprSymbol = TheJIT->findSymbol("__anon_expr");
			assert(ExprSymbol && "Function not found");

			// Get the symbol's address and cast it to the right type (takes no
			// arguments, returns a double) so we can call it as a native function.
			int(*FP)() = (int(*)())cantFail(ExprSymbol.getAddress());
			fprintf(stderr, "Evaluated to %d\n", FP());

			// Delete the anonymous expression module from the JIT.
			TheJIT->removeModule(H);
		}
	}
	else {
		// Skip token for error recovery.
		getNextToken();
	}
}

/// top ::= definition | external | expression | ';'
static void MainLoop() {
	while (true) {
		fprintf(stderr, "ready> ");
		switch (CurTok) {
		case tok_eof:
			return;
		case ';': // ignore top-level semicolons.
			getNextToken();
			break;
		case tok_FUNC:
			HandleDefinition();
			break;
		default:
			HandleTopLevelExpression();
			break;
		}
	}
}

//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

#ifdef LLVM_ON_WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X) {
	fputc((char)X, stderr);
	return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X) {
	fprintf(stderr, "%f\n", X);
	return 0;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main() {
	InitializeNativeTarget();
	InitializeNativeTargetAsmPrinter();
	InitializeNativeTargetAsmParser();

	// Install standard binary operators.
	// 1 is lowest precedence.
	BinopPrecedence['+'] = 10;
	BinopPrecedence['-'] = 10;
	BinopPrecedence['*'] = 20;
	BinopPrecedence['/'] = 20; // highest.

							   // Prime the first token.
	fprintf(stderr, "ready> ");
	getNextToken();

	TheJIT = llvm::make_unique<KaleidoscopeJIT>();

	InitializeModuleAndPassManager();

	// Run the main "interpreter loop" now.
	MainLoop();
	TheModule->print(errs(), nullptr);
	return 0;
}
