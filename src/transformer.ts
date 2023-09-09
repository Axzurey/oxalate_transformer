import ts, { Block, ExpressionStatement, SignatureDeclaration, isNonNullExpression } from "typescript";
import {} from "ts-expose-internals";
import { is_an_expression_shortable, is_function_call_shortable, is_variable_assignment_shortable, short_circuit_function_call, short_circuit_variable_assignment } from "./create_shortcircuit";
import { find_first_child, find_first_descendant } from "./utility";


export interface TransformerConfig {
	_: void;
}

/**
 * This is a utility object to pass around your dependencies.
 *
 * You can also use this object to store state, e.g prereqs.
 */
export class TransformContext {
	public factory: ts.NodeFactory;

	constructor(
		public program: ts.Program,
		public context: ts.TransformationContext,
		public config: TransformerConfig,
	) {
		this.factory = context.factory;
	}

	/**
	 * Transforms the children of the specified node.
	 */
	transform<T extends ts.Node>(node: T): T {
		return ts.visitEachChild(node, (node) => visitNode(this, node), this.context);
	}
}

function visitBlock(context: TransformContext, node: ts.Block) {
	const {factory} = context;

	let i = 0;
	for (let child of node.getChildren()) {
		if (ts.isSyntaxList(child)) {
			for (let c of child.getChildren()) {
				if (!find_first_descendant(c, (e) => ts.isNonNullExpression(e))) continue;
				if ((ts.isExpressionStatement(c) || ts.isVariableStatement(c)) && !find_first_descendant(c, e => ts.isExpressionStatement(e))) {
					console.log("l")
					if (find_first_descendant(node, (e) => isNonNullExpression(e))) {
						if (is_function_call_shortable(c)) {
							console.log("l2")
							let [temp_var, return_statement, value_set] = short_circuit_function_call(context, c)!;

							let children = child.getChildren();

							children[i] = temp_var;
							
							children.splice(i + 1, 0, return_statement);

							if (value_set) {
								children.splice(i + 2, 0, value_set);
							}
						
							return factory.updateBlock(node, node.getChildren().map(v => ts.isSyntaxList(v) ? v.getChildren() : v).flat(1) as ts.Statement[]);
						}
						else if (is_variable_assignment_shortable(c) || is_an_expression_shortable(c)) {
							console.log("l3")
							let [return_statement, value_set] = short_circuit_variable_assignment(context, c)!;

							let children = child.getChildren();

							children[i] = return_statement;
							
							if (value_set) {
								children.splice(i + 1, 0, value_set);
							}
						
							return factory.updateBlock(node, node.getChildren().map(v => ts.isSyntaxList(v) ? v.getChildren() : v).flat(1) as ts.Statement[]);
						}
					}
				}
				i ++;
			}
		}
	}

	return context.transform(node);
}

let inc = 0;

function visitNonNull(context: TransformContext, node: ts.NonNullExpression, declaration?: ts.VariableDeclaration) {
    const {factory} = context;

    let children = node.getChildren();

	console.log('start');

    for (let child of children) {
        if (ts.isCallExpression(child)) {
			let left = child.getChildAt(0);

			let typechecker = context.program.getTypeChecker();

			if (ts.isIdentifier(left)) {

				if (declaration) {
					let _type = typechecker.getTypeAtLocation(left);

					if (!_type) return context.transform(node);

					let declarationx = _type.symbol.getDeclarations()![0] as SignatureDeclaration;

					let type = typechecker.getReturnTypeOfSignature(typechecker.getSignatureFromDeclaration(declarationx)!);

					console.log(type.symbol.getEscapedName());
					
					if (type.symbol.getEscapedName() !== "Result" && type.symbol.getEscapedName() !== "Option") return context.transform(node);

					return factory.createIfStatement(
						factory.createCallExpression(
							factory.createPropertyAccessExpression(
								declaration.getChildAt(0) as ts.Identifier, type.symbol.getEscapedName() === "Option" ? "is_none" : "is_err"
							), undefined, []
						),
						factory.createBlock(
							[factory.createReturnStatement(declaration.getChildAt(0) as ts.Identifier)],
							true
						)
					);
				}
				else {
					let _type = typechecker.getTypeAtLocation(left);

					if (!_type) return context.transform(node);

					let declarationx = _type.symbol.getDeclarations()![0] as SignatureDeclaration;

					let type = typechecker.getReturnTypeOfSignature(typechecker.getSignatureFromDeclaration(declarationx)!);

					console.log(type.symbol.getEscapedName());
					
					if (type.symbol.getEscapedName() !== "Result" && type.symbol.getEscapedName() !== "Option") return context.transform(node);

					let variable_declaration = factory.createVariableStatement(
						undefined,
						factory.createVariableDeclarationList([
							factory.createVariableDeclaration(
								`_temp_${inc}`, undefined, undefined,
								node.parent.parent.getChildAt(0) as ts.Expression
							)
						])
					);

					node.parent.parent.getChildren().push(variable_declaration);

					return factory.createIfStatement(
						factory.createCallExpression(
							factory.createPropertyAccessExpression(
								variable_declaration.getChildAt(0) as ts.Identifier, type.symbol.getEscapedName() === "Option" ? "is_none" : "is_err"
							), undefined, []
						),
						factory.createBlock(
							[factory.createReturnStatement(variable_declaration.getChildAt(0) as ts.Identifier)],
							true
						)
					);
				}
			}
			else if (ts.isCallExpression(left)) {
				console.log('doing call expression')
				let l = left.getChildAt(0) as ts.Identifier;

				let type = typechecker.getTypeAtLocation(l);

				if (!type) return context.transform(node);
				
				if (type.symbol.getEscapedName() !== "Result" && type.symbol.getEscapedName() !== "Option") return context.transform(node);

				if (declaration) {
					factory.updateVariableDeclaration(declaration, declaration.name, undefined, undefined, l);
				}
				return factory.createIfStatement(
					factory.createCallExpression(
						factory.createPropertyAccessExpression(
							l, type.symbol.getEscapedName() === "Option" ? "is_none" : "is_err"
						), undefined, []
					),
					factory.createBlock(
						[factory.createReturnStatement(l)],
						true
					)
				);
			}
        }
    }

	return context.transform(node);
}


function visitNode(context: TransformContext, node: ts.Node): ts.Node | ts.Node[] {
	if (ts.isBlock(node)) {
		console.log('vis block')
		return visitBlock(context, node);
	};

	return context.transform(node);
}