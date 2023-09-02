import ts, { Block, ExpressionStatement } from "typescript";
import {} from "ts-expose-internals";


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
				if (ts.isExpressionStatement(c)) {
					for (let subchild of c.getChildren()) {
						if (ts.isNonNullExpression(subchild)) {
							let out = visitNonNull(context, subchild);
							let children = child.getChildren();
							children[i] = out;
							return factory.updateBlock(node, node.getChildren().map(v => ts.isSyntaxList(v) ? v.getChildren() : v).flat(1) as ts.Statement[]);
						}
					}
				}
				else if (ts.isVariableStatement(c)) {
					let v_declarationlist = c.getChildAt(0) as ts.VariableDeclarationList;
					let v_declaration = v_declarationlist.getChildAt(1).getChildAt(0) as ts.VariableDeclaration;

					for (let subchild of v_declaration.getChildren()) {
						if (ts.isNonNullExpression(subchild)) {
							let out = visitNonNull(context, subchild);
							let children = child.getChildren();
							children[i] = out;
							children.splice(i + 1, 0, 
								factory.createVariableStatement(
									undefined, factory.createVariableDeclarationList([
										factory.createVariableDeclaration(
											v_declaration.getChildAt(0) as ts.Identifier,
											undefined,
											undefined,
											subchild.getChildAt(0) as ts.Expression
										)
									], v_declarationlist.flags)
								)
							)
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

function visitNonNull(context: TransformContext, node: ts.NonNullExpression) {
    const {factory} = context;

    let children = node.getChildren();

	console.log('start');

    for (let child of children) {
        if (ts.isCallExpression(child)) {
            for (let subchild of child.getChildren()) {
                if (ts.isPropertyAccessExpression(subchild)) {
                    let [left, right] = [subchild.getChildAt(0) as ts.Identifier, subchild.getChildAt(1) as ts.Identifier];
					return factory.createIfStatement(
						factory.createCallExpression(
							factory.createPropertyAccessExpression(
								left, "is_none"
							), undefined, []
						),
						factory.createBlock(
						  [factory.createReturnStatement(left)],
						  true
						)
					);
                }
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