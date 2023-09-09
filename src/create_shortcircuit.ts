import ts, { VariableDeclarationList } from "typescript";
import { find_first_child, find_first_descendant } from "./utility";
import { TransformContext } from "./transformer";

let temp_variable_increment = 0;

export function get_return_type_from_function_identifier(context: TransformContext, node: ts.Identifier) {
    let typechecker = context.program.getTypeChecker();

    let _type = typechecker.getTypeAtLocation(node);

    let declarationx = _type.symbol.getDeclarations()![0] as ts.SignatureDeclaration;

	let type = typechecker.getReturnTypeOfSignature(typechecker.getSignatureFromDeclaration(declarationx)!);

    return type;
}

export function get_variable_type(context: TransformContext, node: ts.Identifier) {
    let typechecker = context.program.getTypeChecker();

    let type = typechecker.getTypeAtLocation(node);

    return type;
}

export function is_function_call_shortable(node: ts.VariableStatement | ts.ExpressionStatement): boolean {
    return find_first_descendant(node, (e) => ts.isCallExpression(e)) !== undefined;
}

export function is_variable_assignment_shortable(node: ts.VariableStatement | ts.ExpressionStatement): boolean {
    return find_first_descendant(node, (e) => {
        return ts.isVariableDeclaration(e) && ts.isIdentifier(e.getChildAt(0)) && ts.isNonNullExpression(e.getChildAt(2)) && ts.isIdentifier(e.getChildAt(2).getChildAt(0))}
    ) !== undefined;
}

/**
this should be the final check
 */
export function is_an_expression_shortable(node: ts.VariableStatement | ts.ExpressionStatement): boolean {
    return find_first_descendant(node, (e) => ts.isExpression(e)) !== undefined;
}

export function short_circuit_variable_assignment(context: TransformContext, statement: ts.VariableStatement | ts.ExpressionStatement) {
    const { factory } = context;
    
    if (ts.isVariableStatement(statement)) {
        let variable_declaration_list = statement.getChildAt(0) as VariableDeclarationList;
        let variable_declaration_list_syntax = statement.getChildAt(0).getChildAt(1); //dive through the syntax list
        let variable_declaration = variable_declaration_list_syntax.getChildAt(0) as ts.VariableDeclaration;

        let non_null_expression = find_first_child(variable_declaration, (e) => ts.isNonNullExpression(e))! as ts.NonNullExpression;
        
        let variable_identifier = variable_declaration.getChildAt(0) as ts.Identifier;
        let variable_identifier_target = non_null_expression.getChildAt(0) as ts.Identifier;

        let fn_return_name = get_variable_type(context, variable_identifier_target).symbol.getEscapedName();

        console.log(fn_return_name)

        if (fn_return_name !== "Result" && fn_return_name !== "Option") return;

        let return_check_statement = factory.createIfStatement(
            factory.createCallExpression(
                factory.createPropertyAccessExpression(
                    variable_identifier_target, fn_return_name === "Option" ? "is_none" : "is_err"
                ), undefined, []
            ),
            factory.createBlock(
                [factory.createReturnStatement(variable_identifier_target)],
                true
            )
        );

        let value_set = factory.createVariableStatement(
            undefined,
            factory.createVariableDeclarationList([
                factory.createVariableDeclaration(
                    variable_identifier, undefined, undefined,
                    variable_identifier_target
                )
            ], variable_declaration_list.flags)
        )

        temp_variable_increment ++;

        return [return_check_statement, value_set] as const;
    }
    else {

        console.log(statement.getChildren().map((v) => ts.Debug.formatSyntaxKind(v.kind)))

        let non_null_expression = find_first_child(statement, (e) => ts.isNonNullExpression(e))! as ts.NonNullExpression;
        let variable_identifier = find_first_descendant(non_null_expression, (e) => ts.isIdentifier(e)) as ts.Identifier;

        let fn_return_name = get_variable_type(context, variable_identifier).symbol.getEscapedName();

        if (fn_return_name !== "Result" && fn_return_name !== "Option") return;

        let return_check_statement = factory.createIfStatement(
            factory.createCallExpression(
                factory.createPropertyAccessExpression(
                    non_null_expression, fn_return_name === "Option" ? "is_none" : "is_err"
                ), undefined, []
            ),
            factory.createBlock(
                [factory.createReturnStatement(non_null_expression)],
                true
            )
        );

        temp_variable_increment ++;

        return [return_check_statement] as const;
    }
}

/**
 * let x = somefunc(args)!
 * 
 * becomes
 * 
 * let _temp_x = somefunc(args);
 * 
 * if (_temp_x.is_none()) return _temp_x;
 * 
 * let x = _temp_x;
 */
export function short_circuit_function_call(context: TransformContext, statement: ts.VariableStatement | ts.ExpressionStatement) {
    const { factory } = context;
    
    if (ts.isVariableStatement(statement)) {
        let variable_declaration_list = statement.getChildAt(0) as VariableDeclarationList;
        let variable_declaration_list_syntax = statement.getChildAt(0).getChildAt(1); //dive through the syntax list
        let variable_declaration = variable_declaration_list_syntax.getChildAt(0) as ts.VariableDeclaration;

        let variable_identifier = find_first_child(variable_declaration, (e) => ts.isIdentifier(e))! as ts.Identifier;

        let non_null_expression = find_first_child(variable_declaration, (e) => ts.isNonNullExpression(e))! as ts.NonNullExpression;
        let call_expression = find_first_descendant(non_null_expression, (e) => ts.isCallExpression(e))! as ts.CallExpression;
        let call_identifier = find_first_descendant(call_expression, (e) => ts.isIdentifier(e)) as ts.Identifier;

        let fn_return_type = get_return_type_from_function_identifier(context, call_identifier);

        let fn_return_name = fn_return_type.symbol.getEscapedName();

        if (fn_return_name !== "Result" && fn_return_name !== "Option") return;

        let v_dec = factory.createVariableDeclaration(
            `_temp_v${temp_variable_increment}`, undefined, undefined,
            call_expression
        );

        let temporary_variable_statement = factory.createVariableStatement(
            undefined,
            factory.createVariableDeclarationList([
                v_dec
            ], ts.NodeFlags.Const)
        );

        let return_check_statement = factory.createIfStatement(
            factory.createCallExpression(
                factory.createPropertyAccessExpression(
                    factory.createIdentifier(`_temp_v${temp_variable_increment}`), fn_return_name === "Option" ? "is_none" : "is_err"
                ), undefined, []
            ),
            factory.createBlock(
                [factory.createReturnStatement(factory.createIdentifier(`_temp_v${temp_variable_increment}`))],
                true
            )
        );

        let value_set = factory.createVariableStatement(
            undefined,
            factory.createVariableDeclarationList([
                factory.createVariableDeclaration(
                    variable_identifier, undefined, undefined,
                    factory.createIdentifier(`_temp_v${temp_variable_increment}`)
                )
            ], variable_declaration_list.flags)
        )

        temp_variable_increment ++;

        return [temporary_variable_statement, return_check_statement, value_set] as const;
    }
    else {

        let non_null_expression = find_first_child(statement, (e) => ts.isNonNullExpression(e))! as ts.NonNullExpression;
        let call_expression = find_first_descendant(non_null_expression, (e) => ts.isCallExpression(e))! as ts.CallExpression;
        let call_identifier = find_first_descendant(call_expression, (e) => ts.isIdentifier(e)) as ts.Identifier;

        let fn_return_type = get_return_type_from_function_identifier(context, call_identifier);

        let fn_return_name = fn_return_type.symbol.getEscapedName();

        if (fn_return_name !== "Result" && fn_return_name !== "Option") return;

        let v_dec = factory.createVariableDeclaration(
            `_temp_v${temp_variable_increment}`, undefined, undefined,
            call_expression
        );

        let temporary_variable_statement = factory.createVariableStatement(
            undefined,
            factory.createVariableDeclarationList([
                v_dec
            ], ts.NodeFlags.Const)
        );

        let return_check_statement = factory.createIfStatement(
            factory.createCallExpression(
                factory.createPropertyAccessExpression(
                    factory.createIdentifier(`_temp_v${temp_variable_increment}`), fn_return_name === "Option" ? "is_none" : "is_err"
                ), undefined, []
            ),
            factory.createBlock(
                [factory.createReturnStatement(factory.createIdentifier(`_temp_v${temp_variable_increment}`))],
                true
            )
        );

        temp_variable_increment ++;

        return [temporary_variable_statement, return_check_statement] as const;
    }
}