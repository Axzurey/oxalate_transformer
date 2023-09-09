import ts from 'typescript';

export function find_first_child(node: ts.Node, predicate: (e: ts.Node) => boolean) {
    for (let child of node.getChildren()) {
        if (predicate(child)) return child;
    }
};

export function find_first_descendant(node: ts.Node, predicate: (e: ts.Node) => boolean) {
    let q = [...node.getChildren()];

    while (q.length > 0) {
        let e = q.shift()!;
        if (predicate(e)) return e;
        let ch = e.getChildren();
        if (ch.length > 0) {
            q.push(...ch)
        }
    }
}