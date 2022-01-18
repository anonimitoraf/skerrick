type NamespaceValuesByKey = Map<string, any>;

const namespaces = new Map<string, NamespaceValuesByKey>();

let activeNamespace: string | undefined = undefined;

export function registerMember(namespace: string, key: string, value: any) {
    const ns = namespaces.get(namespace) || new Map<string, any>();
    namespaces.set(namespace, ns);
    ns.set(key, value);
    if (namespace === activeNamespace) {
        global[key] = value;
    }
}

export function setActiveNamespace(namespace: string) {
    const oldNs = activeNamespace && namespaces.get(activeNamespace);
    if (oldNs) {
        for (const k of oldNs.keys()) {
            delete global[k];
        }
    }

    const newNs = namespaces.get(namespace) || new Map<string, any>();
    namespaces.set(namespace, newNs);
    for (const [k, v] of newNs.entries()) {
        global[k] = v;
    }
    activeNamespace = namespace;
}

// Tests

setActiveNamespace('module_a')
registerMember('module_a', 'a1', function (v) { console.log('this is module a -', v) })
registerMember('module_a', 'a2', 42)

eval('a1(a2)')

setActiveNamespace('module_b')
registerMember('module_b', 'a1', function (v) { console.log('this is module b -', v) })
registerMember('module_b', 'b1', 3)

// eval('a1(a2)') // Error
eval('a1(b1)')
