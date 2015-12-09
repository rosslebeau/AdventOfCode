import Foundation

var resolvedVals = [String: Int]()

func wiresFromInput(input: String) -> [String: String] {
    var wires = [String: String]()
    let lines = input.componentsSeparatedByCharactersInSet(NSCharacterSet.newlineCharacterSet()).filter{!$0.isEmpty}
    
    for line in lines {
        let components = line.componentsSeparatedByString(" -> ")
        if components.count == 2 {
            let sig = components[0]
            let name = components[1]
            wires[name] = sig
        }
    }

    return wires
}

enum ResolutionError : ErrorType {
    case WireNameNotFound(name: String)
}

func resolveVal(name: String, wires: [String: String]) throws -> Int {
    var val: Int
    if let explicitVal = Int(name) {
        val = explicitVal
    }
    else if let foundVal = resolvedVals[name] {
        val = foundVal
    }
    else {
        guard let sig = wires[name] else {
            throw ResolutionError.WireNameNotFound(name: name)
        }
        
        do {
            if sig.componentsSeparatedByString(" AND ").count == 2 {
                let components = sig.componentsSeparatedByString(" AND ")
                val = try resolveVal(components[0], wires: wires) & resolveVal(components[1], wires: wires)
            }
            else if sig.componentsSeparatedByString(" OR ").count == 2 {
                let components = sig.componentsSeparatedByString(" OR ")
                val = try resolveVal(components[0], wires: wires) | resolveVal(components[1], wires: wires)
            }
            else if sig.componentsSeparatedByString("NOT ").count == 2 {
                let components = sig.componentsSeparatedByString("NOT ")
                val = try ~resolveVal(components[1], wires: wires)
            }
            else if sig.componentsSeparatedByString(" LSHIFT ").count == 2 {
                let components = sig.componentsSeparatedByString(" LSHIFT ")
                val = try resolveVal(components[0], wires: wires) << resolveVal(components[1], wires: wires)
            }
            else if sig.componentsSeparatedByString(" RSHIFT ").count == 2 {
                let components = sig.componentsSeparatedByString(" RSHIFT ")
                val = try (resolveVal(components[0], wires: wires) >> resolveVal(components[1], wires: wires))
            }
            else {
                val = try resolveVal(sig, wires: wires)
            }
        }
        catch let caught as ResolutionError {
            throw caught
        }
    }
    
    resolvedVals[name] = val
    return val
}

do {
    let path = "input"
    let fileContent = try NSString(contentsOfFile: path, encoding: NSUTF8StringEncoding)
    let wires = wiresFromInput(fileContent as String)
    
    let val = try resolveVal("a", wires: wires)
    print("Part 1: \(val)")
    
    resolvedVals = [String: Int]()
    resolvedVals["b"] = val
    let val2 = try resolveVal("a", wires: wires)
    print("Part 2: \(val2)")
}
catch ResolutionError.WireNameNotFound(let name) {
    print("could not resolve: " + name)
}
catch let caught as NSError {
    print("error: " + caught.description)
}
