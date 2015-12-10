import Foundation

func replaceWith(string: String, matchRegex: NSRegularExpression, replacement: String) -> String {
    let cleanString = NSMutableString(string: string)
    matchRegex.replaceMatchesInString(cleanString, options: NSMatchingOptions(rawValue: 0), range: NSMakeRange(0, cleanString.length), withTemplate:replacement);
    return cleanString as String
}

func trimEnds(string: String) -> String {
    let trimEnds = Range<String.CharacterView.Index>(start: string.characters.startIndex.successor(), end:string.characters.endIndex.predecessor())
    return string[trimEnds]
}

func part1(lines: [String]) throws -> Int {
    let hexMatch = "[0-9a-f]"
    let escapeDecodeRegex = try NSRegularExpression(pattern: "\\\\\\\\|\\\\\\\"|\\\\x" + hexMatch + hexMatch, options:NSRegularExpressionOptions(rawValue: 0))
    
    // Trim the quotes off either end of the string, then replace any escape patterns with a placeholder character
    let unescapedLines =
        lines
        .map({line in trimEnds(line)})
        .map({line in replaceWith(line, matchRegex: escapeDecodeRegex, replacement: ".")})
    
    return
        lines.map({line in line.characters.count}).reduce(0, combine:+) -
        unescapedLines.map({line in line.characters.count}).reduce(0, combine:+)
}

func part2(lines: [String]) throws -> Int {
    let escapeEncodeRegex = try NSRegularExpression(pattern: "(\\\\|\\\")", options:NSRegularExpressionOptions(rawValue: 0))
    
    let escapedLines =
        lines
        .map({line in replaceWith(line, matchRegex: escapeEncodeRegex, replacement: "\\\\$1")})
        .map({line in "\"" + line + "\""})
    
    return
        escapedLines.map({line in line.characters.count}).reduce(0, combine:+) -
        lines.map({line in line.characters.count}).reduce(0, combine:+)
}

enum AdventError: ErrorType {
    case InputPathFailed
}

do {
    guard let filePath = NSBundle.mainBundle().pathForResource("input", ofType: nil) else {
        throw AdventError.InputPathFailed
    }
    
    let fileContent = try NSString(contentsOfFile: filePath, encoding: NSUTF8StringEncoding)
    
    let lines = fileContent.componentsSeparatedByCharactersInSet(NSCharacterSet.newlineCharacterSet()).filter{!$0.isEmpty} as [String]
    
    try print("Part 1: \(part1(lines))")
    try print("Part 2: \(part2(lines))")
}
catch let caught as NSError {
    print("error: " + caught.description)
}
